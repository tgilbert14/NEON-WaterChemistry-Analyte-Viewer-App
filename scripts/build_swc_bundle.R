#----------------------------------------------------------------------
# build_swc_bundle.R  —  THE single source of truth for data/neon_swc.rds
# Both entry points (scripts/precompute_neon_data.R = download+cache, and
# scripts/build_rds_from_cache.R = build-from-cache) source this file and call
# build_swc_bundle(). One transform, one contract — they cannot drift.
#----------------------------------------------------------------------
suppressWarnings(suppressMessages({ library(dplyr); library(tidyr) }))
source(file.path("scripts", "water_unit_contract.R"))

PRODUCT_CODE <- "DP1.20093.001"
PRODUCT_URL  <- "https://data.neonscience.org/data-products/DP1.20093.001"

SITE_LABELS <- c(
  SYCA="Sycamore Creek, AZ", ARIK="Arikaree River, CO", BARC="Barco Lake, FL",
  BIGC="Upper Big Creek, CA", BLDE="Blacktail Deer Creek, WY", BLUE="Blue River, OK",
  BLWA="Black Warrior River, AL", CARI="Caribou Creek, AK", COMO="Como Creek, CO",
  CRAM="Crampton Lake, WI", CUPE="Rio Cupeyes, PR", FLNT="Flint River, GA",
  GUIL="Rio Guilarte, PR", HOPB="Hop Brook, MA", KING="Kings Creek, KS",
  LECO="LeConte Creek, TN", LEWI="Lewis Run, VA", LIRO="Little Rock Lake, WI",
  MART="Martha Creek, WA", MAYF="Mayfield Creek, AL", MCDI="McDiffett Creek, KS",
  MCRA="McRae Creek, OR", OKSR="Oksrukuyik Creek, AK", POSE="Posey Creek, VA",
  PRIN="Pringle Creek, TX", PRLA="Prairie Lake, ND", PRPO="Prairie Pothole, ND",
  REDB="Red Butte Creek, UT", SUGG="Suggs Lake, FL", TECR="Teakettle Creek, CA",
  TOMB="Lower Tombigbee River, AL", TOOK="Toolik Lake, AK",
  WALK="Walker Branch, TN", WLOU="West St Louis Creek, CO")

# NEON ships below-detection as the strings "ND"/"BDL" (sometimes "1") — NOT 0/1.
.below_codes <- c("1", "ND", "BDL", "BD", "TRUE", "true")

# API/cache enumeration order is not scientific information. Canonically order
# the effective rows after unit-policy filtering so floating-point mean/sd
# reductions and every derived byte reproduce from the signed, sorted replay.
canonical_water_raw_order <- function(x) {
  order_columns <- c(
    "site", "collectDate", "analyte", "value", "units", "below",
    "labFlag", "laboratoryName", "source"
  )
  stopifnot(is.data.frame(x), all(order_columns %in% names(x)))
  order_keys <- lapply(x[order_columns], function(value) {
    if (is.character(value)) enc2utf8(value) else value
  })
  row_order <- do.call(order, c(
    order_keys, list(na.last = TRUE, method = "radix")
  ))
  x[row_order, , drop = FALSE]
}

# Replicate groups can legitimately carry more than one external-lab quality
# flag. Preserve every distinct nonempty code in a deterministic representation
# instead of selecting whichever flagged row happened to arrive first.
collapse_water_lab_flags <- function(x) {
  flags <- enc2utf8(as.character(x))
  flags <- sort(unique(flags[!is.na(flags) & nzchar(flags)]), method = "radix")
  if (!length(flags)) NA_character_ else paste(flags, collapse = " | ")
}

# lab_raw  : stacked external-lab rows; cols = site, collectDate, analyte,
#            analyteConcentration, analyteUnits, belowDetectionQF, externalLabDataQF
# field_raw: stacked field-probe rows; cols = site, collectDate, waterTemp,
#            dissolvedOxygen, specificConductance
# coords   : per-site tibble; cols = site, neonName, domain, state, lat, long, siteType
# partial  : TRUE if built from an incomplete pull
build_swc_bundle <- function(lab_raw, field_raw, coords, partial = FALSE) {

  if (!"laboratoryName" %in% names(lab_raw)) {
    lab_raw$laboratoryName <- NA_character_
  }

  lab_long <- lab_raw %>%
    transmute(site, collectDate = as.Date(substr(collectDate, 1, 10)),
              analyte, value = suppressWarnings(as.numeric(analyteConcentration)),
              units = analyteUnits,
              below = as.character(belowDetectionQF) %in% .below_codes,
              labFlag = as.character(externalLabDataQF),
              laboratoryName = as.character(laboratoryName),
              source = "External Lab") %>%
    filter(!is.na(value), !is.na(collectDate))

  field_long <- if (!is.null(field_raw) && nrow(field_raw)) {
    field_raw %>%
      mutate(collectDate = as.Date(substr(collectDate, 1, 10))) %>%
      pivot_longer(any_of(c("waterTemp","dissolvedOxygen","specificConductance")),
                   names_to = "a0", values_to = "v") %>%
      filter(!is.na(v), !is.na(collectDate)) %>%
      transmute(site, collectDate,
                analyte = recode(a0, waterTemp = "waterTemp",
                                 dissolvedOxygen = "dissolvedOxygenField",
                                 specificConductance = "specificConductanceField"),
                value = suppressWarnings(as.numeric(v)),
                units = recode(a0, waterTemp = "celsius",
                               dissolvedOxygen = "milligramsPerLiter",
                               specificConductance = "microsiemensPerCentimeter"),
                below = FALSE, labFlag = NA_character_, source = "Field Probe")
  } else tibble()

  raw_long <- bind_rows(lab_long, field_long)

  # ---- FAIR units (review finding #4): explicit label-repair contract --------
  # Missing metadata labels may be filled only under the exact established
  # target map. Non-missing mismatches are never relabelled: only audited legacy
  # identities may be quarantined. In particular, residual TPC/TPN `milligram`
  # labels conflict with NEON's current conversion notice; they remain unresolved
  # legacy anomalies and are excluded with a receipt rather than guessed at.
  values_before_unit_labels <- raw_long$value
  unit_result <- canonicalize_water_unit_labels(
    raw_long$site, raw_long$collectDate, as.character(raw_long$analyte),
    raw_long$value, raw_long$units, raw_long$laboratoryName
  )
  raw_long <- raw_long[unit_result$keep, , drop = FALSE]
  raw_long$units <- unit_result$units
  stopifnot(identical(raw_long$value,
                      values_before_unit_labels[unit_result$keep]))
  raw_long <- canonical_water_raw_order(raw_long)

  # Collapse replicates -> one row per site/date/analyte, KEEPING the replicate
  # count + spread + a real below-detection flag (any rep below DL). Units are now
  # canonical per analyte, and every distinct lab flag is retained.
  swc_long <- raw_long %>%
    group_by(site, collectDate, analyte) %>%
    summarise(value_sd = stats::sd(value, na.rm = TRUE),   # spread BEFORE collapse
              n_reps = dplyr::n(),
              belowDetection = as.integer(any(below, na.rm = TRUE)),
              units = dplyr::first(units),
              source = dplyr::first(source),
              labFlag = collapse_water_lab_flags(labFlag),
              value = mean(value, na.rm = TRUE),           # collapse LAST
              .groups = "drop") %>%
    relocate(value, .after = analyte) %>%
    arrange(site, analyte, collectDate)

  # Fail loud if the grain is wrong (a silent pivot-mean would hide it)
  stopifnot(!anyDuplicated(swc_long[c("site", "collectDate", "analyte")]))

  swc_wide <- swc_long %>% select(site, collectDate, analyte, value) %>%
    pivot_wider(names_from = analyte, values_from = value)

  analyte_meta <- swc_long %>% group_by(analyte) %>%
    summarise(units = dplyr::first(units), n = dplyr::n(), n_sites = dplyr::n_distinct(site),
              n_below = sum(belowDetection), source = dplyr::first(source), .groups = "drop") %>%
    arrange(desc(n))

  site_cov <- swc_long %>% group_by(site) %>%
    summarise(n_obs = dplyr::n(), n_analytes = dplyr::n_distinct(analyte),
              first = min(collectDate), last = max(collectDate),
              n_dates = dplyr::n_distinct(collectDate), .groups = "drop")

  present <- sort(unique(swc_long$site))
  sites_meta <- tibble(site = present, siteName = unname(SITE_LABELS[present])) %>%
    left_join(coords, by = "site") %>% left_join(site_cov, by = "site")
  # stable schema even if a coord fetch failed
  for (cc in c("neonName","domain","state","siteType")) if (!cc %in% names(sites_meta)) sites_meta[[cc]] <- NA_character_
  for (cc in c("lat","long")) if (!cc %in% names(sites_meta)) sites_meta[[cc]] <- NA_real_

  built <- list(
    when = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    product = PRODUCT_CODE, partial = isTRUE(partial),
    n_obs = nrow(swc_long), n_sites = dplyr::n_distinct(swc_long$site),
    n_analytes = dplyr::n_distinct(swc_long$analyte),
    data_through = as.character(max(swc_long$collectDate)),
    n_below = sum(swc_long$belowDetection),
    unit_policy = WATER_UNIT_POLICY,
    n_unit_labels_rewritten = as.integer(sum(
      unit_result$label_receipt$n_rewritten
    )),
    n_unit_rows_excluded = as.integer(sum(
      unit_result$exclusion_receipt$n_excluded
    )),
    n_unit_values_changed = 0L,
    unit_label_rewrites = unit_result$label_receipt,
    unit_rewrite_receipt_sha256 = unit_result$label_receipt_sha256,
    unit_row_exclusions = unit_result$exclusion_receipt,
    unit_exclusion_receipt_sha256 = unit_result$exclusion_receipt_sha256)

  bundle <- list(swc_long = swc_long, swc_wide = swc_wide, sites_meta = sites_meta,
                 analyte_meta = analyte_meta, built = built)
  validate_bundle(bundle)
  bundle
}

# Contract assertion — stops the build if the schema the app depends on drifts.
validate_bundle <- function(b) {
  need_long <- c("site","collectDate","analyte","value","value_sd","n_reps",
                 "units","source","belowDetection","labFlag")
  units_by_analyte <- split(as.character(b$swc_long$units),
                            as.character(b$swc_long$analyte))
  canonical_units <- vapply(
    units_by_analyte,
    function(x) !anyNA(x) && all(nzchar(x)) && length(unique(x)) == 1L,
    logical(1)
  )
  stopifnot(all(need_long %in% names(b$swc_long)),
            inherits(b$swc_long$collectDate, "Date"),
            length(canonical_units) > 0L,
            all(canonical_units),
            all(c("site","siteName","domain","state","lat","long",
                  "n_obs","n_analytes","first","last") %in% names(b$sites_meta)),
            all(c("when","product","partial","n_obs","n_sites","n_analytes",
                  WATER_UNIT_RECEIPT_FIELDS) %in% names(b$built)),
            identical(b$built$unit_policy, WATER_UNIT_POLICY),
            is.integer(b$built$n_unit_labels_rewritten),
            length(b$built$n_unit_labels_rewritten) == 1L,
            !is.na(b$built$n_unit_labels_rewritten),
            b$built$n_unit_labels_rewritten >= 0L,
            is.integer(b$built$n_unit_rows_excluded),
            length(b$built$n_unit_rows_excluded) == 1L,
            !is.na(b$built$n_unit_rows_excluded),
            b$built$n_unit_rows_excluded >= 0L,
            identical(b$built$n_unit_values_changed, 0L),
            is.data.frame(b$built$unit_label_rewrites),
            is.data.frame(b$built$unit_row_exclusions),
            identical(
              b$built$unit_rewrite_receipt_sha256,
              water_unit_receipt_sha256(b$built$unit_label_rewrites)
            ),
            identical(
              b$built$unit_exclusion_receipt_sha256,
              water_unit_receipt_sha256(b$built$unit_row_exclusions)
            ),
            identical(
              b$built$n_unit_labels_rewritten,
              as.integer(sum(b$built$unit_label_rewrites$n_rewritten))
            ),
            identical(
              b$built$n_unit_rows_excluded,
              as.integer(sum(b$built$unit_row_exclusions$n_excluded))
            ))
  invisible(TRUE)
}

# ---- Versioned column-level codebook (review finding #5) --------------------
# Emits codebook.csv next to neon_swc.rds. The dictionary is built by iterating
# the ACTUAL columns the app's tidy long export emits (the keep-vector) so it can
# never drift from what ships; every column carries type + units-or-NA + allowed +
# definition + NA-semantics. CODEBOOK_VERSION is stamped into the header.
CODEBOOK_VERSION <- "1.1.0"
LEGACY_CODEBOOK_VERSION <- "1.0.0"
# The keep-vector = the exact columns output$dl_long / long_slice() transmutes.
# Keep this list in lock-step with app.R long_slice().
LONG_EXPORT_KEEP <- c("site","collectDate","analyte","analyte_label","value","units",
                      "n_reps","value_sd","below_detection","implausible_extreme",
                      "lab_flag","source","product")
.CODEBOOK_DEFS <- list(
  site = list(type="character", units=NA, allowed="NEON 4-letter site code",
              def="NEON aquatic site where the sample was collected",
              na="never NA"),
  collectDate = list(type="Date", units="ISO date", allowed="YYYY-MM-DD",
              def="Field collection date (sub-day time dropped)", na="never NA"),
  analyte = list(type="character", units=NA, allowed="raw analyte code",
              def="NEON analyte identifier (join key to the analyte dictionary)", na="never NA"),
  analyte_label = list(type="character", units=NA, allowed="free text",
              def="Human-readable analyte name", na="never NA"),
  value = list(type="numeric", units="see units column (canonical per analyte)", allowed=">= 0 typical",
              def="Replicate-mean concentration / measurement for the site-date-analyte",
              na="NA only if all replicates were non-numeric"),
  units = list(type="character", units=NA, allowed="canonical NEON unit string",
              def="Pinned established unit for the analyte; UV absorbance = 'absorbance units'",
              na="never NA after canonicalization"),
  n_reps = list(type="integer", units="count", allowed=">= 1",
              def="Number of lab/field replicates collapsed into value", na="never NA"),
  value_sd = list(type="numeric", units="same as value", allowed=">= 0",
              def="Standard deviation across replicates before collapse",
              na="NA when n_reps == 1"),
  below_detection = list(type="logical", units=NA, allowed="TRUE/FALSE",
              def="Any replicate reported below the analytical detection limit (value kept, never substituted)",
              na="never NA"),
  implausible_extreme = list(type="logical", units=NA, allowed="TRUE/FALSE",
              def="Flagged above the plausibility ceiling; kept in this raw export, excluded from fits/maps/STL/glm",
              na="never NA"),
  lab_flag = list(type="character", units=NA,
              allowed="sorted distinct NEON externalLabDataQF codes joined by ' | '",
              def="All distinct external-lab quality flags across the collapsed replicates",
              na="NA when every replicate is unflagged"),
  source = list(type="character", units=NA, allowed="External Lab | Field Probe",
              def="Measurement origin", na="never NA"),
  product = list(type="character", units=NA, allowed="DP1.20093.001",
              def="NEON data product code", na="never NA")
)
write_codebook <- function(bundle, out = file.path("data","codebook.csv")) {
  receipt_present <- WATER_UNIT_RECEIPT_FIELDS %in% names(bundle$built)
  if (any(receipt_present) && !all(receipt_present)) {
    stop("Cannot write a codebook for a partial unit-contract receipt.",
         call. = FALSE)
  }
  current_producer <- all(receipt_present)
  if (current_producer &&
      !identical(bundle$built$unit_policy, WATER_UNIT_POLICY)) {
    stop("Cannot write a current codebook for an unexpected unit policy.",
         call. = FALSE)
  }
  codebook_version <- if (current_producer) {
    CODEBOOK_VERSION
  } else {
    LEGACY_CODEBOOK_VERSION
  }
  keep <- LONG_EXPORT_KEEP
  rows <- lapply(keep, function(col) {
    d <- .CODEBOOK_DEFS[[col]]
    if (identical(col, "lab_flag") && !current_producer) {
      d <- list(
        type = "character", units = NA,
        allowed = "NEON externalLabDataQF codes",
        def = "External-lab quality flag (e.g. legacyData, formatChange)",
        na = "NA when unflagged"
      )
    }
    if (is.null(d)) d <- list(type="", units=NA, allowed="", def="(undocumented)", na="")
    tibble::tibble(name = col, type = d$type,
                   units = ifelse(is.na(d$units), "NA", d$units),
                   allowed = d$allowed, definition = d$def, na_semantics = d$na)
  })
  long_cb <- dplyr::bind_rows(rows)
  long_cb$section <- "tidy_long_export"

  # analyte dictionary section: one row per analyte actually emitted, with its
  # canonical unit + coverage + below-detection fraction
  am <- bundle$analyte_meta
  am$pct_below <- ifelse(is.finite(am$n) & am$n > 0, round(am$n_below / am$n, 4), NA_real_)
  dict_cb <- tibble::tibble(
    name = am$analyte, type = "numeric",
    units = ifelse(is.na(am$units) | !nzchar(am$units), "NA", am$units),
    allowed = ">= 0 typical",
    definition = sprintf("Analyte '%s': %d obs across %d sites; canonical unit shown",
                         am$analyte, am$n, am$n_sites),
    na_semantics = sprintf("%s below detection (kept, not substituted)",
                           ifelse(is.na(am$pct_below), "0%", paste0(round(100*am$pct_below), "%"))),
    section = "analyte_dictionary")

  cb <- dplyr::bind_rows(long_cb, dict_cb)
  hdr <- c(
    sprintf("# NEON Surface Water Chemistry codebook | version %s | product %s | built %s",
            codebook_version, bundle$built$product,
            substr(bundle$built$when, 1, 10)),
    "# section=tidy_long_export documents the in-app Tidy CSV columns (the keep-vector); section=analyte_dictionary documents every emitted analyte",
    "# units 'NA' = not applicable for schema rows; emitted analyte rows carry explicit reviewed targets")
  writeLines(hdr, out)
  suppressWarnings(suppressMessages(
    utils::write.table(cb, out, append = TRUE, sep = ",", row.names = FALSE,
                       col.names = TRUE, qmethod = "double")))
  invisible(cb)
}

# Write a pending bundle first, then preserve and verify the exact current bytes
# before promotion. A colliding or failed backup stops before the live file is
# touched; a failed promotion is restored from the verified backup.
save_bundle <- function(bundle, out = file.path("data", "neon_swc.rds"),
                        backup_time = Sys.time(), copy_file = file.copy) {
  if (!is.function(copy_file)) {
    stop("copy_file must be a function.", call. = FALSE)
  }
  out_dir <- dirname(out)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  pending <- tempfile(paste0(basename(out), ".pending-"), tmpdir = out_dir)
  on.exit(if (file.exists(pending)) unlink(pending), add = TRUE)
  saveRDS(bundle, pending)
  pending_md5 <- unname(tools::md5sum(pending))
  if (is.na(pending_md5)) {
    stop("Could not hash the pending bundle.", call. = FALSE)
  }

  backup <- NULL
  if (file.exists(out)) {
    backup <- sub(
      "[.]rds$", format(backup_time, "_%Y%m%d-%H%M%S.rds"), out
    )
    if (identical(backup, out) || file.exists(backup)) {
      stop(sprintf("Refusing colliding bundle backup: %s", backup),
           call. = FALSE)
    }
    current_md5 <- unname(tools::md5sum(out))
    if (is.na(current_md5)) {
      stop("Could not hash the current bundle before backup.", call. = FALSE)
    }
    copied <- copy_file(out, backup, overwrite = FALSE)
    if (!isTRUE(copied) || !file.exists(backup) ||
        !identical(unname(tools::md5sum(backup)), current_md5)) {
      if (file.exists(backup)) unlink(backup)
      stop(sprintf("Could not create an exact bundle backup: %s", backup),
           call. = FALSE)
    }
  }

  promoted <- copy_file(pending, out, overwrite = TRUE)
  promotion_ok <- isTRUE(promoted) && file.exists(out) &&
    identical(unname(tools::md5sum(out)), pending_md5)
  if (!promotion_ok) {
    restored <- !is.null(backup) && file.exists(backup) &&
      isTRUE(copy_file(backup, out, overwrite = TRUE)) &&
      identical(unname(tools::md5sum(out)),
                unname(tools::md5sum(backup)))
    stop(sprintf(
      "Bundle promotion failed; prior bytes restored: %s",
      if (restored) "yes" else "no"
    ), call. = FALSE)
  }
  invisible(out)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
