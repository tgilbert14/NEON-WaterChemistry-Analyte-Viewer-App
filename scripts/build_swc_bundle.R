#----------------------------------------------------------------------
# build_swc_bundle.R  —  THE single source of truth for data/neon_swc.rds
# Both entry points (scripts/precompute_neon_data.R = download+cache, and
# scripts/build_rds_from_cache.R = build-from-cache) source this file and call
# build_swc_bundle(). One transform, one contract — they cannot drift.
#----------------------------------------------------------------------
suppressWarnings(suppressMessages({ library(dplyr); library(tidyr) }))

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

# Modal (most-frequent) non-NA value of a character vector. Used to pick ONE
# canonical unit per analyte instead of dplyr::first(), which grabbed whatever
# row sorted first and left 20/34 analytes carrying a stray non-modal unit.
.mode_chr <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# UV absorbance has no real NEON unit string (it is an absorbance ratio); stamp a
# canonical label so the dictionary stops exporting NA.
.UV_ABS_CODES <- c("UV Absorbance (254 nm)", "UV Absorbance (250 nm)", "UV Absorbance (280 nm)")

# lab_raw  : stacked external-lab rows; cols = site, collectDate, analyte,
#            analyteConcentration, analyteUnits, belowDetectionQF, externalLabDataQF
# field_raw: stacked field-probe rows; cols = site, collectDate, waterTemp,
#            dissolvedOxygen, specificConductance
# coords   : per-site tibble; cols = site, neonName, domain, state, lat, long, siteType
# partial  : TRUE if built from an incomplete pull
build_swc_bundle <- function(lab_raw, field_raw, coords, partial = FALSE) {

  lab_long <- lab_raw %>%
    transmute(site, collectDate = as.Date(substr(collectDate, 1, 10)),
              analyte, value = suppressWarnings(as.numeric(analyteConcentration)),
              units = analyteUnits,
              below = as.character(belowDetectionQF) %in% .below_codes,
              labFlag = as.character(externalLabDataQF),
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

  # ---- FAIR units (review finding #4): one canonical unit per analyte ---------
  # Pick the MODAL unit per analyte (not dplyr::first(), which left 20/34 analytes
  # mislabeled). Then coerce every analyte to that one canonical unit:
  #   * GENUINE µg/L rows whose analyte is canonically mg/L are divided by 1000
  #     (true unit mismatch — converted BY VALUE, not just relabeled);
  #   * UV absorbance analytes are stamped "absorbance units" (no NEON unit string).
  # This makes the bundle self-consistent so the runtime no longer has to patch it.
  canon_tbl <- raw_long %>%
    dplyr::group_by(analyte) %>%
    dplyr::summarise(canon_unit = .mode_chr(units), .groups = "drop") %>%
    dplyr::mutate(canon_unit = ifelse(analyte %in% .UV_ABS_CODES, "absorbance units", canon_unit))
  canon_map <- setNames(canon_tbl$canon_unit, canon_tbl$analyte)

  raw_long <- raw_long %>%
    dplyr::mutate(
      canon_unit = unname(canon_map[analyte]),
      # genuine µg/L -> mg/L conversion BY VALUE (1000 µg = 1 mg) only where the
      # analyte's canonical unit is mg/L and THIS row was reported in µg/L
      value = dplyr::if_else(
        !is.na(canon_unit) & canon_unit == "milligramsPerLiter" &
          units == "microgramsPerLiter" & is.finite(value),
        value / 1000, value),
      units = dplyr::coalesce(canon_unit, units)
    ) %>%
    dplyr::select(-canon_unit)

  # Collapse replicates -> one row per site/date/analyte, KEEPING the replicate
  # count + spread + a real below-detection flag (any rep below DL). Units are now
  # canonical per analyte, so first() is safe here.
  swc_long <- raw_long %>%
    group_by(site, collectDate, analyte) %>%
    summarise(value_sd = stats::sd(value, na.rm = TRUE),   # spread BEFORE collapse
              n_reps = dplyr::n(),
              belowDetection = as.integer(any(below, na.rm = TRUE)),
              units = dplyr::first(units),
              source = dplyr::first(source),
              labFlag = dplyr::first(stats::na.omit(labFlag)) %||% NA_character_,
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
    n_below = sum(swc_long$belowDetection))

  bundle <- list(swc_long = swc_long, swc_wide = swc_wide, sites_meta = sites_meta,
                 analyte_meta = analyte_meta, built = built)
  validate_bundle(bundle)
  bundle
}

# Contract assertion — stops the build if the schema the app depends on drifts.
validate_bundle <- function(b) {
  need_long <- c("site","collectDate","analyte","value","value_sd","n_reps",
                 "units","source","belowDetection","labFlag")
  stopifnot(all(need_long %in% names(b$swc_long)),
            inherits(b$swc_long$collectDate, "Date"),
            all(c("site","siteName","domain","state","lat","long",
                  "n_obs","n_analytes","first","last") %in% names(b$sites_meta)),
            all(c("when","product","partial","n_obs","n_sites","n_analytes") %in% names(b$built)))
  invisible(TRUE)
}

# ---- Versioned column-level codebook (review finding #5) --------------------
# Emits codebook.csv next to neon_swc.rds. The dictionary is built by iterating
# the ACTUAL columns the app's tidy long export emits (the keep-vector) so it can
# never drift from what ships; every column carries type + units-or-NA + allowed +
# definition + NA-semantics. CODEBOOK_VERSION is stamped into the header.
CODEBOOK_VERSION <- "1.0.0"
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
              def="Canonical (modal) unit for the analyte; UV absorbance = 'absorbance units'",
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
  lab_flag = list(type="character", units=NA, allowed="NEON externalLabDataQF codes",
              def="External-lab quality flag (e.g. legacyData, formatChange)", na="NA when unflagged"),
  source = list(type="character", units=NA, allowed="External Lab | Field Probe",
              def="Measurement origin", na="never NA"),
  product = list(type="character", units=NA, allowed="DP1.20093.001",
              def="NEON data product code", na="never NA")
)
write_codebook <- function(bundle, out = file.path("data","codebook.csv")) {
  keep <- LONG_EXPORT_KEEP
  rows <- lapply(keep, function(col) {
    d <- .CODEBOOK_DEFS[[col]]
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
            CODEBOOK_VERSION, bundle$built$product, substr(bundle$built$when, 1, 10)),
    "# section=tidy_long_export documents the in-app Tidy CSV columns (the keep-vector); section=analyte_dictionary documents every emitted analyte",
    "# units 'NA' = dimensionless or no canonical unit string (e.g. pH, UV absorbance ratio)")
  writeLines(hdr, out)
  suppressWarnings(suppressMessages(
    utils::write.table(cb, out, append = TRUE, sep = ",", row.names = FALSE,
                       col.names = TRUE, qmethod = "double")))
  invisible(cb)
}

# write bundle with a timestamped backup of any existing file
save_bundle <- function(bundle, out = file.path("data","neon_swc.rds")) {
  if (file.exists(out))
    file.copy(out, sub("[.]rds$", format(Sys.time(), "_%Y%m%d-%H%M%S.rds"), out), overwrite = FALSE)
  saveRDS(bundle, out)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
