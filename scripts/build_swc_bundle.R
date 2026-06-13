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

  # Collapse replicates -> one row per site/date/analyte, KEEPING the replicate
  # count + spread + a real below-detection flag (any rep below DL).
  swc_long <- bind_rows(lab_long, field_long) %>%
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

# write bundle with a timestamped backup of any existing file
save_bundle <- function(bundle, out = file.path("data","neon_swc.rds")) {
  if (file.exists(out))
    file.copy(out, sub("[.]rds$", format(Sys.time(), "_%Y%m%d-%H%M%S.rds"), out), overwrite = FALSE)
  saveRDS(bundle, out)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
