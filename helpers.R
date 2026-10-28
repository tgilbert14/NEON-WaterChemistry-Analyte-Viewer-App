#----------------------------------------------------------------------
# helpers.R  —  pure helpers for the NEON Analyte Viewer (sourced by app.R)
# Display names + units (Aquatics spec), CVD-safe colors, plotly/ggplot theme
# helpers, and the three real computations: correlation_table(), fit_lm(),
# predict_glm(). No Shiny reactivity here — everything takes plain data frames.
#----------------------------------------------------------------------
suppressWarnings(suppressMessages({
  # app.R loads the visualization stack before sourcing this file. Keeping this
  # shared layer's eager imports to its data-frame dependencies lets the
  # deterministic search/codebook builder reuse the scientific gates without
  # loading plotly/ggplot/lubridate merely to define functions.
  library(dplyr); library(tidyr)
}))
source(file.path("scripts", "water_unit_contract.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
# Vectorized NA-coalesce: element-wise "a unless NA, else b" (b recycled). Used
# to apply a canonical-unit lookup while falling back to the original string for
# any unmapped element.
`%|na|%` <- function(a, b) { a <- as.character(a); b <- as.character(b); ifelse(is.na(a), b, a) }

## ---- Colors (Okabe-Ito CVD-safe; brand teal primary) ---------------------
# Desert-night creative system. The brand teal stays the primary line color, but
# the dark-mode chart surfaces (bg_d / fg_d / grid_d) now read the desert-night
# navy so charts re-theme with the page. The data palettes (SEASON_PAL, the map
# YlGnBu colorscale, the lolli reliable/grey) are CVD-safe and stay as-is.
COL <- list(
  main      = "#0E7C9B",   # main analyte (brand teal — primary line)
  secondary = "#D55E00",   # secondary analyte (Okabe-Ito vermillion)
  good      = "#2E8B6F",   # strong relationship
  warn      = "#C98A1E",   # caveat amber
  grid_l    = "rgba(0,0,0,.07)", grid_d = "rgba(255,255,255,.12)",
  bg_l = "#ffffff", bg_d = "#0c3350", fg_l = "#13242c", fg_d = "#e6f4fb"
)

## ---- PDF report palette (PRINTS ON WHITE PAPER — stays LIGHT) -------------
# The one-click site report (app.R output$dl_report) renders to a base pdf()
# device on white paper. It is DECOUPLED from the desert-night chart palette:
# reading the dark navy chart hexes here would print near-invisible on white.
# These are the LIGHT house hexes, kept legible on paper.
PG <- list(
  main      = "#0E7C9B",   # teal — same family, dark enough to read on white
  secondary = "#C44536",   # cardinal/terracotta accent for the second series
  ink       = "#13242c"
)
SEASON_PAL <- c(Winter = "#56B4E9", Spring = "#009E73", Summer = "#E69F00", Fall = "#CC79A7")

## ---- Analyte display metadata (corrected per Aquatics) --------------------
# code = the value in swc_long$analyte ; display = pretty name ; group for picker
ANALYTE_TBL <- tibble::tribble(
  ~code,                       ~display,                              ~group,         ~indicator,
  "specificConductanceField",  "Specific Conductance (field)",        "Physical",     "Total dissolved ionic strength",
  "specificConductance",       "Specific Conductance (lab)",          "Physical",     "Lab-measured ionic strength",
  "waterTemp",                 "Water Temperature (field)",           "Physical",     "Controls DO saturation & metabolism",
  "dissolvedOxygenField",      "Dissolved Oxygen (field)",            "Physical",     "Aquatic-life habitat indicator",
  "pH",                        "pH",                                  "Physical",     "Acid-base status",
  "ANC",                       "Acid Neutralizing Capacity (ANC)",    "Buffering",    "Buffering against acidification",
  "HCO3",                      "Bicarbonate (HCO₃⁻)",        "Buffering",    "Dominant alkalinity species",
  "CO3",                       "Carbonate (CO₃²⁻)",     "Buffering",    "Carbonate alkalinity (high pH)",
  "DIC",                       "Dissolved Inorganic Carbon (DIC)",    "Buffering",    "Carbonate system / productivity",
  "Ca",                        "Calcium (Ca²⁺)",             "Major ions",   "Hardness, carbonate geology",
  "Mg",                        "Magnesium (Mg²⁺)",           "Major ions",   "Hardness, dolomitic geology",
  "Na",                        "Sodium (Na⁺)",                    "Major ions",   "Salinity / road salt / weathering",
  "K",                         "Potassium (K⁺)",                  "Major ions",   "Weathering / agricultural signal",
  "Cl",                        "Chloride (Cl⁻)",                  "Major ions",   "Road salt / wastewater tracer",
  "SO4",                       "Sulfate (SO₄²⁻)",       "Major ions",   "Acid deposition / mineral / redox",
  "Br",                        "Bromide (Br⁻)",                   "Major ions",   "Evaporation / brine tracer",
  "F",                         "Fluoride (F⁻)",                   "Major ions",   "Geologic / anthropogenic anion",
  "Si",                        "Silica (Si)",                         "Major ions",   "Weathering; diatom growth",
  "NO3+NO2 - N",               "Nitrate + Nitrite (as N)",            "Nutrients",    "Oxidized dissolved inorganic N",
  "NO2 - N",                   "Nitrite (NO₂⁻ as N)",        "Nutrients",    "Nitrification intermediate",
  "NH4 - N",                   "Ammonium (NH₄⁺ as N)",       "Nutrients",    "Reduced inorganic N; sewage signal",
  "TN",                        "Total Nitrogen (TN)",                 "Nutrients",    "Nutrient-criterion N variable",
  "TDN",                       "Total Dissolved Nitrogen (TDN)",      "Nutrients",    "Dissolved organic + inorganic N",
  "Ortho - P",                 "Orthophosphate (PO₄ as P)",       "Nutrients",    "Bioavailable dissolved P (SRP)",
  "TP",                        "Total Phosphorus (TP)",               "Nutrients",    "Eutrophication / criterion variable",
  "TDP",                       "Total Dissolved Phosphorus (TDP)",    "Nutrients",    "Dissolved organic + inorganic P",
  "DOC",                       "Dissolved Organic Carbon (DOC)",      "Carbon/DOM",   "Organic-matter loading",
  "TOC",                       "Total Organic Carbon (TOC)",          "Carbon/DOM",   "Dissolved + particulate organic C",
  "UV Absorbance (254 nm)",    "UV Absorbance 254 nm",                "Carbon/DOM",   "Aromatic/colored DOM proxy (SUVA)",
  "UV Absorbance (250 nm)",    "UV Absorbance 250 nm",                "Carbon/DOM",   "Colored DOM proxy",
  "UV Absorbance (280 nm)",    "UV Absorbance 280 nm",                "Carbon/DOM",   "Aromatic DOM proxy",
  "Fe",                        "Iron (Fe)",                           "Metals",       "Redox-sensitive; low-DO signal",
  "Mn",                        "Manganese (Mn)",                      "Metals",       "Redox-sensitive; co-varies with Fe",
  "TDS",                       "Total Dissolved Solids (TDS)",        "Solids",       "Bulk dissolved-ion load",
  "TSS",                       "Total Suspended Solids (TSS)",        "Solids",       "Suspended sediment / turbidity proxy",
  "TSS - Dry Mass",            "Suspended Sediment Dry Mass",         "Solids",       "Gravimetric suspended sediment",
  "TPC",                       "Total Particulate Carbon (TPC)",      "Solids",       "Seston / suspended C",
  "TPN",                       "Total Particulate Nitrogen (TPN)",    "Solids",       "Particulate N (seston, algae)"
)

# NEON verbose unit string -> pretty symbol
UNIT_PRETTY <- c(
  milligramsPerLiter = "mg/L", microgramsPerLiter = "µg/L", microgram = "µg",
  microsiemensPerCentimeter = "µS/cm", milliequivalentsPerLiter = "meq/L",
  celsius = "°C", formazinNephelometricUnit = "FNU", quinineSulfateUnit = "QSU"
)

## ---- Canonical unit per analyte (review finding #3, units) ----------------
# The runtime first passes through canonicalize_runtime_water_units(): exact
# registered mismatch identities are quarantined and registered missing labels
# are filled only with an observed target label. Once that boundary succeeds,
# every retained row must already carry its explicit established target. The
# map below is therefore a validator/presentation lookup, never a modal guess.
canonical_units <- function(swc_long) {
  present <- unique(as.character(swc_long$analyte))
  unknown <- setdiff(present, names(WATER_ESTABLISHED_UNIT_TARGETS))
  if (length(unknown)) {
    stop(sprintf("Unregistered analyte(s) reached the runtime: %s",
                 paste(sort(unknown), collapse = ", ")), call. = FALSE)
  }
  expected <- unname(WATER_ESTABLISHED_UNIT_TARGETS[as.character(swc_long$analyte)])
  if (anyNA(swc_long$units) || any(!nzchar(swc_long$units)) ||
      !identical(as.character(swc_long$units), expected)) {
    stop("Runtime units differ from the explicit established targets.",
         call. = FALSE)
  }
  WATER_ESTABLISHED_UNIT_TARGETS[present]
}
# Resolve the canonical unit for one code against a precomputed map, falling back
# to the raw string when the analyte isn't mapped (defensive).
canon_unit_of <- function(code, map, fallback = NA_character_) {
  u <- unname(map[code]); ifelse(is.na(u), fallback, u)
}

# Reconcile the legacy or current bundle once at load. The same function is used
# by app.R and build_search_index.R, so the UI, exports, metadata, wide matrix,
# and search catalogue all exclude the identical audited mismatch rows.
apply_runtime_water_unit_contract <- function(bundle) {
  if (!is.list(bundle) || !is.data.frame(bundle$swc_long) ||
      !is.data.frame(bundle$sites_meta) || !is.list(bundle$built)) {
    stop("Runtime water bundle schema is invalid.", call. = FALSE)
  }
  result <- canonicalize_runtime_water_units(bundle$swc_long)
  bundle$swc_long <- result$data
  # Columns the v5 producer adds (2026-10 finish round): partialBelow (some but
  # not all replicates below detection) and release (the NEON release tag of the
  # row's site-month, or "PROVISIONAL"). A bundle from an earlier producer lacks
  # them; NA means "not recorded", never "no".
  if (!"partialBelow" %in% names(bundle$swc_long)) {
    bundle$swc_long$partialBelow <- rep(NA_integer_, nrow(bundle$swc_long))
  }
  if (!"release" %in% names(bundle$swc_long)) {
    bundle$swc_long$release <- rep(NA_character_, nrow(bundle$swc_long))
  }

  bundle$swc_wide <- bundle$swc_long |>
    dplyr::select(site, collectDate, analyte, value) |>
    tidyr::pivot_wider(names_from = analyte, values_from = value)
  bundle$analyte_meta <- bundle$swc_long |>
    dplyr::group_by(analyte) |>
    dplyr::summarise(
      units = dplyr::first(units), n = dplyr::n(),
      n_sites = dplyr::n_distinct(site),
      n_below = sum(belowDetection), source = dplyr::first(source),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n))

  site_coverage <- bundle$swc_long |>
    dplyr::group_by(site) |>
    dplyr::summarise(
      n_obs = dplyr::n(), n_analytes = dplyr::n_distinct(analyte),
      first = min(collectDate), last = max(collectDate),
      n_dates = dplyr::n_distinct(collectDate), .groups = "drop"
    )
  static_site_meta <- bundle$sites_meta |>
    dplyr::select(-dplyr::any_of(c(
      "n_obs", "n_analytes", "first", "last", "n_dates"
    )))
  bundle$sites_meta <- static_site_meta |>
    dplyr::inner_join(site_coverage, by = "site")

  bundle$built$n_obs <- as.integer(nrow(bundle$swc_long))
  bundle$built$n_sites <- as.integer(dplyr::n_distinct(bundle$swc_long$site))
  bundle$built$n_analytes <- as.integer(dplyr::n_distinct(bundle$swc_long$analyte))
  bundle$built$n_below <- as.integer(sum(bundle$swc_long$belowDetection))
  bundle$built$runtime_unit_policy <- WATER_UNIT_POLICY
  bundle$built$n_runtime_unit_rows_excluded <-
    result$n_collapsed_rows_excluded
  bundle$built$n_runtime_unit_source_rows_excluded <-
    result$n_source_rows_excluded
  bundle$built$n_runtime_unit_labels_rewritten <-
    result$n_missing_labels_rewritten
  # identity-keyed value quarantine (USGS_NWQA Fe/Mn, NEON changeLog
  # 2026-04-21); a subset of the two exclusion counts above, surfaced so the
  # app and the index can report it on its own
  bundle$built$n_runtime_value_quarantine_rows_excluded <-
    result$n_value_quarantine_rows_excluded
  bundle$built$n_runtime_value_quarantine_source_rows_excluded <-
    result$n_value_quarantine_source_rows_excluded
  attr(bundle, "runtime_unit_exclusions") <- result$excluded
  bundle
}

## ---- Per-analyte plausibility gate (review finding #2, outlier QC) ---------
# A single impossible singleton (ANC = 927 meq/L at CARI, site median 0.73; Fe =
# 931 mg/L, median 0.011) must never drive a fit, an STL trend, the glm, or the
# map colorbar. plausibility_ceilings() learns a ceiling once at app load. A value
# is flagged implausible only when it clears BOTH guards — the review's
# "> p99.9 + a unit-sanity check":
#   (a) it exceeds the reference p99.9 quantile, AND
#   (b) it exceeds a generous multiple of the reference median (unit-sanity).
# Requiring BOTH gates a lone 1000×-median artifact in an otherwise tight
# distribution (ANC 927 dwarfs both its p99.9 and 50× its median).
#
# SITE-AWARE branch (review finding #2 fix): for high-cross-site-variance
# analytes (specific conductance, TDS, the major ions, ANC) a legitimately
# extreme site — a saline prairie pothole like PRPO — sits far above the
# CONTINENT-WIDE distribution yet is entirely real. A single global ceiling
# silently trimmed those genuine grabs against this file's own intent. So for
# HIVAR_ANALYTES the reference distribution is the SITE's own history, not the
# pooled one: PRPO's saline values are judged against PRPO, so they stay IN,
# while a true within-site artifact (a 1000× spike in an otherwise tight site
# record) is still gated. Low-variance analytes keep the pooled global ceiling.
HIVAR_ANALYTES <- c(
  "specificConductanceField", "specificConductance", "TDS",
  "ANC", "HCO3", "CO3", "DIC",
  "Ca", "Mg", "Na", "K", "Cl", "SO4", "Br", "F", "Si"
)
plausibility_ceilings <- function(swc_long, q = 0.999, sanity_mult = 50,
                                  site_q = 0.995, site_mult = 30, site_min_n = 8) {
  d <- dplyr::filter(swc_long, is.finite(value))
  # global (pooled) ceiling — applies to every analyte
  glob <- d |>
    dplyr::group_by(analyte) |>
    dplyr::summarise(
      .groups = "drop",
      med  = stats::median(value, na.rm = TRUE),
      p999 = stats::quantile(value, q, na.rm = TRUE, names = FALSE),
      sanity  = ifelse(is.finite(med) & med > 0, abs(med) * sanity_mult, Inf),
      ceiling = pmax(p999, sanity, na.rm = TRUE),
      scope = "analyte"
    )
  # per-site ceiling for the high-variance analytes only, where a site has
  # enough of its own observations to characterise its native range
  site <- d |>
    dplyr::filter(analyte %in% HIVAR_ANALYTES) |>
    dplyr::group_by(analyte, site) |>
    dplyr::filter(dplyr::n() >= site_min_n) |>
    dplyr::summarise(
      .groups = "drop",
      med  = stats::median(value, na.rm = TRUE),
      p999 = stats::quantile(value, site_q, na.rm = TRUE, names = FALSE),
      sanity  = ifelse(is.finite(med) & med > 0, abs(med) * site_mult, Inf),
      ceiling = pmax(p999, sanity, na.rm = TRUE),
      scope = "analyte_site"
    )
  list(global = glob, site = site)
}
# Named lookups for is_plausible(): the pooled ceiling per analyte, plus a
# per-(analyte|site) ceiling for the high-variance analytes.
ceiling_map <- function(ceil) {
  if (is.list(ceil) && !is.data.frame(ceil)) {
    list(global = setNames(ceil$global$ceiling, ceil$global$analyte),
         site   = setNames(ceil$site$ceiling,
                           paste(ceil$site$analyte, ceil$site$site, sep = "\r")))
  } else {
    # back-compat: a plain analyte->ceiling tibble
    list(global = setNames(ceil$ceiling, ceil$analyte), site = character(0))
  }
}
# Is each value plausible? TRUE = keep. For a high-variance analyte WITH a
# site-specific ceiling, judge against that site's own range; otherwise the
# pooled analyte ceiling. NA/non-finite -> keep (they drop from finite filters).
is_plausible <- function(value, analyte, ceil_map, site = NULL) {
  g <- ceil_map$global; s <- ceil_map$site
  a <- as.character(analyte)
  ceil <- unname(g[a])
  if (length(s) && !is.null(site)) {
    key <- paste(a, as.character(site), sep = "\r")
    site_ceil <- unname(s[key])
    ceil <- ifelse(!is.na(site_ceil), site_ceil, ceil)
  }
  ok <- !is.finite(value) | is.na(ceil) | (value <= ceil)
  ok[is.na(ok)] <- TRUE
  unname(ok & in_physical_range(value, a))
}

# Physical plausibility range for the field-probe analytes (audit 2026-10-18 P2).
# A data-derived ceiling can never fire on these (a stuck probe or a sign error
# sits inside the pooled quantiles) and it has no lower bound. A listed analyte
# is kept only when lo < value <= hi (lo <= value when lo_closed). Ions and lab
# analytes are not floored: a true zero or a tiny value is chemistry there.
PLAUSIBLE_RANGE <- tibble::tribble(
  ~analyte,                   ~lo,  ~hi, ~lo_closed, ~units,
  "specificConductanceField", 0.5,  Inf, FALSE,      "µS/cm",
  "specificConductance",      0.5,  Inf, FALSE,      "µS/cm",
  "dissolvedOxygenField",     0,    25,  TRUE,       "mg/L",
  "waterTemp",                -1,   40,  FALSE,      "°C"
)
# TRUE = inside the physical range (or analyte not listed, or value not finite).
in_physical_range <- function(value, analyte) {
  i  <- match(as.character(analyte), PLAUSIBLE_RANGE$analyte)
  lo <- PLAUSIBLE_RANGE$lo[i]; hi <- PLAUSIBLE_RANGE$hi[i]
  lc <- PLAUSIBLE_RANGE$lo_closed[i]
  above_lo <- ifelse(lc %in% TRUE, value >= lo, value > lo)
  ok <- is.na(i) | !is.finite(value) | (above_lo & value <= hi)
  ok[is.na(ok)] <- TRUE
  unname(ok)
}
# Site median that keeps blank-concentration non-detects (value NA, flagged
# below detection) in the count by ranking them below every reported value.
# NA when the middle of the record falls among them: the median is then below
# the detection limit and has no number. Search, its verifier and the map share it.
nd_median <- function(value, below) {
  v <- ifelse(is.na(value) & below %in% 1, -Inf, value)
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_real_)
  m <- stats::median(v)
  if (is.finite(m)) m else NA_real_
}

# One plain-language sentence stating the gate exactly (export headers, report).
plausibility_rule_text <- function() {
  rng <- vapply(seq_len(nrow(PLAUSIBLE_RANGE)), function(i) {
    r <- PLAUSIBLE_RANGE[i, ]
    lo <- paste0(if (isTRUE(r$lo_closed)) ">= " else "> ", r$lo)
    paste0(analyte_display(r$analyte), " ", lo,
           if (is.finite(r$hi)) paste0(" and <= ", r$hi) else "", " ", r$units)
  }, character(1))
  paste0("a value is excluded from fits, trends, maps and search when it is above its analyte ceiling ",
         "(the larger of the pooled 99.9th percentile and 50x the pooled median; for conductance, TDS, ANC, ",
         "DIC and the major ions the site's own 99.5th percentile and 30x its median, once the site has 8 values) ",
         "or outside its physical range (kept only when ", paste(rng, collapse = "; "), ")")
}

## ---- Heavy-tail map colour scale (review finding #1, map washout) ---------
# A single saline site (PRPO specific conductance ~30x the median) on a RAW
# linear YlGnBu colorbar washes ~24/34 sites into the bottom bin. The suite
# COLOUR-SCALE STANDARD: for a heavy-tailed cross-site quantity, colour on
# log10 with the colorbar TICKING IN TRUE UNITS, and clamp the colour domain to
# robust quantiles (p5/p95) so one outlier can't floor everyone else. Bounded
# metrics (pH, %, richness) are passed through linear (clamp = FALSE) so we never
# log a thing that should not be logged.
#
# Returns a list the map trace consumes directly:
#   $z          numeric colour channel (log10(value) for heavy-tail, raw else)
#   $cmin/$cmax clamped colour-domain endpoints on the same channel
#   $tickvals   colorbar tick positions ON the colour channel
#   $ticktext   the same ticks printed in TRUE units (µS/cm, mg/L, …)
# `linear_codes` = analytes that must NOT be log-scaled (bounded/near-symmetric).
LINEAR_MAP_CODES <- c("pH", "waterTemp", "dissolvedOxygenField", "dissolvedOxygen")
map_colour_scale <- function(value, code = NULL, p_lo = 0.05, p_hi = 0.95) {
  v <- value[is.finite(value)]
  linear <- (!is.null(code) && code %in% LINEAR_MAP_CODES) || any(v <= 0, na.rm = TRUE)
  if (!length(v)) return(list(z = value, cmin = 0, cmax = 1, log = FALSE,
                              tickvals = c(0, 1), ticktext = c(0, 1)))
  if (linear || length(v) < 3) {
    qs <- stats::quantile(v, c(p_lo, p_hi), na.rm = TRUE, names = FALSE)
    cmin <- qs[1]; cmax <- qs[2]
    if (!is.finite(cmin) || !is.finite(cmax) || cmin == cmax) { cmin <- min(v); cmax <- max(v) }
    z <- pmin(pmax(value, cmin), cmax)
    ticks <- pretty(c(cmin, cmax), n = 5)
    ticks <- ticks[ticks >= cmin & ticks <= cmax]
    return(list(z = z, cmin = cmin, cmax = cmax, log = FALSE,
                tickvals = ticks, ticktext = signif(ticks, 3)))
  }
  # heavy-tail branch: work in log10, clamp domain to p5/p95
  lz   <- log10(value)
  qs   <- stats::quantile(log10(v), c(p_lo, p_hi), na.rm = TRUE, names = FALSE)
  cmin <- qs[1]; cmax <- qs[2]
  if (!is.finite(cmin) || !is.finite(cmax) || cmin == cmax) { cmin <- min(log10(v)); cmax <- max(log10(v)) }
  z <- pmin(pmax(lz, cmin), cmax)
  # ticks at "nice" true-unit values spanning the clamped log domain
  decades <- seq(floor(cmin), ceiling(cmax))
  cand <- as.numeric(outer(c(1, 2, 5), 10^decades))
  cand <- sort(unique(cand))
  cand <- cand[log10(cand) >= cmin & log10(cand) <= cmax]
  if (length(cand) < 2) cand <- signif(10^(pretty(c(cmin, cmax), n = 4)), 2)
  list(z = z, cmin = cmin, cmax = cmax, log = TRUE,
       tickvals = log10(cand), ticktext = formatC(cand, format = "fg", big.mark = ","))
}

analyte_display <- function(code) {
  d <- ANALYTE_TBL$display[match(code, ANALYTE_TBL$code)]
  ifelse(is.na(d), code, d)
}
analyte_indicator <- function(code) {
  d <- ANALYTE_TBL$indicator[match(code, ANALYTE_TBL$code)]
  ifelse(is.na(d), "", d)
}
# Vectorized: works for scalars AND whole columns (used inside plotly/ggplot formulas)
pretty_unit <- function(neon_unit, code = NULL) {
  nu <- as.character(neon_unit); nu[is.na(nu)] <- ""
  u  <- unname(UNIT_PRETTY[nu])                 # NA where no symbol known
  out <- ifelse(is.na(u), "", u)
  if (!is.null(code)) {
    cc <- as.character(code); cc[is.na(cc)] <- ""
    uv <- grepl("^UV Absorbance", cc)
    out[out == "" & uv] <- "abs/cm"             # pH and unknowns stay ""
  }
  out
}
axis_title <- function(code, neon_unit) {
  u <- pretty_unit(neon_unit, code)
  paste0(analyte_display(code), if (nzchar(u)) paste0("  (", u, ")") else "")
}

# Build grouped choices (named list of named vectors) for selectInput,
# restricted to codes actually present in `present_codes`.
analyte_choices <- function(present_codes) {
  tbl <- ANALYTE_TBL[ANALYTE_TBL$code %in% present_codes, ]
  # keep any present code not in the table (defensive) appended under "Other"
  extra <- setdiff(present_codes, ANALYTE_TBL$code)
  if (length(extra)) tbl <- bind_rows(tbl, tibble(code = extra, display = extra, group = "Other", indicator = ""))
  grp_order <- c("Physical","Buffering","Major ions","Nutrients","Carbon/DOM","Metals","Solids","Other")
  out <- list()
  for (g in grp_order) {
    sub <- tbl[tbl$group == g, ]
    if (!nrow(sub)) next
    v <- setNames(sub$code, sub$display)
    out[[g]] <- v
  }
  out
}

## ---- Scientifically grounded preset comparisons (Aquatics) ----------------
# Preset names are input values (selectInput sends them back), so they stay ASCII:
# under a C locale (manifest.json declares locale "C") R cannot hold a non-ASCII
# name as UTF-8, and a browser value like "a \u2194 b" would never match it.
PRESETS <- list(
  "Ionic strength vs buffering (flagship)" = c("specificConductanceField", "ANC"),
  "Ionic strength vs calcium"               = c("specificConductanceField", "Ca"),
  "Inorganic vs total dissolved N"             = c("TDN", "NO3+NO2 - N"),
  "Total vs bioavailable phosphorus"           = c("TP", "Ortho - P"),
  "DOM quantity vs quality (SUVA)"          = c("DOC", "UV Absorbance (254 nm)"),
  "Carbonate system (DIC vs ANC)"           = c("DIC", "ANC"),
  "Redox metals (iron vs manganese)"        = c("Fe", "Mn")
)
PRESET_REASON <- c(
  "Ionic strength vs buffering (flagship)" = "Specific conductance integrates all dissolved ions; in carbonate terrain it tracks ANC tightly, the README's Sycamore Creek example.",
  "Ionic strength vs calcium"               = "Conductance should rise with the dominant cation; calcium is a primary driver of ionic strength in most streams.",
  "Inorganic vs total dissolved N"             = "Nitrate+nitrite is the oxidized fraction within total dissolved N; the relationship shows how much of the N pool is inorganic.",
  "Total vs bioavailable phosphorus"           = "Orthophosphate (SRP) is the bioavailable fraction of total P; the ratio diagnoses dissolved-reactive vs particle-bound P.",
  "DOM quantity vs quality (SUVA)"          = "UV absorbance per unit DOC (SUVA) indexes aromaticity / terrestrial origin of organic matter (Weishaar et al. 2003).",
  "Carbonate system (DIC vs ANC)"           = "Dissolved inorganic carbon and acid neutralizing capacity are tied together by carbonate equilibria; NEON recommends estimating bicarbonate from either one.",
  "Redox metals (iron vs manganese)"        = "Fe and Mn are both mobilized under low-oxygen conditions; they co-vary in groundwater-fed and wetland-influenced streams."
)

season_of <- function(d) factor(c("Winter","Spring","Summer","Fall")[(lubridate::month(d) %% 12) %/% 3 + 1],
                                 levels = c("Winter","Spring","Summer","Fall"))

## ---- NEON provenance: release tags, DOIs, data notes ----------------------
# DP1.20093.001 release DOIs, from GET /api/v0/products/DP1.20093.001
# (releases[].productDoi.url), read 2026-10-22. config/neon-product-watch.json
# carries the same list for the refresh workflow's release watch, and a test
# keeps the two equal. A release tag missing here renders without a DOI, and the
# watch opens an issue to add it.
NEON_PRODUCT_NAME <- "Chemical properties of surface water"
NEON_RELEASE_DOI <- c(
  "RELEASE-2021" = "https://doi.org/10.48443/05k7-e011",
  "RELEASE-2022" = "https://doi.org/10.48443/tx4b-zx50",
  "RELEASE-2023" = "https://doi.org/10.48443/wf2d-h925",
  "RELEASE-2024" = "https://doi.org/10.48443/fdfd-d514",
  "RELEASE-2025" = "https://doi.org/10.48443/a8cf-fj36",
  "RELEASE-2026" = "https://doi.org/10.48443/7b2c-ga32"
)

# What a bundle records about NEON releases. The v5 producer writes
# built$neon_releases (columns release, n, first, last; one row per tag,
# "PROVISIONAL" included) and built$provisional_from (the first collectDate of a
# provisional row, or NA). Earlier bundles carry neither, so recorded = FALSE.
neon_release_info <- function(built) {
  rel <- built$neon_releases
  recorded <- is.data.frame(rel) && nrow(rel) > 0 && "release" %in% names(rel)
  tags <- if (recorded) as.character(rel$release) else character(0)
  released <- sort(unique(tags[!is.na(tags) & grepl("^RELEASE-[0-9]{4}$", tags)]))
  latest <- if (length(released)) released[length(released)] else NA_character_
  pf <- built$provisional_from
  pf <- if (length(pf) == 1 && !is.na(pf) && nzchar(as.character(pf))) as.character(pf) else NA_character_
  list(
    recorded = recorded,
    releases = released,
    latest = latest,
    doi = if (!is.na(latest) && latest %in% names(NEON_RELEASE_DOI))
      unname(NEON_RELEASE_DOI[latest]) else NA_character_,
    provisional_from = pf,
    n_provisional = if (recorded && "n" %in% names(rel))
      as.integer(sum(rel$n[tags %in% "PROVISIONAL"])) else NA_integer_
  )
}

# NEON's citation for this bundle, one plain-text sentence group per element.
neon_citation_lines <- function(built) {
  info <- neon_release_info(built)
  accessed <- substr(as.character(built$when %||% ""), 1, 10)
  page <- "https://data.neonscience.org/data-products/DP1.20093.001"
  out <- character(0)
  if (!is.na(info$latest)) {
    out <- c(out, sprintf(
      "NEON (National Ecological Observatory Network). %s (DP1.20093.001), %s. %sDataset accessed from %s on %s.",
      NEON_PRODUCT_NAME, info$latest,
      if (!is.na(info$doi)) paste0(info$doi, ". ") else "",
      paste0(page, "/", info$latest), accessed))
  }
  if (!is.na(info$provisional_from)) {
    out <- c(out, sprintf(
      "NEON (National Ecological Observatory Network). %s (DP1.20093.001), provisional data from %s onward. Dataset accessed from %s on %s. Provisional data have no DOI and can still change.",
      NEON_PRODUCT_NAME, info$provisional_from, page, accessed))
  }
  if (!isTRUE(info$recorded)) {
    out <- c(out, sprintf(
      "NEON (National Ecological Observatory Network). %s (DP1.20093.001). Dataset accessed from %s on %s. This bundle does not record release tags, so its most recent months may be provisional.",
      NEON_PRODUCT_NAME, page, accessed))
  }
  out
}

# Notes a reader of a site's record needs: entries from NEON's DP1.20093.001
# change log (GET /api/v0/products/DP1.20093.001, changeLogs[]) plus facts
# measured on the bundle (id "bundle-..."). Every change-log id here is reviewed
# with a matching disposition in config/neon-product-watch.json, and a test checks
# it. scope: "site" (the sites listed), "network" (every site) or "analyte" (the
# analytes listed). Lists are "|"-separated.
NEON_DATA_NOTES <- tibble::tribble(
  ~id,              ~scope,    ~sites,                          ~analytes,                                  ~from,        ~to,          ~text,
  "16402",          "site",    "BARC|SUGG|CRAM|LIRO|PRLA|PRPO", "",                                         "2012-01-01", "2018-01-01", "Until 2018 NEON also sampled this lake's inlet and outlet. Those samples are averaged with the lake samples taken on the same day.",
  "60906",          "site",    "SYCA",                          "",                                         "2022-01-26", "2022-01-26", "NEON moved this study reach upstream on 2022-01-26, so samples before and after that date come from different stretches of the creek.",
  "68004",          "site",    "OKSR|TOOK",                     "",                                         "2020-03-23", "2022-12-31", "Quarantine rules at Toolik Field Station cut NEON's field visits from March 2020 through 2022, so this record has fewer samples in those years.",
  "68005",          "site",    "BLDE",                          "",                                         "2022-06-12", "2022-10-31", "Flooding closed the roads into Yellowstone from 2022-06-12 to 2022-10-31, so no samples were collected here in that window.",
  "17910",          "network", "",                              "",                                         "2020-03-23", "2021-12-31", "COVID-19 safety rules reduced or cancelled NEON sampling from March 2020 through 2021, so most records thin out in those years.",
  "16403",          "analyte", "",                              "ANC",                                      "2018-01-01", "2018-01-01", "NEON cut ANC sampling to once a month in 2018.",
  "bundle-lab-end", "analyte", "",                              "ANC|specificConductance|TSS - Dry Mass",   "2020-03-19", "2020-03-19", "This bundle's values for this analyte end on 2020-03-19.",
  "37504",          "analyte", "",                              "specificConductance|TSS - Dry Mass",       "2021-03-23", "2021-03-23", "NEON's external lab stopped reporting this analyte in 2021. For conductance, use the field probe's specific conductance, which continues."
)
# Rows of NEON_DATA_NOTES that apply to a site and/or a set of analytes; network
# notes always apply.
neon_notes_for <- function(site = NULL, analytes = NULL) {
  n <- NEON_DATA_NOTES
  in_list <- function(field, keys) vapply(strsplit(field, "|", fixed = TRUE),
                                          function(x) any(keys %in% x), logical(1))
  site_hit <- n$scope == "site" & length(site) > 0 & in_list(n$sites, site)
  an_hit <- n$scope == "analyte" & length(analytes) > 0 & in_list(n$analytes, analytes)
  n[site_hit | an_hit | n$scope == "network", , drop = FALSE]
}

# One layout call for every time-axis context mark on a plotly chart, so the
# marks never overwrite each other (plotly replaces `shapes` wholesale): the
# dashed 2019-07-01 lab method-change line (when `code` is a method-change
# analyte and [d1, d2] straddles the date) and a light band over NEON's
# provisional months (when the bundle records built$provisional_from).
add_time_context <- function(p, code, d1, d2, built = NULL, color = "#8a949c") {
  shapes <- list(); notes <- list()
  if (spans_method_change(code, d1, d2)) {
    shapes[[length(shapes) + 1]] <- list(
      type = "line", xref = "x", yref = "paper",
      x0 = METHOD_CHANGE_DATE, x1 = METHOD_CHANGE_DATE, y0 = 0, y1 = 1,
      line = list(color = color, width = 1.2, dash = "dash"))
    notes[[length(notes) + 1]] <- list(
      x = METHOD_CHANGE_DATE, y = 1, xref = "x", yref = "paper",
      text = "lab method change", showarrow = FALSE, xanchor = "left",
      yanchor = "top", xshift = 3, font = list(size = 10, color = color))
  }
  pf <- if (!is.null(built)) neon_release_info(built)$provisional_from else NA_character_
  if (!is.na(pf)) {
    x0 <- max(as.Date(pf), as.Date(d1)); x1 <- as.Date(d2)
    if (!is.na(x0) && !is.na(x1) && x1 > x0) {
      shapes[[length(shapes) + 1]] <- list(
        type = "rect", xref = "x", yref = "paper", x0 = x0, x1 = x1, y0 = 0, y1 = 1,
        fillcolor = "rgba(138,148,156,0.12)", line = list(width = 0), layer = "below")
      notes[[length(notes) + 1]] <- list(
        x = x0, y = 0, xref = "x", yref = "paper", text = "provisional",
        showarrow = FALSE, xanchor = "left", yanchor = "bottom", xshift = 3,
        font = list(size = 10, color = color))
    }
  }
  if (!length(shapes)) return(p)
  plotly::layout(p, shapes = shapes, annotations = notes)
}

# HTML for one pinned card (www/pincards.js reads it from a point's customdata).
# `rows` is a named character vector: label -> value. Every string is escaped.
pin_card_html <- function(title, rows) {
  esc <- htmltools::htmlEscape
  cells <- paste0("<div class='wc-pin-r'><span class='wc-pin-k'>", esc(names(rows)),
                  "</span> <span class='wc-pin-v'>", esc(unname(rows)), "</span></div>",
                  collapse = "")
  paste0("<div class='wc-pin-card'><div class='wc-pin-t'>", esc(title), "</div>",
         cells, "</div>")
}

## ---- In-app sibling links (suite chrome) ---------------------------------
# The "Explore the NEON series" block surfaced in the About modal. Registry +
# live URLs mirror docs/index.html so the in-app links and the landing grid can
# never drift. Each sibling hyperlinks to its live github.io cover.
SIBLINGS <- tibble::tribble(
  ~name,                                ~url,                                                                  ~current,
  "Small Mammal Tracker",               "https://tgilbert14.github.io/NEON-Small-Mammal-Tracker-App/",         FALSE,
  "Plant Diversity",                    "https://tgilbert14.github.io/NEON-Plant-Diversity/",                  FALSE,
  "Breeding Birds",                     "https://tgilbert14.github.io/NEON-Breeding-Birds/",                   FALSE,
  "Plant Phenology",                    "https://tgilbert14.github.io/NEON-Plant-Phenology-Explorer/",         FALSE,
  "Vegetation Structure",               "https://tgilbert14.github.io/NEON-Vegetation-Structure-Explorer/",    FALSE,
  "Ground Beetle Tracker",              "https://tgilbert14.github.io/NEON-Ground-Beetle-Tracker/",            FALSE,
  "Water Chemistry Analyte Viewer",     "https://tgilbert14.github.io/NEON-WaterChemistry-Analyte-Viewer-App/", TRUE,
  "Mosquito Pulse",                     "https://tgilbert14.github.io/NEON-Mosquito-Pulse/",                   FALSE,
  "My Little Inverts",                  "https://tgilbert14.github.io/NEON-My-Little-Inverts/",                FALSE,
  "NEON Cross-Product Response Atlas",  "https://tgilbert14.github.io/NEON-Driver-Cascade/",                   FALSE
)
sibling_block <- function() {
  htmltools::tagList(
    htmltools::tags$hr(),
    htmltools::tags$p(htmltools::tags$b("Explore the NEON series"),
      htmltools::tags$span(class = "scope-note", .noWS = "before",
        ": other NEON explorers, each on a different measurement:")),
    htmltools::tags$div(
      style = "display:flex; flex-wrap:wrap; gap:.4rem .6rem; margin:.3rem 0 .2rem",
      lapply(seq_len(nrow(SIBLINGS)), function(i) {
        if (isTRUE(SIBLINGS$current[i]))
          htmltools::tags$span(class = "sibling-chip sibling-current", `aria-current` = "page",
            SIBLINGS$name[i], htmltools::tags$span(class = "visually-hidden", " (this app)"),
            htmltools::tags$span(class = "sibling-here", `aria-hidden` = "true", " · you are here"))
        else
          htmltools::tags$a(href = SIBLINGS$url[i], target = "_blank", rel = "noopener",
            class = "sibling-chip", SIBLINGS$name[i])
      }))
  )
}

## ---- Theme helpers -------------------------------------------------------
theme_neon <- function(base = 13) {
  ggplot2::theme_minimal(base_size = base, base_family = "Inter") +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = base + 2),
      plot.subtitle = ggplot2::element_text(color = "#5f6b7a", size = base - 2),
      plot.title.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#00000012"),  # hex+alpha: valid in both browser & PDF device
      axis.title    = ggplot2::element_text(color = "#3c4757"),
      legend.position = "bottom")
}

# `big = TRUE` (narrow / phone viewport) bumps font so chart text is legible on a phone
plotly_theme <- function(p, mode = "light", big = FALSE) {
  dark <- identical(mode, "dark")
  bg <- if (dark) COL$bg_d else COL$bg_l
  fg <- if (dark) COL$fg_d else COL$fg_l
  grid <- if (dark) COL$grid_d else COL$grid_l
  # tooltip: teal-edged dark glass on dark, clean white on light
  hl <- if (dark)
    list(font = list(size = if (big) 15 else 12, color = "#e6f4fb"),
         bgcolor = "rgba(4,17,30,.96)", bordercolor = "#34c6d8")
  else
    list(font = list(size = if (big) 15 else 12))
  plotly::layout(p,
    paper_bgcolor = bg, plot_bgcolor = bg,
    font = list(family = "Inter", color = fg, size = if (big) 15 else 12),
    hoverlabel = hl,
    xaxis = list(gridcolor = grid, zerolinecolor = grid),
    yaxis = list(gridcolor = grid, zerolinecolor = grid))
}

plotly_clean <- function(p, filename = "neon_analyte_chart") {
  plotly::config(p, displaylogo = FALSE, responsive = TRUE,
    modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d",
                               "hoverClosestCartesian","hoverCompareCartesian","toggleSpikelines"),
    toImageButtonOptions = list(format = "png", scale = 3, filename = filename))
}

# tiny sparkline for a value_box showcase (no axes, transparent, area fill)
spark <- function(x, y, col) {
  ok <- is.finite(y)
  if (sum(ok) < 2) return(NULL)
  plotly::plot_ly(x = x[ok], y = y[ok], type = "scatter", mode = "lines", height = 70,
                  line = list(color = col, width = 2, shape = "spline"),
                  fill = "tozeroy", fillcolor = paste0(col, "26"), hoverinfo = "skip") |>
    plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                   margin = list(l = 0, r = 0, t = 0, b = 0),
                   paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE) |>
    plotly::config(displayModeBar = FALSE)
}

# Empty / error state as a real annotated plot (never a vanished validate())
plotly_message <- function(msg, mode = "light") {
  plot_ly() |>
    plotly::layout(
      annotations = list(text = msg, showarrow = FALSE,
                         font = list(size = 15, color = if (identical(mode,"dark")) "#9fb0bd" else "#8a94a6")),
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)) |>
    plotly_theme(mode)
}

safe_plotly <- function(expr, mode = "light") {
  tryCatch(force(expr), error = function(e) {
    # req()/validate()/frozen-input cancellations are Shiny control flow, not
    # render failures: let them through so the output simply waits, instead of
    # painting a "snag" over a view that is only mid-update.
    if (inherits(e, "shiny.silent.error")) stop(e)
    message("plot render error: ", conditionMessage(e))
    plotly_message("This view hit a snag. Nudge a control to reload.", mode)
  })
}

## ---- Real computations ---------------------------------------------------

# NEON nutrient method change (changeLog 2021-12-03): from 2019-07-01 these
# analytes moved to frozen samples with lower detection limits, so a series
# spanning the date mixes two detection regimes. Charts mark the boundary.
METHOD_CHANGE_2019 <- c("NO3+NO2 - N", "NO2 - N", "NH4 - N", "Ortho - P", "TP", "TDP")
METHOD_CHANGE_DATE <- as.Date("2019-07-01")
# TRUE when `code` is a method-change analyte and [d1, d2] straddles the date.
spans_method_change <- function(code, d1, d2) {
  isTRUE(code %in% METHOD_CHANGE_2019) && isTRUE(d1 < METHOD_CHANGE_DATE) &&
    isTRUE(d2 >= METHOD_CHANGE_DATE)
}

# Dashed vertical marker + small label at the 2019-07-01 method change (plotly).
add_method_change_marker <- function(p, color = "#8a949c") {
  plotly::layout(p,
    shapes = list(list(type = "line", xref = "x", yref = "paper",
                       x0 = METHOD_CHANGE_DATE, x1 = METHOD_CHANGE_DATE, y0 = 0, y1 = 1,
                       line = list(color = color, width = 1.2, dash = "dash"))),
    annotations = list(list(x = METHOD_CHANGE_DATE, y = 1, xref = "x", yref = "paper",
                            text = "lab method change", showarrow = FALSE,
                            xanchor = "left", yanchor = "top", xshift = 3,
                            font = list(size = 10, color = color))))
}
# Does a series mix detection-limit eras? TRUE when one calendar year is >= 50%
# below detection while another is <= 10% (a DL regime change, e.g. Br), or the
# whole window is > 25% below detection. Then a fitted trend partly tracks the
# detection limit, not the water.
# A calendar year counts toward the era test only with >= min_n samples, so a
# lone below-DL sample in a thin year cannot flip the trend grey by itself.
bdl_mixes_eras <- function(dates, bdl, min_n = 6) {
  b <- !is.na(bdl) & bdl == 1
  if (!length(b)) return(FALSE)
  yr <- as.integer(format(dates, "%Y"))
  n_yr <- tapply(b, yr, length)
  sh <- tapply(b, yr, mean)[n_yr >= min_n]
  (length(sh) >= 2 && any(sh >= 0.50) && any(sh <= 0.10)) || mean(b) > 0.25
}

# Flag text for a heavily censored correlation row: name which member drives it.
censor_flag <- function(pb, pbm, pba, thresh = 0.25) {
  pct <- function(x) sprintf("%.0f%%", 100 * x)
  mh <- is.finite(pbm) & pbm >= thresh; ph <- is.finite(pba) & pba >= thresh
  ifelse(mh & ph, sprintf("censored (both %s / %s <DL)", pct(pbm), pct(pba)),
  ifelse(mh,      sprintf("censored (main %s <DL)", pct(pbm)),
  ifelse(ph,      sprintf("censored (partner %s <DL)", pct(pba)),
                  sprintf("censored (pair %s <DL)", pct(pb)))))
}

# Censored share of an analyte pair on its OWN matched samples in a site/window.
# `sl` is the long slice (site, collectDate, analyte, value, belowDetection);
# matched = dates where both A and B were sampled (a finite value or a
# below-detection row). A pair row is censored when either member is below
# detection. n here can exceed the fit's n once NA-valued non-detects exist;
# the share then describes the sampled pairs, not only the fitted ones.
# Returns list(n, share, pA, pB).
pair_censor_share <- function(sl, A, B) {
  empty <- list(n = 0L, share = NA_real_, pA = NA_real_, pB = NA_real_)
  if (is.null(sl) || !nrow(sl) || identical(A, B)) return(empty)
  # a date counts as sampled when the analyte has a finite value OR is a
  # below-detection row (post-2022 NDs carry value NA); so NA non-detects stay
  # in the censoring denominator even though they can enter no fit
  sampled <- is.finite(sl$value) | (!is.na(sl$belowDetection) & sl$belowDetection == 1)
  a <- sl[sl$analyte == A & sampled, c("collectDate", "belowDetection")]
  b <- sl[sl$analyte == B & sampled, c("collectDate", "belowDetection")]
  a <- a[!duplicated(a$collectDate), , drop = FALSE]
  b <- b[!duplicated(b$collectDate), , drop = FALSE]
  j <- merge(a, b, by = "collectDate", suffixes = c("_a", "_b"))
  if (!nrow(j)) return(empty)
  ba <- !is.na(j$belowDetection_a) & j$belowDetection_a == 1
  bb <- !is.na(j$belowDetection_b) & j$belowDetection_b == 1
  list(n = nrow(j), share = mean(ba | bb), pA = mean(ba), pB = mean(bb))
}
# One-clause note naming which member(s) are censored, or NULL under thresh.
pair_censor_note <- function(pc, A, B, thresh = 0.25) {
  if (is.null(pc) || !is.finite(pc$share) || pc$share < thresh) return(NULL)
  who <- c(if (is.finite(pc$pA) && pc$pA >= thresh) sprintf("%s %.0f%%", analyte_display(A), 100 * pc$pA),
           if (is.finite(pc$pB) && pc$pB >= thresh) sprintf("%s %.0f%%", analyte_display(B), 100 * pc$pB))
  lead <- if (length(who)) paste(who, collapse = " & ") else
    sprintf("%.0f%% of %s / %s pairs", 100 * pc$share, analyte_display(A), analyte_display(B))
  paste0(lead, " below detection in this window, exploratory only")
}

# Lag-1 autocorrelation of a time-ordered series (NA-safe, signed).
lag1_acf <- function(z) suppressWarnings(tryCatch({
  z <- z[is.finite(z)]
  if (length(z) < 4 || stats::sd(z) == 0) return(NA_real_)
  stats::acf(z, lag.max = 1, plot = FALSE)$acf[2]
}, error = function(e) NA_real_))
# Effective sample size of a correlation between two autocorrelated series:
# n * (1 - rx*ry) / (1 + rx*ry) when rx*ry > 0, else NA (no adjustment).
effective_n <- function(n, rx, ry) {
  k <- rx * ry
  ifelse(is.finite(k) & k > 0 & n > 3, n * (1 - k) / (1 + k), NA_real_)
}
# 95% interval for Spearman rho: Fisher z with the Bonett-Wright (2000) se
# sqrt((1 + rho^2/2) / (n_use - 3)), n_use = n_eff when finite, else n.
# NA unless n_use > 3 and n >= min_n. Returns list(lo, hi, n_use).
spearman_ci <- function(rho, n, n_eff = NA_real_, min_n = 8, level = 0.95) {
  n_use <- ifelse(is.finite(n_eff), n_eff, n)
  ok <- is.finite(rho) & is.finite(n_use) & n_use > 3 & n >= min_n
  se <- sqrt((1 + rho^2 / 2) / pmax(n_use - 3, 1e-9))
  zq <- stats::qnorm(1 - (1 - level) / 2)
  z  <- atanh(pmin(pmax(rho, -0.999999), 0.999999))
  list(lo = ifelse(ok, tanh(z - zq * se), NA_real_),
       hi = ifelse(ok, tanh(z + zq * se), NA_real_), n_use = n_use)
}

# Correlation of `main` vs every other numeric column of a per-site wide frame.
# Reports n (paired), Pearson r, Spearman rho, p-value of the chosen method,
# and an honesty flag. min_n is the reliability floor (default 8).
# censor_map (named analyte -> fraction below detection) + censor_thresh let the
# table down-weight heavily-censored analytes (Br 57%, Mn/Fe/F ~34-39% below DL):
# a Spearman/Pearson there is dominated by detection-limit ties, not chemistry
# (Helsel 2012), so those rows are marked heavy_censor and greyed like low-n.
# bdl_site (preferred; audit 2026-10-18 item 20): the SAME site/window as
# wide_site, pivoted from belowDetection (one row per collectDate). When given,
# the censored share is measured on the pair's own matched samples — a row is
# censored when EITHER member is below detection — so a site that is clean for
# an analyte the network censors heavily is not greyed, and vice versa. The
# network-wide censor_map is only the fallback when bdl_site is NULL.
# wide_site must keep its collectDate column for the bdl_site alignment
# (a Date column is non-numeric, so it never enters the screened analytes).
correlation_table <- function(wide_site, main, method = c("spearman","pearson"),
                              min_n = 8, censor_map = NULL, censor_thresh = 0.25,
                              bdl_site = NULL) {
  method <- match.arg(method)
  if (is.null(wide_site) || !main %in% names(wide_site)) return(NULL)
  m <- suppressWarnings(as.numeric(wide_site[[main]]))
  num <- names(wide_site)[vapply(wide_site, is.numeric, logical(1))]
  others <- setdiff(num, main)
  pct_bdl <- function(a) if (!is.null(censor_map) && a %in% names(censor_map)) unname(censor_map[a]) else NA_real_
  use_site_bdl <- !is.null(bdl_site) && "collectDate" %in% names(wide_site) &&
    "collectDate" %in% names(bdl_site)
  bidx <- if (use_site_bdl) match(wide_site$collectDate, bdl_site$collectDate) else NULL
  # per-row below-detection indicator aligned to wide_site (FALSE when unknown)
  bdl_col <- function(a) {
    if (!a %in% names(bdl_site)) return(rep(FALSE, nrow(wide_site)))
    v <- suppressWarnings(as.numeric(bdl_site[[a]]))[bidx]
    !is.na(v) & v == 1
  }
  bm_all <- if (use_site_bdl) bdl_col(main) else NULL
  ptest <- function(x, y, mth) suppressWarnings(tryCatch(stats::cor.test(x, y, method = mth)$p.value, error = function(e) NA_real_))
  # time order for the lag-1 ACFs (pivot_wider does not guarantee date order)
  ord <- if ("collectDate" %in% names(wide_site)) order(wide_site$collectDate) else seq_along(m)
  rows <- lapply(others, function(a) {
    y <- suppressWarnings(as.numeric(wide_site[[a]]))
    ok <- is.finite(m) & is.finite(y)
    n  <- sum(ok)
    pe <- if (n >= 4) suppressWarnings(stats::cor(m[ok], y[ok], method = "pearson"))  else NA_real_
    sp <- if (n >= 4) suppressWarnings(stats::cor(m[ok], y[ok], method = "spearman")) else NA_real_
    pp <- if (n >= 4) ptest(m[ok], y[ok], "pearson")  else NA_real_
    ps <- if (n >= 4) ptest(m[ok], y[ok], "spearman") else NA_real_
    ties <- n >= 4 && (any(duplicated(m[ok])) || any(duplicated(y[ok])))
    # signed lag-1 ACF of each member of the pair, on the paired dates in time order
    oo <- ord[ok[ord]]
    rx <- if (n >= 6) lag1_acf(m[oo]) else NA_real_
    ry <- if (n >= 6) lag1_acf(y[oo]) else NA_real_
    ac <- if (n >= 6) suppressWarnings(max(abs(c(rx, ry)), na.rm = TRUE)) else NA_real_
    if (!is.finite(ac)) ac <- NA_real_
    if (use_site_bdl) {
      # denominator = dates where BOTH analytes were sampled (finite value or a
      # below-detection row, so NA-valued non-detects count); numerator = either
      # member below detection on those dates
      ba_all <- bdl_col(a)
      both <- (is.finite(m) | bm_all) & (is.finite(y) | ba_all)
      if (any(both)) {
        bm <- bm_all[both]; ba <- ba_all[both]
        pb <- mean(bm | ba); pbm <- mean(bm); pba <- mean(ba)
      } else pb <- pbm <- pba <- NA_real_
    } else {
      pb <- pct_bdl(a); pbm <- pct_bdl(main); pba <- pct_bdl(a)
    }
    tibble(code = a, display = analyte_display(a), n = n,
           pearson = pe, spearman = sp, p_pearson = pp, p_spearman = ps, ties = ties,
           acf1 = ac, acf_main = rx, acf_partner = ry,
           pct_below = pb, pct_below_main = pbm, pct_below_partner = pba)
  })
  out <- bind_rows(rows)
  if (!nrow(out)) return(out)
  out$coef <- if (method == "spearman") out$spearman else out$pearson
  out$p    <- if (method == "spearman") out$p_spearman else out$p_pearson
  # Effective n for two autocorrelated series (audit 2026-10-18 P3):
  # n_eff = n * (1 - rx*ry) / (1 + rx*ry), rx and ry the signed lag-1 ACFs,
  # applied whenever rx*ry > 0 (no |ACF| cliff). The two-sided t-test p is
  # recomputed on n_eff and reported beside the raw p.
  out$n_eff <- effective_n(out$n, out$acf_main, out$acf_partner)
  out$p_eff <- with(out, {
    ne <- n_eff; r <- coef
    tval <- abs(r) * sqrt((ne - 2) / pmax(1e-9, 1 - r^2))
    p <- 2 * stats::pt(tval, df = pmax(1, ne - 2), lower.tail = FALSE)
    ifelse(is.finite(ne) & is.finite(r) & abs(r) < 1, pmin(1, p), NA_real_)
  })
  # 95% interval on Spearman rho (Fisher z, Bonett-Wright se) on n_eff when set
  ci <- spearman_ci(out$spearman, out$n, out$n_eff, min_n = min_n)
  out$rho_lo <- ci$lo; out$rho_hi <- ci$hi
  out <- out |> filter(n >= 4, is.finite(coef)) |>
    mutate(heavy_censor = is.finite(pct_below) & pct_below >= censor_thresh,
           # reliable drives the grey-out: a row is reliable only if it clears the
           # n floor AND isn't dominated by below-detection ties.
           reliable = n >= min_n & !heavy_censor,
           flag = dplyr::case_when(
             n < min_n            ~ sprintf("insufficient (n=%d)", n),
             heavy_censor         ~ censor_flag(pct_below, pct_below_main,
                                                    pct_below_partner, censor_thresh),
             coef >=  0.7         ~ "strong +",
             coef <= -0.7         ~ "strong −",
             abs(coef) >= 0.4     ~ "moderate",
             TRUE                 ~ "weak/none")) |>
    arrange(desc(abs(coef)))
  # Benjamini-Hochberg false-discovery q across the screened analytes (computed
  # on the active method's p, over the rows that survive the n filter).
  out$q_bh <- stats::p.adjust(out$p, method = "BH")
  out
}

# OLS fit of paired (x, y) with the stats a showcase should surface.
# If df carries collectDate, rows are time-ordered first so the lag-1 residual
# autocorrelation is meaningful (it's nonsense on arbitrary row order).
fit_lm <- function(df) {
  if (is.null(df)) return(NULL)
  # complete cases only: an NA-valued non-detect must not reach lm() (it would
  # be dropped silently there while n below still counted it)
  df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
  if (nrow(df) < 3) return(NULL)
  if ("collectDate" %in% names(df)) df <- df[order(df$collectDate), , drop = FALSE]
  m  <- stats::lm(y ~ x, df)
  s  <- summary(m)
  dw <- tryCatch({ r <- stats::residuals(m); sum(diff(r)^2) / sum(r^2) }, error = function(e) NA_real_)
  ac <- tryCatch(stats::acf(stats::residuals(m), plot = FALSE)$acf[2], error = function(e) NA_real_)
  list(model = m,
       n = nrow(df),
       r2 = s$r.squared, adj_r2 = s$adj.r.squared,
       slope = stats::coef(m)[2], slope_se = s$coefficients[2, 2],
       intercept = stats::coef(m)[1],
       p = s$coefficients[2, 4],
       durbin_watson = dw, lag1_acf = ac)
}

# k-fold CV RMSE for a glm of `target ~ predictors` on a wide frame (base R).
kfold_rmse <- function(data, target, predictors, k = 10, reps = 5, seed = 42) {
  d <- data[, c(target, predictors), drop = FALSE]
  d <- d[stats::complete.cases(d), , drop = FALSE]
  null_rmse <- if (nrow(d) >= 2) sqrt(mean((d[[target]] - mean(d[[target]]))^2)) else NA_real_
  if (nrow(d) < (k + length(predictors) + 2)) return(list(rmse = NA_real_, n = nrow(d), null_rmse = null_rmse))
  set.seed(seed)
  form <- stats::as.formula(paste0("`", target, "` ~ ", paste(sprintf("`%s`", predictors), collapse = " + ")))
  errs <- c()
  for (r in seq_len(reps)) {
    folds <- sample(rep(seq_len(k), length.out = nrow(d)))
    for (i in seq_len(k)) {
      tr <- d[folds != i, , drop = FALSE]; te <- d[folds == i, , drop = FALSE]
      fit <- tryCatch(stats::glm(form, data = tr), error = function(e) NULL)
      if (is.null(fit)) next
      pr <- tryCatch(stats::predict(fit, te), error = function(e) rep(NA_real_, nrow(te)))
      errs <- c(errs, (te[[target]] - pr)^2)
    }
  }
  list(rmse = sqrt(mean(errs, na.rm = TRUE)), n = nrow(d), null_rmse = null_rmse)
}
