#----------------------------------------------------------------------
# helpers.R  —  pure helpers for the NEON Analyte Viewer (sourced by app.R)
# Display names + units (Aquatics spec), CVD-safe colors, plotly/ggplot theme
# helpers, and the three real computations: correlation_table(), fit_lm(),
# predict_glm(). No Shiny reactivity here — everything takes plain data frames.
#----------------------------------------------------------------------
suppressWarnings(suppressMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(plotly); library(lubridate)
}))

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
# The bundle's analyte_meta took dplyr::first(units), so 20/34 analytes carry a
# stray non-modal unit string and UV254/UV280 publish a literal NA. We DON'T
# rebuild the bundle; instead canonical_units() picks the MODAL unit per analyte
# from the long frame at app load and coerces every read through it. The six
# µg/L-vs-mg/L mixers (verified mislabeled, NOT 1000× off — TP "µg/L" median
# 0.057 vs mg/L 0.025) are forced to mg/L by label only; values are NOT scaled.
# UV absorbance has no NEON unit string at all (it's an absorbance ratio) — we
# stamp the canonical "absorbance units" so the dictionary stops exporting NA.
CANON_UNIT_OVERRIDE <- c(
  TP = "milligramsPerLiter", TDP = "milligramsPerLiter",
  `Ortho - P` = "milligramsPerLiter", `NH4 - N` = "milligramsPerLiter",
  `NO2 - N` = "milligramsPerLiter", `NO3+NO2 - N` = "milligramsPerLiter",
  TPC = "microgramsPerLiter", TPN = "microgramsPerLiter",
  `UV Absorbance (254 nm)` = "absorbance units",
  `UV Absorbance (250 nm)` = "absorbance units",
  `UV Absorbance (280 nm)` = "absorbance units"
)
# Modal (most-frequent) non-NA unit per analyte; overrides win. Returns a named
# character vector code -> canonical NEON unit string.
canonical_units <- function(swc_long) {
  tab <- swc_long |>
    dplyr::filter(!is.na(units), nzchar(units)) |>
    dplyr::count(analyte, units, name = "k") |>
    dplyr::group_by(analyte) |>
    dplyr::slice_max(k, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  out <- setNames(tab$units, tab$analyte)
  ov  <- intersect(names(CANON_UNIT_OVERRIDE), unique(swc_long$analyte))
  out[ov] <- CANON_UNIT_OVERRIDE[ov]
  # any analyte with no usable unit string at all (pure-NA) still gets its override
  miss <- setdiff(names(CANON_UNIT_OVERRIDE), names(out))
  if (length(miss)) out[miss] <- CANON_UNIT_OVERRIDE[miss]
  out
}
# Resolve the canonical unit for one code against a precomputed map, falling back
# to the raw string when the analyte isn't mapped (defensive).
canon_unit_of <- function(code, map, fallback = NA_character_) {
  u <- unname(map[code]); ifelse(is.na(u), fallback, u)
}

## ---- Per-analyte plausibility gate (review finding #2, outlier QC) ---------
# A single impossible singleton (ANC = 927 meq/L at CARI, site median 0.73; Fe =
# 931 mg/L, median 0.011) must never drive a fit, an STL trend, the glm, or the
# map colorbar. plausibility_ceilings() learns a per-analyte ceiling once at app
# load. A value is flagged implausible only when it clears BOTH guards — the
# review's "> p99.9 + a unit-sanity check":
#   (a) it exceeds the analyte's p99.9 quantile, AND
#   (b) it exceeds a generous multiple of the analyte's median (unit-sanity).
# Requiring BOTH keeps a genuinely heavy-tailed-but-real analyte intact (PRPO
# conductance 7,924 µS/cm stays IN — it is well within conductance's own p99.9),
# while a lone 1000×-median artifact in an otherwise tight distribution is gated
# OUT (ANC 927 dwarfs both its p99.9 and 50× its median). The ceiling is each
# analyte's own max(p99.9, median×mult), so it is unit-agnostic / self-calibrating.
plausibility_ceilings <- function(swc_long, q = 0.999, sanity_mult = 50) {
  swc_long |>
    dplyr::filter(is.finite(value)) |>
    dplyr::group_by(analyte) |>
    dplyr::summarise(
      .groups = "drop",
      med  = stats::median(value, na.rm = TRUE),
      p999 = stats::quantile(value, q, na.rm = TRUE, names = FALSE),
      sanity  = ifelse(is.finite(med) & med > 0, abs(med) * sanity_mult, Inf),
      # ceiling = larger of the two guards; a value must clear BOTH to be flagged,
      # which is exactly value > max(p999, sanity).
      ceiling = pmax(p999, sanity, na.rm = TRUE)
    )
}
# Named lookup analyte -> ceiling for is_plausible().
ceiling_map <- function(ceil_tbl) setNames(ceil_tbl$ceiling, ceil_tbl$analyte)
# Is each value plausible? TRUE = keep. NA/non-finite are treated as keep (they
# drop out of finite filters downstream anyway). `ceil_map` is a named vector
# analyte -> ceiling from ceiling_map(plausibility_ceilings()).
is_plausible <- function(value, analyte, ceil_map) {
  ceil <- ceil_map[as.character(analyte)]
  ok <- !is.finite(value) | is.na(ceil) | (value <= ceil)
  ok[is.na(ok)] <- TRUE
  unname(ok)
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
PRESETS <- list(
  "Ionic strength ↔ buffering (flagship)" = c("specificConductanceField", "ANC"),
  "Ionic strength ↔ calcium"               = c("specificConductanceField", "Ca"),
  "Inorganic vs total dissolved N"             = c("TDN", "NO3+NO2 - N"),
  "Total vs bioavailable phosphorus"           = c("TP", "Ortho - P"),
  "DOM quantity ↔ quality (SUVA)"          = c("DOC", "UV Absorbance (254 nm)"),
  "Carbonate system (alkalinity ↔ pH)"     = c("HCO3", "pH"),
  "Redox metals (iron ↔ manganese)"        = c("Fe", "Mn")
)
PRESET_REASON <- c(
  "Ionic strength ↔ buffering (flagship)" = "Specific conductance integrates all dissolved ions; in carbonate terrain it tracks ANC tightly, the README's Sycamore Creek example.",
  "Ionic strength ↔ calcium"               = "Conductance should rise with the dominant cation; calcium is a primary driver of ionic strength in most streams.",
  "Inorganic vs total dissolved N"             = "Nitrate+nitrite is the oxidized fraction within total dissolved N; the relationship shows how much of the N pool is inorganic.",
  "Total vs bioavailable phosphorus"           = "Orthophosphate (SRP) is the bioavailable fraction of total P; the ratio diagnoses dissolved-reactive vs particle-bound P.",
  "DOM quantity ↔ quality (SUVA)"          = "UV absorbance per unit DOC (SUVA) indexes aromaticity / terrestrial origin of organic matter (Weishaar et al. 2003).",
  "Carbonate system (alkalinity ↔ pH)"     = "Bicarbonate dominance and pH are coupled through carbonate equilibria; together they define the buffering regime.",
  "Redox metals (iron ↔ manganese)"        = "Fe and Mn are both mobilized under low-oxygen conditions; they co-vary in groundwater-fed and wetland-influenced streams."
)

season_of <- function(d) factor(c("Winter","Spring","Summer","Fall")[(lubridate::month(d) %% 12) %/% 3 + 1],
                                 levels = c("Winter","Spring","Summer","Fall"))

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
    message("plot render error: ", conditionMessage(e))
    plotly_message("This view hit a snag. Nudge a control to reload.", mode)
  })
}

## ---- Real computations ---------------------------------------------------

# Correlation of `main` vs every other numeric column of a per-site wide frame.
# Reports n (paired), Pearson r, Spearman rho, p-value of the chosen method,
# and an honesty flag. min_n is the reliability floor (default 8).
# censor_map (named analyte -> fraction below detection) + censor_thresh let the
# table down-weight heavily-censored analytes (Br 57%, Mn/Fe/F ~34-39% below DL):
# a Spearman/Pearson there is dominated by detection-limit ties, not chemistry
# (Helsel 2012), so those rows are marked heavy_censor and greyed like low-n.
correlation_table <- function(wide_site, main, method = c("spearman","pearson"),
                              min_n = 8, censor_map = NULL, censor_thresh = 0.25) {
  method <- match.arg(method)
  if (is.null(wide_site) || !main %in% names(wide_site)) return(NULL)
  m <- suppressWarnings(as.numeric(wide_site[[main]]))
  num <- names(wide_site)[vapply(wide_site, is.numeric, logical(1))]
  others <- setdiff(num, main)
  pct_bdl <- function(a) if (!is.null(censor_map) && a %in% names(censor_map)) unname(censor_map[a]) else NA_real_
  ptest <- function(x, y, mth) suppressWarnings(tryCatch(stats::cor.test(x, y, method = mth)$p.value, error = function(e) NA_real_))
  rows <- lapply(others, function(a) {
    y <- suppressWarnings(as.numeric(wide_site[[a]]))
    ok <- is.finite(m) & is.finite(y)
    n  <- sum(ok)
    pe <- if (n >= 4) suppressWarnings(stats::cor(m[ok], y[ok], method = "pearson"))  else NA_real_
    sp <- if (n >= 4) suppressWarnings(stats::cor(m[ok], y[ok], method = "spearman")) else NA_real_
    pp <- if (n >= 4) ptest(m[ok], y[ok], "pearson")  else NA_real_
    ps <- if (n >= 4) ptest(m[ok], y[ok], "spearman") else NA_real_
    ties <- n >= 4 && (any(duplicated(m[ok])) || any(duplicated(y[ok])))
    tibble(code = a, display = analyte_display(a), n = n,
           pearson = pe, spearman = sp, p_pearson = pp, p_spearman = ps, ties = ties,
           pct_below = pct_bdl(a))
  })
  out <- bind_rows(rows)
  if (!nrow(out)) return(out)
  out$coef <- if (method == "spearman") out$spearman else out$pearson
  out$p    <- if (method == "spearman") out$p_spearman else out$p_pearson
  out <- out |> filter(n >= 4, is.finite(coef)) |>
    mutate(heavy_censor = is.finite(pct_below) & pct_below >= censor_thresh,
           # reliable drives the grey-out: a row is reliable only if it clears the
           # n floor AND isn't dominated by below-detection ties.
           reliable = n >= min_n & !heavy_censor,
           flag = dplyr::case_when(
             n < min_n            ~ sprintf("insufficient (n=%d)", n),
             heavy_censor         ~ sprintf("censored (%.0f%% <DL)", 100 * pct_below),
             coef >=  0.7         ~ "strong +",
             coef <= -0.7         ~ "strong −",
             abs(coef) >= 0.4     ~ "moderate",
             TRUE                 ~ "weak/none")) |>
    arrange(desc(abs(coef)))
  out
}

# OLS fit of paired (x, y) with the stats a showcase should surface.
# If df carries collectDate, rows are time-ordered first so the lag-1 residual
# autocorrelation is meaningful (it's nonsense on arbitrary row order).
fit_lm <- function(df) {
  if (is.null(df) || nrow(df) < 3) return(NULL)
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
