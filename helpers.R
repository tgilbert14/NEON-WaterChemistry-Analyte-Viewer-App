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

## ---- Colors (Okabe-Ito CVD-safe; brand teal primary) ---------------------
COL <- list(
  main      = "#0E7C9B",   # main analyte (brand teal)
  secondary = "#D55E00",   # secondary analyte (Okabe-Ito vermillion)
  good      = "#2E8B6F",   # strong relationship
  warn      = "#C98A1E",   # caveat amber
  grid_l    = "rgba(0,0,0,.07)", grid_d = "rgba(255,255,255,.10)",
  bg_l = "#ffffff", bg_d = "#161d26", fg_l = "#13242c", fg_d = "#e8eef2"
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
  "Ionic strength ↔ buffering (flagship)" = "Specific conductance integrates all dissolved ions; in carbonate terrain it tracks ANC tightly — the README's Sycamore Creek example.",
  "Ionic strength ↔ calcium"               = "Conductance should rise with the dominant cation; calcium is a primary driver of ionic strength in most streams.",
  "Inorganic vs total dissolved N"             = "Nitrate+nitrite is the oxidized fraction within total dissolved N — the relationship shows how much of the N pool is inorganic.",
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
      panel.grid.major = ggplot2::element_line(color = "rgba(0,0,0,.07)"),
      axis.title    = ggplot2::element_text(color = "#3c4757"),
      legend.position = "bottom")
}

# `big = TRUE` (narrow / phone viewport) bumps font so chart text is legible on a phone
plotly_theme <- function(p, mode = "light", big = FALSE) {
  dark <- identical(mode, "dark")
  bg <- if (dark) COL$bg_d else COL$bg_l
  fg <- if (dark) COL$fg_d else COL$fg_l
  grid <- if (dark) COL$grid_d else COL$grid_l
  plotly::layout(p,
    paper_bgcolor = bg, plot_bgcolor = bg,
    font = list(family = "Inter", color = fg, size = if (big) 15 else 12),
    hoverlabel = list(font = list(size = if (big) 15 else 12)),
    xaxis = list(gridcolor = grid, zerolinecolor = grid),
    yaxis = list(gridcolor = grid, zerolinecolor = grid))
}

plotly_clean <- function(p, filename = "neon_analyte_chart") {
  plotly::config(p, displaylogo = FALSE, responsive = TRUE,
    modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d",
                               "hoverClosestCartesian","hoverCompareCartesian","toggleSpikelines"),
    toImageButtonOptions = list(format = "png", scale = 3, filename = filename))
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
    plotly_message("This view hit a snag — nudge a control to reload.", mode)
  })
}

## ---- Real computations ---------------------------------------------------

# Correlation of `main` vs every other numeric column of a per-site wide frame.
# Reports n (paired), Pearson r, Spearman rho, p-value of the chosen method,
# and an honesty flag. min_n is the reliability floor (default 8).
correlation_table <- function(wide_site, main, method = c("spearman","pearson"), min_n = 8) {
  method <- match.arg(method)
  if (is.null(wide_site) || !main %in% names(wide_site)) return(NULL)
  m <- suppressWarnings(as.numeric(wide_site[[main]]))
  num <- names(wide_site)[vapply(wide_site, is.numeric, logical(1))]
  others <- setdiff(num, main)
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
           pearson = pe, spearman = sp, p_pearson = pp, p_spearman = ps, ties = ties)
  })
  out <- bind_rows(rows)
  if (!nrow(out)) return(out)
  out$coef <- if (method == "spearman") out$spearman else out$pearson
  out$p    <- if (method == "spearman") out$p_spearman else out$p_pearson
  out <- out |> filter(n >= 4, is.finite(coef)) |>
    mutate(reliable = n >= min_n,
           flag = dplyr::case_when(
             n < min_n            ~ sprintf("insufficient (n=%d)", n),
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
