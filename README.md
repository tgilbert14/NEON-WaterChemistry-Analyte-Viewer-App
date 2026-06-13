# NEON Surface Water Chemistry — Analyte Viewer

An interactive R/Shiny app for exploring **water-chemistry analytes** measured across
[NEON](https://www.neonscience.org/) aquatic field sites. Pick a site, a date range, and two
analytes — then explore how they move together through time, how they relate, what else they
correlate with, their seasonal pattern, and a quick machine-learning estimate.

Built on the **Surface Water Chemistry** product
[(DP1.20093.001)](https://data.neonscience.org/data-products/DP1.20093.001).

> Rebuilt 2026 — modern [bslib](https://rstudio.github.io/bslib/) UI, real NEON data bundled for
> instant loads, and statistics that show their work. (The original 2021 shinydashboard version
> is preserved in [`legacy/`](legacy/).)

---

## What it does

| Tab | What you get |
|-----|--------------|
| **Compare** | Two analytes through time. *Normalized* mode (z-scores on one axis) compares **shape** honestly; *Dual axis (raw)* shows original units with an explicit "independent axes" caveat. Below-detection samples are drawn as open markers. |
| **Relationship** | Ordinary-least-squares regression of the two analytes with a 95% confidence band, R² / adjusted R² / slope / p-value / n, and a temporal-autocorrelation flag. |
| **Correlations** | The main analyte vs every other analyte as a lollipop (anchored at 0) plus a sortable table. Spearman by default, Pearson alongside, **n shown per row**, and rows with n < 8 flagged as unreliable. |
| **Seasonal** | Monthly climatology (box-and-jitter) and a real **STL** seasonal-trend decomposition of the actual monthly record. No fabricated forecasts. |
| **Predictor** | A `glm` that estimates the main analyte from its three best-correlated analytes, with cross-validated RMSE. Move the sliders for a live estimate. |
| **Data** | The wide table behind every tab, with CSV / Excel download for exactly the current site and range. |
| **Site map** | All NEON aquatic sites; the selected one highlighted. |

The flagship default view — **Sycamore Creek, AZ (SYCA): specific conductance vs ANC** — loads a
real, strong relationship on first visit (Pearson r ≈ 0.86 over the full record).

---

## Architecture

```
app.R                         # the whole app: bslib UI + server
helpers.R                     # analyte names/units, colors, theme + chart helpers,
                              #   correlation_table(), fit_lm(), kfold_rmse()
data/neon_swc.rds             # REAL NEON SWC, precomputed (loaded once at startup)
scripts/precompute_neon_data.R  # pulls SWC from the NEON public API (resumable, cached)
scripts/build_rds_from_cache.R  # builds data/neon_swc.rds from the cache
legacy/                       # the original 2021 app + old data files
```

**Why a precomputed `.rds`?** The original app called `neonUtilities::loadByProduct()` on every
selection — slow, network-dependent, and (per the original author's own note) it blocked
publishing to shinyapps.io. The rebuild pulls the data **once** from the NEON public API into a
compact bundle the app loads instantly. No runtime NEON calls, no `neonUtilities` dependency.

`data/neon_swc.rds` is a list:

| Object | Shape |
|--------|-------|
| `swc_long` | `site, collectDate (Date), analyte, value, units, source ("External Lab"/"Field Probe"), belowDetection (0/1)` |
| `swc_wide` | one row per `site × collectDate`, one numeric column per analyte |
| `sites_meta` | `site, siteName, domain, state, lat, long, n_obs, n_analytes, first, last` |
| `analyte_meta` | per-analyte coverage (`units, n, n_sites, source`) |
| `built` | provenance: build timestamp, product code, totals |

### Refreshing the data

```sh
# from the project root, with R on PATH:
Rscript scripts/precompute_neon_data.R     # pulls/refreshes the cache (resumable)
Rscript scripts/build_rds_from_cache.R     # rebuilds data/neon_swc.rds from the cache
```

The cache (`data/.neon_cache/`) is git-ignored; the built `data/neon_swc.rds` is committed.

---

## Running locally

```r
# install once:
install.packages(c("shiny","bslib","bsicons","plotly","DT","ggplot2",
                   "dplyr","tidyr","readr","lubridate","shinycssloaders"))
# then, from the project root:
shiny::runApp("app.R")
```

## Honesty notes (by design)

- **Every statistic shows its sample size.** Correlations default to Spearman (robust to the
  skew typical of water chemistry), guard at n ≥ 8, and print a multiple-comparisons caveat.
- **The seasonal view is a real STL decomposition** of the measured monthly series — not the
  synthetic sine-wave "forecast" the original app shipped.
- **Analyte names and units are chemically correct** (e.g. Br = bromide, not bicarbonate;
  Cl = chloride; ANC in meq/L; pH unitless) and units are read from the data, not hard-coded.
- **Below-detection values are flagged, not hidden.**
- Regression carries explicit caveats: correlation ≠ causation, and repeated-measures p-values
  are optimistic (a lag-1 autocorrelation flag quantifies it).

## Credits

Original app and concept: **Timothy Gilbert** (tsgilbert@arizona.edu). 2026 rebuild data
product: NEON DP1.20093.001 (National Ecological Observatory Network, operated by Battelle,
funded by NSF).
