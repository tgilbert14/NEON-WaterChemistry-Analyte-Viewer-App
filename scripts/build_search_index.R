# ===========================================================================
# build_search_index.R — precompute the small, bundled "Search the network"
# index from the COMMITTED bundle (data/neon_swc.rds). NO live fetch.
#
#   Rscript scripts/build_search_index.R
#
# Writes data/search_index.rds: a tiny list the app loads once at boot (like
# site_index) and filters in memory, so the search is instant and the app keeps
# its fast bundled load.
#
# This is an ANALYTE app (no taxa). The index holds, for every (analyte, site)
# pair, the app's honest per-site summary — mean / median / n / below-detection
# share / first & last year — computed on the SAME plausibility-gated long frame
# the app's map mean and stats tabs read from (helpers.R: plausibility_ceilings
# + is_plausible). That way a threshold query returns exactly the sites the rest
# of the app would, with no live recompute.
# ===========================================================================
suppressWarnings(suppressMessages({
  library(dplyr); library(tidyr)
}))

root <- tryCatch(dirname(dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))), error = function(e) NA)
if (is.na(root) || !nzchar(root)) root <- getwd()
# tolerate being run from scripts/ or repo root
if (!file.exists(file.path(root, "data", "neon_swc.rds")) &&
    file.exists(file.path(getwd(), "data", "neon_swc.rds"))) root <- getwd()
setwd(root)

source("helpers.R")   # canonical_units, plausibility_ceilings, ceiling_map,
                      # is_plausible, analyte_display, pretty_unit, ANALYTE_TBL

D <- readRDS("data/neon_swc.rds")
L <- D$swc_long

## ---- Apply the EXACT app-load gate so the index agrees with the app --------
CANON_MAP <- canonical_units(L)
CEIL_TBL  <- plausibility_ceilings(L)
CEIL_MAP  <- ceiling_map(CEIL_TBL)

L$units       <- unname(CANON_MAP[L$analyte]) %|na|% L$units
L$implausible <- !is_plausible(L$value, L$analyte, CEIL_MAP, site = L$site)

Lg <- L |> filter(!implausible, is.finite(value))   # the gated frame the stats read from

## ---- Per-(analyte, site) honest summary ------------------------------------
# below_detection share is reported (not used to drop rows) so a threshold over a
# heavily-censored analyte stays auditable. n is the gated sample count.
per_site <- Lg |>
  mutate(year = as.integer(format(collectDate, "%Y"))) |>
  group_by(analyte, site) |>
  summarise(
    .groups   = "drop",
    n         = dplyr::n(),
    mean      = mean(value, na.rm = TRUE),
    median    = stats::median(value, na.rm = TRUE),
    min       = min(value, na.rm = TRUE),
    max       = max(value, na.rm = TRUE),
    n_below   = sum(belowDetection == 1, na.rm = TRUE),
    year_min  = suppressWarnings(min(year, na.rm = TRUE)),
    year_max  = suppressWarnings(max(year, na.rm = TRUE))
  ) |>
  mutate(pct_below = ifelse(n > 0, n_below / n, NA_real_)) |>
  # join site display + analyte display/unit so the app renders without lookups
  left_join(D$sites_meta |> select(site, siteName, state, domain), by = "site") |>
  mutate(
    display = analyte_display(analyte),
    units   = unname(CANON_MAP[analyte]) %|na|% NA_character_,
    pretty  = pretty_unit(units, analyte)
  ) |>
  arrange(analyte, desc(mean))

## ---- The analyte catalogue (for the picker + units in the UI) --------------
analytes <- per_site |>
  group_by(analyte) |>
  summarise(
    .groups = "drop",
    display = first(display),
    units   = first(units),
    pretty  = first(pretty),
    n_sites = dplyr::n_distinct(site),
    n_obs   = sum(n),
    vmin    = min(min, na.rm = TRUE),
    vmax    = max(max, na.rm = TRUE)
  ) |>
  arrange(desc(n_obs))

idx <- list(
  per_site  = per_site,
  analytes  = analytes,
  n_sites   = dplyr::n_distinct(per_site$site),
  built     = list(
    when    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    product = D$built$product %||% "DP1.20093.001",
    source  = "data/neon_swc.rds (committed bundle; plausibility-gated)"
  )
)

saveRDS(idx, "data/search_index.rds", compress = "xz")

sz <- file.info("data/search_index.rds")$size
cat(sprintf("search_index.rds written: %s  |  %d (analyte,site) rows  |  %d analytes  |  %d sites\n",
            format(structure(sz, class = "object_size"), units = "auto"),
            nrow(per_site), nrow(analytes), idx$n_sites))
