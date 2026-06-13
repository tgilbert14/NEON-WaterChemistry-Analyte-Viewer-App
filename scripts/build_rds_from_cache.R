#----------------------------------------------------------------------
# build_rds_from_cache.R  —  builds data/neon_swc.rds from cached NEON CSVs
# (written by precompute_neon_data.R). NO data downloads — safe to run anytime,
# including while the background pull continues. Fetches only lightweight site
# coordinates. All assembly is delegated to scripts/build_swc_bundle.R.
#----------------------------------------------------------------------
suppressWarnings(suppressMessages({ library(dplyr); library(readr); library(jsonlite) }))
options(timeout = 60)
ROOT <- getwd()
source(file.path(ROOT, "scripts", "build_swc_bundle.R"))   # build_swc_bundle(), SITE_LABELS, save_bundle()
CACHE_DIR <- file.path(ROOT, "data", ".neon_cache")

rd <- function(f) suppressWarnings(suppressMessages(read_csv(f, show_col_types = FALSE)))

a_files <- list.files(CACHE_DIR, pattern = "_analyte\\.csv$", full.names = TRUE)
p_files <- list.files(CACHE_DIR, pattern = "_parent\\.csv$",  full.names = TRUE)
cat(sprintf("cache: %d analyte files, %d parent files\n", length(a_files), length(p_files)))
if (!length(a_files)) stop("No cached analyte CSVs yet — let the pull run a bit longer.")

lab_raw <- bind_rows(lapply(a_files, function(f) {
  d <- tryCatch(rd(f), error = function(e) NULL); if (is.null(d) || !nrow(d)) return(NULL)
  if (!all(c("siteID","collectDate","analyte","analyteConcentration") %in% names(d))) return(NULL)
  tibble(site = d$siteID, collectDate = d$collectDate, analyte = d$analyte,
         analyteConcentration = d$analyteConcentration, analyteUnits = d$analyteUnits,
         belowDetectionQF = if ("belowDetectionQF" %in% names(d)) d$belowDetectionQF else NA,
         externalLabDataQF = if ("externalLabDataQF" %in% names(d)) d$externalLabDataQF else NA)
}))

field_raw <- bind_rows(lapply(p_files, function(f) {
  d <- tryCatch(rd(f), error = function(e) NULL); if (is.null(d) || !nrow(d)) return(NULL)
  keep <- intersect(c("siteID","collectDate","waterTemp","dissolvedOxygen","specificConductance"), names(d))
  if (!all(c("siteID","collectDate") %in% keep)) return(NULL)
  out <- d[, keep, drop = FALSE]; names(out)[names(out) == "siteID"] <- "site"; out
}))

# Lightweight coordinates for sites we actually have data for
present <- sort(unique(lab_raw$site))
getJSON <- function(u) tryCatch(jsonlite::fromJSON(paste(readLines(url(u), warn = FALSE), collapse = "")),
                                error = function(e) NULL)
coords <- bind_rows(lapply(present, function(s) {
  j <- getJSON(sprintf("https://data.neonscience.org/api/v0/sites/%s", s))
  if (is.null(j)) return(tibble(site = s, neonName = NA_character_, domain = NA_character_,
                                state = NA_character_, lat = NA_real_, long = NA_real_, siteType = NA_character_))
  d <- j$data
  tibble(site = s, neonName = d$siteName %||% NA, domain = d$domainCode %||% NA,
         state = d$stateCode %||% NA, lat = as.numeric(d$siteLatitude %||% NA),
         long = as.numeric(d$siteLongitude %||% NA), siteType = d$siteType %||% NA)
}))

# A cache build is PARTIAL unless every one of the 34 sites is present
partial <- length(present) < length(SITE_LABELS)
bundle  <- build_swc_bundle(lab_raw, field_raw, coords, partial = partial)
save_bundle(bundle, file.path(ROOT, "data", "neon_swc.rds"))

# ship a data dictionary alongside the bundle (provenance for downstream users)
write_csv(bundle$analyte_meta, file.path(ROOT, "data", "analyte_coverage.csv"))
cat(sprintf("BUILT data/neon_swc.rds: obs=%d sites=%d analytes=%d below-detection=%d partial=%s\n",
            bundle$built$n_obs, bundle$built$n_sites, bundle$built$n_analytes,
            bundle$built$n_below, bundle$built$partial))
