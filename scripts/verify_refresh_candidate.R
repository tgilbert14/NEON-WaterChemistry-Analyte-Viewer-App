# Verify a proposed surface-water-chemistry release using only base R and
# jsonlite. The optional first argument is the committed bundle captured before
# candidate extraction; it makes the independent validator enforce non-shrink
# and exact-roster behavior without trusting producer state.

fail <- function(...) stop(sprintf(...), call. = FALSE)
need <- function(ok, ...) if (!isTRUE(ok)) fail(...)

required_paths <- c(
  "app.R", "helpers.R", "data/codebook.csv",
  "data/neon_swc.rds", "data/search_index.rds", "manifest.json"
)
missing_paths <- required_paths[!file.exists(required_paths)]
need(!length(missing_paths), "Missing required candidate path(s): %s",
     paste(missing_paths, collapse = ", "))

expected_sites <- sort(c(
  "ARIK", "BARC", "BIGC", "BLDE", "BLUE", "BLWA", "CARI", "COMO", "CRAM",
  "CUPE", "FLNT", "GUIL", "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART",
  "MAYF", "MCDI", "MCRA", "OKSR", "POSE", "PRIN", "PRLA", "PRPO", "REDB",
  "SUGG", "SYCA", "TECR", "TOMB", "TOOK", "WALK", "WLOU"
))

bundle <- tryCatch(readRDS("data/neon_swc.rds"), error = function(e) fail(
  "Cannot read data/neon_swc.rds: %s", conditionMessage(e)
))
need(identical(names(bundle), c("swc_long", "swc_wide", "sites_meta",
                                "analyte_meta", "built")),
     "Bundle top-level schema changed.")
need(is.data.frame(bundle$swc_long) && nrow(bundle$swc_long) > 0,
     "swc_long is empty or invalid.")
need(all(c("site", "collectDate", "analyte", "value", "belowDetection",
           "units", "source") %in% names(bundle$swc_long)),
     "swc_long is missing required columns.")
need(is.data.frame(bundle$sites_meta) && is.data.frame(bundle$analyte_meta),
     "Bundle metadata tables are invalid.")

site_roster <- sort(unique(as.character(bundle$sites_meta$site)))
observation_sites <- sort(unique(as.character(bundle$swc_long$site)))
need(identical(site_roster, expected_sites),
     "Candidate site roster is not the canonical 34-site contract.")
need(identical(observation_sites, expected_sites),
     "Observation table does not cover the canonical 34-site contract.")
need(!anyNA(bundle$swc_long$site) && !anyNA(bundle$swc_long$analyte),
     "Observation identity contains missing site or analyte values.")
need(is.list(bundle$built), "Bundle build receipt is missing.")
need(identical(as.integer(bundle$built$n_obs), nrow(bundle$swc_long)),
     "Build receipt n_obs does not match swc_long.")
need(identical(as.integer(bundle$built$n_sites), length(expected_sites)),
     "Build receipt n_sites is not 34.")
need(!isTRUE(bundle$built$partial), "Candidate build receipt is partial.")
need(identical(as.character(bundle$built$product), "DP1.20093.001"),
     "Unexpected NEON product in build receipt.")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) && nzchar(args[[1]])) {
  baseline <- tryCatch(readRDS(args[[1]]), error = function(e) fail(
    "Cannot read validation baseline: %s", conditionMessage(e)
  ))
  baseline_sites <- sort(unique(as.character(baseline$sites_meta$site)))
  need(identical(site_roster, baseline_sites),
       "Candidate site roster differs from the committed baseline.")
  need(nrow(bundle$swc_long) >= nrow(baseline$swc_long),
       "Candidate observation table shrank from %d to %d rows.",
       nrow(baseline$swc_long), nrow(bundle$swc_long))
  old_through <- as.Date(baseline$built$data_through)
  new_through <- as.Date(bundle$built$data_through)
  need(!is.na(old_through) && !is.na(new_through) && new_through >= old_through,
       "Candidate data-through date regressed.")
}

index <- tryCatch(readRDS("data/search_index.rds"), error = function(e) fail(
  "Cannot read data/search_index.rds: %s", conditionMessage(e)
))
need(is.list(index) && is.data.frame(index$per_site) &&
       is.data.frame(index$analytes), "Search index schema is invalid.")
need(identical(sort(unique(as.character(index$per_site$site))), expected_sites),
     "Search index site roster is incomplete.")
need(identical(sort(unique(as.character(index$per_site$analyte))),
               sort(unique(as.character(bundle$swc_long$analyte)))),
     "Search index analyte roster disagrees with the source bundle.")
need(identical(as.integer(index$n_sites), length(expected_sites)),
     "Search index n_sites is not 34.")
need(identical(as.character(index$built$when),
               as.character(bundle$built$when)),
     "Search index provenance is not derived from the source bundle.")

if (!requireNamespace("jsonlite", quietly = TRUE))
  fail("jsonlite is required to verify manifest.json.")
manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
manifest_files <- names(manifest$files)
expected_manifest_files <- sort(setdiff(required_paths, "manifest.json"))
need(identical(sort(manifest_files), expected_manifest_files),
     "Manifest runtime file allowlist changed.")
for (path in manifest_files) {
  actual <- unname(tools::md5sum(path))
  declared <- manifest$files[[path]]$checksum
  need(identical(actual, declared),
       "Manifest checksum mismatch for %s.", path)
}
packages <- names(manifest$packages)
need(!any(tolower(packages) %in% c("neonutilities", "arrow")),
     "Data-pull packages leaked into the runtime manifest.")
need(all(c("shiny", "dplyr", "ggplot2", "plotly", "leaflet", "lubridate") %in% packages),
     "Manifest is missing required runtime packages.")
need("users" %in% names(manifest), "Manifest lacks canonical users metadata.")
manifest_text <- paste(readLines("manifest.json", warn = FALSE), collapse = "\n")
need(!grepl("cloud[.]r-project[.]org|cran[.]rstudio[.]com|cran/(?:__linux__/jammy/)?latest",
            manifest_text, perl = TRUE),
     "Manifest contains a moving package repository.")
need(grepl("packagemanager[.]posit[.]co/cran/__linux__/jammy/2026-07-15",
           manifest_text),
     "Manifest does not carry the pinned 2026-07-15 package snapshot.")

cat(sprintf(
  "Verified water-chemistry candidate: %d observations, %d analytes, %d sites, through %s.\n",
  nrow(bundle$swc_long), length(unique(bundle$swc_long$analyte)),
  length(site_roster), bundle$built$data_through
))
