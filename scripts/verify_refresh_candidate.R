# Verify a proposed surface-water-chemistry release using only base R and
# jsonlite. The optional first argument is the committed bundle captured before
# candidate extraction; it makes the independent validator enforce non-shrink
# and exact-roster behavior without trusting producer state.

fail <- function(...) stop(sprintf(...), call. = FALSE)
need <- function(ok, ...) if (!isTRUE(ok)) fail(...)
show_set <- function(x) if (length(x)) paste(x, collapse = ", ") else "<none>"

args <- commandArgs(trailingOnly = TRUE)
require_unit_contract <- "--require-current-unit-contract" %in% args
positional_args <- args[!startsWith(args, "--")]

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

if (length(positional_args) && nzchar(positional_args[[1]])) {
  baseline <- tryCatch(readRDS(positional_args[[1]]), error = function(e) fail(
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

# Legacy committed bundles predate the canonical-unit receipt, so code-only PR
# validation may exercise them while the exact-source fixture tests the new
# builder. Every real full-refresh candidate must carry and satisfy this receipt.
has_unit_contract <- !is.null(bundle$built$unit_policy)
if (require_unit_contract || has_unit_contract) {
  need(has_unit_contract,
       "Candidate lacks the required canonical-unit policy receipt.")
  need(identical(as.character(bundle$built$unit_policy),
                 "canonical-labels-value-invariant-v1"),
       "Candidate carries an unexpected canonical-unit policy: %s.",
       show_set(as.character(bundle$built$unit_policy)))
  need(identical(as.integer(bundle$built$n_unit_values_changed), 0L),
       "Candidate reports numeric value changes during unit canonicalization.")
  need(length(bundle$built$n_unit_labels_rewritten) == 1L &&
         is.finite(as.numeric(bundle$built$n_unit_labels_rewritten)) &&
         as.numeric(bundle$built$n_unit_labels_rewritten) >= 0,
       "Candidate has an invalid unit-label rewrite count.")

  units_by_analyte <- split(as.character(bundle$swc_long$units),
                            as.character(bundle$swc_long$analyte))
  noncanonical <- names(units_by_analyte)[!vapply(
    units_by_analyte,
    function(x) !anyNA(x) && all(nzchar(x)) && length(unique(x)) == 1L,
    logical(1)
  )]
  need(!length(noncanonical),
       "Candidate analytes lack one canonical non-missing unit: %s.",
       show_set(noncanonical))
}

if (!requireNamespace("jsonlite", quietly = TRUE))
  fail("jsonlite is required to verify manifest.json.")
manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
manifest_files <- names(manifest$files)
expected_manifest_files <- sort(setdiff(required_paths, "manifest.json"))
missing_manifest_files <- setdiff(expected_manifest_files, manifest_files)
unexpected_manifest_files <- setdiff(manifest_files, expected_manifest_files)
need(!length(missing_manifest_files) && !length(unexpected_manifest_files),
     paste0(
       "Manifest runtime file allowlist changed (missing: %s; ",
       "unexpected: %s)."
     ),
     show_set(missing_manifest_files), show_set(unexpected_manifest_files))
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
  paste0(
    "Verified water-chemistry candidate: %d observations, %d analytes, ",
    "%d sites, through %s; unit policy %s (%s label rewrites, 0 value changes).\n"
  ),
  nrow(bundle$swc_long), length(unique(bundle$swc_long$analyte)),
  length(site_roster), bundle$built$data_through,
  if (has_unit_contract) bundle$built$unit_policy else "legacy-not-required",
  if (has_unit_contract) bundle$built$n_unit_labels_rewritten else "n/a"
))
