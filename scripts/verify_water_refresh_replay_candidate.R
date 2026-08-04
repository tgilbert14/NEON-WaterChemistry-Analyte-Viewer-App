# Independently rebuild the scientific Water candidate from the signed replay
# artifact and require exact bundle equality apart from the producer timestamp.
# Run from the repository root after extracting the producer candidate:
#   Rscript --vanilla scripts/verify_water_refresh_replay_candidate.R \
#     /path/to/water-refresh-review <exact-source-sha>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || anyNA(args) || any(!nzchar(args))) {
  stop(paste0(
    "Usage: verify_water_refresh_replay_candidate.R ",
    "<review-directory> <exact-source-sha>"
  ), call. = FALSE)
}

review_dir <- normalizePath(args[[1]], mustWork = TRUE)
expected_source_sha <- args[[2]]

source(file.path("scripts", "water_refresh_review.R"))
source(file.path("scripts", "build_swc_bundle.R"))

receipt <- validate_water_refresh_review(
  review_dir, expected_source_sha = expected_source_sha
)
if (!identical(
  water_review_exact_integer(
    receipt$n_unapproved_identities[[1]], "n_unapproved_identities"
  ),
  0L
)) {
  stop("Refresh replay contains unapproved unit-review identities.",
       call. = FALSE)
}

lab_raw <- readRDS(file.path(
  review_dir, WATER_REFRESH_REVIEW_CONTENT_FILES[["lab_raw"]]
))
field_raw <- readRDS(file.path(
  review_dir, WATER_REFRESH_REVIEW_CONTENT_FILES[["field_raw"]]
))
coords <- readRDS(file.path(
  review_dir, WATER_REFRESH_REVIEW_CONTENT_FILES[["coords"]]
))

candidate_path <- file.path("data", "neon_swc.rds")
candidate <- readRDS(candidate_path)
validate_bundle(candidate)
producer_when <- candidate$built$when
if (!is.character(producer_when) || length(producer_when) != 1L ||
    is.na(producer_when) ||
    !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
           producer_when, perl = TRUE)) {
  stop("Candidate producer timestamp is malformed.", call. = FALSE)
}

rebuilt <- build_swc_bundle(lab_raw, field_raw, coords, partial = FALSE)
rebuilt$built$when <- producer_when
if (!identical(rebuilt, candidate)) {
  stop(paste0(
    "Candidate scientific bundle does not exactly reproduce from the ",
    "signed replay after normalizing only its producer timestamp."
  ), call. = FALSE)
}

runtime_result <- canonicalize_runtime_water_units(candidate$swc_long)
if (!identical(runtime_result$n_collapsed_rows_excluded, 0L) ||
    !identical(runtime_result$n_source_rows_excluded, 0L) ||
    !identical(runtime_result$n_missing_labels_rewritten, 0L)) {
  stop("Producer candidate is not canonical at the runtime unit boundary.",
       call. = FALSE)
}
if (!identical(candidate$built$n_unit_values_changed, 0L)) {
  stop("Producer candidate reports a numeric unit conversion.", call. = FALSE)
}

candidate_sha256 <- digest::digest(
  file = candidate_path, algo = "sha256"
)
cat(sprintf(
  paste0(
    "Independently replayed Water candidate: %d observations, %d analytes, ",
    "%d sites, through %s; %d missing-label rewrites, %d source rows ",
    "excluded, 0 value changes, runtime 0/0; bundle SHA-256 %s.\n"
  ),
  candidate$built$n_obs, candidate$built$n_analytes,
  candidate$built$n_sites, candidate$built$data_through,
  candidate$built$n_unit_labels_rewritten,
  candidate$built$n_unit_rows_excluded, candidate_sha256
))
