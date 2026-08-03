#----------------------------------------------------------------------
# refresh_data.R — CI entry point for the monthly auto-refresh.
#
# Reuses the canonical pull+build (scripts/precompute_neon_data.R, which hits the
# NEON public API directly and writes data/neon_swc.rds via the single shared
# build_swc_bundle()), then GUARDS the result so a bad-API day can't ship a
# shrunken bundle. A guard failure stop()s the run, so the workflow's PR step
# never opens and the committed bundle is left intact.
#
# Run from the project root:  Rscript scripts/refresh_data.R
#----------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

OUT <- file.path("data", "neon_swc.rds")

# Baseline from the CURRENTLY COMMITTED bundle (on disk before we overwrite it),
# so the guard is relative to what we already ship rather than a magic number.
base <- tryCatch(readRDS(OUT)$built, error = function(e) NULL)
base_bundle <- tryCatch(readRDS(OUT), error = function(e) NULL)
base_obs   <- as.numeric(base$n_obs   %||% 0)
base_sites <- as.numeric(base$n_sites %||% 0)
base_roster <- sort(unique(as.character(base_bundle$sites_meta$site)))
if (!length(base_roster) || anyNA(base_roster) || any(!nzchar(base_roster)))
  stop("Committed bundle has no trustworthy site roster; refusing an unauditable refresh.")
cat(sprintf("Baseline (committed): %d obs, %d sites, through %s\n",
            base_obs, base_sites, base$data_through %||% "—"))

# Canonical pull + build (downloads, caches, rebuilds data/neon_swc.rds + coverage).
source(file.path("scripts", "precompute_neon_data.R"))

b <- tryCatch(readRDS(OUT)$built, error = function(e) NULL)
if (is.null(b)) stop("Refresh produced no readable data/neon_swc.rds — aborting.")
cat(sprintf("Rebuilt: %d obs, %d sites, partial=%s, through %s\n",
            b$n_obs, b$n_sites, isTRUE(b$partial), b$data_through %||% "—"))

# Completeness guard: a scheduled candidate must retain the exact committed
# network roster and may not silently lose observations. A legitimate NEON
# correction that contracts history is reviewable, but never auto-promotable.
candidate <- readRDS(OUT)
candidate_roster <- sort(unique(as.character(candidate$sites_meta$site)))
if (!identical(candidate_roster, base_roster)) {
  stop(sprintf(
    "Candidate roster differs from the committed %d-site contract (missing: %s; added: %s).",
    length(base_roster),
    paste(setdiff(base_roster, candidate_roster), collapse = ", "),
    paste(setdiff(candidate_roster, base_roster), collapse = ", ")
  ))
}
if (b$n_obs < base_obs)
  stop(sprintf("Candidate has %d observations, fewer than committed baseline %d; refusing automatic publication.",
               b$n_obs, base_obs))
if (isTRUE(b$partial))
  stop("Candidate is flagged partial; refusing automatic publication.")

cat("Guard passed — exact roster retained, observations did not shrink, and candidate is not partial.\n")
