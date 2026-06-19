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
base_obs   <- as.numeric(base$n_obs   %||% 0)
base_sites <- as.numeric(base$n_sites %||% 0)
cat(sprintf("Baseline (committed): %d obs, %d sites, through %s\n",
            base_obs, base_sites, base$data_through %||% "—"))

# Canonical pull + build (downloads, caches, rebuilds data/neon_swc.rds + coverage).
source(file.path("scripts", "precompute_neon_data.R"))

b <- tryCatch(readRDS(OUT)$built, error = function(e) NULL)
if (is.null(b)) stop("Refresh produced no readable data/neon_swc.rds — aborting.")
cat(sprintf("Rebuilt: %d obs, %d sites, partial=%s, through %s\n",
            b$n_obs, b$n_sites, isTRUE(b$partial), b$data_through %||% "—"))

# Completeness guard: refuse to ship a bundle that lost a meaningful share of the
# record (mass API failure). A few missing site-months are fine; a collapse is not.
# Floors are relative to the committed baseline so they self-adjust as data grows.
floor_obs   <- as.integer(0.90 * base_obs)
floor_sites <- max(28L, as.integer(0.90 * base_sites))   # 34 sites; tolerate a couple offline
if (b$n_obs < floor_obs)
  stop(sprintf("Only %d obs (< %d = 90%% of the committed %d) — aborting before commit so a mass NEON-pull failure can't ship a shrunken bundle.",
               b$n_obs, floor_obs, base_obs))
if (b$n_sites < floor_sites)
  stop(sprintf("Only %d sites (< %d) — aborting before commit so a mass NEON-pull failure can't ship a shrunken bundle.",
               b$n_sites, floor_sites))
if (isTRUE(b$partial))
  cat("NOTE: pull flagged partial (a site/month was missing) but counts cleared the guard — shipping.\n")

cat("Guard passed — bundle is complete enough to ship.\n")
