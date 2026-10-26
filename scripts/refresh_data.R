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
FETCH_RECEIPT <- file.path("data", "precompute_fetch_receipt.json")

# Pure helpers (tests/test_precompute_fetch_contract.R loads them by name).

# Read the precompute fetch receipt; NULL when it is missing or unreadable.
water_read_fetch_receipt <- function(path) {
  if (!file.exists(path)) return(NULL)
  r <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                error = function(e) NULL)
  if (!is.list(r)) return(NULL)
  for (nm in c("misses", "file_hosts", "lab_columns")) {
    r[[nm]] <- as.character(unlist(r[[nm]]))
  }
  r
}

# Miss-rate gate: a warning for any missed site-month, an error above 1%.
water_fetch_receipt_gate <- function(receipt, max_share = 0.01) {
  if (is.null(receipt)) {
    return(list(warning = NULL,
                error = "The precompute fetch receipt is missing; the miss rate cannot be checked."))
  }
  n_sm <- as.numeric(receipt$n_site_months %||% NA)
  n_miss <- as.numeric(receipt$n_miss %||% NA)
  if (length(n_sm) != 1L || length(n_miss) != 1L || is.na(n_sm) ||
      is.na(n_miss) || n_sm < 0 || n_miss < 0) {
    return(list(warning = NULL,
                error = "The precompute fetch receipt has no valid site-month counts."))
  }
  warn <- if (n_miss > 0) sprintf(
    "%d of %d NEON site-months could not be fetched (first: %s).",
    as.integer(n_miss), as.integer(n_sm),
    paste(utils::head(receipt$misses, 5L), collapse = "; ")
  ) else NULL
  err <- if (n_miss > max_share * n_sm) sprintf(
    "%d of %d NEON site-months missed, more than %.0f%%; refusing the refresh.",
    as.integer(n_miss), as.integer(n_sm), 100 * max_share
  ) else NULL
  list(warning = warn, error = err)
}

# Job-summary lines for the refresh (plain text, one fact per line).
water_refresh_summary_lines <- function(receipt, base, built) {
  fmt <- function(x) if (is.null(x) || !length(x) || is.na(x[[1]])) "not recorded" else as.character(x[[1]])
  qr <- built$value_quarantine_receipt
  c(
    "### Water chemistry refresh",
    sprintf("- NEON site-months listed: %s", fmt(receipt$n_site_months)),
    sprintf("- Site-months fetched this run: %s", fmt(receipt$n_fetched)),
    sprintf("- Site-months missed: %s", fmt(receipt$n_miss)),
    sprintf("- Blank-like lab rows (sample IDs naming a blank, kept): %s",
            fmt(receipt$n_blank_like_rows)),
    sprintf("- File hosts: %s", if (length(receipt$file_hosts))
      paste(receipt$file_hosts, collapse = ", ") else "none"),
    sprintf("- Observations: %s to %s", fmt(base$n_obs), fmt(built$n_obs)),
    sprintf("- Data through: %s to %s", fmt(base$data_through),
            fmt(built$data_through)),
    sprintf("- Fe/Mn value quarantine: %s source rows over %s identities",
            fmt(built$n_value_rows_quarantined),
            if (is.data.frame(qr)) as.character(sum(qr$n_excluded > 0L)) else "not recorded")
  )
}

water_write_step_summary <- function(lines) {
  path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
  if (nzchar(path)) cat(lines, file = path, sep = "\n", append = TRUE)
  invisible(nzchar(path))
}

# Content key: TRUE when the rebuild matches the committed bundle in every field
# but its producer timestamp built$when.
water_same_content <- function(candidate, base) {
  if (!is.list(candidate) || !is.list(base) ||
      !is.list(candidate$built) || !is.list(base$built)) return(FALSE)
  candidate$built$when <- "<NORMALIZED>"
  base$built$when <- "<NORMALIZED>"
  identical(candidate, base)
}

# Baseline from the CURRENTLY COMMITTED bundle (on disk before we overwrite it),
# so the guard is relative to what we already ship rather than a magic number.
base <- tryCatch(readRDS(OUT)$built, error = function(e) NULL)
base_bundle <- tryCatch(readRDS(OUT), error = function(e) NULL)
# The committed bytes, put back below when the rebuild's content is unchanged.
base_bytes <- if (file.exists(OUT)) readBin(OUT, "raw", file.size(OUT)) else raw()
base_obs   <- as.numeric(base$n_obs   %||% 0)
base_sites <- as.numeric(base$n_sites %||% 0)
base_roster <- sort(unique(as.character(base_bundle$sites_meta$site)))
if (!length(base_roster) || anyNA(base_roster) || any(!nzchar(base_roster)))
  stop("Committed bundle has no trustworthy site roster; refusing an unauditable refresh.")
cat(sprintf("Baseline (committed): %d obs, %d sites, through %s\n",
            base_obs, base_sites, base$data_through %||% "none"))

# Canonical pull + build (downloads, caches, rebuilds data/neon_swc.rds + coverage).
# A build stop still writes what the fetch receipt knows to the job summary.
if (file.exists(FETCH_RECEIPT)) unlink(FETCH_RECEIPT)   # never read a stale one
built_ok <- FALSE
tryCatch({
  source(file.path("scripts", "precompute_neon_data.R"))
  built_ok <- TRUE
}, finally = if (!built_ok) {
  water_write_step_summary(c(
    water_refresh_summary_lines(water_read_fetch_receipt(FETCH_RECEIPT) %||% list(),
                                base, list()),
    "- The refresh stopped before a bundle was built; the committed bundle is unchanged."
  ))
})

b <- tryCatch(readRDS(OUT)$built, error = function(e) NULL)
if (is.null(b)) stop("Refresh produced no readable data/neon_swc.rds: aborting.")

fetch_receipt <- water_read_fetch_receipt(FETCH_RECEIPT)
water_write_step_summary(water_refresh_summary_lines(
  fetch_receipt %||% list(), base, b
))
gate <- water_fetch_receipt_gate(fetch_receipt)
if (!is.null(gate$warning)) cat(sprintf("::warning::%s\n", gate$warning))
if (!is.null(gate$error)) stop(gate$error)
cat(sprintf("Rebuilt: %d obs, %d sites, partial=%s, through %s\n",
            b$n_obs, b$n_sites, isTRUE(b$partial), b$data_through %||% "none"))

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

cat("Guard passed: exact roster retained, observations did not shrink, and candidate is not partial.\n")

# Content-key no-op: a rebuild that matches the committed bundle in every field
# but its timestamp gets the committed bytes back, and the codebook is rewritten
# from them, so an unchanged release rebuilds byte-identical and the publisher
# ends with "No validated data change" instead of pushing a review branch.
if (length(base_bytes) && water_same_content(candidate, base_bundle)) {
  writeBin(base_bytes, OUT)
  write_codebook(base_bundle, file.path("data", "codebook.csv"))
  cat(sprintf("Content unchanged from the committed bundle; kept its bytes and build time %s.\n",
              base_bundle$built$when))
}
