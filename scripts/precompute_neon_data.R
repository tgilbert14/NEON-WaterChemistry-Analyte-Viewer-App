#----------------------------------------------------------------------
# precompute_neon_data.R
# Pulls REAL NEON Surface Water Chemistry (DP1.20093.001) directly from the
# NEON public API (no neonUtilities dependency) and bakes a compact .rds
# bundle the Shiny app loads instantly. Resumable: every site-month CSV is
# cached to disk, so re-runs skip what's already downloaded.
#
# Output: data/neon_swc.rds  (swc_long, swc_wide, sites_meta, analyte_meta, built)
#----------------------------------------------------------------------
suppressWarnings(suppressMessages({
  library(jsonlite); library(dplyr); library(tidyr); library(readr)
}))
options(timeout = 90, stringsAsFactors = FALSE)
`%||%`    <- function(a, b) if (is.null(a)) b else a

ROOT      <- getwd()                       # run from project root
CACHE_DIR <- file.path(ROOT, "data", ".neon_cache")
LOG       <- file.path(ROOT, "data", "precompute_log.txt")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
source(file.path(ROOT, "scripts", "build_swc_bundle.R"))  # SITE_LABELS, PRODUCT_CODE, build_swc_bundle(), save_bundle()
source(file.path(ROOT, "scripts", "water_refresh_review.R"))
PRODUCT <- PRODUCT_CODE
SITES   <- names(SITE_LABELS)

logmsg <- function(...) {
  line <- paste0(format(Sys.time(), "%H:%M:%S"), "  ", sprintf(...))
  cat(line, "\n"); cat(line, "\n", file = LOG, append = TRUE)
}

# NEON requires an API token for downloads from 2026-06-30 (and moves CC0 -> CC BY
# 4.0). The token rides as an X-API-Token header on every API + file request; supply
# it via the NEON_TOKEN env (the refresh workflow passes the GitHub Actions secret).
# Without it, post-2026-06-30 pulls 401/403 and the completeness guard stop()s the
# job before a shrunken bundle can ship, so the deployed app keeps its last bundle.
# Pure helpers (tests/test_precompute_fetch_contract.R loads them by name from
# this file, so keep them free of top-level state).

# Lower-case host of a URL ("" when it has none). Userinfo makes it "" so
# "https://data.neonscience.org@other.example/" never counts as a NEON host.
water_url_host <- function(url) {
  url <- as.character(url)
  out <- rep("", length(url))
  ok <- !is.na(url) & grepl("^[A-Za-z][A-Za-z0-9+.-]*://", url)
  authority <- sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/?#]*).*$", "\\1", url[ok])
  host <- tolower(sub(":[0-9]*$", "", authority))
  host[grepl("@", authority, fixed = TRUE)] <- ""
  out[ok] <- host
  out
}

# The token goes only to https URLs on data.neonscience.org or another
# *.neonscience.org host; signed file URLs on storage hosts get no header.
water_neon_token_host_ok <- function(url) {
  host <- water_url_host(url)
  !is.na(url) & grepl("^https://", url, ignore.case = TRUE) & nzchar(host) &
    (host == "data.neonscience.org" | endsWith(host, ".neonscience.org"))
}

water_neon_headers <- function(url, token) {
  if (length(url) != 1L || !nzchar(token) || !water_neon_token_host_ok(url)) {
    return(NULL)
  }
  c("X-API-Token" = token)
}

# A CI run without the token would pull nothing from data endpoints.
water_require_neon_token <- function(ci, token) {
  if (identical(ci, "true") && !nzchar(token)) {
    stop(paste0(
      "NEON_TOKEN is empty in CI: set the NEON_TOKEN repository secret ",
      "(neonscience.org account, API token) and map it into the fetch step."
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# Rows of an external-lab table that look like blanks (report only; never
# dropped here). Checks whichever identifier columns exist.
water_blank_like_rows <- function(d) {
  if (is.null(d) || !nrow(d)) return(0L)
  cols <- intersect(c("sampleID", "sampleCode", "parentSampleID",
                      "namedLocation"), names(d))
  if (!length(cols)) return(0L)
  hit <- rep(FALSE, nrow(d))
  for (cc in cols) {
    hit <- hit | grepl("blank", as.character(d[[cc]]), ignore.case = TRUE)
  }
  as.integer(sum(hit))
}

# The fetch receipt written to data/precompute_fetch_receipt.json.
water_fetch_receipt <- function(n_site_months, n_fetched, misses, file_urls,
                                lab_columns, n_blank_like_rows) {
  hosts <- water_url_host(file_urls)
  list(
    n_site_months = as.integer(n_site_months),
    n_fetched = as.integer(n_fetched),
    n_miss = as.integer(length(misses)),
    misses = I(as.character(utils::head(misses, 100L))),
    file_hosts = I(sort(unique(hosts[nzchar(hosts)]), method = "radix")),
    lab_columns = I(as.character(lab_columns)),
    n_blank_like_rows = as.integer(n_blank_like_rows)
  )
}

NEON_TOKEN <- Sys.getenv("NEON_TOKEN", "")
water_require_neon_token(Sys.getenv("CI", ""), NEON_TOKEN)
if (!nzchar(NEON_TOKEN)) message("NOTE: no NEON_TOKEN set - NEON requires a token for downloads from 2026-06-30.")

getJSON <- function(u, tries = 3) {
  for (i in seq_len(tries)) {
    r <- tryCatch({
      tmp <- tempfile(fileext = ".json"); on.exit(unlink(tmp), add = TRUE)
      download.file(u, tmp, mode = "wb", quiet = TRUE, method = "libcurl",
                    headers = water_neon_headers(u, NEON_TOKEN))
      jsonlite::fromJSON(tmp)
    }, error = function(e) NULL)
    if (!is.null(r)) return(r)
    Sys.sleep(1.5 * i)
  }
  NULL
}

# 1) Site metadata (coords, domain, state) + available months for the SWC product
logmsg("Fetching site metadata for %d sites...", length(SITES))
site_rows <- list(); site_months <- list()
for (s in SITES) {
  j <- getJSON(sprintf("https://data.neonscience.org/api/v0/sites/%s", s))
  if (is.null(j)) { logmsg("  [meta MISS] %s", s); next }
  d  <- j$data
  dp <- d$dataProducts
  m  <- tryCatch(unlist(dp$availableMonths[dp$dataProductCode == PRODUCT]), error = function(e) character(0))
  site_months[[s]] <- sort(unique(m))
  site_rows[[s]] <- tibble(
    site = s, siteName = SITE_LABELS[[s]], neonName = d$siteName %||% NA,
    domain = d$domainCode %||% NA, state = d$stateCode %||% NA,
    lat = as.numeric(d$siteLatitude %||% NA), long = as.numeric(d$siteLongitude %||% NA),
    siteType = d$siteType %||% NA, n_months = length(m)
  )
  Sys.sleep(0.15)
}
sites_meta <- bind_rows(site_rows)
logmsg("Got metadata for %d sites; total site-months = %d",
       nrow(sites_meta), sum(lengths(site_months)))

# 2) Download + cache the two basic CSVs per site-month (newest first = resumable & recent-weighted)
fetch_csv <- function(file_url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) return(suppressWarnings(suppressMessages(read_csv(dest, show_col_types = FALSE))))
  ok <- tryCatch({
    download.file(file_url, dest, mode = "wb", quiet = TRUE, method = "libcurl",
                  headers = water_neon_headers(file_url, NEON_TOKEN))
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(dest) || !isTRUE(file.info(dest)$size > 0)) {
    if (file.exists(dest)) unlink(dest)      # never cache a partial file
    return(NULL)
  }
  suppressWarnings(suppressMessages(read_csv(dest, show_col_types = FALSE)))
}

analyte_acc <- list(); field_acc <- list(); k <- 0
total_sm <- sum(lengths(site_months))
fetch_misses <- character(); n_fetched <- 0L; file_urls <- character()
lab_columns <- NULL; n_blank_like <- 0L
for (s in SITES) {
  months <- rev(site_months[[s]])            # newest first
  for (mo in months) {
    k <- k + 1
    a_dest <- file.path(CACHE_DIR, sprintf("%s_%s_analyte.csv", s, mo))
    p_dest <- file.path(CACHE_DIR, sprintf("%s_%s_parent.csv",  s, mo))
    r_dest <- file.path(CACHE_DIR, sprintf("%s_%s_release.txt", s, mo))
    a_have <- file.exists(a_dest); p_have <- file.exists(p_dest)
    if (!a_have || !p_have) {
      j <- getJSON(sprintf("https://data.neonscience.org/api/v0/data/%s/%s/%s", PRODUCT, s, mo))
      Sys.sleep(0.2)
      if (is.null(j) || is.null(j$data$files)) {
        logmsg("  [data MISS] %s %s", s, mo)
        fetch_misses <- c(fetch_misses, sprintf("%s %s data-api", s, mo))
        next
      }
      n_fetched <- n_fetched + 1L
      # The API v0 site-month response names its release (e.g. RELEASE-2026 or
      # PROVISIONAL). Cache it beside the CSVs so a resumed run keeps it.
      rel <- j$data$release
      rel <- if (is.character(rel) && length(rel) == 1L && !is.na(rel) &&
                 nzchar(rel)) rel else NA_character_
      writeLines(if (is.na(rel)) "" else rel, r_dest)
      files <- j$data$files
      ai <- grep("swc_externalLabDataByAnalyte.*basic.*csv$", files$name)
      pi <- grep("swc_fieldSuperParent.*basic.*csv$",         files$name)
      failed <- character()
      if (length(ai)) {
        file_urls <- c(file_urls, files$url[ai][1])
        if (is.null(fetch_csv(files$url[ai][1], a_dest))) failed <- c(failed, "analyte-csv")
      }
      if (length(pi)) {
        file_urls <- c(file_urls, files$url[pi][1])
        if (is.null(fetch_csv(files$url[pi][1], p_dest))) failed <- c(failed, "parent-csv")
      }
      if (length(failed)) {
        logmsg("  [file MISS] %s %s %s", s, mo, paste(failed, collapse = "+"))
        fetch_misses <- c(fetch_misses, sprintf("%s %s %s", s, mo,
                                                paste(failed, collapse = "+")))
      }
    }
    ad <- if (file.exists(a_dest)) suppressWarnings(suppressMessages(read_csv(a_dest, show_col_types = FALSE))) else NULL
    pd <- if (file.exists(p_dest)) suppressWarnings(suppressMessages(read_csv(p_dest, show_col_types = FALSE))) else NULL
    month_release <- if (file.exists(r_dest)) {
      r <- readLines(r_dest, warn = FALSE)
      if (length(r) && nzchar(r[[1]])) r[[1]] else NA_character_
    } else NA_character_                       # cached before release capture
    if (!is.null(ad) && is.null(lab_columns)) lab_columns <- names(ad)
    n_blank_like <- n_blank_like + water_blank_like_rows(ad)
    if (!is.null(ad) && nrow(ad)) {
      if (!"laboratoryName" %in% names(ad)) ad$laboratoryName <- NA_character_
      analyte_acc[[length(analyte_acc)+1]] <- ad %>%
        transmute(site = siteID, collectDate, analyte,
                  analyteConcentration, analyteUnits,
                  laboratoryName = as.character(laboratoryName),
                  release = if ("release" %in% names(ad))
                    as.character(ad$release) else month_release,
                  belowDetectionQF  = as.character(belowDetectionQF),   # keep raw "ND"/"BDL"
                  externalLabDataQF = as.character(externalLabDataQF))
    }
    if (!is.null(pd) && nrow(pd)) {
      keep <- intersect(c("siteID","collectDate","waterTemp","dissolvedOxygen","specificConductance"), names(pd))
      fr <- pd[, keep, drop = FALSE]; names(fr)[names(fr) == "siteID"] <- "site"
      fr$release <- if ("release" %in% names(pd)) as.character(pd$release) else
        rep(month_release, nrow(fr))
      field_acc[[length(field_acc)+1]] <- fr
    }
    if (k %% 25 == 0) logmsg("  ... %d / %d site-months processed", k, total_sm)
  }
  logmsg("[site done] %s", s)
}

# Fetch receipt (gitignored): refresh_data.R reads it for the job summary and
# the miss-rate gate. Hosts only, never a URL query string.
fetch_receipt <- water_fetch_receipt(total_sm, n_fetched, fetch_misses,
                                     file_urls, lab_columns %||% character(),
                                     n_blank_like)
jsonlite::write_json(fetch_receipt,
                     file.path(ROOT, "data", "precompute_fetch_receipt.json"),
                     auto_unbox = TRUE, pretty = TRUE)
logmsg("Fetch receipt: %d site-months, %d fetched, %d missed, %d blank-like lab rows",
       fetch_receipt$n_site_months, fetch_receipt$n_fetched,
       fetch_receipt$n_miss, fetch_receipt$n_blank_like_rows)

# 3) Hand off to the SINGLE shared builder (no divergent tidy logic here)
logmsg("Stacking and building bundle via build_swc_bundle()...")
lab_raw   <- bind_rows(analyte_acc)
field_raw <- bind_rows(field_acc)
coords    <- sites_meta %>% select(site, neonName, domain, state, lat, long, siteType)
partial   <- dplyr::n_distinct(lab_raw$site) < length(SITE_LABELS)

# Every full-fetch workflow supplies a runner-temp destination. Persist only the
# selected public inputs and deterministic unit evidence before the strict
# builder can stop on a new scientific identity. No credential or cache path is
# accepted by the writer, and these bytes are not candidate/publisher inputs.
review_dir <- Sys.getenv("WATER_REFRESH_REVIEW_DIR", "")
if (nzchar(review_dir)) {
  logmsg("Persisting public replay inputs and unit-mismatch review evidence...")
  review_result <- write_water_refresh_review(
    lab_raw, field_raw, coords, review_dir,
    source_sha = Sys.getenv("SOURCE_SHA", "")
  )
  logmsg(
    "Review evidence ready: %d mismatch identities; %d require review",
    review_result$receipt$n_mismatch_identities,
    review_result$receipt$n_unapproved_identities
  )
}

bundle <- build_swc_bundle(lab_raw, field_raw, coords, partial = partial)
save_bundle(bundle, file.path(ROOT, "data", "neon_swc.rds"))
readr::write_csv(bundle$analyte_meta, file.path(ROOT, "data", "analyte_coverage.csv"))
write_codebook(bundle, file.path(ROOT, "data", "codebook.csv"))

logmsg("DONE. obs=%d sites=%d analytes=%d below-detection=%d partial=%s -> data/neon_swc.rds",
       bundle$built$n_obs, bundle$built$n_sites, bundle$built$n_analytes,
       bundle$built$n_below, bundle$built$partial)
print(bundle$built)
