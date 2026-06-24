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
NEON_TOKEN <- Sys.getenv("NEON_TOKEN", "")
NEON_HDR   <- if (nzchar(NEON_TOKEN)) c("X-API-Token" = NEON_TOKEN) else NULL
if (!nzchar(NEON_TOKEN)) message("NOTE: no NEON_TOKEN set - NEON requires a token for downloads from 2026-06-30.")

getJSON <- function(u, tries = 3) {
  for (i in seq_len(tries)) {
    r <- tryCatch({
      tmp <- tempfile(fileext = ".json"); on.exit(unlink(tmp), add = TRUE)
      download.file(u, tmp, mode = "wb", quiet = TRUE, method = "libcurl", headers = NEON_HDR)
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
  ok <- tryCatch({ download.file(file_url, dest, mode = "wb", quiet = TRUE, method = "libcurl", headers = NEON_HDR); TRUE }, error = function(e) FALSE)
  if (!ok || !file.exists(dest)) return(NULL)
  suppressWarnings(suppressMessages(read_csv(dest, show_col_types = FALSE)))
}

analyte_acc <- list(); field_acc <- list(); k <- 0
total_sm <- sum(lengths(site_months))
for (s in SITES) {
  months <- rev(site_months[[s]])            # newest first
  for (mo in months) {
    k <- k + 1
    a_dest <- file.path(CACHE_DIR, sprintf("%s_%s_analyte.csv", s, mo))
    p_dest <- file.path(CACHE_DIR, sprintf("%s_%s_parent.csv",  s, mo))
    a_have <- file.exists(a_dest); p_have <- file.exists(p_dest)
    if (!a_have || !p_have) {
      j <- getJSON(sprintf("https://data.neonscience.org/api/v0/data/%s/%s/%s", PRODUCT, s, mo))
      Sys.sleep(0.2)
      if (is.null(j) || is.null(j$data$files)) { logmsg("  [data MISS] %s %s", s, mo); next }
      files <- j$data$files
      ai <- grep("swc_externalLabDataByAnalyte.*basic.*csv$", files$name)
      pi <- grep("swc_fieldSuperParent.*basic.*csv$",         files$name)
      if (length(ai)) fetch_csv(files$url[ai][1], a_dest)
      if (length(pi)) fetch_csv(files$url[pi][1], p_dest)
    }
    ad <- if (file.exists(a_dest)) suppressWarnings(suppressMessages(read_csv(a_dest, show_col_types = FALSE))) else NULL
    pd <- if (file.exists(p_dest)) suppressWarnings(suppressMessages(read_csv(p_dest, show_col_types = FALSE))) else NULL
    if (!is.null(ad) && nrow(ad)) {
      analyte_acc[[length(analyte_acc)+1]] <- ad %>%
        transmute(site = siteID, collectDate, analyte,
                  analyteConcentration, analyteUnits,
                  belowDetectionQF  = as.character(belowDetectionQF),   # keep raw "ND"/"BDL"
                  externalLabDataQF = as.character(externalLabDataQF))
    }
    if (!is.null(pd) && nrow(pd)) {
      keep <- intersect(c("siteID","collectDate","waterTemp","dissolvedOxygen","specificConductance"), names(pd))
      fr <- pd[, keep, drop = FALSE]; names(fr)[names(fr) == "siteID"] <- "site"
      field_acc[[length(field_acc)+1]] <- fr
    }
    if (k %% 25 == 0) logmsg("  ... %d / %d site-months processed", k, total_sm)
  }
  logmsg("[site done] %s", s)
}

# 3) Hand off to the SINGLE shared builder (no divergent tidy logic here)
logmsg("Stacking and building bundle via build_swc_bundle()...")
lab_raw   <- bind_rows(analyte_acc)
field_raw <- bind_rows(field_acc)
coords    <- sites_meta %>% select(site, neonName, domain, state, lat, long, siteType)
partial   <- dplyr::n_distinct(lab_raw$site) < length(SITE_LABELS)

bundle <- build_swc_bundle(lab_raw, field_raw, coords, partial = partial)
save_bundle(bundle, file.path(ROOT, "data", "neon_swc.rds"))
readr::write_csv(bundle$analyte_meta, file.path(ROOT, "data", "analyte_coverage.csv"))

logmsg("DONE. obs=%d sites=%d analytes=%d below-detection=%d partial=%s -> data/neon_swc.rds",
       bundle$built$n_obs, bundle$built$n_sites, bundle$built$n_analytes,
       bundle$built$n_below, bundle$built$partial)
print(bundle$built)
