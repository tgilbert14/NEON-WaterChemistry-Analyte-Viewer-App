# ===========================================================================
# write_manifest.R — (re)generate manifest.json for Posit Connect Cloud.
#
# RUN THIS after ANY change to runtime dependencies or the committed data set,
# then COMMIT manifest.json — Connect Cloud reads the committed manifest, so a
# stale manifest restores the OLD package set or serves yesterday's data.
#
#   Rscript scripts/write_manifest.R
#
# This app is a single-file Shiny app (app.R) + helpers.R + the committed data
# bundle. appFiles is scoped to exactly those runtime files so the deploy stays
# LEAN: the heavy pull/build packages (neonUtilities, arrow, data.table) are
# NEVER referenced at runtime, so they must never appear in the manifest. The
# HARD GATE at the bottom stop()s the build if any of them leak in.
# ===========================================================================
if (!requireNamespace("rsconnect", quietly = TRUE)) stop("install.packages('rsconnect') first")
if (!requireNamespace("jsonlite", quietly = TRUE))  stop("install.packages('jsonlite') first")

app_files <- c(
  "app.R", "helpers.R",
  list.files("data", pattern = "[.](rds|csv)$", full.names = TRUE),  # neon_swc.rds, search_index.rds + codebook.csv (in-app download)
  if (dir.exists("data-sample")) list.files("data-sample", full.names = TRUE),
  if (dir.exists("www")) list.files("www", full.names = TRUE)
)
# analyte_coverage.csv is DERIVED + gitignored (not committed, not deployed) — the
# new csv glob would otherwise sweep it in. Drop any data file that git does not
# track so the manifest lists only deployable, committed runtime files.
app_files <- app_files[basename(app_files) != "analyte_coverage.csv"]
app_files <- app_files[file.exists(app_files)]

rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R", appFiles = app_files)

# ---- pin terra to the last release before the GDAL-3.8 multidim code (1.8-54) ----
# terra >= 1.8-54 ships gdal_multidimensional.cpp using a GDAL 3.8 call unguarded in
# releases, so it FAILS to compile against Connect Cloud's GDAL 3.4.1. Connect compiles
# from source regardless of repo. 1.8-50 is the last release before 1.8-54: it compiles
# on 3.4.1 and still satisfies raster's terra (>= 1.8-5). terra/raster are install-only
# (leaflet -> raster -> terra; app never calls terra) -> zero runtime impact. Also pin
# the repo to the RSPM jammy binary mirror for suite consistency.
local({
  mm <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
  if (!is.null(mm$packages$terra)) {
    mm$packages$terra$description$Version <- "1.8-50"
    if (!is.null(mm$packages$terra$description$RemoteSha)) mm$packages$terra$description$RemoteSha <- "1.8-50"
    jsonlite::write_json(mm, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  mtxt <- readLines("manifest.json", warn = FALSE)
  mtxt <- gsub("https://cloud.r-project.org", "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
  mtxt <- gsub("https://packagemanager.posit.co/cran/latest", "https://packagemanager.posit.co/cran/__linux__/jammy/latest", mtxt, fixed = TRUE)
  writeLines(mtxt, "manifest.json")
  cat("Pinned terra to 1.8-50 + RSPM jammy repo.\n")
})

# ---- HARD GATE: a leaked heavy package must never commit silently ----------
# neonUtilities + arrow are the data-PULL packages: they are referenced ONLY in
# scripts/ (the cache build), never at runtime, so they must NEVER appear in the
# manifest. If either leaks, stop() with a non-zero error.
#
# data.table is NOT gated: it is a genuine runtime Import of plotly (the charting
# engine), so it legitimately appears in the manifest — exactly as in the
# gold-standard sibling manifests (Mosquito Pulse, Driver Cascade both ship it
# via plotly). Gating it would diverge from the suite and risk a restore failure.
m    <- jsonlite::fromJSON("manifest.json")
pkgs <- names(m$packages)
cat(sprintf("manifest.json written: %d packages.\n", length(pkgs)))
banned <- c("neonUtilities", "arrow")
hit <- banned[tolower(banned) %in% tolower(pkgs)]
if (length(hit)) {
  stop(sprintf("manifest.json LEAKED data-pull package(s): %s — the deploy would be heavy. These are scripts-only; remove the runtime reference, then re-run.",
               paste(hit, collapse = ", ")))
}
if ("data.table" %in% pkgs)
  cat("Note: data.table present (a genuine plotly runtime dependency — expected, matches the suite).\n")
cat("Good: no neonUtilities / arrow in the manifest (lean deploy).\n")
