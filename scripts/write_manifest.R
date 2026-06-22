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
  list.files("data", pattern = "[.]rds$", full.names = TRUE),  # neon_swc.rds only
  if (dir.exists("data-sample")) list.files("data-sample", full.names = TRUE),
  if (dir.exists("www")) list.files("www", full.names = TRUE)
)
app_files <- app_files[file.exists(app_files)]

rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R", appFiles = app_files)

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
