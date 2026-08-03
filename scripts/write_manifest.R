# ===========================================================================
# write_manifest.R — (re)generate manifest.json for Posit Connect Cloud.
#
# RUN THIS after ANY change to a committed runtime file, then COMMIT
# manifest.json. Connect Cloud reads the committed manifest, so stale file
# checksums can serve yesterday's data. The committed package-lock fixture is
# an explicit release lock: ordinary data rebuilds preserve it byte-for-byte.
# A dependency change therefore requires a separately reviewed fixture update.
#
#   Rscript scripts/write_manifest.R
#
# This app is a single-file Shiny app (app.R) + helpers.R + the committed data
# bundle. appFiles is scoped to exactly those runtime files so the deploy stays
# lean. The heavy pull/build packages neonUtilities and arrow are never
# referenced at runtime and must never appear in the manifest. data.table is a
# genuine plotly runtime dependency and is expected.
# ===========================================================================
if (!requireNamespace("rsconnect", quietly = TRUE)) stop("install.packages('rsconnect') first")
if (!requireNamespace("jsonlite", quietly = TRUE))  stop("install.packages('jsonlite') first")

source(file.path("scripts", "runtime_manifest_files.R"), local = TRUE)
app_files <- water_runtime_files()

lock_path <- file.path("config", "connect-manifest-packages-v1.json")
if (!file.exists(lock_path)) {
  stop("The reviewed Connect package-lock fixture is missing.", call. = FALSE)
}

locked <- jsonlite::fromJSON(lock_path, simplifyVector = FALSE)
snapshot <- "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
if (!identical(locked$schema_version, 1L) ||
    !identical(locked$platform, "4.5.2") ||
    !identical(locked$locale, "C") ||
    !identical(locked$repository, snapshot) ||
    !identical(locked$source_commit,
               "31b2e921a80aa262741c44f2282c781f394e1a90") ||
    !length(locked$packages)) {
  stop("The reviewed Connect package-lock fixture metadata is invalid.",
       call. = FALSE)
}

locked_names <- sort(names(locked$packages))
locked_sources <- vapply(
  locked$packages,
  function(record) if (is.null(record$Source)) "" else record$Source,
  character(1)
)
locked_remote_types <- vapply(
  locked$packages,
  function(record) {
    value <- record$description$RemoteType
    if (is.null(value)) "" else value
  },
  character(1)
)
locked_repositories <- vapply(
  locked$packages, function(record) record$Repository, character(1)
)
locked_remote_repositories <- vapply(
  locked$packages, function(record) record$description$RemoteRepos, character(1)
)
locked_versions <- vapply(
  locked$packages, function(record) record$description$Version, character(1)
)
locked_remote_shas <- vapply(
  locked$packages,
  function(record) {
    value <- record$description$RemoteSha
    if (is.null(value)) "" else value
  },
  character(1)
)
standard <- locked_remote_types == "standard"
if (length(locked_names) != 103L ||
    any(locked_sources != "CRAN") ||
    any(locked_remote_types == "url") ||
    any(locked_repositories != snapshot) ||
    any(locked_remote_repositories != snapshot) ||
    any(standard & locked_remote_shas != locked_versions)) {
  stop("The reviewed Connect lock must contain 103 exact, dated, standard CRAN records.",
       call. = FALSE)
}
if (!identical(locked$packages$terra$description$Version, "1.8-50")) {
  stop("The reviewed Connect lock must retain terra 1.8-50 for GDAL 3.4 compatibility.",
       call. = FALSE)
}

# Generate fresh file checksums and independently discover the package-name
# closure. Refuse dependency drift, then restore the reviewed package metadata.
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R", appFiles = app_files)
generated <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
generated_names <- sort(names(generated$packages))
if (!identical(generated_names, locked_names)) {
  added <- setdiff(generated_names, locked_names)
  removed <- setdiff(locked_names, generated_names)
  stop(sprintf(
    paste0("Runtime dependency-name drift requires a separately reviewed Connect lock update. ",
           "Added: %s; removed: %s"),
    if (length(added)) paste(added, collapse = ", ") else "<none>",
    if (length(removed)) paste(removed, collapse = ", ") else "<none>"
  ), call. = FALSE)
}
generated$packages <- locked$packages
generated$platform <- locked$platform
generated$locale <- locked$locale
jsonlite::write_json(
  generated, "manifest.json", auto_unbox = TRUE, pretty = TRUE, null = "null"
)
cat(sprintf(
  "Refreshed six runtime checksums and preserved %d reviewed Connect package records.\n",
  length(locked_names)
))

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
