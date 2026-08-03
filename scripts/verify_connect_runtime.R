# Verify the exact Posit Connect runtime envelope without starting a server.
# Invoke from the repository root, or pass --app-dir=/path/to/six-file-stage.

args <- commandArgs(trailingOnly = TRUE)
known <- grepl("^--app-dir=", args) | args == "--require-exact-packages"
if (any(!known)) {
  stop(sprintf("Unknown argument(s): %s", paste(args[!known], collapse = ", ")),
       call. = FALSE)
}

app_arg <- args[grepl("^--app-dir=", args)]
if (length(app_arg) > 1L) stop("--app-dir may be supplied only once.", call. = FALSE)
app_dir <- if (length(app_arg)) sub("^--app-dir=", "", app_arg) else "."
app_dir <- normalizePath(app_dir, mustWork = TRUE)
require_exact <- "--require-exact-packages" %in% args

manifest_path <- file.path(app_dir, "manifest.json")
if (!file.exists(manifest_path)) stop("manifest.json is missing from the runtime stage.", call. = FALSE)
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required.", call. = FALSE)

manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
expected_files <- sort(c(
  "app.R",
  "helpers.R",
  "scripts/water_unit_contract.R",
  "data/codebook.csv",
  "data/neon_swc.rds",
  "data/search_index.rds"
))
actual_files <- sort(names(manifest$files))
if (!identical(actual_files, expected_files)) {
  stop(sprintf("Runtime allowlist differs: %s", paste(actual_files, collapse = ", ")),
       call. = FALSE)
}

declared_md5 <- vapply(manifest$files, function(record) record$checksum, character(1))
paths <- file.path(app_dir, names(declared_md5))
if (!all(file.exists(paths))) {
  stop(sprintf("Runtime file(s) missing: %s",
               paste(names(declared_md5)[!file.exists(paths)], collapse = ", ")),
       call. = FALSE)
}
actual_md5 <- unname(tools::md5sum(paths))
if (!identical(tolower(actual_md5), tolower(unname(declared_md5)))) {
  bad <- names(declared_md5)[tolower(actual_md5) != tolower(unname(declared_md5))]
  stop(sprintf("Manifest checksum mismatch: %s", paste(bad, collapse = ", ")),
       call. = FALSE)
}

if (!identical(manifest$platform, "4.5.2")) {
  stop(sprintf("Connect manifest platform is %s, expected 4.5.2.", manifest$platform),
       call. = FALSE)
}
sources <- vapply(manifest$packages, function(record) record$Source, character(1))
remote_types <- vapply(
  manifest$packages,
  function(record) {
    value <- record$description$RemoteType
    if (is.null(value)) "" else value
  },
  character(1)
)
if (any(sources != "CRAN") || any(remote_types == "url")) {
  stop("Connect package lock contains a direct URL/non-CRAN package record.",
       call. = FALSE)
}

if (require_exact) {
  expected_versions <- vapply(
    manifest$packages, function(record) record$description$Version, character(1)
  )
  installed <- rownames(utils::installed.packages())
  missing <- setdiff(names(expected_versions), installed)
  if (length(missing)) {
    stop(sprintf("Manifest package(s) missing: %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  actual_versions <- vapply(
    names(expected_versions),
    function(package) {
      # packageVersion() normalizes CRAN's hyphenated versions (1.8-50 becomes
      # 1.8.50), so compare the raw DESCRIPTION field to the manifest string.
      unname(utils::packageDescription(package, fields = "Version"))
    },
    character(1)
  )
  bad <- names(expected_versions)[actual_versions != expected_versions]
  if (length(bad)) {
    detail <- sprintf("%s %s != %s", bad, actual_versions[bad], expected_versions[bad])
    stop(sprintf("Installed packages differ from manifest: %s", paste(detail, collapse = "; ")),
         call. = FALSE)
  }
}

old_wd <- setwd(app_dir)
on.exit(setwd(old_wd), add = TRUE)
isolated_home <- tempfile("water-connect-home-")
isolated_tmp <- tempfile("water-connect-tmp-")
dir.create(isolated_home)
dir.create(isolated_tmp)
on.exit(unlink(c(isolated_home, isolated_tmp), recursive = TRUE, force = TRUE), add = TRUE)
Sys.setenv(
  HOME = isolated_home,
  R_USER = isolated_home,
  TMPDIR = isolated_tmp,
  LANG = "C",
  LC_ALL = "C"
)

result <- source("app.R", local = new.env(parent = globalenv()), chdir = FALSE)
if (!inherits(result$value, "shiny.appobj")) {
  stop("app.R did not return a shiny.appobj.", call. = FALSE)
}
cat(sprintf(
  "Connect six-file cold source passed (%d packages; exact_versions=%s).\n",
  length(manifest$packages), require_exact
))
