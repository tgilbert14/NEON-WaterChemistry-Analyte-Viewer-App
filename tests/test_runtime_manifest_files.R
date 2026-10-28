args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
source(file.path(repo_root, "scripts", "runtime_manifest_files.R"))

# app.R builds its runtime receipt from its own WATER_RUNTIME_FILES list; the
# manifest is built from scripts/runtime_manifest_files.R. The two must name
# exactly the same files, or the receipt and the deployed bundle drift apart.
app_exprs <- parse(file.path(repo_root, "app.R"), keep.source = FALSE)
app_def <- Filter(function(e) {
  is.call(e) && identical(e[[1]], as.name("<-")) &&
    identical(e[[2]], as.name("WATER_RUNTIME_FILES"))
}, as.list(app_exprs))
stopifnot(length(app_def) == 1L)
app_list <- eval(app_def[[1]][[3]], envir = baseenv())
stopifnot(is.character(app_list), !anyDuplicated(app_list))
stopifnot(identical(sort(app_list), sort(WATER_RUNTIME_FILES)))

# every allowlisted file exists in the repository
stopifnot(all(file.exists(file.path(repo_root, WATER_RUNTIME_FILES))))

fixture <- tempfile("water-manifest-")
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)
required <- file.path(fixture, WATER_RUNTIME_FILES)
for (d in unique(dirname(required))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
stopifnot(all(file.create(required)))

# These files exist during a real full refresh but are producer diagnostics or
# recovery bytes, not Connect runtime inputs.
stopifnot(file.create(file.path(fixture, "data", "neon_swc_20260803-060000.rds")))
stopifnot(file.create(file.path(fixture, "data", "analyte_coverage.csv")))
stopifnot(file.create(file.path(fixture, "www", "decoy.txt")))

actual <- water_runtime_files(fixture)
stopifnot(identical(actual, WATER_RUNTIME_FILES))
stopifnot("scripts/water_unit_contract.R" %in% actual)
stopifnot(all(c("www/pincards.js", "www/vendor/html-to-image-1.11.11.js") %in% actual))
stopifnot(!any(grepl("neon_swc_[0-9]", actual)))
stopifnot(!"data/analyte_coverage.csv" %in% actual)
stopifnot(!"www/decoy.txt" %in% actual)

file.remove(file.path(fixture, "helpers.R"))
missing_error <- tryCatch({
  water_runtime_files(fixture)
  ""
}, error = conditionMessage)
stopifnot(grepl("helpers[.]R", missing_error))

cat("Runtime manifest allowlist regression passed.\n")
