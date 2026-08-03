args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
source(file.path(repo_root, "scripts", "runtime_manifest_files.R"))

fixture <- tempfile("water-manifest-")
dir.create(file.path(fixture, "data"), recursive = TRUE)
dir.create(file.path(fixture, "scripts"), recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE, force = TRUE), add = TRUE)

required <- file.path(fixture, WATER_RUNTIME_FILES)
stopifnot(all(file.create(required)))

# These files exist during a real full refresh but are producer diagnostics or
# recovery bytes, not Connect runtime inputs.
stopifnot(file.create(file.path(fixture, "data", "neon_swc_20260803-060000.rds")))
stopifnot(file.create(file.path(fixture, "data", "analyte_coverage.csv")))
dir.create(file.path(fixture, "www"))
stopifnot(file.create(file.path(fixture, "www", "decoy.txt")))

actual <- water_runtime_files(fixture)
stopifnot(identical(actual, WATER_RUNTIME_FILES))
stopifnot(identical(length(actual), 6L))
stopifnot("scripts/water_unit_contract.R" %in% actual)
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
