# Runtime bytes intentionally deployed to Posit Connect Cloud. Keep this list
# explicit: data/neon_swc_*.rds files are recoverability backups created during a
# full refresh, not application inputs, and must never be swept into a manifest.
WATER_RUNTIME_FILES <- c(
  "app.R",
  "helpers.R",
  "data/codebook.csv",
  "data/neon_swc.rds",
  "data/search_index.rds"
)

water_runtime_files <- function(root = ".") {
  stopifnot(is.character(root), length(root) == 1L, nzchar(root),
            !anyDuplicated(WATER_RUNTIME_FILES))
  paths <- file.path(root, WATER_RUNTIME_FILES)
  missing <- WATER_RUNTIME_FILES[!file.exists(paths)]
  if (length(missing)) {
    stop(sprintf(
      "Missing required runtime file(s): %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  not_files <- WATER_RUNTIME_FILES[file.info(paths)$isdir %in% TRUE]
  if (length(not_files)) {
    stop(sprintf(
      "Runtime allowlist path(s) are not files: %s",
      paste(not_files, collapse = ", ")
    ), call. = FALSE)
  }
  WATER_RUNTIME_FILES
}
