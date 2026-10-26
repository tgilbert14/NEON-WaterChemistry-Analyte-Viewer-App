args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}

# The workflow's NEON watch reads config/neon-product-watch.json; the app cites
# helpers.R NEON_RELEASE_DOI and shows NEON_DATA_NOTES. The three must agree.
watch <- jsonlite::fromJSON(file.path(repo_root, "config", "neon-product-watch.json"),
                            simplifyVector = TRUE)
stopifnot(all(c("schema", "product", "source", "reviewed_at", "releases", "changelog")
              %in% names(watch)))
stopifnot(identical(watch$schema, "water-neon-product-watch-v1"))
stopifnot(identical(watch$product, "DP1.20093.001"))
stopifnot(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", watch$reviewed_at))
stopifnot(is.data.frame(watch$releases), all(c("release", "doi") %in% names(watch$releases)))
stopifnot(is.data.frame(watch$changelog),
          all(c("id", "issue_date", "disposition", "note") %in% names(watch$changelog)))
stopifnot(is.character(watch$changelog$id), !anyDuplicated(watch$changelog$id))
stopifnot(!anyDuplicated(watch$releases$release))

# Evaluate only the two contract assignments from helpers.R, so the test needs
# no app packages beyond tibble.
exprs <- parse(file.path(repo_root, "helpers.R"), keep.source = FALSE)
env <- new.env()
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("NEON_RELEASE_DOI", "NEON_DATA_NOTES")) {
    eval(e, env)
  }
}
stopifnot(exists("NEON_RELEASE_DOI", env, inherits = FALSE),
          exists("NEON_DATA_NOTES", env, inherits = FALSE))

doi <- env$NEON_RELEASE_DOI
stopifnot(identical(names(doi), watch$releases$release))
stopifnot(identical(unname(doi), watch$releases$doi))

notes <- env$NEON_DATA_NOTES
logged <- notes[!startsWith(notes$id, "bundle-"), , drop = FALSE]
stopifnot(nrow(logged) > 0)
for (i in seq_len(nrow(logged))) {
  hit <- watch$changelog[watch$changelog$id == logged$id[[i]], , drop = FALSE]
  if (nrow(hit) != 1L) stop("NEON_DATA_NOTES id ", logged$id[[i]], " is not in the change-log watch list")
  want <- paste0(logged$scope[[i]], "-note")
  if (!identical(hit$disposition, want)) {
    stop("change-log id ", logged$id[[i]], " has disposition ", hit$disposition, ", expected ", want)
  }
}

cat("NEON product watch contract passed.\n")
