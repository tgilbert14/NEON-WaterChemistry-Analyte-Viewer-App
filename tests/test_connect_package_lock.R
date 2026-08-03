args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}

if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite is required.")
manifest <- jsonlite::fromJSON(
  file.path(repo_root, "manifest.json"), simplifyVector = FALSE
)
lock <- jsonlite::fromJSON(
  file.path(repo_root, "config", "connect-manifest-packages-v1.json"),
  simplifyVector = FALSE
)
snapshot <- "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"

stopifnot(
  identical(lock$schema_version, 1L),
  identical(lock$platform, "4.5.2"),
  identical(lock$locale, "C"),
  identical(lock$repository, snapshot),
  identical(lock$source_commit, "31b2e921a80aa262741c44f2282c781f394e1a90"),
  length(lock$packages) == 103L,
  identical(manifest$platform, lock$platform),
  identical(manifest$locale, lock$locale),
  identical(manifest$packages, lock$packages)
)

sources <- vapply(lock$packages, function(record) record$Source, character(1))
repositories <- vapply(
  lock$packages, function(record) record$Repository, character(1)
)
remote_types <- vapply(
  lock$packages,
  function(record) {
    value <- record$description$RemoteType
    if (is.null(value)) "" else value
  },
  character(1)
)
remote_repositories <- vapply(
  lock$packages, function(record) record$description$RemoteRepos, character(1)
)
versions <- vapply(
  lock$packages, function(record) record$description$Version, character(1)
)
remote_shas <- vapply(
  lock$packages,
  function(record) {
    value <- record$description$RemoteSha
    if (is.null(value)) "" else value
  },
  character(1)
)
standard <- remote_types == "standard"

stopifnot(
  all(sources == "CRAN"),
  all(repositories == snapshot),
  all(remote_repositories == snapshot),
  !any(remote_types == "url"),
  all(remote_shas[standard] == versions[standard]),
  identical(lock$packages$terra$description$Version, "1.8-50")
)

# The exact cold-start gate must compare raw DESCRIPTION strings. R's
# packageVersion() canonicalizes hyphens to dots and would falsely reject an
# exact install such as MASS 7.3-65.
mass_raw <- unname(utils::packageDescription("MASS", fields = "Version"))
stopifnot(
  identical(mass_raw, lock$packages$MASS$description$Version),
  grepl("-", mass_raw, fixed = TRUE),
  !identical(as.character(utils::packageVersion("MASS")), mass_raw)
)
cat("Connect package lock regression passed: 103 dated standard CRAN records.\n")
