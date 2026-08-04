args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required for the verifier adversarial regression.")
}

runtime_env <- new.env(parent = baseenv())
sys.source(
  file.path(repo_root, "scripts", "runtime_manifest_files.R"),
  envir = runtime_env
)
runtime_files <- runtime_env$water_runtime_files(repo_root)

fixture_root <- tempfile("water-verifier-adversarial-")
dir.create(fixture_root)
on.exit(unlink(fixture_root, recursive = TRUE), add = TRUE)

copy_fixture_file <- function(path) {
  source_path <- file.path(repo_root, path)
  target_path <- file.path(fixture_root, path)
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(source_path, target_path, overwrite = TRUE))
  invisible(target_path)
}

invisible(lapply(runtime_files, copy_fixture_file))
dir.create(file.path(fixture_root, "config"), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(
  file.path(repo_root, "config", "connect-manifest-packages-v1.json"),
  file.path(fixture_root, "config", "connect-manifest-packages-v1.json"),
  overwrite = TRUE
))
invisible(lapply(
  c(
    "scripts/verify_refresh_candidate.R",
    "scripts/water_unit_contract.R",
    "scripts/build_search_index.R",
    "scripts/build_swc_bundle.R"
  ),
  function(path) if (!file.exists(file.path(fixture_root, path))) {
    copy_fixture_file(path)
  }
))

candidate_path <- file.path(fixture_root, "data", "neon_swc.rds")
index_path <- file.path(fixture_root, "data", "search_index.rds")
codebook_path <- file.path(fixture_root, "data", "codebook.csv")
baseline_path <- file.path(fixture_root, "trusted-base.rds")
stopifnot(file.copy(candidate_path, baseline_path, overwrite = FALSE))

# Rebuild both derived artifacts under the current policy before constructing
# the valid adversarial fixture. The committed baseline can legitimately carry
# the prior policy receipt until promotion; verifier tests must exercise the
# exact candidate-producing path rather than hand-patching only its metadata.
old_wd <- setwd(fixture_root)
build_output <- suppressWarnings(system2(
  file.path(R.home("bin"), "Rscript"),
  c("--vanilla", "scripts/build_search_index.R"),
  stdout = TRUE, stderr = TRUE
))
setwd(old_wd)
build_status <- attr(build_output, "status")
if (is.null(build_status)) build_status <- 0L
if (!identical(build_status, 0L)) {
  stop(sprintf(
    "Could not build the current-policy verifier fixture:\n%s",
    paste(build_output, collapse = "\n")
  ), call. = FALSE)
}

# The committed legacy index predates deterministic source provenance. The
# production workflow rebuilds it before validation, so mirror that one field in
# this isolated fixture without relying on the producer's index builder.
candidate_original <- readRDS(candidate_path)
index_original <- readRDS(index_path)
index_original$built$when <- candidate_original$built$when
index_original$built$product <- candidate_original$built$product
index_original$built$source <-
  "data/neon_swc.rds (committed bundle; plausibility-gated)"

# Make the fixture independent of whether tracked derived bytes have already
# been promoted on this code-review branch. The release codebook describes the
# effective runtime view, so derive that view with the pure policy helper before
# adversarial mutations (no ggplot/plotly runtime stack required here).
policy_env <- new.env(parent = baseenv())
sys.source(
  file.path(repo_root, "scripts", "water_unit_contract.R"),
  envir = policy_env
)
candidate_receipt_present <- policy_env$WATER_UNIT_RECEIPT_FIELDS %in%
  names(candidate_original$built)
stopifnot(all(candidate_receipt_present) || !any(candidate_receipt_present))
current_candidate <- all(candidate_receipt_present)
runtime_long <- policy_env$canonicalize_runtime_water_units(
  candidate_original$swc_long
)
index_original$built$runtime_unit_policy <- policy_env$WATER_UNIT_POLICY
index_original$built$n_runtime_unit_rows_excluded <-
  runtime_long$n_collapsed_rows_excluded
index_original$built$n_runtime_unit_source_rows_excluded <-
  runtime_long$n_source_rows_excluded
index_original$built$n_runtime_unit_labels_rewritten <-
  runtime_long$n_missing_labels_rewritten
index_original$built$runtime_unit_exclusion_sha256 <-
  policy_env$water_unit_receipt_sha256(runtime_long$excluded)
runtime_long <- runtime_long$data
runtime_rows <- split(
  seq_len(nrow(runtime_long)), as.character(runtime_long$analyte)
)
codebook_header <- readLines(codebook_path, warn = FALSE)[1:3]
codebook_table <- utils::read.csv(
  codebook_path, comment.char = "#", check.names = FALSE,
  stringsAsFactors = FALSE, na.strings = character(0)
)
flag_row <- which(
  codebook_table$section == "tidy_long_export" &
    codebook_table$name == "lab_flag"
)
stopifnot(
  grepl(
    if (current_candidate) "version 1.1.0" else "version 1.0.0",
    codebook_header[[1]], fixed = TRUE
  ),
  length(flag_row) == 1L,
  identical(
    codebook_table$allowed[[flag_row]],
    if (current_candidate) {
      "sorted distinct NEON externalLabDataQF codes joined by ' | '"
    } else {
      "NEON externalLabDataQF codes"
    }
  ),
  identical(
    codebook_table$definition[[flag_row]],
    if (current_candidate) {
      paste0(
        "All distinct external-lab quality flags across the collapsed ",
        "replicates"
      )
    } else {
      "External-lab quality flag (e.g. legacyData, formatChange)"
    }
  ),
  identical(
    codebook_table$na_semantics[[flag_row]],
    if (current_candidate) {
      "NA when every replicate is unflagged"
    } else {
      "NA when unflagged"
    }
  )
)
dictionary_rows <- which(codebook_table$section == "analyte_dictionary")
for (row in dictionary_rows) {
  analyte <- codebook_table$name[[row]]
  indices <- runtime_rows[[analyte]]
  n <- length(indices)
  n_sites <- length(unique(as.character(runtime_long$site[indices])))
  n_below <- sum(runtime_long$belowDetection[indices])
  pct_below <- if (n > 0L) round(n_below / n, 4L) else NA_real_
  codebook_table$units[[row]] <- unique(as.character(runtime_long$units[indices]))
  codebook_table$definition[[row]] <- sprintf(
    "Analyte '%s': %d obs across %d sites; canonical unit shown",
    analyte, n, n_sites
  )
  codebook_table$na_semantics[[row]] <- sprintf(
    "%s below detection (kept, not substituted)",
    ifelse(is.na(pct_below), "0%", paste0(round(100 * pct_below), "%"))
  )
}
writeLines(codebook_header, codebook_path)
suppressWarnings(utils::write.table(
  codebook_table, codebook_path, append = TRUE, sep = ",", row.names = FALSE,
  col.names = TRUE, qmethod = "double"
))
codebook_original <- readLines(codebook_path, warn = FALSE)

write_fixture_manifest <- function() {
  files <- stats::setNames(lapply(runtime_files, function(path) {
    list(checksum = unname(tools::md5sum(file.path(fixture_root, path))))
  }), runtime_files)
  lock <- jsonlite::fromJSON(
    file.path(fixture_root, "config", "connect-manifest-packages-v1.json"),
    simplifyVector = FALSE
  )
  jsonlite::write_json(
    list(
      version = 1L, locale = lock$locale, platform = lock$platform,
      metadata = list(
        appmode = "shiny", primary_rmd = NULL, primary_html = NULL,
        content_category = NULL, has_parameters = FALSE
      ),
      packages = lock$packages, files = files, users = list()
    ),
    file.path(fixture_root, "manifest.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
}

reset_fixture <- function() {
  stopifnot(file.copy(baseline_path, candidate_path, overwrite = TRUE))
  saveRDS(index_original, index_path, compress = "xz")
  writeLines(codebook_original, codebook_path)
  write_fixture_manifest()
}

run_verifier <- function(expected_pattern = NULL) {
  old_wd <- setwd(fixture_root)
  on.exit(setwd(old_wd), add = TRUE)
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "scripts/verify_refresh_candidate.R", baseline_path),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  output_text <- paste(output, collapse = "\n")
  if (is.null(expected_pattern)) {
    if (!identical(status, 0L)) {
      stop(sprintf("Valid verifier fixture failed:\n%s", output_text), call. = FALSE)
    }
  } else {
    stopifnot(status != 0L, grepl(expected_pattern, output_text, fixed = TRUE))
  }
  invisible(output_text)
}

mutate_bundle <- function(mutate) {
  reset_fixture()
  candidate <- readRDS(candidate_path)
  candidate <- mutate(candidate)
  saveRDS(candidate, candidate_path)
  write_fixture_manifest()
}

mutate_index <- function(mutate) {
  reset_fixture()
  index <- readRDS(index_path)
  index <- mutate(index)
  saveRDS(index, index_path, compress = "xz")
  write_fixture_manifest()
}

write_codebook_table <- function(header, table) {
  writeLines(header, codebook_path)
  suppressWarnings(utils::write.table(
    table, codebook_path, append = TRUE, sep = ",", row.names = FALSE,
    col.names = TRUE, qmethod = "double"
  ))
}

reset_fixture()
run_verifier()

if (!current_candidate) {
  # Legacy compatibility is a byte-exact exception, not a general receipt-free
  # mode. Even a schema-valid timestamp-only mutation with matching index
  # provenance must be rejected because its bundle SHA is no longer pinned.
  reset_fixture()
  receipt_free_candidate <- readRDS(candidate_path)
  receipt_free_index <- readRDS(index_path)
  mutated_when <- sub(
    "T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", "T00:00:00Z",
    receipt_free_candidate$built$when
  )
  if (identical(mutated_when, receipt_free_candidate$built$when)) {
    mutated_when <- sub("T00:00:00Z$", "T00:00:01Z", mutated_when)
  }
  receipt_free_candidate$built$when <- mutated_when
  receipt_free_index$built$when <- mutated_when
  saveRDS(receipt_free_candidate, candidate_path)
  saveRDS(receipt_free_index, index_path, compress = "xz")
  write_fixture_manifest()
  run_verifier("Receipt-free candidate is not the exact known legacy bundle")
}

mutate_bundle(function(x) {
  x$built$n_obs <- x$built$n_obs + 0.5
  x
})
run_verifier("Build receipt n_obs must be an exact integer")

mutate_bundle(function(x) {
  x$built$n_sites <- as.numeric(x$built$n_sites)
  x
})
run_verifier("Build receipt n_sites must be the exact integer 34")

mutate_bundle(function(x) {
  x$built$n_analytes <- "garbage"
  x
})
run_verifier("Build receipt n_analytes must be an exact integer")

mutate_bundle(function(x) {
  x$built$n_below <- x$built$n_below + 1L
  x
})
run_verifier("Build receipt n_below must be an exact integer")

mutate_bundle(function(x) {
  x$built$partial <- NA
  x
})
run_verifier("Candidate build receipt partial must be the exact logical FALSE")

mutate_index(function(x) {
  x$n_sites <- as.numeric(x$n_sites)
  x
})
run_verifier("Search index n_sites must be the exact integer 34")

mutate_index(function(x) {
  x$per_site$n[[1]] <- 0.5
  x
})
run_verifier("Search index per-site counts must be exact bounded integers")

mutate_index(function(x) {
  x$analytes$n_obs[[1]] <- x$analytes$n_obs[[1]] + 1L
  x
})
run_verifier("Search index analyte counts disagree with per-site rows")

mutate_index(function(x) {
  x$per_site$units[[1]] <- "inventedUnit"
  x
})
run_verifier("Search index units differ from the established runtime targets")

mutate_index(function(x) {
  x$built$n_runtime_unit_rows_excluded <-
    x$built$n_runtime_unit_rows_excluded + 1L
  x
})
run_verifier("Search index runtime-unit receipt disagrees with the source bundle")

mutate_index(function(x) {
  x$per_site$mean[[1]] <- x$per_site$mean[[1]] + 0.25
  x
})
run_verifier("Search index per-site rows differ from independent recomputation")

reset_fixture()
bad_header <- codebook_original
detected_version <- if (current_candidate) "1.1.0" else "1.0.0"
bad_header[[1]] <- sub(
  paste("version", detected_version), "version 9.9.9",
  bad_header[[1]], fixed = TRUE
)
writeLines(bad_header, codebook_path)
write_fixture_manifest()
run_verifier("Codebook header, version, product, or build provenance changed")

reset_fixture()
bad_provenance <- codebook_original
bad_provenance[[1]] <- sub(
  substr(candidate_original$built$when, 1L, 10L), "1900-01-01",
  bad_provenance[[1]], fixed = TRUE
)
writeLines(bad_provenance, codebook_path)
write_fixture_manifest()
run_verifier("Codebook header, version, product, or build provenance changed")

reset_fixture()
codebook_header <- codebook_original[1:3]
codebook_table <- utils::read.csv(
  codebook_path, comment.char = "#", check.names = FALSE,
  stringsAsFactors = FALSE, na.strings = character(0)
)
first_analyte <- which(codebook_table$section == "analyte_dictionary")[[1]]
codebook_table$name[[first_analyte]] <- "inventedAnalyte"
write_codebook_table(codebook_header, codebook_table)
write_fixture_manifest()
run_verifier("Codebook analyte roster or scalar contract changed")

reset_fixture()
codebook_table <- utils::read.csv(
  codebook_path, comment.char = "#", check.names = FALSE,
  stringsAsFactors = FALSE, na.strings = character(0)
)
first_analyte <- which(codebook_table$section == "analyte_dictionary")[[1]]
codebook_table$units[[first_analyte]] <- "inventedUnit"
write_codebook_table(codebook_original[1:3], codebook_table)
write_fixture_manifest()
run_verifier("Codebook analyte units disagree with the candidate bundle")

reset_fixture()
codebook_table <- utils::read.csv(
  codebook_path, comment.char = "#", check.names = FALSE,
  stringsAsFactors = FALSE, na.strings = character(0)
)
first_analyte <- which(codebook_table$section == "analyte_dictionary")[[1]]
codebook_table$definition[[first_analyte]] <-
  "Analyte count intentionally corrupted"
write_codebook_table(codebook_original[1:3], codebook_table)
write_fixture_manifest()
run_verifier("Codebook analyte counts disagree with the candidate bundle")

for (column in c("allowed", "definition", "na_semantics")) {
  reset_fixture()
  codebook_table <- utils::read.csv(
    codebook_path, comment.char = "#", check.names = FALSE,
    stringsAsFactors = FALSE, na.strings = character(0)
  )
  first_tidy <- which(codebook_table$section == "tidy_long_export")[[1]]
  codebook_table[[column]][[first_tidy]] <- paste("tampered", column)
  write_codebook_table(codebook_original[1:3], codebook_table)
  write_fixture_manifest()
  run_verifier(paste0(
    "Codebook tidy-export allowed values, definitions, or NA semantics ",
    "differ from the reviewed contract"
  ))
}

reset_fixture()
manifest_path <- file.path(fixture_root, "manifest.json")
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
manifest$packages$htmltools <- NULL
jsonlite::write_json(
  manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE
)
run_verifier("Manifest is missing direct runtime package(s): htmltools")

reset_fixture()
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
manifest$packages$shiny$description$Version <- "999.0.0"
jsonlite::write_json(
  manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE, null = "null"
)
run_verifier("Manifest package records differ from the reviewed Connect lock")

reset_fixture()
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
manifest$packages$sf$Source <- "URL"
manifest$packages$sf$description$RemoteType <- "url"
jsonlite::write_json(
  manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE, null = "null"
)
run_verifier("Manifest package lock must use only the fixed standard CRAN snapshot")

reset_fixture()
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
manifest$packages$dplyr$Repository <-
  "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
manifest$packages$dplyr$description$RemoteRepos <-
  "https://packagemanager.posit.co/cran/__linux__/jammy/latest"
jsonlite::write_json(
  manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE, null = "null"
)
run_verifier("Manifest contains a moving package repository")

cat(paste0(
  "Independent verifier rejected fractional/malformed bundle and index ",
  "receipts; complete index drift; adversarial codebook version, provenance, ",
  "roster, unit, count, and reviewed-text mutations; plus package removal, ",
  "version drift, direct URL sources, and moving repositories.\n"
))
