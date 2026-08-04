args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
source(file.path(repo_root, "scripts", "water_refresh_review.R"))

expect_error <- function(expr) {
  message <- tryCatch({
    force(expr)
    ""
  }, error = conditionMessage)
  stopifnot(nzchar(message))
  invisible(message)
}

make_rows <- function(site, collect_date, analyte, values, units, labs,
                      below = "0", flags = NA_character_) {
  n <- length(values)
  data.frame(
    site = rep(site, n), collectDate = rep(collect_date, n),
    analyte = rep(analyte, n), analyteConcentration = values,
    analyteUnits = rep(units, length.out = n),
    laboratoryName = rep(labs, length.out = n),
    belowDetectionQF = rep(below, length.out = n),
    externalLabDataQF = rep(flags, length.out = n),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

run_tests <- function() {
  secret_marker <- "NEON_TOKEN_MUST_NOT_ENTER_REVIEW_8e1324"
  old_token <- Sys.getenv("NEON_TOKEN", unset = NA_character_)
  old_review_dir <- Sys.getenv("WATER_REFRESH_REVIEW_DIR",
                               unset = NA_character_)
  on.exit({
    if (is.na(old_token)) Sys.unsetenv("NEON_TOKEN") else
      Sys.setenv(NEON_TOKEN = old_token)
    if (is.na(old_review_dir)) Sys.unsetenv("WATER_REFRESH_REVIEW_DIR") else
      Sys.setenv(WATER_REFRESH_REVIEW_DIR = old_review_dir)
  }, add = TRUE)

  lab_raw <- rbind(
    # Registered TPN/milligram pair but a new, unaudited exact identity.
    make_rows("CRAM", "2017-08-30T10:00:00Z", "TPN", c("4", "2"),
              "milligram", c("Zulu_Lab", NA_character_),
              c("ND", "0"), c("legacyData", NA_character_)),
    # Exact audited identity: reviewable evidence, not an unapproved change.
    make_rows("WALK", "2019-07-01", "TP", "0.057",
              "microgramsPerLiter", "Florida International University",
              "0", NA_character_),
    # A new unit pair; no value conversion may be attempted by this helper.
    make_rows("WALK", "2019-06-18", "NO3+NO2 - N", "23",
              "microgram", "Example_Lab", "BDL", "formatChange"),
    # Missing labels exercise both strict-gate failure modes and the one
    # established rewrite that is supportable because its target is observed.
    make_rows("ARIK", "2020-01-01", "Ca", "1.1",
              NA_character_, "Example_Lab"),
    make_rows("BARC", "2020-02-01", "Br", "1.2",
              NA_character_, "EcoCore_CSU"),
    make_rows("BIGC", "2020-03-01", "DOC", "1.3",
              NA_character_, "EcoCore_CSU"),
    make_rows("BIGC", "2020-03-02", "DOC", "1.4",
              "milligramsPerLiter", "Example_Lab"),
    # Target-unit and invalid-value rows never reach the mismatch boundary.
    make_rows("WALK", "2019-07-01", "TP", "0.025",
              "milligramsPerLiter", "Example_Lab"),
    make_rows("CRAM", "2017-08-29", "TPN", "not-a-number",
              "milligram", "Decoy_Lab")
  )
  lab_raw$NEON_TOKEN <- secret_marker
  lab_raw$cache_path <- "/runner/private/cache/must-not-enter-review"
  original_lab <- lab_raw

  field_raw <- data.frame(
    site = "CRAM", collectDate = "2017-08-29", waterTemp = 18.5,
    dissolvedOxygen = 8.4, specificConductance = 201,
    credential = secret_marker, stringsAsFactors = FALSE
  )
  coords <- data.frame(
    site = c("CRAM", "WALK"),
    neonName = c("Crampton Lake", "Walker Branch"),
    domain = c("D05", "D07"), state = c("WI", "TN"),
    lat = c(46.2, 35.9), long = c(-89.5, -84.3),
    siteType = c("Lake", "Wadeable stream"),
    source_path = c(secret_marker, secret_marker),
    stringsAsFactors = FALSE
  )

  review_a <- water_unit_mismatch_review(lab_raw)
  review_b <- water_unit_mismatch_review(
    lab_raw[rev(seq_len(nrow(lab_raw))), , drop = FALSE]
  )
  stopifnot(
    identical(lab_raw, original_lab),
    identical(review_a, review_b),
    nrow(review_a) == 6L,
    all(nchar(review_a$source_rows_sha256) == 64L)
  )

  cram <- review_a$site == "CRAM"
  walk_new <- review_a$site == "WALK" &
    review_a$collectDate == "2019-06-18"
  walk_audited <- review_a$site == "WALK" &
    review_a$collectDate == "2019-07-01"
  ca_missing <- review_a$site == "ARIK" &
    review_a$collectDate == "2020-01-01"
  br_without_target <- review_a$site == "BARC" &
    review_a$collectDate == "2020-02-01"
  doc_with_target <- review_a$site == "BIGC" &
    review_a$collectDate == "2020-03-01"
  stopifnot(
    sum(cram) == 1L, sum(walk_new) == 1L, sum(walk_audited) == 1L,
    sum(ca_missing) == 1L, sum(br_without_target) == 1L,
    sum(doc_with_target) == 1L,
    identical(review_a$analyte[cram], "TPN"),
    identical(review_a$from_unit[cram], "milligram"),
    identical(review_a$n_source_rows[cram], 2L),
    identical(review_a$laboratory_provenance[cram],
              "<MISSING> | Zulu_Lab"),
    identical(review_a$laboratory_row_counts[cram],
              "<MISSING>=1 | Zulu_Lab=1"),
    identical(review_a$review_status[cram], "unapproved-identity"),
    identical(review_a$source_value_min[cram], "2"),
    identical(review_a$source_value_median[cram], "3"),
    identical(review_a$source_value_max[cram], "4"),
    identical(review_a$n_below_detection_rows[cram], 1L),
    identical(review_a$review_status[walk_new], "unapproved-unit-pair"),
    identical(review_a$from_unit[walk_new], "microgram"),
    identical(review_a$source_value_min[walk_new], "23"),
    identical(review_a$review_status[walk_audited], "audited-exclusion"),
    identical(review_a$is_unapproved[walk_audited], "false"),
    identical(review_a$required_laboratory[walk_audited],
              "Florida International University"),
    identical(review_a$provenance_matches_policy[walk_audited], "true"),
    identical(review_a$from_unit[ca_missing], WATER_MISSING_UNIT),
    identical(review_a$review_status[ca_missing],
              "unapproved-missing-unit-label"),
    identical(review_a$n_observed_target_rows[ca_missing], 0L),
    identical(review_a$review_status[br_without_target],
              "missing-label-repair-without-target"),
    identical(review_a$n_observed_target_rows[br_without_target], 0L),
    identical(review_a$review_status[doc_with_target],
              "approved-missing-label-rewrite"),
    identical(review_a$n_observed_target_rows[doc_with_target], 1L),
    identical(review_a$required_laboratory[doc_with_target], "EcoCore_CSU"),
    identical(review_a$provenance_matches_policy[doc_with_target], "true"),
    identical(review_a$is_unapproved[doc_with_target], "false")
  )

  walk_wrong_lab <- make_rows(
    "WALK", "2019-07-01", "TP", "0.057", "microgramsPerLiter",
    "Other_Lab"
  )
  walk_missing_lab <- make_rows(
    "WALK", "2019-07-01", "TP", "0.057", "microgramsPerLiter",
    NA_character_
  )
  walk_wrong_review <- water_unit_mismatch_review(walk_wrong_lab)
  walk_missing_review <- water_unit_mismatch_review(walk_missing_lab)
  for (review in list(walk_wrong_review, walk_missing_review)) {
    stopifnot(
      nrow(review) == 1L,
      identical(review$required_laboratory,
                "Florida International University"),
      identical(review$provenance_matches_policy, "false"),
      identical(review$review_status,
                "audited-identity-provenance-mismatch"),
      identical(review$is_unapproved, "true")
    )
  }

  uv_target <- make_rows(
    "SYCA", "2026-01-02", "UV Absorbance (254 nm)", "0.12",
    "absorbance units", "Illinois State Water Survey"
  )
  uv_missing <- function(labs, values = "0.11") {
    make_rows(
      "SYCA", "2026-01-01", "UV Absorbance (254 nm)", values,
      NA_character_, labs
    )
  }
  uv_cross_lab_rows <- rbind(uv_missing("EcoCore_CSU"), uv_target)
  uv_cross_lab_review <- water_unit_mismatch_review(uv_cross_lab_rows)
  stopifnot(
    nrow(uv_cross_lab_review) == 1L,
    identical(uv_cross_lab_review$review_status,
              "approved-missing-label-rewrite"),
    identical(uv_cross_lab_review$required_laboratory, "EcoCore_CSU"),
    identical(uv_cross_lab_review$provenance_matches_policy, "true"),
    identical(uv_cross_lab_review$n_observed_target_rows, 1L),
    identical(uv_cross_lab_review$is_unapproved, "false")
  )

  uv_wrong_rows <- rbind(uv_missing("Other_Lab"), uv_target)
  uv_missing_lab_rows <- rbind(uv_missing(NA_character_), uv_target)
  uv_mixed_rows <- rbind(
    uv_missing(c("EcoCore_CSU", "Other_Lab"), c("0.10", "0.11")),
    uv_target
  )
  uv_whitespace_rows <- rbind(uv_missing("EcoCore_CSU "), uv_target)
  uv_case_rows <- rbind(uv_missing("ecocore_csu"), uv_target)
  for (rows in list(
    uv_wrong_rows, uv_missing_lab_rows, uv_mixed_rows,
    uv_whitespace_rows, uv_case_rows
  )) {
    review <- water_unit_mismatch_review(rows)
    stopifnot(
      nrow(review) == 1L,
      identical(review$required_laboratory, "EcoCore_CSU"),
      identical(review$provenance_matches_policy, "false"),
      identical(review$review_status,
                "missing-label-rewrite-provenance-mismatch"),
      identical(review$is_unapproved, "true")
    )
  }

  uv_no_target_rows <- uv_missing("EcoCore_CSU")
  uv_invalid_date_target_rows <- rbind(
    uv_no_target_rows,
    make_rows(
      "SYCA", "invalid-date", "UV Absorbance (254 nm)", "0.12",
      "absorbance units", "Illinois State Water Survey"
    )
  )
  uv_nonnumeric_target_rows <- rbind(
    uv_no_target_rows,
    make_rows(
      "SYCA", "2026-01-02", "UV Absorbance (254 nm)", "not-a-number",
      "absorbance units", "Illinois State Water Survey"
    )
  )
  for (rows in list(
    uv_no_target_rows, uv_invalid_date_target_rows,
    uv_nonnumeric_target_rows
  )) {
    review <- water_unit_mismatch_review(rows)
    stopifnot(
      nrow(review) == 1L,
      identical(review$required_laboratory, "EcoCore_CSU"),
      identical(review$provenance_matches_policy, "true"),
      identical(review$n_observed_target_rows, 0L),
      identical(review$review_status,
                "missing-label-repair-without-target"),
      identical(review$is_unapproved, "true")
    )
  }

  gate_message <- function(rows) {
    expect_error(canonicalize_water_unit_labels(
      rows$site, as.Date(substr(rows$collectDate, 1L, 10L)), rows$analyte,
      suppressWarnings(as.numeric(rows$analyteConcentration)),
      rows$analyteUnits, rows$laboratoryName
    ))
  }
  ca_rows <- lab_raw[lab_raw$site == "ARIK", , drop = FALSE]
  br_rows <- lab_raw[lab_raw$site == "BARC", , drop = FALSE]
  doc_rows <- lab_raw[lab_raw$site == "BIGC", , drop = FALSE]
  wrong_walk_message <- gate_message(walk_wrong_lab)
  missing_walk_message <- gate_message(walk_missing_lab)
  wrong_rewrite_message <- gate_message(uv_wrong_rows)
  missing_rewrite_message <- gate_message(uv_missing_lab_rows)
  mixed_rewrite_message <- gate_message(uv_mixed_rows)
  whitespace_rewrite_message <- gate_message(uv_whitespace_rows)
  case_rewrite_message <- gate_message(uv_case_rows)
  stopifnot(
    grepl("Unapproved missing unit label", gate_message(ca_rows),
          fixed = TRUE),
    grepl("Missing-label repair lacks an observed target label",
          gate_message(br_rows), fixed = TRUE),
    grepl("required laboratory provenance", wrong_walk_message,
          fixed = TRUE),
    grepl("required laboratory provenance", missing_walk_message,
          fixed = TRUE),
    grepl("required laboratory provenance", wrong_rewrite_message,
          fixed = TRUE),
    grepl("required laboratory provenance", missing_rewrite_message,
          fixed = TRUE),
    grepl("required laboratory provenance", mixed_rewrite_message,
          fixed = TRUE),
    grepl("required laboratory provenance", whitespace_rewrite_message,
          fixed = TRUE),
    grepl("required laboratory provenance", case_rewrite_message,
          fixed = TRUE)
  )
  doc_result <- canonicalize_water_unit_labels(
    doc_rows$site, as.Date(substr(doc_rows$collectDate, 1L, 10L)),
    doc_rows$analyte,
    suppressWarnings(as.numeric(doc_rows$analyteConcentration)),
    doc_rows$analyteUnits, doc_rows$laboratoryName
  )
  stopifnot(
    sum(doc_result$label_receipt$n_rewritten) == 1L,
    identical(doc_result$units,
              rep("milligramsPerLiter", nrow(doc_rows)))
  )

  replay <- water_refresh_replay_inputs(lab_raw, field_raw, coords)
  stopifnot(
    identical(names(replay$lab_raw), WATER_REFRESH_REPLAY_LAB_COLUMNS),
    identical(names(replay$field_raw), WATER_REFRESH_REPLAY_FIELD_COLUMNS),
    identical(names(replay$coords), WATER_REFRESH_REPLAY_COORD_COLUMNS),
    !any(c("NEON_TOKEN", "cache_path", "credential", "source_path") %in%
           unlist(lapply(replay, names), use.names = FALSE)),
    nrow(replay$lab_raw) == nrow(lab_raw),
    identical(
      sort(replay$lab_raw$analyteConcentration, na.last = TRUE),
      sort(as.character(lab_raw$analyteConcentration), na.last = TRUE)
    )
  )

  out_a <- tempfile("water-review-a-")
  out_b <- tempfile("water-review-b-")
  on.exit(unlink(c(out_a, out_b), recursive = TRUE, force = TRUE), add = TRUE)
  Sys.setenv(NEON_TOKEN = secret_marker,
             WATER_REFRESH_REVIEW_DIR = out_a)
  source_sha <- paste(rep("a", 40L), collapse = "")
  result_a <- write_water_refresh_review(
    lab_raw, field_raw, coords, out_a, source_sha = source_sha
  )
  result_b <- write_water_refresh_review(
    lab_raw[rev(seq_len(nrow(lab_raw))), , drop = FALSE],
    field_raw[rev(seq_len(nrow(field_raw))), , drop = FALSE],
    coords[rev(seq_len(nrow(coords))), , drop = FALSE],
    out_b, source_sha = source_sha
  )
  expected_files <- sort(WATER_REFRESH_REVIEW_FILES)
  actual_files <- sort(list.files(out_a, recursive = TRUE))
  stopifnot(identical(actual_files, expected_files),
            identical(sort(result_a$files), expected_files),
            identical(result_a$review, result_b$review),
            identical(result_a$receipt, result_b$receipt))
  validate_water_refresh_review(out_a, expected_source_sha = source_sha)
  validate_water_refresh_review(out_b, expected_source_sha = source_sha)

  file_bytes <- function(path) {
    readBin(path, what = "raw", n = file.info(path)$size)
  }
  for (relative_path in expected_files) {
    stopifnot(identical(
      file_bytes(file.path(out_a, relative_path)),
      file_bytes(file.path(out_b, relative_path))
    ))
  }

  text_files <- expected_files[!grepl("[.]rds$", expected_files)]
  artifact_text <- paste(unlist(lapply(
    file.path(out_a, text_files), readLines, warn = FALSE
  )), collapse = "\n")
  stopifnot(
    !grepl(secret_marker, artifact_text, fixed = TRUE),
    !grepl(out_a, artifact_text, fixed = TRUE),
    !grepl(out_b, artifact_text, fixed = TRUE),
    identical(result_a$receipt$source_sha, source_sha),
    identical(result_a$receipt$n_mismatch_identities, 6L),
    identical(result_a$receipt$n_unapproved_identities, 4L)
  )
  stored_replay <- list(
    lab_raw = readRDS(file.path(out_a, "replay", "lab_raw.rds")),
    field_raw = readRDS(file.path(out_a, "replay", "field_raw.rds")),
    coords = readRDS(file.path(out_a, "replay", "coords.rds"))
  )
  replay_text <- paste(capture.output(dput(stored_replay)), collapse = "\n")
  stopifnot(
    identical(names(stored_replay$lab_raw),
              WATER_REFRESH_REPLAY_LAB_COLUMNS),
    identical(names(stored_replay$field_raw),
              WATER_REFRESH_REPLAY_FIELD_COLUMNS),
    identical(names(stored_replay$coords),
              WATER_REFRESH_REPLAY_COORD_COLUMNS),
    !grepl(secret_marker, replay_text, fixed = TRUE),
    !grepl(out_a, replay_text, fixed = TRUE),
    !grepl(out_b, replay_text, fixed = TRUE),
    !"NEON_TOKEN" %in% names(formals(write_water_refresh_review))
  )

  receipt_line <- readLines(
    file.path(out_a, "water-refresh-review-receipt.sha256"), warn = FALSE
  )
  stopifnot(identical(
    sub("  water-refresh-review-receipt[.]csv$", "", receipt_line),
    water_review_file_sha256(file.path(
      out_a, "water-refresh-review-receipt.csv"
    ))
  ))

  expect_error(validate_water_refresh_review(
    out_a, expected_source_sha = paste(rep("b", 40L), collapse = "")
  ))

  # A self-consistent envelope is not scientific authority. Mutate the stored
  # lab replay's date, unit, and laboratory, then regenerate every content and
  # receipt hash while deliberately retaining the stale approval review. The
  # validator must recompute that review from replay and reject the disagreement.
  authority_out <- tempfile("water-review-authority-gap-")
  on.exit(unlink(authority_out, recursive = TRUE, force = TRUE), add = TRUE)
  dir.create(file.path(authority_out, "replay"), recursive = TRUE)
  for (relative_path in WATER_REFRESH_REVIEW_FILES) {
    stopifnot(file.copy(
      file.path(out_a, relative_path), file.path(authority_out, relative_path),
      overwrite = FALSE
    ))
  }
  authority_lab_path <- file.path(
    authority_out, WATER_REFRESH_REVIEW_CONTENT_FILES[["lab_raw"]]
  )
  authority_lab <- readRDS(authority_lab_path)
  authority_hit <- which(
    authority_lab$site == "WALK" &
      substr(authority_lab$collectDate, 1L, 10L) == "2019-07-01" &
      authority_lab$analyte == "TP" &
      authority_lab$analyteUnits == "microgramsPerLiter"
  )[[1]]
  authority_lab$collectDate[[authority_hit]] <- "2019-07-02"
  authority_lab$analyteUnits[[authority_hit]] <- "microgram"
  authority_lab$laboratoryName[[authority_hit]] <- "Other_Lab"
  authority_field <- readRDS(file.path(
    authority_out, WATER_REFRESH_REVIEW_CONTENT_FILES[["field_raw"]]
  ))
  authority_coords <- readRDS(file.path(
    authority_out, WATER_REFRESH_REVIEW_CONTENT_FILES[["coords"]]
  ))
  authority_lab <- water_refresh_replay_inputs(
    authority_lab, authority_field, authority_coords
  )$lab_raw
  saveRDS(authority_lab, authority_lab_path, version = 3L, compress = "xz")

  authority_receipt_path <- file.path(
    authority_out, WATER_REFRESH_REVIEW_RECEIPT_FILE
  )
  authority_receipt <- water_review_read_csv(authority_receipt_path)
  authority_hash_columns <- c(
    lab_raw = "lab_raw_sha256", field_raw = "field_raw_sha256",
    coords = "coords_sha256", unit_review = "unit_review_sha256"
  )
  for (name in names(authority_hash_columns)) {
    authority_receipt[[authority_hash_columns[[name]]]] <-
      water_review_file_sha256(file.path(
        authority_out, WATER_REFRESH_REVIEW_CONTENT_FILES[[name]]
      ))
  }
  water_review_write_csv(authority_receipt, authority_receipt_path)
  writeLines(
    sprintf(
      "%s  %s", water_review_file_sha256(authority_receipt_path),
      WATER_REFRESH_REVIEW_RECEIPT_FILE
    ),
    file.path(authority_out, WATER_REFRESH_REVIEW_RECEIPT_SHA_FILE),
    useBytes = TRUE
  )
  authority_message <- expect_error(validate_water_refresh_review(
    authority_out, expected_source_sha = source_sha
  ))
  stopifnot(grepl("does not exactly reproduce", authority_message,
                  fixed = TRUE))

  unexpected_path <- file.path(out_b, "unexpected.txt")
  file.create(unexpected_path)
  expect_error(validate_water_refresh_review(
    out_b, expected_source_sha = source_sha
  ))
  unlink(unexpected_path)
  write("tamper", file.path(out_b, "water-unit-mismatch-review.csv"),
        append = TRUE)
  expect_error(validate_water_refresh_review(
    out_b, expected_source_sha = source_sha
  ))

  expect_error(write_water_refresh_review(
    lab_raw, field_raw, coords, out_a, source_sha = source_sha
  ))
  existing_empty <- tempfile("water-review-existing-")
  dir.create(existing_empty)
  on.exit(unlink(existing_empty, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(write_water_refresh_review(
    lab_raw, field_raw, coords, existing_empty, source_sha = source_sha
  ))
  stopifnot(!length(list.files(existing_empty, all.files = TRUE,
                               no.. = TRUE)))
  failed_out <- tempfile("water-review-failed-")
  bad_lab <- lab_raw[, setdiff(names(lab_raw), "belowDetectionQF"),
                     drop = FALSE]
  expect_error(write_water_refresh_review(
    bad_lab, field_raw, coords, failed_out, source_sha = source_sha
  ))
  staging_prefix <- paste0(".", basename(failed_out), ".staging-")
  sibling_entries <- list.files(dirname(failed_out), all.files = TRUE,
                                no.. = TRUE)
  stopifnot(
    !file.exists(failed_out),
    !any(startsWith(sibling_entries, staging_prefix))
  )
  unspecified_out <- tempfile("water-review-unspecified-")
  on.exit(unlink(unspecified_out, recursive = TRUE, force = TRUE), add = TRUE)
  unspecified <- write_water_refresh_review(
    lab_raw, field_raw, coords, unspecified_out
  )
  stopifnot(identical(unspecified$receipt$source_sha, "<UNSPECIFIED>"))
  validate_water_refresh_review(unspecified_out)
  invalid_dir <- tempfile("water-review-invalid-")
  on.exit(unlink(invalid_dir, recursive = TRUE, force = TRUE), add = TRUE)
  expect_error(write_water_refresh_review(
    lab_raw, field_raw, coords, invalid_dir, source_sha = secret_marker
  ))
  stopifnot(!dir.exists(invalid_dir))

  cat("Deterministic Water refresh replay/review evidence tests passed.\n")
}

run_tests()
