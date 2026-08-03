# Fail-closed evidence capture for a complete public NEON water-chemistry pull.
#
# This helper deliberately accepts only the three public in-memory inputs used by
# build_swc_bundle(). It never reads NEON_TOKEN, cache locations, or runner paths
# into the artifact. The replay files preserve source rows; the review CSV is a
# deterministic, row-order-independent summary of every identity whose unit does
# not already equal its established target, including missing-label identities.

if (!exists("WATER_ESTABLISHED_UNIT_TARGETS", inherits = TRUE)) {
  source(file.path("scripts", "water_unit_contract.R"))
}

WATER_REFRESH_REVIEW_SCHEMA <- "water-refresh-review-v2"
WATER_REFRESH_REVIEW_PRODUCT <- "DP1.20093.001"
WATER_REFRESH_REVIEW_BELOW_CODES <- c("1", "ND", "BDL", "BD", "TRUE", "true")
WATER_REFRESH_REVIEW_STATUSES <- c(
  "unregistered-analyte", "unapproved-missing-unit-label",
  "missing-label-repair-without-target", "approved-missing-label-rewrite",
  "unapproved-unit-pair", "unapproved-identity",
  "audited-identity-over-bound", "audited-identity-provenance-mismatch",
  "audited-exclusion"
)
WATER_REFRESH_REVIEW_APPROVED_STATUSES <- c(
  "audited-exclusion", "approved-missing-label-rewrite"
)

WATER_REFRESH_REPLAY_LAB_COLUMNS <- c(
  "site", "collectDate", "analyte", "analyteConcentration", "analyteUnits",
  "laboratoryName", "belowDetectionQF", "externalLabDataQF"
)
WATER_REFRESH_REPLAY_FIELD_COLUMNS <- c(
  "site", "collectDate", "waterTemp", "dissolvedOxygen",
  "specificConductance"
)
WATER_REFRESH_REPLAY_COORD_COLUMNS <- c(
  "site", "neonName", "domain", "state", "lat", "long", "siteType"
)
WATER_REFRESH_REVIEW_CONTENT_FILES <- c(
  lab_raw = file.path("replay", "lab_raw.rds"),
  field_raw = file.path("replay", "field_raw.rds"),
  coords = file.path("replay", "coords.rds"),
  unit_review = "water-unit-mismatch-review.csv"
)
WATER_REFRESH_REVIEW_RECEIPT_FILE <- "water-refresh-review-receipt.csv"
WATER_REFRESH_REVIEW_RECEIPT_SHA_FILE <-
  "water-refresh-review-receipt.sha256"
WATER_REFRESH_REVIEW_FILES <- c(
  unname(WATER_REFRESH_REVIEW_CONTENT_FILES),
  WATER_REFRESH_REVIEW_RECEIPT_FILE,
  WATER_REFRESH_REVIEW_RECEIPT_SHA_FILE
)
WATER_REFRESH_REVIEW_RECEIPT_COLUMNS <- c(
  "review_schema", "unit_policy", "product", "source_sha", "n_lab_rows",
  "n_field_rows", "n_coord_rows", "n_mismatch_identities",
  "n_mismatch_source_rows", "n_unapproved_identities", "lab_raw_sha256",
  "field_raw_sha256", "coords_sha256", "unit_review_sha256"
)

water_review_require_columns <- function(x, required, label) {
  if (!is.data.frame(x)) {
    stop(sprintf("%s must be a data frame.", label), call. = FALSE)
  }
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(sprintf("%s is missing required column(s): %s",
                 label, paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

water_review_character <- function(x) {
  out <- enc2utf8(as.character(x))
  out
}

water_review_source_sha <- function(source_sha) {
  source_sha <- trimws(as.character(source_sha))
  if (length(source_sha) != 1L || is.na(source_sha)) {
    stop("source_sha must be one exact 40-character Git object ID.",
         call. = FALSE)
  }
  if (!nzchar(source_sha)) return("<UNSPECIFIED>")
  if (
      !grepl("^[0-9a-fA-F]{40}$", source_sha, perl = TRUE)) {
    stop("source_sha must be one exact 40-character Git object ID.",
         call. = FALSE)
  }
  tolower(source_sha)
}

water_refresh_replay_inputs <- function(lab_raw, field_raw, coords) {
  lab_required <- setdiff(WATER_REFRESH_REPLAY_LAB_COLUMNS, "laboratoryName")
  water_review_require_columns(lab_raw, lab_required, "lab_raw")
  if (!"laboratoryName" %in% names(lab_raw)) {
    lab_raw$laboratoryName <- NA_character_
  }
  lab <- data.frame(
    site = water_review_character(lab_raw$site),
    collectDate = water_review_character(lab_raw$collectDate),
    analyte = water_review_character(lab_raw$analyte),
    analyteConcentration = water_review_character(
      lab_raw$analyteConcentration
    ),
    analyteUnits = water_review_character(lab_raw$analyteUnits),
    laboratoryName = water_review_character(lab_raw$laboratoryName),
    belowDetectionQF = water_review_character(lab_raw$belowDetectionQF),
    externalLabDataQF = water_review_character(lab_raw$externalLabDataQF),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!is.data.frame(field_raw)) {
    stop("field_raw must be a data frame.", call. = FALSE)
  }
  if (nrow(field_raw)) {
    water_review_require_columns(field_raw, c("site", "collectDate"),
                                 "field_raw")
  }
  field_value <- function(name) {
    if (name %in% names(field_raw)) {
      suppressWarnings(as.numeric(field_raw[[name]]))
    } else {
      rep(NA_real_, nrow(field_raw))
    }
  }
  field <- data.frame(
    site = if ("site" %in% names(field_raw))
      water_review_character(field_raw$site) else character(),
    collectDate = if ("collectDate" %in% names(field_raw))
      water_review_character(field_raw$collectDate) else character(),
    waterTemp = field_value("waterTemp"),
    dissolvedOxygen = field_value("dissolvedOxygen"),
    specificConductance = field_value("specificConductance"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  water_review_require_columns(coords, WATER_REFRESH_REPLAY_COORD_COLUMNS,
                               "coords")
  coord <- data.frame(
    site = water_review_character(coords$site),
    neonName = water_review_character(coords$neonName),
    domain = water_review_character(coords$domain),
    state = water_review_character(coords$state),
    lat = suppressWarnings(as.numeric(coords$lat)),
    long = suppressWarnings(as.numeric(coords$long)),
    siteType = water_review_character(coords$siteType),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  # Canonical row order makes the public replay bytes and their receipt stable
  # across API/file enumeration order without changing any row or value.
  lab <- water_review_sort_frame(lab)
  field <- water_review_sort_frame(field)
  coord <- water_review_sort_frame(coord)
  row.names(lab) <- row.names(field) <- row.names(coord) <- NULL
  list(lab_raw = lab, field_raw = field, coords = coord)
}

water_review_key_component <- function(x) {
  x <- water_review_character(x)
  x[is.na(x) | !nzchar(x)] <- "<MISSING>"
  x
}

water_review_sort_frame <- function(x) {
  if (!nrow(x)) return(x)
  keys <- lapply(x, function(value) {
    if (is.numeric(value)) {
      value <- ifelse(is.na(value), "<NA>", sprintf("%.17g", value))
    } else {
      value <- water_review_character(value)
      value[is.na(value)] <- "<NA>"
    }
    value
  })
  ord <- do.call(order, c(keys, list(method = "radix")))
  out <- x[ord, , drop = FALSE]
  row.names(out) <- NULL
  out
}

water_review_label_counts <- function(x) {
  labels <- water_review_key_component(x)
  levels <- sort(unique(labels), method = "radix")
  counts <- vapply(levels, function(label) sum(labels == label), integer(1))
  paste(sprintf("%s=%d", levels, counts), collapse = " | ")
}

water_review_number <- function(x) {
  if (!length(x) || all(is.na(x))) return(NA_character_)
  sprintf("%.17g", as.numeric(x[[1]]))
}

water_review_bool <- function(x) {
  if (length(x) != 1L || is.na(x)) return(NA_character_)
  if (isTRUE(x)) "true" else "false"
}

water_unit_mismatch_review_empty <- function() {
  data.frame(
    review_schema = character(), unit_policy = character(),
    site = character(), collectDate = character(), analyte = character(),
    from_unit = character(), target_unit = character(),
    n_source_rows = integer(), laboratory_provenance = character(),
    laboratory_row_counts = character(), unit_rule_reason = character(),
    required_laboratory = character(), is_audited_identity = character(),
    audited_max_source_rows = integer(),
    count_within_audited_bound = character(),
    n_observed_target_rows = integer(),
    provenance_matches_policy = character(), is_unapproved = character(),
    review_status = character(), n_distinct_value_text = integer(),
    source_value_min = character(), source_value_median = character(),
    source_value_max = character(), n_below_detection_rows = integer(),
    below_detection_qf_counts = character(),
    n_external_lab_flagged_rows = integer(),
    external_lab_qf_counts = character(), source_rows_sha256 = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

water_review_status_is_unapproved <- function(status) {
  !as.character(status) %in% WATER_REFRESH_REVIEW_APPROVED_STATUSES
}

water_unit_mismatch_review <- function(lab_raw) {
  replay <- water_refresh_replay_inputs(
    lab_raw, data.frame(),
    data.frame(
      site = character(), neonName = character(), domain = character(),
      state = character(), lat = numeric(), long = numeric(),
      siteType = character(), stringsAsFactors = FALSE
    )
  )
  lab <- replay$lab_raw
  parsed_date <- suppressWarnings(as.Date(substr(lab$collectDate, 1L, 10L)))
  parsed_value <- suppressWarnings(as.numeric(lab$analyteConcentration))
  eligible <- !is.na(parsed_date) & !is.na(parsed_value)
  if (!any(eligible)) return(water_unit_mismatch_review_empty())

  site <- water_review_key_component(lab$site)
  collect_date <- as.character(parsed_date)
  analyte <- water_review_key_component(lab$analyte)
  from_unit <- water_unit_source_label(lab$analyteUnits)
  target_unit <- unname(WATER_ESTABLISHED_UNIT_TARGETS[analyte])
  reviewable <- eligible & (
    is.na(target_unit) | from_unit == WATER_MISSING_UNIT |
      from_unit != target_unit
  )
  if (!any(reviewable)) return(water_unit_mismatch_review_empty())

  row_key <- water_unit_identity_key(site, collect_date, analyte, from_unit)
  keys <- sort(unique(row_key[reviewable]), method = "radix")
  row_groups <- split(which(reviewable), row_key[reviewable])
  target_source <- eligible & !is.na(target_unit) & from_unit == target_unit
  target_counts <- tabulate(
    match(analyte[target_source], names(WATER_ESTABLISHED_UNIT_TARGETS)),
    nbins = length(WATER_ESTABLISHED_UNIT_TARGETS)
  )
  names(target_counts) <- names(WATER_ESTABLISHED_UNIT_TARGETS)
  identities <- WATER_UNIT_EXCLUSION_IDENTITIES
  identity_keys <- water_unit_identity_key(
    identities$site, identities$collectDate, identities$analyte,
    identities$from_unit
  )
  rules <- WATER_UNIT_EXCLUSION_RULES
  rule_keys <- water_unit_rule_key(rules$analyte, rules$from_unit)
  rewrite_rules <- WATER_UNIT_LABEL_REWRITES
  rewrite_keys <- water_unit_rule_key(
    rewrite_rules$analyte, rewrite_rules$from_unit
  )

  rows <- lapply(keys, function(key) {
    hit <- row_groups[[key]]
    i <- hit[[1]]
    n_source_rows <- as.integer(length(hit))
    identity_index <- match(key, identity_keys)
    rule_index <- match(
      water_unit_rule_key(analyte[[i]], from_unit[[i]]), rule_keys
    )
    rewrite_index <- match(
      water_unit_rule_key(analyte[[i]], from_unit[[i]]), rewrite_keys
    )
    audited <- !is.na(identity_index)
    max_rows <- if (audited) identities$max_source_rows[[identity_index]] else NA_integer_
    within_bound <- if (audited) n_source_rows <= max_rows else NA
    required_lab <- if (!is.na(rule_index))
      rules$required_laboratory[[rule_index]] else NA_character_
    observed_labs <- water_review_key_component(lab$laboratoryName[hit])
    provenance_ok <- if (is.na(required_lab)) NA else
      all(observed_labs == required_lab)
    missing_label <- from_unit[[i]] == WATER_MISSING_UNIT
    n_target_source <- if (missing_label && !is.na(target_unit[[i]])) {
      as.integer(target_counts[[analyte[[i]]]])
    } else {
      NA_integer_
    }

    status <- if (is.na(target_unit[[i]])) {
      "unregistered-analyte"
    } else if (missing_label && is.na(rewrite_index)) {
      "unapproved-missing-unit-label"
    } else if (missing_label && n_target_source == 0L) {
      "missing-label-repair-without-target"
    } else if (missing_label) {
      "approved-missing-label-rewrite"
    } else if (is.na(rule_index)) {
      "unapproved-unit-pair"
    } else if (!audited) {
      "unapproved-identity"
    } else if (!isTRUE(within_bound)) {
      "audited-identity-over-bound"
    } else if (!is.na(required_lab) && !isTRUE(provenance_ok)) {
      "audited-identity-provenance-mismatch"
    } else {
      "audited-exclusion"
    }

    values <- parsed_value[hit]
    raw_rows <- water_review_sort_frame(lab[hit, , drop = FALSE])
    data.frame(
      review_schema = WATER_REFRESH_REVIEW_SCHEMA,
      unit_policy = WATER_UNIT_POLICY,
      site = site[[i]], collectDate = collect_date[[i]],
      analyte = analyte[[i]], from_unit = from_unit[[i]],
      target_unit = if (is.na(target_unit[[i]])) NA_character_ else target_unit[[i]],
      n_source_rows = n_source_rows,
      laboratory_provenance = paste(
        sort(unique(observed_labs), method = "radix"), collapse = " | "
      ),
      laboratory_row_counts = water_review_label_counts(lab$laboratoryName[hit]),
      unit_rule_reason = if (!is.na(rule_index)) {
        rules$reason[[rule_index]]
      } else if (!is.na(rewrite_index)) {
        "established-missing-label-rewrite"
      } else {
        NA_character_
      },
      required_laboratory = required_lab,
      is_audited_identity = water_review_bool(audited),
      audited_max_source_rows = as.integer(max_rows),
      count_within_audited_bound = water_review_bool(within_bound),
      n_observed_target_rows = n_target_source,
      provenance_matches_policy = water_review_bool(provenance_ok),
      is_unapproved = water_review_bool(
        water_review_status_is_unapproved(status)
      ),
      review_status = status,
      n_distinct_value_text = as.integer(length(unique(
        lab$analyteConcentration[hit]
      ))),
      source_value_min = water_review_number(min(values)),
      source_value_median = water_review_number(stats::median(values)),
      source_value_max = water_review_number(max(values)),
      n_below_detection_rows = as.integer(sum(
        lab$belowDetectionQF[hit] %in% WATER_REFRESH_REVIEW_BELOW_CODES
      )),
      below_detection_qf_counts = water_review_label_counts(
        lab$belowDetectionQF[hit]
      ),
      n_external_lab_flagged_rows = as.integer(sum(
        !is.na(lab$externalLabDataQF[hit]) &
          nzchar(lab$externalLabDataQF[hit])
      )),
      external_lab_qf_counts = water_review_label_counts(
        lab$externalLabDataQF[hit]
      ),
      source_rows_sha256 = water_unit_receipt_sha256(raw_rows),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

water_review_write_csv <- function(x, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  utils::write.table(
    x, con, sep = ",", row.names = FALSE, col.names = TRUE, quote = TRUE,
    qmethod = "double", na = "<NA>", eol = "\n"
  )
  invisible(path)
}

water_review_file_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("digest is required for the refresh-review receipt.", call. = FALSE)
  }
  digest::digest(path, algo = "sha256", serialize = FALSE, file = TRUE)
}

water_review_read_csv <- function(path) {
  utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = "<NA>", colClasses = "character"
  )
}

water_review_exact_integer <- function(x, label) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value < 0 || value != floor(value) || value > .Machine$integer.max) {
    stop(sprintf("Refresh-review receipt has invalid %s.", label),
         call. = FALSE)
  }
  as.integer(value)
}

validate_water_refresh_review <- function(out_dir, expected_source_sha = NULL) {
  if (length(out_dir) != 1L || is.na(out_dir) || !nzchar(out_dir) ||
      !dir.exists(out_dir)) {
    stop("Refresh-review artifact directory does not exist.", call. = FALSE)
  }

  expected_entries <- sort(
    c("replay", WATER_REFRESH_REVIEW_FILES), method = "radix"
  )
  actual_entries <- sort(list.files(
    out_dir, all.files = TRUE, no.. = TRUE, recursive = TRUE,
    include.dirs = TRUE
  ), method = "radix")
  if (!identical(actual_entries, expected_entries)) {
    stop(sprintf(
      "Refresh-review artifact entries differ from the exact allowlist (missing: %s; unexpected: %s).",
      paste(setdiff(expected_entries, actual_entries), collapse = ", "),
      paste(setdiff(actual_entries, expected_entries), collapse = ", ")
    ), call. = FALSE)
  }
  artifact_paths <- file.path(out_dir, WATER_REFRESH_REVIEW_FILES)
  artifact_info <- file.info(artifact_paths)
  if (anyNA(artifact_info$isdir) || any(artifact_info$isdir)) {
    stop("Refresh-review allowlisted paths must be regular files.",
         call. = FALSE)
  }
  link_targets <- Sys.readlink(file.path(out_dir, actual_entries))
  if (any(!is.na(link_targets) & nzchar(link_targets))) {
    stop("Refresh-review artifact may not contain symbolic links.",
         call. = FALSE)
  }

  receipt_path <- file.path(out_dir, WATER_REFRESH_REVIEW_RECEIPT_FILE)
  receipt <- water_review_read_csv(receipt_path)
  if (nrow(receipt) != 1L ||
      !identical(names(receipt), WATER_REFRESH_REVIEW_RECEIPT_COLUMNS)) {
    stop("Refresh-review receipt schema or row count is invalid.",
         call. = FALSE)
  }
  if (!identical(receipt$review_schema[[1]], WATER_REFRESH_REVIEW_SCHEMA) ||
      !identical(receipt$unit_policy[[1]], WATER_UNIT_POLICY) ||
      !identical(receipt$product[[1]], WATER_REFRESH_REVIEW_PRODUCT)) {
    stop("Refresh-review receipt contract does not match this source revision.",
         call. = FALSE)
  }
  recorded_source_sha <- receipt$source_sha[[1]]
  if (is.na(recorded_source_sha) || !nzchar(recorded_source_sha)) {
    stop("Refresh-review receipt is missing its source revision.",
         call. = FALSE)
  }
  if (!identical(recorded_source_sha, "<UNSPECIFIED>")) {
    recorded_source_sha <- water_review_source_sha(recorded_source_sha)
  }
  if (!is.null(expected_source_sha)) {
    normalized_expected_sha <- if (
      identical(as.character(expected_source_sha), "<UNSPECIFIED>")) {
      "<UNSPECIFIED>"
    } else {
      water_review_source_sha(expected_source_sha)
    }
    if (!identical(recorded_source_sha, normalized_expected_sha)) {
      stop("Refresh-review receipt source revision does not match the workflow head.",
           call. = FALSE)
    }
  }

  hash_columns <- c(
    lab_raw = "lab_raw_sha256", field_raw = "field_raw_sha256",
    coords = "coords_sha256", unit_review = "unit_review_sha256"
  )
  for (name in names(hash_columns)) {
    recorded_hash <- receipt[[hash_columns[[name]]]][[1]]
    actual_hash <- water_review_file_sha256(file.path(
      out_dir, WATER_REFRESH_REVIEW_CONTENT_FILES[[name]]
    ))
    if (is.na(recorded_hash) ||
        !grepl("^[0-9a-f]{64}$", recorded_hash, perl = TRUE) ||
        !identical(recorded_hash, actual_hash)) {
      stop(sprintf("Refresh-review content hash mismatch: %s.", name),
           call. = FALSE)
    }
  }

  receipt_sha_path <- file.path(
    out_dir, WATER_REFRESH_REVIEW_RECEIPT_SHA_FILE
  )
  receipt_sha_lines <- readLines(receipt_sha_path, warn = FALSE)
  expected_receipt_sha_line <- sprintf(
    "%s  %s", water_review_file_sha256(receipt_path),
    WATER_REFRESH_REVIEW_RECEIPT_FILE
  )
  if (!identical(receipt_sha_lines, expected_receipt_sha_line)) {
    stop("Refresh-review receipt checksum is invalid.", call. = FALSE)
  }

  lab <- readRDS(file.path(
    out_dir, WATER_REFRESH_REVIEW_CONTENT_FILES[["lab_raw"]]
  ))
  field <- readRDS(file.path(
    out_dir, WATER_REFRESH_REVIEW_CONTENT_FILES[["field_raw"]]
  ))
  coords <- readRDS(file.path(
    out_dir, WATER_REFRESH_REVIEW_CONTENT_FILES[["coords"]]
  ))
  if (!is.data.frame(lab) ||
      !identical(names(lab), WATER_REFRESH_REPLAY_LAB_COLUMNS) ||
      !is.data.frame(field) ||
      !identical(names(field), WATER_REFRESH_REPLAY_FIELD_COLUMNS) ||
      !is.data.frame(coords) ||
      !identical(names(coords), WATER_REFRESH_REPLAY_COORD_COLUMNS)) {
    stop("Refresh-review replay schema is invalid.", call. = FALSE)
  }

  review <- water_review_read_csv(file.path(
    out_dir, WATER_REFRESH_REVIEW_CONTENT_FILES[["unit_review"]]
  ))
  if (!identical(names(review), names(water_unit_mismatch_review_empty()))) {
    stop("Refresh-review unit-review schema is invalid.", call. = FALSE)
  }
  if (nrow(review)) {
    if (anyNA(review$review_status) ||
        any(!review$review_status %in% WATER_REFRESH_REVIEW_STATUSES) ||
        any(review$review_schema != WATER_REFRESH_REVIEW_SCHEMA) ||
        any(review$unit_policy != WATER_UNIT_POLICY)) {
      stop("Refresh-review unit-review rows violate the review contract.",
           call. = FALSE)
    }
    expected_unapproved <- vapply(
      water_review_status_is_unapproved(review$review_status),
      water_review_bool, character(1)
    )
    if (!identical(review$is_unapproved, expected_unapproved)) {
      stop("Refresh-review approval flags disagree with review statuses.",
           call. = FALSE)
    }
  }
  source_rows <- suppressWarnings(as.numeric(review$n_source_rows))
  if (length(source_rows) &&
      (anyNA(source_rows) || any(!is.finite(source_rows)) ||
       any(source_rows < 1) || any(source_rows != floor(source_rows)))) {
    stop("Refresh-review source-row counts are invalid.", call. = FALSE)
  }

  expected_counts <- c(
    n_lab_rows = nrow(lab), n_field_rows = nrow(field),
    n_coord_rows = nrow(coords), n_mismatch_identities = nrow(review),
    n_mismatch_source_rows = sum(source_rows),
    n_unapproved_identities = sum(
      water_review_status_is_unapproved(review$review_status)
    )
  )
  for (name in names(expected_counts)) {
    recorded_count <- water_review_exact_integer(receipt[[name]][[1]], name)
    if (!identical(recorded_count, as.integer(expected_counts[[name]]))) {
      stop(sprintf("Refresh-review receipt count mismatch: %s.", name),
           call. = FALSE)
    }
  }
  invisible(receipt)
}

write_water_refresh_review <- function(lab_raw, field_raw, coords, out_dir,
                                       source_sha = "") {
  source_sha <- water_review_source_sha(source_sha)
  if (length(out_dir) != 1L || is.na(out_dir) || !nzchar(out_dir)) {
    stop("out_dir must be one explicit non-empty directory.", call. = FALSE)
  }
  if (file.exists(out_dir)) {
    stop("Refusing to overwrite an existing refresh-review path.",
         call. = FALSE)
  }
  parent_dir <- dirname(out_dir)
  if (!dir.exists(parent_dir) &&
      !dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the refresh-review parent directory.",
         call. = FALSE)
  }
  staging_dir <- tempfile(
    pattern = paste0(".", basename(out_dir), ".staging-"),
    tmpdir = parent_dir
  )
  if (!dir.create(staging_dir, showWarnings = FALSE)) {
    stop("Could not create the refresh-review staging directory.",
         call. = FALSE)
  }
  published <- FALSE
  on.exit({
    if (!published && dir.exists(staging_dir)) {
      unlink(staging_dir, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  replay <- water_refresh_replay_inputs(lab_raw, field_raw, coords)
  review <- water_unit_mismatch_review(replay$lab_raw)
  replay_dir <- file.path(staging_dir, "replay")
  if (!dir.create(replay_dir, showWarnings = FALSE)) {
    stop("Could not create the replay directory.", call. = FALSE)
  }

  relative_files <- WATER_REFRESH_REVIEW_CONTENT_FILES
  saveRDS(replay$lab_raw, file.path(staging_dir, relative_files[["lab_raw"]]),
          version = 3L, compress = "xz")
  saveRDS(replay$field_raw, file.path(staging_dir, relative_files[["field_raw"]]),
          version = 3L, compress = "xz")
  saveRDS(replay$coords, file.path(staging_dir, relative_files[["coords"]]),
          version = 3L, compress = "xz")
  water_review_write_csv(
    review, file.path(staging_dir, relative_files[["unit_review"]])
  )

  file_hash <- vapply(relative_files, function(relative_path) {
    water_review_file_sha256(file.path(staging_dir, relative_path))
  }, character(1))
  receipt <- data.frame(
    review_schema = WATER_REFRESH_REVIEW_SCHEMA,
    unit_policy = WATER_UNIT_POLICY,
    product = WATER_REFRESH_REVIEW_PRODUCT,
    source_sha = source_sha,
    n_lab_rows = as.integer(nrow(replay$lab_raw)),
    n_field_rows = as.integer(nrow(replay$field_raw)),
    n_coord_rows = as.integer(nrow(replay$coords)),
    n_mismatch_identities = as.integer(nrow(review)),
    n_mismatch_source_rows = as.integer(sum(review$n_source_rows)),
    n_unapproved_identities = as.integer(sum(
      water_review_status_is_unapproved(review$review_status)
    )),
    lab_raw_sha256 = unname(file_hash[["lab_raw"]]),
    field_raw_sha256 = unname(file_hash[["field_raw"]]),
    coords_sha256 = unname(file_hash[["coords"]]),
    unit_review_sha256 = unname(file_hash[["unit_review"]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  receipt_path <- file.path(staging_dir, WATER_REFRESH_REVIEW_RECEIPT_FILE)
  water_review_write_csv(receipt, receipt_path)
  receipt_sha256 <- water_review_file_sha256(receipt_path)
  writeLines(
    sprintf("%s  %s", receipt_sha256, WATER_REFRESH_REVIEW_RECEIPT_FILE),
    file.path(staging_dir, WATER_REFRESH_REVIEW_RECEIPT_SHA_FILE),
    useBytes = TRUE
  )
  validate_water_refresh_review(
    staging_dir, expected_source_sha = source_sha
  )
  if (!isTRUE(file.rename(staging_dir, out_dir)) || !dir.exists(out_dir)) {
    stop("Could not atomically publish the complete refresh-review directory.",
         call. = FALSE)
  }
  published <- TRUE
  invisible(list(review = review, receipt = receipt,
                 files = WATER_REFRESH_REVIEW_FILES))
}
