# Fail-closed evidence capture for a complete public NEON water-chemistry pull.
#
# This helper deliberately accepts only the three public in-memory inputs used by
# build_swc_bundle(). It never reads NEON_TOKEN, cache locations, or runner paths
# into the artifact. The replay files preserve source rows; the review CSV is a
# deterministic, row-order-independent summary of every non-target unit identity.

if (!exists("WATER_ESTABLISHED_UNIT_TARGETS", inherits = TRUE)) {
  source(file.path("scripts", "water_unit_contract.R"))
}

WATER_REFRESH_REVIEW_SCHEMA <- "water-refresh-review-v1"
WATER_REFRESH_REVIEW_PRODUCT <- "DP1.20093.001"
WATER_REFRESH_REVIEW_BELOW_CODES <- c("1", "ND", "BDL", "BD", "TRUE", "true")

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
  mismatch <- eligible & (
    is.na(target_unit) |
      (from_unit != WATER_MISSING_UNIT & from_unit != target_unit)
  )
  if (!any(mismatch)) return(water_unit_mismatch_review_empty())

  row_key <- water_unit_identity_key(site, collect_date, analyte, from_unit)
  keys <- sort(unique(row_key[mismatch]), method = "radix")
  identities <- WATER_UNIT_EXCLUSION_IDENTITIES
  identity_keys <- water_unit_identity_key(
    identities$site, identities$collectDate, identities$analyte,
    identities$from_unit
  )
  rules <- WATER_UNIT_EXCLUSION_RULES
  rule_keys <- water_unit_rule_key(rules$analyte, rules$from_unit)

  rows <- lapply(keys, function(key) {
    hit <- mismatch & row_key == key
    i <- which(hit)[[1]]
    n_source_rows <- as.integer(sum(hit))
    identity_index <- match(key, identity_keys)
    rule_index <- match(
      water_unit_rule_key(analyte[[i]], from_unit[[i]]), rule_keys
    )
    audited <- !is.na(identity_index)
    max_rows <- if (audited) identities$max_source_rows[[identity_index]] else NA_integer_
    within_bound <- if (audited) n_source_rows <= max_rows else NA
    required_lab <- if (!is.na(rule_index))
      rules$required_laboratory[[rule_index]] else NA_character_
    observed_labs <- water_review_key_component(lab$laboratoryName[hit])
    provenance_ok <- if (is.na(required_lab)) NA else
      all(observed_labs == required_lab)

    status <- if (is.na(target_unit[[i]])) {
      "unregistered-analyte"
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
      unit_rule_reason = if (is.na(rule_index)) NA_character_ else
        rules$reason[[rule_index]],
      required_laboratory = required_lab,
      is_audited_identity = water_review_bool(audited),
      audited_max_source_rows = as.integer(max_rows),
      count_within_audited_bound = water_review_bool(within_bound),
      provenance_matches_policy = water_review_bool(provenance_ok),
      is_unapproved = water_review_bool(status != "audited-exclusion"),
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

write_water_refresh_review <- function(lab_raw, field_raw, coords, out_dir,
                                       source_sha = "") {
  source_sha <- water_review_source_sha(source_sha)
  if (length(out_dir) != 1L || is.na(out_dir) || !nzchar(out_dir)) {
    stop("out_dir must be one explicit non-empty directory.", call. = FALSE)
  }
  if (dir.exists(out_dir) &&
      length(list.files(out_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Refusing to overwrite a non-empty refresh-review directory.",
         call. = FALSE)
  }
  if (!dir.exists(out_dir) &&
      !dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create the refresh-review directory.", call. = FALSE)
  }

  replay <- water_refresh_replay_inputs(lab_raw, field_raw, coords)
  review <- water_unit_mismatch_review(replay$lab_raw)
  replay_dir <- file.path(out_dir, "replay")
  if (!dir.create(replay_dir, showWarnings = FALSE)) {
    stop("Could not create the replay directory.", call. = FALSE)
  }

  relative_files <- c(
    lab_raw = file.path("replay", "lab_raw.rds"),
    field_raw = file.path("replay", "field_raw.rds"),
    coords = file.path("replay", "coords.rds"),
    unit_review = "water-unit-mismatch-review.csv"
  )
  saveRDS(replay$lab_raw, file.path(out_dir, relative_files[["lab_raw"]]),
          version = 3L, compress = "xz")
  saveRDS(replay$field_raw, file.path(out_dir, relative_files[["field_raw"]]),
          version = 3L, compress = "xz")
  saveRDS(replay$coords, file.path(out_dir, relative_files[["coords"]]),
          version = 3L, compress = "xz")
  water_review_write_csv(
    review, file.path(out_dir, relative_files[["unit_review"]])
  )

  file_hash <- vapply(relative_files, function(relative_path) {
    water_review_file_sha256(file.path(out_dir, relative_path))
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
      review$review_status != "audited-exclusion"
    )),
    lab_raw_sha256 = unname(file_hash[["lab_raw"]]),
    field_raw_sha256 = unname(file_hash[["field_raw"]]),
    coords_sha256 = unname(file_hash[["coords"]]),
    unit_review_sha256 = unname(file_hash[["unit_review"]]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  receipt_path <- file.path(out_dir, "water-refresh-review-receipt.csv")
  water_review_write_csv(receipt, receipt_path)
  receipt_sha256 <- water_review_file_sha256(receipt_path)
  writeLines(
    sprintf("%s  water-refresh-review-receipt.csv", receipt_sha256),
    file.path(out_dir, "water-refresh-review-receipt.sha256"),
    useBytes = TRUE
  )
  invisible(list(review = review, receipt = receipt,
                 files = c(unname(relative_files),
                           "water-refresh-review-receipt.csv",
                           "water-refresh-review-receipt.sha256")))
}
