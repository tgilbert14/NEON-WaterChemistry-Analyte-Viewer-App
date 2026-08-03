# Fail-closed unit policy for the surface-water-chemistry producer and validator.
# Numeric values are never converted by this policy. Missing labels may be filled
# only for explicit established analytes. Non-missing unit mismatches are never
# relabelled: a small audited legacy identity set is quarantined, and every other
# mismatch stops the build for scientific review.

WATER_UNIT_POLICY <-
  "explicit-targets-audited-exclusions-value-invariant-v3"
WATER_MISSING_UNIT <- "<MISSING>"
WATER_EXCLUDED_UNIT <- "<EXCLUDED>"
WATER_LEGACY_BUNDLE_SHA256 <-
  "dce312128e4392aaecdba535bc92be5d9ae8f1dc0e938745b530bc2a4f7f0868"

# Exact presentation target for every analyte in the established 34-analyte
# bundle. The 31 external-lab analytes arrive with row-level unit metadata, so
# their source-label transitions are checked here. The three field-probe
# analytes are emitted by fixed extraction columns without row-level unit labels;
# their established units are explicit extraction assumptions, not observed
# upstream unit-transition receipts. A new analyte or unreviewed target fails.
WATER_ESTABLISHED_UNIT_TARGETS <- c(
  ANC = "milliequivalentsPerLiter",
  Br = "milligramsPerLiter",
  Ca = "milligramsPerLiter",
  Cl = "milligramsPerLiter",
  DIC = "milligramsPerLiter",
  dissolvedOxygenField = "milligramsPerLiter",
  DOC = "milligramsPerLiter",
  F = "milligramsPerLiter",
  Fe = "milligramsPerLiter",
  K = "milligramsPerLiter",
  Mg = "milligramsPerLiter",
  Mn = "milligramsPerLiter",
  Na = "milligramsPerLiter",
  `NH4 - N` = "milligramsPerLiter",
  `NO2 - N` = "milligramsPerLiter",
  `NO3+NO2 - N` = "milligramsPerLiter",
  `Ortho - P` = "milligramsPerLiter",
  Si = "milligramsPerLiter",
  SO4 = "milligramsPerLiter",
  specificConductance = "microsiemensPerCentimeter",
  specificConductanceField = "microsiemensPerCentimeter",
  TDN = "milligramsPerLiter",
  TDP = "milligramsPerLiter",
  TDS = "milligramsPerLiter",
  TN = "milligramsPerLiter",
  TOC = "milligramsPerLiter",
  TP = "milligramsPerLiter",
  TPC = "microgramsPerLiter",
  TPN = "microgramsPerLiter",
  TSS = "milligramsPerLiter",
  `TSS - Dry Mass` = "microgram",
  `UV Absorbance (254 nm)` = "absorbance units",
  `UV Absorbance (280 nm)` = "absorbance units",
  waterTemp = "celsius"
)

# Missing labels are metadata omissions, not alternate unit claims. Fill them
# only for these exact analyte/target pairs and only when a source row carrying
# the target label is present in the same build.
WATER_UNIT_LABEL_REWRITES <- data.frame(
  analyte = c(
    "Br", "Cl", "DIC", "DOC", "F", "SO4", "TDN", "TDS", "TN", "TOC",
    "UV Absorbance (254 nm)", "UV Absorbance (280 nm)"
  ),
  from_unit = rep(WATER_MISSING_UNIT, 12L),
  to_unit = unname(WATER_ESTABLISHED_UNIT_TARGETS[c(
    "Br", "Cl", "DIC", "DOC", "F", "SO4", "TDN", "TDS", "TN", "TOC",
    "UV Absorbance (254 nm)", "UV Absorbance (280 nm)"
  )]),
  stringsAsFactors = FALSE
)

# Non-missing mismatches are excluded, never relabelled. Six concentration
# pairs are the audited WALK-2019 label defect. The residual TPC/TPN `milligram`
# identities conflict with NEON's current product change log, which says EcoCore
# particulate C/N was converted to microgramsPerLiter. They are therefore
# unresolved legacy unit anomalies: quarantine them conservatively until their
# source history is reconciled rather than guessing at a conversion.
WATER_UNIT_EXCLUSION_RULES <- data.frame(
  analyte = c(
    "NH4 - N", "NO2 - N", "NO3+NO2 - N", "Ortho - P", "TDP", "TP",
    "TPC", "TPN"
  ),
  from_unit = c(rep("microgramsPerLiter", 6L), "milligram", "milligram"),
  reason = c(
    rep("audited-legacy-mislabeled-concentration", 6L),
    rep("unresolved-legacy-particulate-unit-anomaly", 2L)
  ),
  required_laboratory = c(rep(NA_character_, 6L),
                          "EcoCore_CSU", "EcoCore_CSU"),
  stringsAsFactors = FALSE
)

.water_exclusion_identity_rows <- c(
  "WALK\t2019-06-18\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-01\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-09-16\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-10-07\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-06-18\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-07-01\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-08-13\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-09-04\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-09-16\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-10-07\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-11-04\tNO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tNO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-01\tNO3+NO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-08-13\tNO3+NO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-10-07\tNO3+NO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-06-18\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-01\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-08-13\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-09-04\tOrtho - P\tmicrogramsPerLiter\t3",
  "WALK\t2019-09-16\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-10-07\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-11-04\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-22\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-08-13\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-09-16\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-10-07\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-11-04\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-06-18\tTP\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-01\tTP\tmicrogramsPerLiter\t2",
  "WALK\t2019-08-13\tTP\tmicrogramsPerLiter\t2",
  "WALK\t2019-09-04\tTP\tmicrogramsPerLiter\t4",
  "WALK\t2019-11-04\tTP\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tTP\tmicrogramsPerLiter\t2",
  "CRAM\t2017-08-29\tTPC\tmilligram\t12",
  "CUPE\t2017-07-11\tTPC\tmilligram\t2",
  "LECO\t2017-11-27\tTPC\tmilligram\t1",
  "LEWI\t2017-12-13\tTPC\tmilligram\t2",
  "OKSR\t2017-05-28\tTPC\tmilligram\t2",
  "REDB\t2017-04-18\tTPC\tmilligram\t2",
  "REDB\t2017-04-25\tTPC\tmilligram\t2",
  "WALK\t2017-11-20\tTPC\tmilligram\t1",
  "LECO\t2017-11-27\tTPN\tmilligram\t1",
  "OKSR\t2017-05-28\tTPN\tmilligram\t2",
  "REDB\t2017-04-25\tTPN\tmilligram\t2",
  "WALK\t2017-11-20\tTPN\tmilligram\t1"
)
.water_identity_parts <- strsplit(.water_exclusion_identity_rows, "\t",
                                  fixed = TRUE)
WATER_UNIT_EXCLUSION_IDENTITIES <- data.frame(
  site = vapply(.water_identity_parts, `[[`, character(1), 1L),
  collectDate = vapply(.water_identity_parts, `[[`, character(1), 2L),
  analyte = vapply(.water_identity_parts, `[[`, character(1), 3L),
  from_unit = vapply(.water_identity_parts, `[[`, character(1), 4L),
  max_source_rows = as.integer(vapply(.water_identity_parts, `[[`,
                                      character(1), 5L)),
  stringsAsFactors = FALSE
)
rm(.water_identity_parts, .water_exclusion_identity_rows)

WATER_UNIT_RECEIPT_FIELDS <- c(
  "unit_policy", "n_unit_labels_rewritten", "n_unit_rows_excluded",
  "n_unit_values_changed", "unit_label_rewrites",
  "unit_rewrite_receipt_sha256", "unit_row_exclusions",
  "unit_exclusion_receipt_sha256"
)

water_unit_source_label <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- WATER_MISSING_UNIT
  x
}

water_unit_rule_key <- function(analyte, from_unit) {
  paste(as.character(analyte), as.character(from_unit), sep = "\r")
}

water_unit_identity_key <- function(site, collectDate, analyte, from_unit) {
  paste(as.character(site), as.character(collectDate), as.character(analyte),
        as.character(from_unit), sep = "\r")
}

water_legacy_unit_contract_ok <- function(candidate_sha256,
                                          baseline_sha256) {
  identical(candidate_sha256, WATER_LEGACY_BUNDLE_SHA256) &&
    identical(candidate_sha256, baseline_sha256)
}

# Apply the same fail-closed unit boundary to an already-collapsed runtime
# bundle. This protects the exact known legacy bundle while a refreshed bundle
# is under review: all registered non-missing mismatch identities are removed,
# their represented source-row counts must stay within the audited bounds, and
# registered missing labels are filled only when the established target label is
# also observed in the retained data. Numeric values are never changed.
canonicalize_runtime_water_units <- function(swc_long) {
  validate_water_unit_policy()
  required <- c("site", "collectDate", "analyte", "value", "units", "n_reps")
  missing_columns <- setdiff(required, names(swc_long))
  if (!is.data.frame(swc_long) || length(missing_columns)) {
    stop(sprintf(
      "Runtime water data are missing required column(s): %s",
      paste(missing_columns, collapse = ", ")
    ), call. = FALSE)
  }

  analyte <- as.character(swc_long$analyte)
  unknown <- setdiff(unique(analyte), names(WATER_ESTABLISHED_UNIT_TARGETS))
  if (length(unknown)) {
    stop(sprintf("Unregistered runtime analyte(s) require unit review: %s",
                 paste(sort(unknown), collapse = ", ")), call. = FALSE)
  }
  if (!is.integer(swc_long$n_reps) || anyNA(swc_long$n_reps) ||
      any(swc_long$n_reps < 1L)) {
    stop("Runtime replicate counts must be exact positive integers.",
         call. = FALSE)
  }

  source_labels <- water_unit_source_label(swc_long$units)
  targets <- unname(WATER_ESTABLISHED_UNIT_TARGETS[analyte])
  mismatch <- source_labels != WATER_MISSING_UNIT & source_labels != targets
  row_keys <- water_unit_identity_key(
    swc_long$site, swc_long$collectDate, analyte, source_labels
  )
  identities <- WATER_UNIT_EXCLUSION_IDENTITIES
  identity_keys <- water_unit_identity_key(
    identities$site, identities$collectDate, identities$analyte,
    identities$from_unit
  )
  identity_index <- match(row_keys, identity_keys)
  if (any(mismatch & is.na(identity_index))) {
    bad <- unique(row_keys[mismatch & is.na(identity_index)])
    stop(sprintf("Unapproved runtime unit mismatch requires review: %s",
                 paste(bad, collapse = ", ")), call. = FALSE)
  }
  if (anyDuplicated(row_keys[mismatch])) {
    stop("Runtime unit mismatch identities are not at the collapsed grain.",
         call. = FALSE)
  }
  over_bound <- mismatch &
    swc_long$n_reps > identities$max_source_rows[identity_index]
  if (any(over_bound, na.rm = TRUE)) {
    stop(sprintf(
      "Runtime unit mismatch count increased and requires review: %s",
      paste(unique(row_keys[over_bound]), collapse = ", ")
    ), call. = FALSE)
  }

  keep <- !mismatch
  retained <- swc_long[keep, , drop = FALSE]
  values_before <- retained$value
  retained_source <- source_labels[keep]
  retained_analyte <- analyte[keep]
  missing <- retained_source == WATER_MISSING_UNIT
  rewrite_rules <- WATER_UNIT_LABEL_REWRITES
  rewrite_index <- match(
    water_unit_rule_key(retained_analyte, retained_source),
    water_unit_rule_key(rewrite_rules$analyte, rewrite_rules$from_unit)
  )
  if (any(missing & is.na(rewrite_index))) {
    bad <- unique(retained_analyte[missing & is.na(rewrite_index)])
    stop(sprintf("Unapproved runtime missing unit label requires review: %s",
                 paste(sort(bad), collapse = ", ")), call. = FALSE)
  }
  active_rules <- unique(rewrite_index[missing])
  active_rules <- active_rules[!is.na(active_rules)]
  for (i in active_rules) {
    target_observed <- any(
      retained_analyte == rewrite_rules$analyte[[i]] &
        retained_source == rewrite_rules$to_unit[[i]]
    )
    if (!target_observed) {
      stop(sprintf(
        "Runtime missing-label repair lacks an observed target label: %s",
        rewrite_rules$analyte[[i]]
      ), call. = FALSE)
    }
  }
  retained$units[missing] <- rewrite_rules$to_unit[rewrite_index[missing]]
  final_targets <- unname(WATER_ESTABLISHED_UNIT_TARGETS[retained_analyte])
  if (anyNA(retained$units) || any(!nzchar(retained$units)) ||
      !identical(as.character(retained$units), final_targets)) {
    stop("Runtime water units do not match the established targets after filtering.",
         call. = FALSE)
  }
  if (!identical(retained$value, values_before)) {
    stop("Runtime unit handling changed numeric values.", call. = FALSE)
  }

  excluded <- data.frame(
    site = as.character(swc_long$site[mismatch]),
    collectDate = as.character(swc_long$collectDate[mismatch]),
    analyte = analyte[mismatch],
    from_unit = source_labels[mismatch],
    n_source_rows = swc_long$n_reps[mismatch],
    stringsAsFactors = FALSE
  )
  list(
    data = retained,
    excluded = excluded,
    n_collapsed_rows_excluded = as.integer(nrow(excluded)),
    n_source_rows_excluded = as.integer(sum(excluded$n_source_rows)),
    n_missing_labels_rewritten = as.integer(sum(missing))
  )
}

validate_water_unit_policy <- function() {
  targets <- WATER_ESTABLISHED_UNIT_TARGETS
  rewrites <- WATER_UNIT_LABEL_REWRITES
  exclusions <- WATER_UNIT_EXCLUSION_RULES
  identities <- WATER_UNIT_EXCLUSION_IDENTITIES
  stopifnot(
    is.character(targets), length(targets) == 34L,
    !is.null(names(targets)), !anyDuplicated(names(targets)),
    all(nzchar(names(targets))), all(nzchar(targets)),
    identical(names(rewrites), c("analyte", "from_unit", "to_unit")),
    all(rewrites$from_unit == WATER_MISSING_UNIT),
    !anyDuplicated(water_unit_rule_key(rewrites$analyte,
                                       rewrites$from_unit)),
    all(rewrites$analyte %in% names(targets)),
    identical(unname(targets[rewrites$analyte]), rewrites$to_unit),
    identical(names(exclusions), c("analyte", "from_unit", "reason",
                                    "required_laboratory")),
    !anyDuplicated(water_unit_rule_key(exclusions$analyte,
                                       exclusions$from_unit)),
    all(exclusions$analyte %in% names(targets)),
    all(exclusions$from_unit != unname(targets[exclusions$analyte])),
    identical(names(identities), c("site", "collectDate", "analyte",
                                    "from_unit", "max_source_rows")),
    !anyDuplicated(water_unit_identity_key(
      identities$site, identities$collectDate, identities$analyte,
      identities$from_unit
    )),
    is.integer(identities$max_source_rows),
    all(!is.na(identities$max_source_rows) &
          identities$max_source_rows > 0L),
    all(water_unit_rule_key(identities$analyte, identities$from_unit) %in%
          water_unit_rule_key(exclusions$analyte, exclusions$from_unit))
  )
  invisible(TRUE)
}

water_unit_receipt_text <- function(receipt) {
  stopifnot(is.data.frame(receipt))
  values <- lapply(receipt, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    x
  })
  rows <- do.call(paste, c(values, sep = "\t"))
  paste(c(paste(names(receipt), collapse = "\t"), rows), collapse = "\n")
}

water_unit_receipt_sha256 <- function(receipt) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("digest is required for the unit receipt.", call. = FALSE)
  }
  digest::digest(water_unit_receipt_text(receipt), algo = "sha256",
                 serialize = FALSE)
}

canonicalize_water_unit_labels <- function(site, collectDate, analyte, value,
                                           units, laboratoryName = NULL) {
  validate_water_unit_policy()
  n <- length(analyte)
  stopifnot(length(site) == n, length(collectDate) == n, length(value) == n,
            length(units) == n)
  if (is.null(laboratoryName)) laboratoryName <- rep(NA_character_, n)
  stopifnot(length(laboratoryName) == n)

  site <- as.character(site)
  collectDate <- as.character(collectDate)
  analyte <- as.character(analyte)
  units <- as.character(units)
  laboratoryName <- as.character(laboratoryName)

  unknown <- setdiff(unique(analyte), names(WATER_ESTABLISHED_UNIT_TARGETS))
  if (length(unknown)) {
    stop(sprintf("Unregistered analyte(s) require unit review: %s",
                 paste(sort(unknown), collapse = ", ")), call. = FALSE)
  }

  source_labels <- water_unit_source_label(units)
  targets <- unname(WATER_ESTABLISHED_UNIT_TARGETS[analyte])
  mismatch <- source_labels != WATER_MISSING_UNIT & source_labels != targets

  identities <- WATER_UNIT_EXCLUSION_IDENTITIES
  identity_keys <- water_unit_identity_key(
    identities$site, identities$collectDate, identities$analyte,
    identities$from_unit
  )
  row_keys <- water_unit_identity_key(site, collectDate, analyte, source_labels)
  identity_index <- match(row_keys, identity_keys)
  if (any(mismatch & is.na(identity_index))) {
    bad <- unique(row_keys[mismatch & is.na(identity_index)])
    stop(sprintf(
      "Unapproved unit mismatch identity requires review: %s",
      paste(bad, collapse = ", ")
    ), call. = FALSE)
  }

  excluded_counts <- tabulate(identity_index[mismatch], nbins = nrow(identities))
  too_many <- excluded_counts > identities$max_source_rows
  if (any(too_many)) {
    stop(sprintf(
      "Audited unit exclusion count increased and requires review: %s",
      paste(identity_keys[too_many], collapse = ", ")
    ), call. = FALSE)
  }

  exclusion_rules <- WATER_UNIT_EXCLUSION_RULES
  rule_index <- match(
    water_unit_rule_key(analyte, source_labels),
    water_unit_rule_key(exclusion_rules$analyte,
                        exclusion_rules$from_unit)
  )
  required_lab <- exclusion_rules$required_laboratory[rule_index]
  provenance_bound_row <- mismatch & !is.na(required_lab)
  if (any(provenance_bound_row &
          (is.na(laboratoryName) | laboratoryName != required_lab))) {
    stop("TPC/TPN anomaly lacks the required EcoCore_CSU provenance receipt.",
         call. = FALSE)
  }

  keep <- !mismatch
  kept_analyte <- analyte[keep]
  kept_source <- source_labels[keep]
  kept_units <- units[keep]
  missing <- kept_source == WATER_MISSING_UNIT
  rewrite_rules <- WATER_UNIT_LABEL_REWRITES
  rewrite_index <- match(
    water_unit_rule_key(kept_analyte, kept_source),
    water_unit_rule_key(rewrite_rules$analyte, rewrite_rules$from_unit)
  )
  if (any(missing & is.na(rewrite_index))) {
    bad <- unique(kept_analyte[missing & is.na(rewrite_index)])
    stop(sprintf("Unapproved missing unit label requires review: %s",
                 paste(sort(bad), collapse = ", ")), call. = FALSE)
  }

  n_rewritten <- tabulate(rewrite_index[missing], nbins = nrow(rewrite_rules))
  n_target_source <- vapply(seq_len(nrow(rewrite_rules)), function(i) {
    sum(kept_analyte == rewrite_rules$analyte[[i]] &
          kept_source == rewrite_rules$to_unit[[i]])
  }, integer(1))
  active_rewrite <- n_rewritten > 0L
  if (any(active_rewrite & n_target_source == 0L)) {
    bad <- rewrite_rules[active_rewrite & n_target_source == 0L, , drop = FALSE]
    stop(sprintf("Missing-label repair lacks an observed target label: %s",
                 paste(bad$analyte, collapse = ", ")), call. = FALSE)
  }
  kept_units[missing] <- rewrite_rules$to_unit[rewrite_index[missing]]

  final_targets <- unname(WATER_ESTABLISHED_UNIT_TARGETS[kept_analyte])
  if (any(is.na(kept_units) | !nzchar(kept_units) |
          kept_units != final_targets)) {
    bad <- unique(kept_analyte[
      is.na(kept_units) | !nzchar(kept_units) | kept_units != final_targets
    ])
    stop(sprintf("Established analyte unit target changed: %s",
                 paste(sort(bad), collapse = ", ")), call. = FALSE)
  }

  label_receipt <- data.frame(
    analyte = rewrite_rules$analyte,
    from_unit = rewrite_rules$from_unit,
    to_unit = rewrite_rules$to_unit,
    n_rewritten = as.integer(n_rewritten),
    n_target_source = as.integer(n_target_source),
    stringsAsFactors = FALSE
  )
  label_receipt <- label_receipt[order(label_receipt$analyte), , drop = FALSE]
  rownames(label_receipt) <- NULL

  exclusion_rule_index <- match(
    water_unit_rule_key(identities$analyte, identities$from_unit),
    water_unit_rule_key(exclusion_rules$analyte,
                        exclusion_rules$from_unit)
  )
  exclusion_receipt <- data.frame(
    site = identities$site,
    collectDate = identities$collectDate,
    analyte = identities$analyte,
    from_unit = identities$from_unit,
    reason = exclusion_rules$reason[exclusion_rule_index],
    max_source_rows = identities$max_source_rows,
    n_excluded = as.integer(excluded_counts),
    stringsAsFactors = FALSE
  )
  exclusion_receipt <- exclusion_receipt[order(
    exclusion_receipt$analyte, exclusion_receipt$site,
    exclusion_receipt$collectDate
  ), , drop = FALSE]
  rownames(exclusion_receipt) <- NULL

  list(
    keep = keep,
    units = kept_units,
    label_receipt = label_receipt,
    label_receipt_sha256 = water_unit_receipt_sha256(label_receipt),
    exclusion_receipt = exclusion_receipt,
    exclusion_receipt_sha256 = water_unit_receipt_sha256(exclusion_receipt)
  )
}

validate_water_unit_policy()
