args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
source(file.path(repo_root, "scripts", "build_swc_bundle.R"))

stopifnot(water_legacy_unit_contract_ok(
  WATER_LEGACY_BUNDLE_SHA256, WATER_LEGACY_BUNDLE_SHA256
))
mutated_legacy_sha <- paste0(
  "a", substring(WATER_LEGACY_BUNDLE_SHA256, 2L)
)
stopifnot(!water_legacy_unit_contract_ok(mutated_legacy_sha,
                                         mutated_legacy_sha))
stopifnot(!water_legacy_unit_contract_ok(WATER_LEGACY_BUNDLE_SHA256,
                                         mutated_legacy_sha))

make_rows <- function(site, collectDate, analyte, values, units,
                      laboratoryName = "Example_Lab") {
  n <- length(values)
  data.frame(
    site = rep(site, n),
    collectDate = rep(as.character(as.Date(collectDate)), n),
    analyte = rep(analyte, n),
    analyteConcentration = values,
    analyteUnits = units,
    laboratoryName = rep(laboratoryName, length.out = n),
    belowDetectionQF = rep("0", n),
    externalLabDataQF = rep(NA_character_, n),
    stringsAsFactors = FALSE
  )
}

lab_raw <- rbind(
  # The audited WALK-2019 alternate label is quarantined, never divided or
  # relabelled. The established mg/L replicate remains.
  make_rows("WALK", "2019-07-01", "TP", c(0.025, 0.057),
            c("milligramsPerLiter", "microgramsPerLiter"),
            c("Example_Lab", "Florida International University")),
  # The residual EcoCore milligram label is an unresolved legacy anomaly.
  # Exclude it; retain only the independently reported target-unit row.
  make_rows("WALK", "2017-11-20", "TPC", c(518, 0.5185),
            c("microgramsPerLiter", "milligram"),
            c("Example_Lab", "EcoCore_CSU")),
  # Missing UV metadata is the only kind of label rewrite in this fixture.
  make_rows("SYCA", "2026-01-01", "UV Absorbance (254 nm)", 0.11, NA,
            "EcoCore_CSU"),
  make_rows("SYCA", "2026-01-02", "UV Absorbance (254 nm)", 0.12,
            "absorbance units", "Illinois State Water Survey")
)
coords <- tibble::tibble(
  site = c("SYCA", "WALK"),
  neonName = c("Sycamore Creek", "Walker Branch"),
  domain = c("D14", "D07"), state = c("AZ", "TN"),
  lat = c(33.08, 35.96), long = c(-111.50, -84.28),
  siteType = c("Wadeable stream", "Wadeable stream")
)

bundle <- build_swc_bundle(lab_raw, data.frame(), coords)
out <- bundle$swc_long

stopifnot(identical(
  sort(as.numeric(out$value)), sort(c(0.025, 518, 0.11, 0.12))
))
stopifnot(!any(out$value %in% c(0.057, 0.000057, 0.5185)))
units_by_analyte <- split(as.character(out$units), as.character(out$analyte))
stopifnot(identical(unique(units_by_analyte$TP), "milligramsPerLiter"))
stopifnot(identical(unique(units_by_analyte$TPC), "microgramsPerLiter"))
stopifnot(identical(unique(units_by_analyte[["UV Absorbance (254 nm)"]]),
                    "absorbance units"))

stopifnot(identical(bundle$built$unit_policy, WATER_UNIT_POLICY))
stopifnot(identical(bundle$built$n_unit_labels_rewritten, 1L))
stopifnot(identical(bundle$built$n_unit_rows_excluded, 2L))
stopifnot(identical(bundle$built$n_unit_values_changed, 0L))
stopifnot(identical(
  bundle$built$unit_rewrite_receipt_sha256,
  water_unit_receipt_sha256(bundle$built$unit_label_rewrites)
))
stopifnot(identical(
  bundle$built$unit_exclusion_receipt_sha256,
  water_unit_receipt_sha256(bundle$built$unit_row_exclusions)
))

label_receipt <- bundle$built$unit_label_rewrites
uv_hit <- label_receipt$analyte == "UV Absorbance (254 nm)"
stopifnot(sum(uv_hit) == 1L,
          identical(label_receipt$n_rewritten[uv_hit], 1L),
          identical(label_receipt$required_laboratory[uv_hit],
                    "EcoCore_CSU"),
          identical(label_receipt$n_target_source[uv_hit], 1L))
exclusion_receipt <- bundle$built$unit_row_exclusions
tp_hit <- exclusion_receipt$site == "WALK" &
  exclusion_receipt$collectDate == "2019-07-01" &
  exclusion_receipt$analyte == "TP"
tpc_hit <- exclusion_receipt$site == "WALK" &
  exclusion_receipt$collectDate == "2017-11-20" &
  exclusion_receipt$analyte == "TPC"
stopifnot(sum(tp_hit) == 1L, sum(tpc_hit) == 1L,
          identical(exclusion_receipt$n_excluded[tp_hit], 1L),
          identical(exclusion_receipt$n_excluded[tpc_hit], 1L))

expect_build_error <- function(x, field_raw = data.frame()) {
  msg <- tryCatch({
    build_swc_bundle(x, field_raw, coords)
    ""
  }, error = conditionMessage)
  stopifnot(nzchar(msg))
  invisible(msg)
}

# Even the first sparse row of a genuine transition is rejected when its exact
# identity was not audited; there is no fraction-based exception.
sparse_transition <- rbind(
  make_rows("SYCA", "2026-02-01", "TP", rep(0.025, 1000),
            rep("milligramsPerLiter", 1000)),
  make_rows("SYCA", "2026-02-02", "TP", 57,
            "microgramsPerLiter")
)
expect_build_error(sparse_transition)

# Row-labelled external-lab targets fail closed on unreviewed transitions.
expect_build_error(make_rows("SYCA", "2026-03-01", "Ca", 57,
                             "microgramsPerLiter"))
expect_build_error(make_rows("SYCA", "2026-03-01", "ANC", 500,
                             "microequivalentsPerLiter"))

# Production field extraction supplies three value columns without row-level
# unit metadata. Test the real field_raw path and its explicit fixed unit
# assumptions rather than pretending a lab-row fixture observes field metadata.
field_raw <- data.frame(
  site = "SYCA", collectDate = "2026-03-01",
  waterTemp = 14.5, dissolvedOxygen = 8.2,
  specificConductance = 210,
  stringsAsFactors = FALSE
)
field_bundle <- build_swc_bundle(lab_raw[0, , drop = FALSE], field_raw, coords)
field_units <- setNames(as.character(field_bundle$swc_long$units),
                        as.character(field_bundle$swc_long$analyte))
field_values <- setNames(as.numeric(field_bundle$swc_long$value),
                         as.character(field_bundle$swc_long$analyte))
stopifnot(
  identical(unname(field_units["waterTemp"]), "celsius"),
  identical(unname(field_units["dissolvedOxygenField"]),
            "milligramsPerLiter"),
  identical(unname(field_units["specificConductanceField"]),
            "microsiemensPerCentimeter"),
  identical(unname(field_values[c(
    "waterTemp", "dissolvedOxygenField", "specificConductanceField"
  )]), c(14.5, 8.2, 210))
)

# API/cache row enumeration is not part of the scientific record. Exercise the
# exact shared builder with numerically different replicates, multiple distinct
# lab flags, and repeated field rows; arbitrary independent input permutations
# must produce one identical bundle after normalizing only built$when.
permutation_lab <- make_rows(
  "SYCA", "2026-04-01", "Ca", c(1e16, 1, 1e-8, 7),
  rep("milligramsPerLiter", 4), "Example_Lab"
)
permutation_lab$belowDetectionQF <- c("0", "ND", "0", "BDL")
permutation_lab$externalLabDataQF <- c(
  "legacyData", "formatChange", "legacyData", NA_character_
)
permutation_field <- data.frame(
  site = rep("SYCA", 3L),
  collectDate = rep("2026-04-01", 3L),
  waterTemp = c(1e16, 1, 3),
  dissolvedOxygen = c(8.1, 9.2, 7.3),
  specificConductance = c(100, 110, 90),
  stringsAsFactors = FALSE
)
permutation_a <- build_swc_bundle(
  permutation_lab, permutation_field, coords
)
permutation_b <- build_swc_bundle(
  permutation_lab[c(2L, 4L, 1L, 3L), , drop = FALSE],
  permutation_field[c(3L, 1L, 2L), , drop = FALSE],
  coords[c(2L, 1L), , drop = FALSE]
)
permutation_a$built$when <- "<NORMALIZED>"
permutation_b$built$when <- "<NORMALIZED>"
stopifnot(identical(permutation_a, permutation_b))
permutation_ca <- permutation_a$swc_long[
  permutation_a$swc_long$analyte == "Ca", , drop = FALSE
]
stopifnot(
  nrow(permutation_ca) == 1L,
  identical(permutation_ca$n_reps, 4L),
  identical(permutation_ca$belowDetection, 1L),
  identical(permutation_ca$labFlag, "formatChange | legacyData"),
  all(permutation_a$swc_long$n_reps[
    permutation_a$swc_long$source == "Field Probe"
  ] == 3L)
)
current_codebook_path <- tempfile("water-current-codebook-")
write_codebook(permutation_a, current_codebook_path)
current_codebook_lines <- readLines(current_codebook_path, warn = FALSE)
current_codebook <- utils::read.csv(
  current_codebook_path, comment.char = "#", check.names = FALSE,
  stringsAsFactors = FALSE, na.strings = character(0)
)
current_flag_row <- current_codebook[
  current_codebook$section == "tidy_long_export" &
    current_codebook$name == "lab_flag", , drop = FALSE
]
stopifnot(
  grepl("version 1.1.0", current_codebook_lines[[1]], fixed = TRUE),
  nrow(current_flag_row) == 1L,
  identical(
    current_flag_row$allowed,
    "sorted distinct NEON externalLabDataQF codes joined by ' | '"
  ),
  identical(
    current_flag_row$definition,
    "All distinct external-lab quality flags across the collapsed replicates"
  ),
  identical(current_flag_row$na_semantics,
            "NA when every replicate is unflagged")
)
unlink(current_codebook_path)

# Unknown analytes and unregistered missing labels require policy review.
expect_build_error(make_rows("SYCA", "2026-03-01", "Future analyte", 1,
                             "newUnit"))
expect_build_error(make_rows("SYCA", "2026-03-01", "Ca", 1, NA))

# Missing-label rewrites are EcoCore-bound, while the observed target support
# remains global and can come from Illinois State Water Survey. Wrong, missing,
# or mixed source provenance must fail before any label is filled.
rewrite_support <- make_rows(
  "SYCA", "2026-03-02", "UV Absorbance (254 nm)", 0.12,
  "absorbance units", "Illinois State Water Survey"
)
wrong_rewrite_lab <- rbind(
  make_rows("SYCA", "2026-03-01", "UV Absorbance (254 nm)", 0.11, NA,
            "Other_Lab"),
  rewrite_support
)
missing_rewrite_lab <- rbind(
  make_rows("SYCA", "2026-03-01", "UV Absorbance (254 nm)", 0.11, NA,
            NA_character_),
  rewrite_support
)
mixed_rewrite_lab <- rbind(
  make_rows("SYCA", "2026-03-01", "UV Absorbance (254 nm)",
            c(0.10, 0.11), c(NA, NA), c("EcoCore_CSU", "Other_Lab")),
  rewrite_support
)
whitespace_rewrite_lab <- rbind(
  make_rows("SYCA", "2026-03-01", "UV Absorbance (254 nm)", 0.11, NA,
            "EcoCore_CSU "),
  rewrite_support
)
case_rewrite_lab <- rbind(
  make_rows("SYCA", "2026-03-01", "UV Absorbance (254 nm)", 0.11, NA,
            "ecocore_csu"),
  rewrite_support
)
wrong_rewrite_message <- expect_build_error(wrong_rewrite_lab)
missing_rewrite_message <- expect_build_error(missing_rewrite_lab)
mixed_rewrite_message <- expect_build_error(mixed_rewrite_lab)
whitespace_rewrite_message <- expect_build_error(whitespace_rewrite_lab)
case_rewrite_message <- expect_build_error(case_rewrite_lab)
stopifnot(
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

missing_without_target <- make_rows(
  "SYCA", "2026-03-01", "UV Absorbance (254 nm)", 0.11, NA,
  "EcoCore_CSU"
)
invalid_date_target_row <- make_rows(
  "SYCA", "2026-03-02", "UV Absorbance (254 nm)", 0.12,
  "absorbance units", "Illinois State Water Survey"
)
invalid_date_target_row$collectDate <- "invalid-date"
invalid_date_target <- rbind(missing_without_target, invalid_date_target_row)
nonnumeric_target <- rbind(
  missing_without_target,
  make_rows("SYCA", "2026-03-02", "UV Absorbance (254 nm)", NA_real_,
            "absorbance units", "Illinois State Water Survey")
)
expect_build_error(missing_without_target)
expect_build_error(invalid_date_target)
expect_build_error(nonnumeric_target)

# Audited identity bounds and both laboratory receipts are strict.
too_many_audited <- make_rows(
  "WALK", "2019-07-01", "TP", c(0.057, 0.058, 0.059),
  rep("microgramsPerLiter", 3), "Florida International University"
)
expect_build_error(too_many_audited)
wrong_walk_lab <- make_rows(
  "WALK", "2019-07-01", "TP", 0.057, "microgramsPerLiter", "Other_Lab"
)
missing_walk_lab <- make_rows(
  "WALK", "2019-07-01", "TP", 0.057, "microgramsPerLiter", NA_character_
)
mixed_walk_lab <- make_rows(
  "WALK", "2019-07-01", "TP", c(0.057, 0.058),
  "microgramsPerLiter",
  c("Florida International University", "Other_Lab")
)
whitespace_walk_lab <- make_rows(
  "WALK", "2019-07-01", "TP", 0.057, "microgramsPerLiter",
  "Florida International University "
)
case_walk_lab <- make_rows(
  "WALK", "2019-07-01", "TP", 0.057, "microgramsPerLiter",
  "florida international university"
)
wrong_walk_lab_message <- expect_build_error(wrong_walk_lab)
missing_walk_lab_message <- expect_build_error(missing_walk_lab)
mixed_walk_lab_message <- expect_build_error(mixed_walk_lab)
whitespace_walk_lab_message <- expect_build_error(whitespace_walk_lab)
case_walk_lab_message <- expect_build_error(case_walk_lab)
wrong_mass_lab <- make_rows(
  "WALK", "2017-11-20", "TPC", 0.5185, "milligram", "Other_Lab"
)
wrong_lab_message <- expect_build_error(wrong_mass_lab)
stopifnot(
  grepl("provenance", wrong_walk_lab_message, fixed = TRUE),
  grepl("provenance", missing_walk_lab_message, fixed = TRUE),
  grepl("provenance", mixed_walk_lab_message, fixed = TRUE),
  grepl("provenance", whitespace_walk_lab_message, fixed = TRUE),
  grepl("provenance", case_walk_lab_message, fixed = TRUE),
  grepl("provenance", wrong_lab_message, fixed = TRUE)
)

# Exact identity keys are atomic: adjacent date, wrong site, wrong analyte, or
# alternate unit claims never inherit an audited quarantine.
expect_build_error(make_rows(
  "WALK", "2019-07-02", "TP", 0.057, "microgramsPerLiter",
  "Florida International University"
))
expect_build_error(make_rows(
  "SYCA", "2019-07-01", "TP", 0.057, "microgramsPerLiter",
  "Florida International University"
))
expect_build_error(make_rows(
  "WALK", "2019-07-01", "TDS", 0.057, "microgramsPerLiter",
  "Florida International University"
))
expect_build_error(make_rows(
  "WALK", "2019-07-01", "TP", 0.057, "microgram",
  "Florida International University"
))

# The committed bundle can be either the exact receipt-free legacy bytes or a
# producer-canonicalized candidate. Derive the runtime expectation from the
# bundle itself so this regression exercises both states without confusing the
# 73-row producer audit receipt with rows that remain at the runtime boundary.
legacy_path <- file.path(repo_root, "data", "neon_swc.rds")
legacy <- readRDS(legacy_path)
producer_receipt_present <- WATER_UNIT_RECEIPT_FIELDS %in% names(legacy$built)
stopifnot(all(producer_receipt_present) || !any(producer_receipt_present))
legacy_codebook_path <- tempfile("water-legacy-codebook-")
write_codebook(legacy, legacy_codebook_path)
legacy_codebook_lines <- readLines(legacy_codebook_path, warn = FALSE)
legacy_codebook <- utils::read.csv(
  legacy_codebook_path, comment.char = "#", check.names = FALSE,
  stringsAsFactors = FALSE, na.strings = character(0)
)
legacy_flag_row <- legacy_codebook[
  legacy_codebook$section == "tidy_long_export" &
    legacy_codebook$name == "lab_flag", , drop = FALSE
]
stopifnot(
  grepl(
    if (all(producer_receipt_present)) {
      "version 1.1.0"
    } else {
      "version 1.0.0"
    },
    legacy_codebook_lines[[1]], fixed = TRUE
  ),
  nrow(legacy_flag_row) == 1L,
  identical(
    legacy_flag_row$allowed,
    if (all(producer_receipt_present)) {
      "sorted distinct NEON externalLabDataQF codes joined by ' | '"
    } else {
      "NEON externalLabDataQF codes"
    }
  ),
  identical(
    legacy_flag_row$definition,
    if (all(producer_receipt_present)) {
      paste0(
        "All distinct external-lab quality flags across the collapsed ",
        "replicates"
      )
    } else {
      "External-lab quality flag (e.g. legacyData, formatChange)"
    }
  ),
  identical(
    legacy_flag_row$na_semantics,
    if (all(producer_receipt_present)) {
      "NA when every replicate is unflagged"
    } else {
      "NA when unflagged"
    }
  )
)
unlink(legacy_codebook_path)
runtime_source_labels <- water_unit_source_label(legacy$swc_long$units)
runtime_targets <- unname(WATER_ESTABLISHED_UNIT_TARGETS[
  as.character(legacy$swc_long$analyte)
])
runtime_mismatch <- runtime_source_labels != WATER_MISSING_UNIT &
  runtime_source_labels != runtime_targets
expected_runtime_exclusions <- data.frame(
  site = as.character(legacy$swc_long$site[runtime_mismatch]),
  collectDate = as.character(legacy$swc_long$collectDate[runtime_mismatch]),
  analyte = as.character(legacy$swc_long$analyte[runtime_mismatch]),
  from_unit = runtime_source_labels[runtime_mismatch],
  n_source_rows = legacy$swc_long$n_reps[runtime_mismatch],
  stringsAsFactors = FALSE
)
runtime_result <- canonicalize_runtime_water_units(legacy$swc_long)
stopifnot(
  identical(runtime_result$excluded, expected_runtime_exclusions),
  identical(runtime_result$n_collapsed_rows_excluded,
            as.integer(sum(runtime_mismatch))),
  identical(runtime_result$n_source_rows_excluded,
            as.integer(sum(legacy$swc_long$n_reps[runtime_mismatch]))),
  nrow(runtime_result$data) ==
    nrow(legacy$swc_long) - sum(runtime_mismatch),
  all(as.character(runtime_result$data$units) == unname(
    WATER_ESTABLISHED_UNIT_TARGETS[as.character(runtime_result$data$analyte)]
  ))
)

if (all(producer_receipt_present)) {
  producer_receipt <- legacy$built$unit_row_exclusions
  producer_keys <- water_unit_identity_key(
    producer_receipt$site, producer_receipt$collectDate,
    producer_receipt$analyte, producer_receipt$from_unit
  )
  policy_keys <- water_unit_identity_key(
    WATER_UNIT_EXCLUSION_IDENTITIES$site,
    WATER_UNIT_EXCLUSION_IDENTITIES$collectDate,
    WATER_UNIT_EXCLUSION_IDENTITIES$analyte,
    WATER_UNIT_EXCLUSION_IDENTITIES$from_unit
  )
  stopifnot(
    identical(legacy$built$unit_policy, WATER_UNIT_POLICY),
    nrow(producer_receipt) == 73L,
    identical(sort(producer_keys), sort(policy_keys)),
    is.integer(producer_receipt$n_excluded),
    !anyNA(producer_receipt$n_excluded),
    all(producer_receipt$n_excluded >= 0L),
    all(producer_receipt$n_excluded <= producer_receipt$max_source_rows),
    identical(legacy$built$n_unit_rows_excluded,
              as.integer(sum(producer_receipt$n_excluded))),
    identical(legacy$built$unit_exclusion_receipt_sha256,
              water_unit_receipt_sha256(producer_receipt)),
    identical(runtime_result$n_collapsed_rows_excluded, 0L),
    identical(runtime_result$n_source_rows_excluded, 0L)
  )
} else {
  legacy_sha256 <- digest::digest(file = legacy_path, algo = "sha256")
  stopifnot(
    identical(legacy_sha256, WATER_LEGACY_BUNDLE_SHA256),
    identical(runtime_result$n_collapsed_rows_excluded, 48L),
    identical(runtime_result$n_source_rows_excluded, 99L)
  )
}

# Exercise runtime fail-closed behavior with an explicit audited identity. A
# producer-canonicalized bundle intentionally contains no residual mismatches,
# so the adversarial fixtures must not depend on finding one in committed data.
audited_identity <- WATER_UNIT_EXCLUSION_IDENTITIES[1, , drop = FALSE]
audited_runtime <- legacy$swc_long[
  which(legacy$swc_long$analyte == audited_identity$analyte)[[1]],
  , drop = FALSE
]
audited_runtime$site[[1]] <- audited_identity$site[[1]]
audited_runtime$collectDate[[1]] <- as.Date(audited_identity$collectDate[[1]])
audited_runtime$analyte[[1]] <- audited_identity$analyte[[1]]
audited_runtime$units[[1]] <- audited_identity$from_unit[[1]]
audited_runtime$n_reps[[1]] <- audited_identity$max_source_rows[[1]]
audited_runtime_result <- canonicalize_runtime_water_units(audited_runtime)
stopifnot(
  identical(audited_runtime_result$n_collapsed_rows_excluded, 1L),
  identical(audited_runtime_result$n_source_rows_excluded,
            audited_identity$max_source_rows[[1]])
)

unknown_runtime <- audited_runtime
unknown_runtime$site[[1]] <- "ZZZZ"
stopifnot(nzchar(tryCatch({
  canonicalize_runtime_water_units(unknown_runtime)
  ""
}, error = conditionMessage)))

over_bound_runtime <- audited_runtime
over_bound_runtime$n_reps[[1]] <- audited_identity$max_source_rows[[1]] + 1L
stopifnot(nzchar(tryCatch({
  canonicalize_runtime_water_units(over_bound_runtime)
  ""
}, error = conditionMessage)))

mixed_units <- bundle
mixed_units$swc_long$units[[1]] <- "decoyMixedUnit"
stopifnot(nzchar(tryCatch({
  validate_bundle(mixed_units)
  ""
}, error = conditionMessage)))

changed_values <- bundle
changed_values$built$n_unit_values_changed <- 1L
stopifnot(nzchar(tryCatch({
  validate_bundle(changed_values)
  ""
}, error = conditionMessage)))

fractional_values <- bundle
fractional_values$built$n_unit_values_changed <- 0.5
stopifnot(nzchar(tryCatch({
  validate_bundle(fractional_values)
  ""
}, error = conditionMessage)))

fractional_labels <- bundle
fractional_labels$built$n_unit_labels_rewritten <- 0.5
stopifnot(nzchar(tryCatch({
  validate_bundle(fractional_labels)
  ""
}, error = conditionMessage)))

fractional_exclusions <- bundle
fractional_exclusions$built$n_unit_rows_excluded <- 0.5
stopifnot(nzchar(tryCatch({
  validate_bundle(fractional_exclusions)
  ""
}, error = conditionMessage)))

tampered_label <- bundle
tampered_label$built$unit_label_rewrites$n_rewritten[[1]] <-
  tampered_label$built$unit_label_rewrites$n_rewritten[[1]] + 1L
stopifnot(nzchar(tryCatch({
  validate_bundle(tampered_label)
  ""
}, error = conditionMessage)))

tampered_exclusion <- bundle
tampered_exclusion$built$unit_row_exclusions$n_excluded[[1]] <-
  tampered_exclusion$built$unit_row_exclusions$n_excluded[[1]] + 1L
stopifnot(nzchar(tryCatch({
  validate_bundle(tampered_exclusion)
  ""
}, error = conditionMessage)))

cat("Pinned unit targets, audited exclusions, and value-invariance regressions passed.\n")
