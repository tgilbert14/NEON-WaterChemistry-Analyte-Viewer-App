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
            c("milligramsPerLiter", "microgramsPerLiter")),
  # The residual EcoCore milligram label is an unresolved legacy anomaly.
  # Exclude it; retain only the independently reported target-unit row.
  make_rows("WALK", "2017-11-20", "TPC", c(518, 0.5185),
            c("microgramsPerLiter", "milligram"),
            c("Example_Lab", "EcoCore_CSU")),
  # Missing UV metadata is the only kind of label rewrite in this fixture.
  make_rows("SYCA", "2026-01-01", "UV Absorbance (254 nm)", 0.11, NA),
  make_rows("SYCA", "2026-01-02", "UV Absorbance (254 nm)", 0.12,
            "absorbance units")
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
          identical(label_receipt$n_rewritten[uv_hit], 1L))
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

# Unknown analytes and unregistered missing labels require policy review.
expect_build_error(make_rows("SYCA", "2026-03-01", "Future analyte", 1,
                             "newUnit"))
expect_build_error(make_rows("SYCA", "2026-03-01", "Ca", 1, NA))

# Audited identity bounds and the EcoCore laboratory receipt are strict.
too_many_audited <- make_rows(
  "WALK", "2019-07-01", "TP", c(0.057, 0.058, 0.059),
  rep("microgramsPerLiter", 3)
)
expect_build_error(too_many_audited)
wrong_mass_lab <- make_rows(
  "WALK", "2017-11-20", "TPC", 0.5185, "milligram", "Other_Lab"
)
wrong_lab_message <- expect_build_error(wrong_mass_lab)
stopifnot(grepl("provenance", wrong_lab_message, fixed = TRUE))

# The deployed legacy bundle is safe only after the shared runtime boundary
# removes the same 48 collapsed mismatch identities (99 represented source
# rows). Unknown identities and increases beyond audited replicate bounds stop.
legacy <- readRDS(file.path(repo_root, "data", "neon_swc.rds"))
runtime_result <- canonicalize_runtime_water_units(legacy$swc_long)
stopifnot(
  identical(runtime_result$n_collapsed_rows_excluded, 48L),
  identical(runtime_result$n_source_rows_excluded, 99L),
  nrow(runtime_result$data) == nrow(legacy$swc_long) - 48L,
  all(as.character(runtime_result$data$units) == unname(
    WATER_ESTABLISHED_UNIT_TARGETS[as.character(runtime_result$data$analyte)]
  ))
)

unknown_runtime <- legacy$swc_long
hit <- which(unknown_runtime$analyte == "TP" &
               unknown_runtime$units == "microgramsPerLiter")[[1]]
unknown_runtime$site[[hit]] <- "SYCA"
stopifnot(nzchar(tryCatch({
  canonicalize_runtime_water_units(unknown_runtime)
  ""
}, error = conditionMessage)))

over_bound_runtime <- legacy$swc_long
hit <- which(over_bound_runtime$analyte == "TPC" &
               over_bound_runtime$units == "milligram")[[1]]
over_bound_runtime$n_reps[[hit]] <- over_bound_runtime$n_reps[[hit]] + 1L
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
