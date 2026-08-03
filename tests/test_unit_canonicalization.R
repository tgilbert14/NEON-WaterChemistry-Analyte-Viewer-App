args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
source(file.path(repo_root, "scripts", "build_swc_bundle.R"))

lab_raw <- data.frame(
  site = rep("SYCA", 8),
  collectDate = as.character(as.Date("2026-01-01") + 0:7),
  analyte = c(rep("TP", 3), rep("TPC", 3),
              rep("UV Absorbance (254 nm)", 2)),
  analyteConcentration = c(0.010, 0.025, 0.057,
                           12.0, 15.0, 18.0,
                           0.11, 0.12),
  analyteUnits = c("milligramsPerLiter", "milligramsPerLiter",
                   "microgramsPerLiter",
                   "microgramsPerLiter", "microgramsPerLiter", "milligram",
                   NA, "absorbance units"),
  belowDetectionQF = rep("0", 8),
  externalLabDataQF = rep(NA_character_, 8),
  stringsAsFactors = FALSE
)
coords <- tibble::tibble(
  site = "SYCA", neonName = "Sycamore Creek", domain = "D14",
  state = "AZ", lat = 33.08, long = -111.50, siteType = "Wadeable stream"
)

bundle <- build_swc_bundle(lab_raw, data.frame(), coords)
out <- bundle$swc_long[order(bundle$swc_long$collectDate), ]

# Unit canonicalization is strictly label-only. In particular TP=0.057 is
# already mg/L-magnitude and must never become 0.000057.
stopifnot(isTRUE(all.equal(
  as.numeric(out$value), as.numeric(lab_raw$analyteConcentration), tolerance = 0
)))
stopifnot(identical(out$value[out$collectDate == as.Date("2026-01-03")], 0.057))
stopifnot(!any(out$value == 0.000057))

units_by_analyte <- split(as.character(out$units), as.character(out$analyte))
stopifnot(all(vapply(
  units_by_analyte,
  function(x) !anyNA(x) && length(unique(x)) == 1L,
  logical(1)
)))
stopifnot(identical(unique(units_by_analyte$TP), "milligramsPerLiter"))
stopifnot(identical(unique(units_by_analyte$TPC), "microgramsPerLiter"))
stopifnot(identical(unique(units_by_analyte[["UV Absorbance (254 nm)"]]),
                    "absorbance units"))
stopifnot(identical(bundle$built$unit_policy,
                    "canonical-labels-value-invariant-v1"))
stopifnot(identical(bundle$built$n_unit_labels_rewritten, 3L))
stopifnot(identical(bundle$built$n_unit_values_changed, 0L))

mixed_units <- bundle
mixed_units$swc_long$units[[1]] <- "decoyMixedUnit"
mixed_error <- tryCatch({
  validate_bundle(mixed_units)
  ""
}, error = conditionMessage)
stopifnot(nzchar(mixed_error))

changed_values <- bundle
changed_values$built$n_unit_values_changed <- 1L
changed_error <- tryCatch({
  validate_bundle(changed_values)
  ""
}, error = conditionMessage)
stopifnot(nzchar(changed_error))

cat("Unit canonicalization value-invariance regression passed.\n")
