# Identity-keyed value quarantine (unit policy v5): USGS_NWQA Fe/Mn at ARIK,
# BARC, PRLA, PRPO, REDB, SUGG from 2012-01-31 to 2016-06-19 are microgram
# values labelled milligramsPerLiter (NEON change log 2026-04-21). They are
# excluded with a receipt, never rescaled, and every guard fails closed.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
old_wd <- setwd(repo_root)
source(file.path("scripts", "build_swc_bundle.R"))
setwd(old_wd)

lab_row <- function(site, collectDate, analyte, value,
                    units = "milligramsPerLiter", laboratoryName = "USGS_NWQA",
                    below = "0") {
  data.frame(
    site = site, collectDate = collectDate, analyte = analyte,
    analyteConcentration = value, analyteUnits = units,
    laboratoryName = laboratoryName, belowDetectionQF = below,
    externalLabDataQF = NA_character_, stringsAsFactors = FALSE
  )
}
coords <- tibble::tibble(
  site = c("ARIK", "SYCA"), neonName = c("Arikaree River", "Sycamore Creek"),
  domain = c("D10", "D14"), state = c("CO", "AZ"),
  lat = c(39.76, 33.75), long = c(-102.45, -111.51),
  siteType = c("Wadeable stream", "Wadeable stream")
)
expect_error_matching <- function(expr, pattern) {
  msg <- tryCatch({
    force(expr)
    ""
  }, error = conditionMessage)
  if (!nzchar(msg) || !grepl(pattern, msg, fixed = TRUE)) {
    stop(sprintf("Expected an error matching '%s'; got '%s'", pattern, msg),
         call. = FALSE)
  }
  invisible(msg)
}

base <- rbind(
  lab_row("ARIK", "2012-03-07", "Fe", 58.1),
  lab_row("ARIK", "2017-05-02", "Fe", 0.03),   # after the window: kept
  lab_row("SYCA", "2013-04-01", "Fe", 0.05, laboratoryName = "Other")
)

# 1) The listed identity is quarantined (not divided by 1000); the post-window
#    row and the other-site row are kept with their values unchanged.
b <- build_swc_bundle(base, data.frame(), coords)
fe <- b$swc_long[b$swc_long$analyte == "Fe", , drop = FALSE]
stopifnot(
  nrow(fe) == 2L,
  !any(fe$site == "ARIK" & fe$collectDate == as.Date("2012-03-07")),
  !any(abs(fe$value - 0.0581) < 1e-12),
  identical(fe$value[fe$site == "ARIK"], 0.03),
  identical(fe$value[fe$site == "SYCA"], 0.05),
  identical(b$built$unit_policy, WATER_UNIT_POLICY),
  identical(b$built$n_value_rows_quarantined, 1L),
  is.data.frame(b$built$value_quarantine_receipt),
  nrow(b$built$value_quarantine_receipt) == 102L,
  identical(names(b$built$value_quarantine_receipt),
            WATER_VALUE_QUARANTINE_RECEIPT_COLUMNS),
  identical(sum(b$built$value_quarantine_receipt$n_excluded), 1L),
  identical(b$built$value_quarantine_receipt_sha256,
            water_unit_receipt_sha256(b$built$value_quarantine_receipt)),
  !length(water_value_quarantine_receipt_problems(b$built))
)
hit <- b$built$value_quarantine_receipt$site == "ARIK" &
  b$built$value_quarantine_receipt$collectDate == "2012-03-07" &
  b$built$value_quarantine_receipt$analyte == "Fe"
stopifnot(identical(b$built$value_quarantine_receipt$n_excluded[hit], 1L))

# 2) The same identity from any other laboratory stops the build.
wrong_lab <- base
wrong_lab$laboratoryName[[1]] <- "Other"
expect_error_matching(build_swc_bundle(wrong_lab, data.frame(), coords),
                      "required laboratory provenance")
missing_lab <- base
missing_lab$laboratoryName[[1]] <- NA_character_
expect_error_matching(build_swc_bundle(missing_lab, data.frame(), coords),
                      "required laboratory provenance")

# 3) A matched group whose median is below 1 mg/L means NEON already corrected
#    it; the quarantine would drop good data, so the build stops.
corrected <- base
corrected$analyteConcentration[[1]] <- 0.0581
expect_error_matching(build_swc_bundle(corrected, data.frame(), coords),
                      "median for ARIK Fe is below 1 mg/L")

# 4) A matched row tagged RELEASE-2027 (or later) stops the build; PROVISIONAL,
#    an earlier release, and a missing tag are allowed.
released <- base
released$release <- c("RELEASE-2027", "RELEASE-2027", "RELEASE-2027")
expect_error_matching(build_swc_bundle(released, data.frame(), coords),
                      "RELEASE-2027 or later")
later <- base
later$release <- c("RELEASE-2031", NA, NA)
expect_error_matching(build_swc_bundle(later, data.frame(), coords),
                      "RELEASE-2027 or later")
allowed <- base
allowed$release <- c("RELEASE-2026", "PROVISIONAL", NA)
allowed_bundle <- build_swc_bundle(allowed, data.frame(), coords)
allowed_bundle$built$when <- b$built$when
stopifnot(identical(allowed_bundle, b))
odd_tag <- base
odd_tag$release <- c("NIGHTLY", NA, NA)
expect_error_matching(build_swc_bundle(odd_tag, data.frame(), coords),
                      "Unrecognized NEON release tag")

# 5) An unlisted identity inside the window stops the build (unapproved growth),
#    as does a source-row count above the audited bound.
unlisted <- rbind(base, lab_row("ARIK", "2012-03-08", "Fe", 60))
expect_error_matching(build_swc_bundle(unlisted, data.frame(), coords),
                      "Unapproved value-quarantine identity")
over <- rbind(base, lab_row("ARIK", "2012-03-07", "Fe", 59))
expect_error_matching(build_swc_bundle(over, data.frame(), coords),
                      "source-row count increased")

# 6) Per-row floor: a matched value below 0.1 mg/L stops even when the group
#    median clears 1 mg/L.
floor_rows <- rbind(
  lab_row("ARIK", "2012-03-07", "Fe", 58.1),
  lab_row("ARIK", "2012-03-14", "Fe", 62.0),
  lab_row("ARIK", "2012-03-26", "Fe", 0.05)
)
expect_error_matching(build_swc_bundle(floor_rows, data.frame(), coords),
                      "below 0.1 mg/L")

# 7) validate_bundle rejects a tampered or partial quarantine receipt, and a
#    v4 bundle that claims a quarantine receipt.
tampered <- b
tampered$built$value_quarantine_receipt$n_excluded[[1]] <- 5L
stopifnot(nzchar(tryCatch({ validate_bundle(tampered); "" },
                          error = conditionMessage)))
partial <- b
partial$built$value_quarantine_receipt_sha256 <- NULL
stopifnot(nzchar(tryCatch({ validate_bundle(partial); "" },
                          error = conditionMessage)))
v4_with_receipt <- b
v4_with_receipt$built$unit_policy <- WATER_UNIT_PRIOR_POLICY
stopifnot(nzchar(tryCatch({ validate_bundle(v4_with_receipt); "" },
                          error = conditionMessage)))
v4_clean <- v4_with_receipt
v4_clean$built[WATER_VALUE_QUARANTINE_RECEIPT_FIELDS] <- NULL
stopifnot(isTRUE(validate_bundle(v4_clean)))

# 8) Runtime path on a tiny collapsed frame: listed identities are removed with
#    a reason, retained values are unchanged, and every guard fails closed.
collapsed <- data.frame(
  site = c("ARIK", "ARIK", "ARIK", "SYCA"),
  collectDate = as.Date(c("2012-03-07", "2012-03-07", "2017-05-02",
                          "2013-04-01")),
  analyte = c("Fe", "Mn", "Fe", "Fe"),
  value = c(58.1, 24.2, 0.03, 0.05),
  units = "milligramsPerLiter",
  n_reps = 1L, belowDetection = 0L,
  stringsAsFactors = FALSE
)
rt <- canonicalize_runtime_water_units(collapsed)
stopifnot(
  nrow(rt$data) == 2L,
  identical(rt$data$value, c(0.03, 0.05)),
  identical(rt$n_collapsed_rows_excluded, 2L),
  identical(rt$n_source_rows_excluded, 2L),
  identical(rt$n_value_quarantine_rows_excluded, 2L),
  identical(names(rt$excluded), c("site", "collectDate", "analyte",
                                  "from_unit", "n_source_rows", "reason")),
  all(rt$excluded$reason ==
        "neon-changelog-2026-04-21-usgs-nwqa-ugL-labelled-mgL")
)
rt_unlisted <- collapsed
rt_unlisted$collectDate[[1]] <- as.Date("2012-03-08")
expect_error_matching(canonicalize_runtime_water_units(rt_unlisted),
                      "Unapproved runtime value-quarantine identity")
rt_over <- collapsed
rt_over$n_reps[[1]] <- 2L
expect_error_matching(canonicalize_runtime_water_units(rt_over),
                      "Runtime value-quarantine count increased")
rt_corrected <- collapsed
rt_corrected$value[[1]] <- 0.0581
expect_error_matching(canonicalize_runtime_water_units(rt_corrected),
                      "median for ARIK Fe is below 1 mg/L")

# 9) The real committed bundle: 102 collapsed identities / 110 source rows are
#    removed at the runtime boundary (for a v4 bundle) and none remain.
D <- readRDS(file.path(repo_root, "data", "neon_swc.rds"))
real <- canonicalize_runtime_water_units(D$swc_long)
L <- real$data
left <- L$site %in% c("ARIK", "BARC", "PRLA", "PRPO", "REDB", "SUGG") &
  L$analyte %in% c("Fe", "Mn") &
  L$collectDate >= as.Date("2012-01-31") &
  L$collectDate <= as.Date("2016-06-19")
stopifnot(!any(left))
if (identical(D$built$unit_policy, WATER_UNIT_PRIOR_POLICY)) {
  stopifnot(
    identical(real$n_value_quarantine_rows_excluded, 102L),
    identical(real$n_value_quarantine_source_rows_excluded, 110L),
    nrow(L) == nrow(D$swc_long) - 102L
  )
}

cat("Identity-keyed Fe/Mn value quarantine regressions passed.\n")
