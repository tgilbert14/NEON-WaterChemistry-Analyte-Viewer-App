# v5 producer finish round (2026-10): the replicate collapse, the NEON release
# carry, the independent candidate oracles, and a v3 replay round trip through
# scripts/verify_water_refresh_replay_candidate.R.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
old_wd <- setwd(repo_root)
source(file.path("scripts", "build_swc_bundle.R"))
source(file.path("scripts", "water_refresh_review.R"))
setwd(old_wd)

expect_error <- function(expr, pattern) {
  msg <- tryCatch({ force(expr); "" }, error = conditionMessage)
  if (!nzchar(msg) || !grepl(pattern, msg, fixed = TRUE)) {
    stop(sprintf("Expected an error matching '%s', got '%s'", pattern, msg))
  }
  invisible(msg)
}

lab_rows <- function(site, date, analyte, values, below, release = NA_character_,
                     units = "milligramsPerLiter", lab = "Example_Lab") {
  n <- length(values)
  data.frame(
    site = site, collectDate = date, analyte = analyte,
    analyteConcentration = as.character(values),
    analyteUnits = units, laboratoryName = lab,
    belowDetectionQF = rep(below, length.out = n),
    externalLabDataQF = NA_character_,
    release = rep(release, length.out = n),
    stringsAsFactors = FALSE
  )
}
coords <- data.frame(
  site = c("SYCA", "WALK"), neonName = c("Sycamore Creek", "Walker Branch"),
  domain = c("D14", "D07"), state = c("AZ", "TN"),
  lat = c(33.08, 35.96), long = c(-111.50, -84.28),
  siteType = "Wadeable stream", stringsAsFactors = FALSE
)

# ---- 1) Replicate collapse ---------------------------------------------------
lab <- rbind(
  lab_rows("SYCA", "2024-05-02", "Cl", c(1.5, 0.04), c("0", "ND"), "RELEASE-2026"),
  lab_rows("SYCA", "2024-05-02", "Br", c(0.01, 0.02), c("ND", "BDL"), "RELEASE-2026"),
  lab_rows("SYCA", "2024-05-02", "F", c(NA, NA), c("ND", "ND"), "RELEASE-2026"),
  lab_rows("SYCA", "2024-05-02", "Ca", c(4, 6, 0.1), c("0", "0", "ND"), "RELEASE-2026"),
  lab_rows("SYCA", "2026-08-03", "Cl", 2.5, "0", "PROVISIONAL"),
  lab_rows("WALK", "2024-06-11", "Cl", 0.8, "0", "RELEASE-2026")
)
field <- data.frame(site = "SYCA", collectDate = "2026-08-03", waterTemp = 21.5,
                    dissolvedOxygen = NA, specificConductance = 310,
                    release = "PROVISIONAL", stringsAsFactors = FALSE)
b <- build_swc_bundle(lab, field, coords)
row <- function(site, analyte, date) {
  x <- b$swc_long[b$swc_long$site == site & b$swc_long$analyte == analyte &
                    b$swc_long$collectDate == as.Date(date), , drop = FALSE]
  stopifnot(nrow(x) == 1L)
  x
}
cl <- row("SYCA", "Cl", "2024-05-02")   # 1 detect + 1 ND
br <- row("SYCA", "Br", "2024-05-02")   # 2 ND with values
f <- row("SYCA", "F", "2024-05-02")     # 2 blank-concentration ND
ca <- row("SYCA", "Ca", "2024-05-02")   # 2 detects + 1 ND
stopifnot(
  identical(cl$value, 1.5), is.na(cl$value_sd), identical(cl$n_reps, 2L),
  identical(cl$belowDetection, 0L), identical(cl$partialBelow, 1L),
  identical(br$value, mean(c(0.01, 0.02))),
  identical(br$value_sd, stats::sd(c(0.01, 0.02))),
  identical(br$belowDetection, 1L), identical(br$partialBelow, 0L),
  is.na(f$value), is.na(f$value_sd),
  identical(f$belowDetection, 1L), identical(f$partialBelow, 0L),
  identical(ca$value, 5), identical(ca$value_sd, stats::sd(c(4, 6))),
  identical(ca$belowDetection, 0L), identical(ca$partialBelow, 1L),
  is.integer(b$swc_long$partialBelow),
  identical(b$built$n_below, 2L),
  identical(b$analyte_meta$n_below[b$analyte_meta$analyte == "Cl"], 0L),
  identical(b$analyte_meta$n_below[b$analyte_meta$analyte == "Br"], 1L)
)

# ---- 2) Release carry ----------------------------------------------------------
stopifnot(
  is.character(b$swc_long$release), !anyNA(b$swc_long$release),
  identical(row("SYCA", "waterTemp", "2026-08-03")$release, "PROVISIONAL"),
  identical(b$built$neon_releases, data.frame(
    release = c("PROVISIONAL", "RELEASE-2026"), n = c(3L, 5L),
    first = c("2026-08-03", "2024-05-02"), last = c("2026-08-03", "2024-06-11"),
    stringsAsFactors = FALSE
  )),
  identical(b$built$provisional_from, "2026-08-03")
)
# No PROVISIONAL row: provisional_from is NA, the summary still present.
b_rel <- build_swc_bundle(lab[lab$release != "PROVISIONAL", ], field[0, ], coords)
stopifnot(identical(b_rel$built$provisional_from, NA_character_),
          identical(b_rel$built$neon_releases$release, "RELEASE-2026"))
# Input without a release column (build_rds_from_cache.R): all NA, no summary.
b_cache <- build_swc_bundle(lab[setdiff(names(lab), "release")],
                            field[setdiff(names(field), "release")], coords)
stopifnot(all(is.na(b_cache$swc_long$release)),
          is.character(b_cache$swc_long$release),
          !any(c("neon_releases", "provisional_from") %in% names(b_cache$built)))
# Two releases inside one collapsed group stop the build.
mixed <- lab
mixed$release[[2]] <- "RELEASE-2025"
expect_error(build_swc_bundle(mixed, field, coords), "more than one NEON release")
# A tampered summary fails validate_bundle.
tampered <- b
tampered$built$neon_releases$n[[1]] <- 99L
expect_error(validate_bundle(tampered), "")
tampered <- b
tampered$built$provisional_from <- NULL
expect_error(validate_bundle(tampered), "")

# ---- 3) Codebook rows equal the export keep-vector -----------------------------
cb_path <- tempfile(fileext = ".csv")
write_codebook(b, cb_path)
cb <- utils::read.csv(cb_path, comment.char = "#", stringsAsFactors = FALSE)
long_cb <- cb[cb$section == "tidy_long_export", ]
stopifnot(
  grepl("version 1.2.0", readLines(cb_path, n = 1L), fixed = TRUE),
  identical(long_cb$name, LONG_EXPORT_KEEP),
  all(c("partial_below", "release") %in% LONG_EXPORT_KEEP),
  identical(long_cb$na_semantics[long_cb$name == "partial_below"], "never NA"),
  grepl("^Every replicate", long_cb$definition[long_cb$name == "below_detection"])
)
unlink(cb_path)

# ---- 4) Independent oracles (loaded by name from the verifier) -----------------
oracle_env <- new.env(parent = globalenv())
for (e in parse(file.path(repo_root, "scripts", "verify_refresh_candidate.R"),
                keep.source = FALSE)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      grepl("^(water_oracle_|WATER_ORACLE_)", as.character(e[[2]]))) {
    eval(e, oracle_env)
  }
}
stopifnot(is.function(oracle_env$water_oracle_fe_mn),
          is.function(oracle_env$water_oracle_halide_parity),
          identical(oracle_env$WATER_ORACLE_HALIDE_FLOOR, 0.85))
dates_before <- as.Date(c("2014-03-01", "2015-03-01", "2016-03-01"))
dates_after <- as.Date(c("2018-03-01", "2019-03-01", "2020-03-01"))
fe_fixture <- function(before, after, site = "ARIK", analyte = "Fe") {
  data.frame(site = site, analyte = analyte,
             collectDate = c(dates_before, dates_after),
             value = c(before, after), stringsAsFactors = FALSE)
}
run_fe <- function(d) oracle_env$water_oracle_fe_mn(d$site, d$analyte,
                                                    d$collectDate, d$value)
mislabel <- fe_fixture(c(58, 61, 70), c(0.03, 0.02, 0.04))  # ~2000x
clean <- fe_fixture(c(0.3, 0.4, 0.2), c(0.03, 0.02, 0.04))  # ~10x
edge <- fe_fixture(c(3, 3, 3), c(0.03, 0.03, 0.03))         # exactly 100x
short <- fe_fixture(c(58, 61, 70), c(0.03, 0.02, 0.04))[-6, ]
elsewhere <- fe_fixture(c(58, 61, 70), c(0.03, 0.02, 0.04), site = "SYCA")
mn <- fe_fixture(c(58, 61, 70), c(0.03, 0.02, 0.04), analyte = "Mn")
nonfinite <- fe_fixture(c(58, NA, Inf), c(0.03, 0.02, 0.04))
stopifnot(
  identical(nrow(run_fe(mislabel)), 1L),
  identical(run_fe(mislabel)$site, "ARIK"),
  identical(nrow(run_fe(clean)), 0L),
  identical(nrow(run_fe(edge)), 1L),
  identical(nrow(run_fe(short)), 0L),          # fewer than 3 after
  identical(nrow(run_fe(elsewhere)), 0L),      # not one of the six sites
  identical(run_fe(mn)$analyte, "Mn"),
  identical(nrow(run_fe(nonfinite)), 0L)       # only 1 finite value before
)
halide_fixture <- function(n_cl, n_br, n_f, year = 2024L) {
  d <- as.Date(sprintf("%d-06-01", year))
  data.frame(analyte = c(rep("Cl", n_cl), rep("Br", n_br), rep("F", n_f)),
             collectDate = d, stringsAsFactors = FALSE)
}
run_hal <- function(d) oracle_env$water_oracle_halide_parity(d$analyte, d$collectDate)
stopifnot(
  identical(nrow(run_hal(halide_fixture(700, 690, 700))), 0L),
  identical(run_hal(halide_fixture(733, 242, 732))$analyte, "Br"),  # v4 2024
  identical(nrow(run_hal(halide_fixture(700, 595, 595))), 0L),      # exactly 0.85
  identical(nrow(run_hal(halide_fixture(700, 594, 700))), 1L),
  identical(sort(run_hal(halide_fixture(700, 300, 300))$analyte), c("Br", "F")),
  identical(nrow(run_hal(halide_fixture(99, 0, 0))), 0L),           # < 100 Cl rows
  identical(nrow(run_hal(halide_fixture(700, 100, 100, year = 2021L))), 0L)
)

# ---- 5) Replay v3 round trip through the replay verifier ---------------------
stopifnot(identical(WATER_REFRESH_REVIEW_SCHEMA, "water-refresh-review-v3"),
          "release" %in% WATER_REFRESH_REPLAY_LAB_COLUMNS,
          "release" %in% WATER_REFRESH_REPLAY_FIELD_COLUMNS)
fixture <- tempfile("water-replay-v3-")
dir.create(file.path(fixture, "scripts"), recursive = TRUE)
dir.create(file.path(fixture, "data"))
for (f in c("build_swc_bundle.R", "water_unit_contract.R",
            "water_refresh_review.R", "verify_water_refresh_replay_candidate.R")) {
  stopifnot(file.copy(file.path(repo_root, "scripts", f),
                      file.path(fixture, "scripts", f)))
}
review_dir <- file.path(fixture, "review")
sha <- paste(rep("c", 40L), collapse = "")
# The producer's field rows read collectDate as a timestamp; the replay keeps
# its text, and the rebuild must still agree.
field_ts <- field
field_ts$collectDate <- as.POSIXct("2026-08-03 15:20:00", tz = "UTC")
write_water_refresh_review(lab, field_ts, coords, review_dir, source_sha = sha)
replay_lab <- readRDS(file.path(review_dir, "replay", "lab_raw.rds"))
stopifnot(identical(sort(unique(replay_lab$release)),
                    c("PROVISIONAL", "RELEASE-2026")))
producer <- build_swc_bundle(lab, field_ts, coords)
saveRDS(producer, file.path(fixture, "data", "neon_swc.rds"))
run_replay <- function() {
  old <- setwd(fixture); on.exit(setwd(old), add = TRUE)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "scripts/verify_water_refresh_replay_candidate.R",
      review_dir, sha), stdout = TRUE, stderr = TRUE))
  list(status = attr(out, "status") %||% 0L, text = paste(out, collapse = "\n"))
}
ok <- run_replay()
if (!identical(ok$status, 0L)) stop("Replay verifier rejected a valid v3 candidate:\n", ok$text)
# A candidate whose release summary was altered no longer reproduces.
altered <- producer
altered$swc_long$release[altered$swc_long$release == "PROVISIONAL"] <- "RELEASE-2026"
altered$built$neon_releases <- water_release_summary(altered$swc_long$release,
                                                     altered$swc_long$collectDate)
altered$built$provisional_from <- NA_character_
saveRDS(altered, file.path(fixture, "data", "neon_swc.rds"))
bad <- run_replay()
stopifnot(bad$status != 0L,
          grepl("does not exactly reproduce", bad$text, fixed = TRUE))
unlink(fixture, recursive = TRUE)

cat("Replicate collapse, release carry, candidate oracles, and replay v3 round trip passed.\n")
