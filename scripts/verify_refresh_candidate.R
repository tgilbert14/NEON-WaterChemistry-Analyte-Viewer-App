# Verify a proposed surface-water-chemistry release. The one required argument
# is an independently extracted base/main bundle; it makes the validator enforce
# non-shrink, date, roster, and legacy-byte behavior without trusting producer
# state.

fail <- function(...) stop(sprintf(...), call. = FALSE)
need <- function(ok, ...) if (!isTRUE(ok)) fail(...)
show_set <- function(x) if (length(x)) paste(x, collapse = ", ") else "<none>"
scalar_integer <- function(x) {
  is.integer(x) && length(x) == 1L && !is.na(x)
}
scalar_logical <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}
scalar_character <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

source(file.path("scripts", "water_unit_contract.R"))

args <- commandArgs(trailingOnly = TRUE)
need(length(args) == 1L && nzchar(args[[1]]) &&
       !startsWith(args[[1]], "--"),
     paste0(
       "Usage: verify_refresh_candidate.R <trusted-base-bundle.rds>; ",
       "unknown flags and extra positional arguments are forbidden."
     ))
baseline_path <- args[[1]]

required_paths <- c(
  "app.R", "helpers.R", "scripts/water_unit_contract.R", "data/codebook.csv",
  "data/neon_swc.rds", "data/search_index.rds", "manifest.json"
)
missing_paths <- required_paths[!file.exists(required_paths)]
need(!length(missing_paths), "Missing required candidate path(s): %s",
     paste(missing_paths, collapse = ", "))

expected_sites <- sort(c(
  "ARIK", "BARC", "BIGC", "BLDE", "BLUE", "BLWA", "CARI", "COMO", "CRAM",
  "CUPE", "FLNT", "GUIL", "HOPB", "KING", "LECO", "LEWI", "LIRO", "MART",
  "MAYF", "MCDI", "MCRA", "OKSR", "POSE", "PRIN", "PRLA", "PRPO", "REDB",
  "SUGG", "SYCA", "TECR", "TOMB", "TOOK", "WALK", "WLOU"
))

bundle <- tryCatch(readRDS("data/neon_swc.rds"), error = function(e) fail(
  "Cannot read data/neon_swc.rds: %s", conditionMessage(e)
))
need(identical(names(bundle), c("swc_long", "swc_wide", "sites_meta",
                                "analyte_meta", "built")),
     "Bundle top-level schema changed.")
need(is.data.frame(bundle$swc_long) && nrow(bundle$swc_long) > 0,
     "swc_long is empty or invalid.")
need(all(c("site", "collectDate", "analyte", "value", "belowDetection",
           "units", "source") %in% names(bundle$swc_long)),
     "swc_long is missing required columns.")
need(is.data.frame(bundle$sites_meta) && is.data.frame(bundle$analyte_meta),
     "Bundle metadata tables are invalid.")

site_roster <- sort(unique(as.character(bundle$sites_meta$site)))
observation_sites <- sort(unique(as.character(bundle$swc_long$site)))
need(identical(site_roster, expected_sites),
     "Candidate site roster is not the canonical 34-site contract.")
need(identical(observation_sites, expected_sites),
     "Observation table does not cover the canonical 34-site contract.")
need(!anyNA(bundle$swc_long$site) && !anyNA(bundle$swc_long$analyte),
     "Observation identity contains missing site or analyte values.")
candidate_analytes <- sort(unique(as.character(bundle$swc_long$analyte)))
need(length(candidate_analytes) > 0L && !anyNA(candidate_analytes) &&
       all(nzchar(candidate_analytes)),
     "Candidate analyte roster is empty or malformed.")
need(is.integer(bundle$swc_long$belowDetection) &&
       !anyNA(bundle$swc_long$belowDetection) &&
       all(bundle$swc_long$belowDetection %in% c(0L, 1L)),
     "Observation belowDetection values must be exact 0L/1L integers.")
need(is.list(bundle$built), "Bundle build receipt is missing.")
need(scalar_integer(bundle$built$n_obs) &&
       identical(bundle$built$n_obs, as.integer(nrow(bundle$swc_long))),
     "Build receipt n_obs must be an exact integer matching swc_long.")
need(scalar_integer(bundle$built$n_sites) &&
       identical(bundle$built$n_sites, as.integer(length(expected_sites))) &&
       identical(bundle$built$n_sites,
                 as.integer(length(observation_sites))),
     "Build receipt n_sites must be the exact integer 34 and match swc_long.")
need(scalar_integer(bundle$built$n_analytes) &&
       identical(bundle$built$n_analytes,
                 as.integer(length(candidate_analytes))),
     "Build receipt n_analytes must be an exact integer matching swc_long.")
need(scalar_integer(bundle$built$n_below) &&
       identical(bundle$built$n_below,
                 as.integer(sum(bundle$swc_long$belowDetection))),
     "Build receipt n_below must be an exact integer matching swc_long.")
need(scalar_logical(bundle$built$partial) &&
       identical(bundle$built$partial, FALSE),
     "Candidate build receipt partial must be the exact logical FALSE.")
need(identical(bundle$built$product, "DP1.20093.001"),
     "Unexpected NEON product in build receipt.")
need(scalar_character(bundle$built$when) &&
       grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
             bundle$built$when),
     "Build receipt when is missing or malformed.")
built_data_through <- suppressWarnings(tryCatch(
  as.Date(bundle$built$data_through), error = function(e) as.Date(NA)
))
need(scalar_character(bundle$built$data_through) &&
       length(built_data_through) == 1L && !is.na(built_data_through),
     "Build receipt data_through is missing or malformed.")

analyte_meta_required <- c(
  "analyte", "units", "n", "n_sites", "n_below", "source"
)
need(all(analyte_meta_required %in% names(bundle$analyte_meta)),
     "analyte_meta is missing required columns.")
meta_analytes <- as.character(bundle$analyte_meta$analyte)
need(nrow(bundle$analyte_meta) == length(candidate_analytes) &&
       !anyNA(meta_analytes) && !anyDuplicated(meta_analytes) &&
       identical(sort(meta_analytes), candidate_analytes),
     "analyte_meta roster disagrees with swc_long.")
need(is.integer(bundle$analyte_meta$n) &&
       is.integer(bundle$analyte_meta$n_sites) &&
       is.integer(bundle$analyte_meta$n_below) &&
       !anyNA(bundle$analyte_meta$n) &&
       !anyNA(bundle$analyte_meta$n_sites) &&
       !anyNA(bundle$analyte_meta$n_below) &&
       all(bundle$analyte_meta$n > 0L) &&
       all(bundle$analyte_meta$n_sites > 0L) &&
       all(bundle$analyte_meta$n_below >= 0L) &&
       all(bundle$analyte_meta$n_below <= bundle$analyte_meta$n),
     "analyte_meta counts must be exact bounded integers.")
observation_rows <- split(
  seq_len(nrow(bundle$swc_long)), as.character(bundle$swc_long$analyte)
)
meta <- bundle$analyte_meta[
  match(candidate_analytes, meta_analytes), , drop = FALSE
]
expected_meta_n <- vapply(observation_rows[candidate_analytes], length,
                          integer(1))
expected_meta_sites <- vapply(
  observation_rows[candidate_analytes],
  function(i) length(unique(as.character(bundle$swc_long$site[i]))),
  integer(1)
)
expected_meta_below <- vapply(
  observation_rows[candidate_analytes],
  function(i) sum(bundle$swc_long$belowDetection[i]), integer(1)
)
expected_meta_units <- vapply(
  observation_rows[candidate_analytes],
  function(i) as.character(bundle$swc_long$units[i])[[1]], character(1)
)
expected_meta_source <- vapply(
  observation_rows[candidate_analytes],
  function(i) {
    source_values <- unique(as.character(bundle$swc_long$source[i]))
    if (length(source_values) != 1L || is.na(source_values) ||
        !nzchar(source_values)) return(NA_character_)
    source_values
  }, character(1)
)
need(identical(unname(meta$n), unname(expected_meta_n)) &&
       identical(unname(meta$n_sites), unname(expected_meta_sites)) &&
       identical(unname(meta$n_below), unname(expected_meta_below)),
     "analyte_meta counts disagree with swc_long.")
need(identical(as.character(meta$units), unname(expected_meta_units)),
     "analyte_meta units disagree with swc_long provenance.")
need(!anyNA(expected_meta_source) &&
       identical(as.character(meta$source), unname(expected_meta_source)),
     "analyte_meta source provenance disagrees with swc_long.")

baseline <- tryCatch(readRDS(baseline_path), error = function(e) fail(
  "Cannot read trusted validation baseline: %s", conditionMessage(e)
))
need(is.list(baseline) && is.data.frame(baseline$swc_long) &&
       is.data.frame(baseline$sites_meta) && is.list(baseline$built),
     "Trusted validation baseline schema is invalid.")
baseline_sites <- sort(unique(as.character(baseline$sites_meta$site)))
need(identical(site_roster, baseline_sites),
     "Candidate site roster differs from the trusted base/main bundle.")
baseline_analytes <- sort(unique(as.character(baseline$swc_long$analyte)))
need(identical(candidate_analytes, baseline_analytes),
     "Candidate analyte roster differs from the trusted base/main bundle.")
need(identical(candidate_analytes,
               sort(names(WATER_ESTABLISHED_UNIT_TARGETS))),
     "Candidate analyte roster differs from the established 34-analyte contract.")
need(nrow(bundle$swc_long) >= nrow(baseline$swc_long),
     "Candidate observation table shrank from %d to %d rows.",
     nrow(baseline$swc_long), nrow(bundle$swc_long))
old_through <- as.Date(baseline$built$data_through)
new_through <- as.Date(bundle$built$data_through)
need(!is.na(old_through) && !is.na(new_through) && new_through >= old_through,
     "Candidate data-through date regressed.")

if (!requireNamespace("digest", quietly = TRUE)) {
  fail("digest is required to verify candidate and unit-receipt SHA-256 values.")
}
runtime_result <- tryCatch(
  canonicalize_runtime_water_units(bundle$swc_long),
  error = function(e) fail(
    "Candidate cannot produce the fail-closed runtime view: %s",
    conditionMessage(e)
  )
)
runtime_long <- runtime_result$data

# Rebuild the deterministic search tables from the effective runtime rows, not
# from any producer receipt. The verifier deliberately repeats the builder's
# aggregation here so every statistic, label, metadata field, and ordering key
# is checked against the same plausibility boundary the application reads.
search_packages <- c("dplyr", "tidyr", "tibble")
missing_search_packages <- search_packages[!vapply(
  search_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
need(!length(missing_search_packages),
     "Search-index verification requires package(s): %s.",
     show_set(missing_search_packages))
helper_env <- new.env(parent = globalenv())
tryCatch(
  sys.source("helpers.R", envir = helper_env),
  error = function(e) fail(
    "Cannot load the current runtime plausibility logic: %s",
    conditionMessage(e)
  )
)
runtime_canonical_map <- helper_env$canonical_units(runtime_long)
runtime_ceiling_map <- helper_env$ceiling_map(
  helper_env$plausibility_ceilings(runtime_long)
)
expected_search_long <- runtime_long
expected_search_long$units <- unname(
  runtime_canonical_map[expected_search_long$analyte]
)
expected_search_long$implausible <- !helper_env$is_plausible(
  expected_search_long$value, expected_search_long$analyte,
  runtime_ceiling_map, site = expected_search_long$site
)
expected_search_gated <- expected_search_long |>
  dplyr::filter(!implausible, is.finite(value))
expected_per_site <- expected_search_gated |>
  dplyr::mutate(year = as.integer(format(collectDate, "%Y"))) |>
  dplyr::group_by(analyte, site) |>
  dplyr::summarise(
    .groups = "drop",
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    median = stats::median(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    n_below = sum(belowDetection == 1, na.rm = TRUE),
    year_min = suppressWarnings(min(year, na.rm = TRUE)),
    year_max = suppressWarnings(max(year, na.rm = TRUE))
  ) |>
  dplyr::mutate(pct_below = ifelse(n > 0, n_below / n, NA_real_)) |>
  dplyr::left_join(
    bundle$sites_meta |>
      dplyr::select(site, siteName, state, domain),
    by = "site"
  ) |>
  dplyr::mutate(
    display = helper_env$analyte_display(analyte),
    units = unname(runtime_canonical_map[analyte]),
    pretty = helper_env$pretty_unit(units, analyte)
  ) |>
  dplyr::arrange(analyte, dplyr::desc(mean))
expected_index_analytes <- expected_per_site |>
  dplyr::group_by(analyte) |>
  dplyr::summarise(
    .groups = "drop",
    display = dplyr::first(display),
    units = dplyr::first(units),
    pretty = dplyr::first(pretty),
    n_sites = dplyr::n_distinct(site),
    n_obs = sum(n),
    vmin = min(min, na.rm = TRUE),
    vmax = max(max, na.rm = TRUE)
  ) |>
  dplyr::arrange(dplyr::desc(n_obs))

index <- tryCatch(readRDS("data/search_index.rds"), error = function(e) fail(
  "Cannot read data/search_index.rds: %s", conditionMessage(e)
))
need(is.list(index) && is.data.frame(index$per_site) &&
       is.data.frame(index$analytes), "Search index schema is invalid.")
need(all(c("site", "analyte", "n", "n_below", "units") %in%
           names(index$per_site)) &&
       all(c("analyte", "n_sites", "n_obs", "units") %in%
             names(index$analytes)),
     "Search index count columns are missing.")
need(!anyDuplicated(index$per_site[c("analyte", "site")]),
     "Search index has duplicate analyte/site rows.")
need(is.integer(index$per_site$n) &&
       is.integer(index$per_site$n_below) &&
       !anyNA(index$per_site$n) && !anyNA(index$per_site$n_below) &&
       all(index$per_site$n > 0L) &&
       all(index$per_site$n_below >= 0L) &&
       all(index$per_site$n_below <= index$per_site$n),
     "Search index per-site counts must be exact bounded integers.")
index_analytes <- as.character(index$analytes$analyte)
need(!anyNA(index_analytes) && !anyDuplicated(index_analytes) &&
       all(nzchar(index_analytes)),
     "Search index analyte identities are malformed.")
need(is.integer(index$analytes$n_sites) &&
       is.integer(index$analytes$n_obs) &&
       !anyNA(index$analytes$n_sites) &&
       !anyNA(index$analytes$n_obs) &&
       all(index$analytes$n_sites > 0L) &&
       all(index$analytes$n_obs > 0L),
     "Search index analyte counts must be exact positive integers.")
need(identical(sort(unique(as.character(index$per_site$site))), expected_sites),
     "Search index site roster is incomplete.")
need(identical(sort(unique(as.character(index$per_site$analyte))),
               sort(unique(as.character(bundle$swc_long$analyte)))),
     "Search index analyte roster disagrees with the source bundle.")
need(identical(sort(index_analytes), candidate_analytes),
     "Search index analyte catalogue disagrees with the source bundle.")
index_target_units <- unname(WATER_ESTABLISHED_UNIT_TARGETS[
  as.character(index$per_site$analyte)
])
catalogue_target_units <- unname(WATER_ESTABLISHED_UNIT_TARGETS[
  index_analytes
])
need(identical(as.character(index$per_site$units), index_target_units) &&
       identical(as.character(index$analytes$units),
                 catalogue_target_units),
     "Search index units differ from the established runtime targets.")
runtime_key <- paste(runtime_long$analyte, runtime_long$site, sep = "\r")
runtime_group_n <- table(runtime_key)
index_key <- paste(index$per_site$analyte, index$per_site$site, sep = "\r")
available_n <- as.integer(runtime_group_n[index_key])
need(!anyNA(available_n) && all(index$per_site$n <= available_n) &&
       all(index$per_site$n_below <= available_n),
     "Search index counts exceed the fail-closed runtime source view.")
index_rows <- split(
  seq_len(nrow(index$per_site)), as.character(index$per_site$analyte)
)
expected_index_sites <- vapply(
  index_rows[index_analytes],
  function(i) length(unique(as.character(index$per_site$site[i]))),
  integer(1)
)
expected_index_obs <- vapply(
  index_rows[index_analytes],
  function(i) sum(index$per_site$n[i]), integer(1)
)
need(identical(unname(index$analytes$n_sites),
               unname(expected_index_sites)) &&
       identical(unname(index$analytes$n_obs), unname(expected_index_obs)),
     "Search index analyte counts disagree with per-site rows.")
need(scalar_integer(index$n_sites) &&
       identical(index$n_sites, as.integer(length(expected_sites))) &&
       identical(index$n_sites, as.integer(length(unique(index$per_site$site)))),
     "Search index n_sites must be the exact integer 34 and match per-site rows.")
need(is.list(index$built) && scalar_character(index$built$when) &&
       identical(index$built$when, bundle$built$when),
     "Search index provenance is not derived from the source bundle.")
need(identical(index$built$product, bundle$built$product) &&
       identical(index$built$source,
                 "data/neon_swc.rds (committed bundle; plausibility-gated)"),
     "Search index product/source provenance changed.")
need(identical(index$built$runtime_unit_policy, WATER_UNIT_POLICY) &&
       identical(index$built$n_runtime_unit_rows_excluded,
                 runtime_result$n_collapsed_rows_excluded) &&
       identical(index$built$n_runtime_unit_source_rows_excluded,
                 runtime_result$n_source_rows_excluded) &&
       identical(index$built$n_runtime_unit_labels_rewritten,
                 runtime_result$n_missing_labels_rewritten) &&
       identical(index$built$runtime_unit_exclusion_sha256,
                 water_unit_receipt_sha256(runtime_result$excluded)),
     "Search index runtime-unit receipt disagrees with the source bundle.")
need(identical(index$per_site, expected_per_site),
     paste0(
       "Search index per-site rows differ from independent recomputation ",
       "of values, statistics, years, metadata, labels, units, or ordering."
     ))
need(identical(index$analytes, expected_index_analytes),
     paste0(
       "Search index analyte catalogue differs from independent recomputation ",
       "of labels, units, counts, ranges, or ordering."
     ))

# Receipt fields are atomic: a candidate carries all of the current contract or
# none of it. Receipt-free bytes are accepted only for the exact known legacy
# bundle, and only when those bytes also equal the independently extracted
# trusted base/main bundle.
receipt_present <- WATER_UNIT_RECEIPT_FIELDS %in% names(bundle$built)
need(all(receipt_present) || !any(receipt_present),
     "Candidate carries a partial unit-contract receipt: %s.",
     show_set(WATER_UNIT_RECEIPT_FIELDS[receipt_present]))
has_unit_contract <- all(receipt_present)

candidate_bundle_sha256 <- digest::digest(
  file = "data/neon_swc.rds", algo = "sha256"
)
baseline_bundle_sha256 <- digest::digest(
  file = baseline_path, algo = "sha256"
)

if (!has_unit_contract) {
  need(identical(candidate_bundle_sha256, WATER_LEGACY_BUNDLE_SHA256),
       paste0(
         "Receipt-free candidate is not the exact known legacy bundle ",
         "(candidate SHA-256 %s)."
       ), candidate_bundle_sha256)
  need(water_legacy_unit_contract_ok(candidate_bundle_sha256,
                                     baseline_bundle_sha256),
       paste0(
         "Receipt-free candidate differs from the trusted base/main bundle ",
         "(candidate %s; base %s)."
       ), candidate_bundle_sha256, baseline_bundle_sha256)
} else {
  need(identical(bundle$built$unit_policy, WATER_UNIT_POLICY),
       "Candidate carries an unexpected unit policy: %s.",
       show_set(bundle$built$unit_policy))
  need(scalar_integer(bundle$built$n_unit_values_changed) &&
         identical(bundle$built$n_unit_values_changed, 0L),
       "Candidate unit value-change count must be the exact integer 0L.")
  need(scalar_integer(bundle$built$n_unit_labels_rewritten) &&
         bundle$built$n_unit_labels_rewritten >= 0L,
       "Candidate unit-label rewrite count must be a non-negative integer scalar.")
  need(scalar_integer(bundle$built$n_unit_rows_excluded) &&
         bundle$built$n_unit_rows_excluded >= 0L,
       "Candidate unit-row exclusion count must be a non-negative integer scalar.")
  need(scalar_character(bundle$built$unit_rewrite_receipt_sha256) &&
         grepl("^[0-9a-f]{64}$",
               bundle$built$unit_rewrite_receipt_sha256),
       "Candidate unit-rewrite receipt SHA-256 is malformed.")
  need(scalar_character(bundle$built$unit_exclusion_receipt_sha256) &&
         grepl("^[0-9a-f]{64}$",
               bundle$built$unit_exclusion_receipt_sha256),
       "Candidate unit-exclusion receipt SHA-256 is malformed.")

  label_receipt <- bundle$built$unit_label_rewrites
  label_names <- c(
    "analyte", "from_unit", "to_unit", "required_laboratory",
    "n_rewritten", "n_target_source"
  )
  need(is.data.frame(label_receipt) &&
         identical(names(label_receipt), label_names),
       "Candidate unit-rewrite receipt schema changed.")
  label_receipt <- label_receipt[order(label_receipt$analyte),
                                 , drop = FALSE]
  rownames(label_receipt) <- NULL
  expected_labels <- WATER_UNIT_LABEL_REWRITES[
    order(WATER_UNIT_LABEL_REWRITES$analyte), , drop = FALSE
  ]
  rownames(expected_labels) <- NULL
  need(identical(label_receipt[1:4], expected_labels),
       "Candidate unit-rewrite pair allowlist differs from reviewed policy.")
  need(identical(label_receipt$required_laboratory,
                 rep("EcoCore_CSU", 12L)),
       "Candidate unit-rewrite laboratory provenance differs from policy.")
  need(is.integer(label_receipt$n_rewritten) &&
         is.integer(label_receipt$n_target_source) &&
         !anyNA(label_receipt$n_rewritten) &&
         !anyNA(label_receipt$n_target_source) &&
         all(label_receipt$n_rewritten >= 0L) &&
         all(label_receipt$n_target_source >= 0L),
       "Candidate unit-rewrite pair counts are not exact non-negative integers.")
  active <- label_receipt$n_rewritten > 0L
  need(!any(active & label_receipt$n_target_source == 0L),
       "Candidate unit-label rewrite lacks an established target label.")
  need(identical(
    bundle$built$n_unit_labels_rewritten,
    as.integer(sum(label_receipt$n_rewritten))
  ), "Candidate total unit-label rewrite count disagrees with pair receipt.")
  need(identical(
    bundle$built$unit_rewrite_receipt_sha256,
    water_unit_receipt_sha256(label_receipt)
  ), "Candidate unit-rewrite receipt SHA-256 does not match its pair counts.")

  exclusion_receipt <- bundle$built$unit_row_exclusions
  exclusion_names <- c(
    "site", "collectDate", "analyte", "from_unit", "reason",
    "max_source_rows", "n_excluded"
  )
  need(is.data.frame(exclusion_receipt) &&
         identical(names(exclusion_receipt), exclusion_names),
       "Candidate unit-row exclusion receipt schema changed.")
  exclusion_receipt <- exclusion_receipt[order(
    exclusion_receipt$analyte, exclusion_receipt$site,
    exclusion_receipt$collectDate
  ), , drop = FALSE]
  rownames(exclusion_receipt) <- NULL
  exclusion_rule_index <- match(
    water_unit_rule_key(WATER_UNIT_EXCLUSION_IDENTITIES$analyte,
                        WATER_UNIT_EXCLUSION_IDENTITIES$from_unit),
    water_unit_rule_key(WATER_UNIT_EXCLUSION_RULES$analyte,
                        WATER_UNIT_EXCLUSION_RULES$from_unit)
  )
  expected_exclusions <- data.frame(
    site = WATER_UNIT_EXCLUSION_IDENTITIES$site,
    collectDate = WATER_UNIT_EXCLUSION_IDENTITIES$collectDate,
    analyte = WATER_UNIT_EXCLUSION_IDENTITIES$analyte,
    from_unit = WATER_UNIT_EXCLUSION_IDENTITIES$from_unit,
    reason = WATER_UNIT_EXCLUSION_RULES$reason[exclusion_rule_index],
    max_source_rows = WATER_UNIT_EXCLUSION_IDENTITIES$max_source_rows,
    stringsAsFactors = FALSE
  )
  expected_exclusions <- expected_exclusions[order(
    expected_exclusions$analyte, expected_exclusions$site,
    expected_exclusions$collectDate
  ), , drop = FALSE]
  rownames(expected_exclusions) <- NULL
  need(identical(exclusion_receipt[1:6], expected_exclusions),
       "Candidate audited unit-exclusion identities differ from policy.")
  need(is.integer(exclusion_receipt$n_excluded) &&
         !anyNA(exclusion_receipt$n_excluded) &&
         all(exclusion_receipt$n_excluded >= 0L) &&
         all(exclusion_receipt$n_excluded <=
               exclusion_receipt$max_source_rows),
       "Candidate unit exclusion counts violate audited identity bounds.")
  need(identical(
    bundle$built$n_unit_rows_excluded,
    as.integer(sum(exclusion_receipt$n_excluded))
  ), "Candidate total unit-row exclusion count disagrees with identity receipt.")
  need(identical(
    bundle$built$unit_exclusion_receipt_sha256,
    water_unit_receipt_sha256(exclusion_receipt)
  ), "Candidate unit-exclusion receipt SHA-256 does not match its identities.")

  units_by_analyte <- split(as.character(bundle$swc_long$units),
                            as.character(bundle$swc_long$analyte))
  noncanonical <- names(units_by_analyte)[!vapply(
    units_by_analyte,
    function(x) !anyNA(x) && all(nzchar(x)) && length(unique(x)) == 1L,
    logical(1)
  )]
  need(!length(noncanonical),
       "Candidate analytes lack one canonical non-missing unit: %s.",
       show_set(noncanonical))
  wrong_targets <- names(WATER_ESTABLISHED_UNIT_TARGETS)[vapply(
    names(WATER_ESTABLISHED_UNIT_TARGETS),
    function(a) !identical(unique(units_by_analyte[[a]]),
                           unname(WATER_ESTABLISHED_UNIT_TARGETS[[a]])),
    logical(1)
  )]
  need(!length(wrong_targets),
       "Candidate established analyte unit target changed: %s.",
       show_set(wrong_targets))
}

# The codebook is a release artifact, not an advisory file. Validate its version
# and provenance independently, then reconcile its analyte dictionary with the
# exact effective runtime view. For the known receipt-free legacy bundle this is
# intentionally stricter than raw analyte_meta: the shared runtime boundary
# removes the 48 audited mismatch groups before any app/index/export use.
runtime_rows <- split(
  seq_len(nrow(runtime_long)), as.character(runtime_long$analyte)
)
runtime_n <- vapply(runtime_rows[candidate_analytes], length, integer(1))
runtime_n_sites <- vapply(
  runtime_rows[candidate_analytes],
  function(i) length(unique(as.character(runtime_long$site[i]))), integer(1)
)
runtime_n_below <- vapply(
  runtime_rows[candidate_analytes],
  function(i) sum(runtime_long$belowDetection[i]), integer(1)
)
runtime_units <- vapply(
  runtime_rows[candidate_analytes],
  function(i) unique(as.character(runtime_long$units[i])), character(1)
)
codebook_lines <- readLines("data/codebook.csv", warn = FALSE)
expected_codebook_version <- if (has_unit_contract) "1.1.0" else "1.0.0"
expected_codebook_header <- c(
  sprintf(
    paste0(
      "# NEON Surface Water Chemistry codebook | version %s | ",
      "product %s | built %s"
    ),
    expected_codebook_version, bundle$built$product,
    substr(bundle$built$when, 1L, 10L)
  ),
  paste0(
    "# section=tidy_long_export documents the in-app Tidy CSV columns ",
    "(the keep-vector); section=analyte_dictionary documents every emitted analyte"
  ),
  paste0(
    "# units 'NA' = not applicable for schema rows; emitted analyte rows ",
    "carry explicit reviewed targets"
  )
)
need(length(codebook_lines) >= 4L &&
       identical(codebook_lines[seq_along(expected_codebook_header)],
                 expected_codebook_header) &&
       identical(which(startsWith(codebook_lines, "#")), 1:3),
     "Codebook header, version, product, or build provenance changed.")
codebook <- tryCatch(
  utils::read.csv(
    "data/codebook.csv", comment.char = "#", check.names = FALSE,
    stringsAsFactors = FALSE, na.strings = character(0)
  ),
  error = function(e) fail("Cannot parse data/codebook.csv: %s",
                           conditionMessage(e))
)
codebook_columns <- c(
  "name", "type", "units", "allowed", "definition", "na_semantics",
  "section"
)
need(is.data.frame(codebook) && identical(names(codebook), codebook_columns) &&
       !anyNA(codebook) &&
       all(vapply(codebook, is.character, logical(1))),
     "Codebook table schema or scalar types changed.")
need(all(nzchar(codebook$name)) && !anyDuplicated(codebook$name) &&
       all(nzchar(codebook$allowed)) && all(nzchar(codebook$definition)) &&
       all(nzchar(codebook$na_semantics)),
     "Codebook contains duplicate, empty, or undocumented rows.")
need(identical(sort(unique(codebook$section)),
               c("analyte_dictionary", "tidy_long_export")),
     "Codebook section roster changed.")

long_export_keep <- c(
  "site", "collectDate", "analyte", "analyte_label", "value", "units",
  "n_reps", "value_sd", "below_detection", "implausible_extreme",
  "lab_flag", "source", "product"
)
long_types <- c(
  site = "character", collectDate = "Date", analyte = "character",
  analyte_label = "character", value = "numeric", units = "character",
  n_reps = "integer", value_sd = "numeric", below_detection = "logical",
  implausible_extreme = "logical", lab_flag = "character",
  source = "character", product = "character"
)
long_units <- c(
  site = "NA", collectDate = "ISO date", analyte = "NA",
  analyte_label = "NA", value = "see units column (canonical per analyte)",
  units = "NA", n_reps = "count", value_sd = "same as value",
  below_detection = "NA", implausible_extreme = "NA", lab_flag = "NA",
  source = "NA", product = "NA"
)
long_allowed <- c(
  site = "NEON 4-letter site code", collectDate = "YYYY-MM-DD",
  analyte = "raw analyte code", analyte_label = "free text",
  value = ">= 0 typical", units = "canonical NEON unit string",
  n_reps = ">= 1", value_sd = ">= 0",
  below_detection = "TRUE/FALSE", implausible_extreme = "TRUE/FALSE",
  lab_flag = if (has_unit_contract) {
    "sorted distinct NEON externalLabDataQF codes joined by ' | '"
  } else {
    "NEON externalLabDataQF codes"
  },
  source = "External Lab | Field Probe", product = "DP1.20093.001"
)
long_definition <- c(
  site = "NEON aquatic site where the sample was collected",
  collectDate = "Field collection date (sub-day time dropped)",
  analyte = "NEON analyte identifier (join key to the analyte dictionary)",
  analyte_label = "Human-readable analyte name",
  value = paste0(
    "Replicate-mean concentration / measurement for the ",
    "site-date-analyte"
  ),
  units = paste0(
    "Pinned established unit for the analyte; UV absorbance = ",
    "'absorbance units'"
  ),
  n_reps = "Number of lab/field replicates collapsed into value",
  value_sd = "Standard deviation across replicates before collapse",
  below_detection = paste0(
    "Any replicate reported below the analytical detection limit ",
    "(value kept, never substituted)"
  ),
  implausible_extreme = paste0(
    "Flagged above the plausibility ceiling; kept in this raw export, ",
    "excluded from fits/maps/STL/glm"
  ),
  lab_flag = if (has_unit_contract) {
    "All distinct external-lab quality flags across the collapsed replicates"
  } else {
    "External-lab quality flag (e.g. legacyData, formatChange)"
  },
  source = "Measurement origin",
  product = "NEON data product code"
)
long_na_semantics <- c(
  site = "never NA", collectDate = "never NA", analyte = "never NA",
  analyte_label = "never NA",
  value = "NA only if all replicates were non-numeric",
  units = "never NA after canonicalization", n_reps = "never NA",
  value_sd = "NA when n_reps == 1", below_detection = "never NA",
  implausible_extreme = "never NA", lab_flag = if (has_unit_contract) {
    "NA when every replicate is unflagged"
  } else {
    "NA when unflagged"
  },
  source = "never NA", product = "never NA"
)
long_codebook <- codebook[codebook$section == "tidy_long_export", , drop = FALSE]
need(identical(long_codebook$name, long_export_keep),
     "Codebook tidy-export column roster or order changed.")
need(identical(long_codebook$type, unname(long_types[long_export_keep])) &&
       identical(long_codebook$units, unname(long_units[long_export_keep])),
     "Codebook tidy-export types or units changed.")
need(identical(long_codebook$allowed,
               unname(long_allowed[long_export_keep])) &&
       identical(long_codebook$definition,
                 unname(long_definition[long_export_keep])) &&
       identical(long_codebook$na_semantics,
                 unname(long_na_semantics[long_export_keep])),
     paste0(
       "Codebook tidy-export allowed values, definitions, or NA semantics ",
       "differ from the reviewed contract."
     ))

analyte_codebook <- codebook[
  codebook$section == "analyte_dictionary", , drop = FALSE
]
need(nrow(analyte_codebook) == length(candidate_analytes) &&
       identical(sort(analyte_codebook$name), candidate_analytes) &&
       all(analyte_codebook$type == "numeric") &&
       all(analyte_codebook$allowed == ">= 0 typical"),
     "Codebook analyte roster or scalar contract changed.")
analyte_codebook <- analyte_codebook[
  match(candidate_analytes, analyte_codebook$name), , drop = FALSE
]
expected_codebook_units <- ifelse(
  is.na(runtime_units) | !nzchar(runtime_units), "NA", runtime_units
)
expected_codebook_definition <- sprintf(
  "Analyte '%s': %d obs across %d sites; canonical unit shown",
  candidate_analytes, runtime_n, runtime_n_sites
)
meta_pct_below <- ifelse(
  is.finite(runtime_n) & runtime_n > 0L,
  round(runtime_n_below / runtime_n, 4L), NA_real_
)
expected_codebook_na <- sprintf(
  "%s below detection (kept, not substituted)",
  ifelse(
    is.na(meta_pct_below), "0%", paste0(round(100 * meta_pct_below), "%")
  )
)
need(identical(analyte_codebook$units,
               unname(expected_codebook_units)),
     "Codebook analyte units disagree with the candidate bundle.")
need(identical(analyte_codebook$definition,
               unname(expected_codebook_definition)) &&
       identical(analyte_codebook$na_semantics,
                 unname(expected_codebook_na)),
     "Codebook analyte counts disagree with the candidate bundle.")

if (!requireNamespace("jsonlite", quietly = TRUE))
  fail("jsonlite is required to verify manifest.json.")
manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
manifest_files <- names(manifest$files)
expected_manifest_files <- sort(setdiff(required_paths, "manifest.json"))
missing_manifest_files <- setdiff(expected_manifest_files, manifest_files)
unexpected_manifest_files <- setdiff(manifest_files, expected_manifest_files)
need(!length(missing_manifest_files) && !length(unexpected_manifest_files),
     paste0(
       "Manifest runtime file allowlist changed (missing: %s; ",
       "unexpected: %s)."
     ),
     show_set(missing_manifest_files), show_set(unexpected_manifest_files))
for (path in manifest_files) {
  actual <- unname(tools::md5sum(path))
  declared <- manifest$files[[path]]$checksum
  need(identical(actual, declared),
       "Manifest checksum mismatch for %s.", path)
}
packages <- names(manifest$packages)
need(!any(tolower(packages) %in% c("neonutilities", "arrow")),
     "Data-pull packages leaked into the runtime manifest.")
direct_runtime_packages <- c(
  "shiny", "bslib", "bsicons", "dplyr", "tidyr", "readr", "lubridate",
  "plotly", "DT", "ggplot2", "shinycssloaders", "leaflet", "shinyjs",
  "cachem", "digest", "htmltools", "jsonlite", "tibble"
)
missing_runtime_packages <- setdiff(direct_runtime_packages, packages)
need(!length(missing_runtime_packages),
     "Manifest is missing direct runtime package(s): %s.",
     show_set(missing_runtime_packages))
need("users" %in% names(manifest), "Manifest lacks canonical users metadata.")
manifest_text <- paste(readLines("manifest.json", warn = FALSE), collapse = "\n")
need(!grepl("cloud[.]r-project[.]org|cran[.]rstudio[.]com|cran/(?:__linux__/jammy/)?latest",
            manifest_text, perl = TRUE),
     "Manifest contains a moving package repository.")
need(grepl("packagemanager[.]posit[.]co/cran/__linux__/jammy/2026-07-15",
           manifest_text),
     "Manifest does not carry the pinned 2026-07-15 package snapshot.")

lock_path <- file.path("config", "connect-manifest-packages-v1.json")
need(file.exists(lock_path), "Reviewed Connect package-lock fixture is missing.")
connect_lock <- jsonlite::fromJSON(lock_path, simplifyVector = FALSE)
snapshot <- "https://packagemanager.posit.co/cran/__linux__/jammy/2026-07-15"
need(identical(connect_lock$schema_version, 1L) &&
       identical(connect_lock$platform, "4.5.2") &&
       identical(connect_lock$locale, "C") &&
       identical(connect_lock$repository, snapshot) &&
       identical(connect_lock$source_commit,
                 "31b2e921a80aa262741c44f2282c781f394e1a90"),
     "Reviewed Connect package-lock metadata is invalid.")
need(identical(manifest$platform, connect_lock$platform) &&
       identical(manifest$locale, connect_lock$locale),
     "Manifest R platform/locale differs from the reviewed Connect lock.")
need(length(packages) == 103L &&
       identical(sort(packages), sort(names(connect_lock$packages))),
     "Manifest package-name closure differs from the reviewed Connect lock.")
manifest_sources <- vapply(
  manifest$packages, function(record) record$Source, character(1)
)
manifest_repositories <- vapply(
  manifest$packages, function(record) record$Repository, character(1)
)
manifest_remote_types <- vapply(
  manifest$packages,
  function(record) {
    value <- record$description$RemoteType
    if (is.null(value)) "" else value
  },
  character(1)
)
manifest_remote_repositories <- vapply(
  manifest$packages, function(record) record$description$RemoteRepos,
  character(1)
)
need(all(manifest_sources == "CRAN") &&
       all(manifest_repositories == snapshot) &&
       all(manifest_remote_repositories == snapshot) &&
       !any(manifest_remote_types == "url"),
     "Manifest package lock must use only the fixed standard CRAN snapshot.")
need(identical(manifest$packages, connect_lock$packages),
     "Manifest package records differ from the reviewed Connect lock.")

cat(sprintf(
  paste0(
    "Verified water-chemistry candidate: %d observations, %d analytes, ",
    "%d sites, through %s; unit policy %s ",
    paste0(
      "(%s missing-label rewrites, %s excluded source rows, ",
      "0 value changes; bundle SHA-256 %s).\n"
    )
  ),
  nrow(bundle$swc_long), length(unique(bundle$swc_long$analyte)),
  length(site_roster), bundle$built$data_through,
  if (has_unit_contract) bundle$built$unit_policy else "legacy-not-required",
  if (has_unit_contract) bundle$built$n_unit_labels_rewritten else "n/a",
  if (has_unit_contract) bundle$built$n_unit_rows_excluded else "n/a",
  candidate_bundle_sha256
))
