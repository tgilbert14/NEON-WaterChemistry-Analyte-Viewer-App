# Fail-closed unit policy for the surface-water-chemistry producer and validator.
# Numeric values are never converted by this policy. Missing labels may be filled
# only for explicit established analytes. Non-missing unit mismatches are never
# relabelled: a small audited legacy identity set is quarantined, and every other
# mismatch stops the build for scientific review.

WATER_UNIT_POLICY <-
  "explicit-targets-audited-exclusions-value-quarantine-value-invariant-v5"
# A bundle produced under v4 carries the v4 unit receipt but no value-quarantine
# receipt. It remains readable because the runtime boundary below removes the
# quarantined identities from it before any app, index, or export use. A v5
# producer bundle must carry the value-quarantine receipt; a v4 bundle must not.
WATER_UNIT_PRIOR_POLICY <-
  "explicit-targets-audited-exclusions-value-invariant-v4"
WATER_UNIT_ACCEPTED_PRODUCER_POLICIES <- c(
  WATER_UNIT_POLICY, WATER_UNIT_PRIOR_POLICY
)
WATER_MISSING_UNIT <- "<MISSING>"
WATER_EXCLUDED_UNIT <- "<EXCLUDED>"
WATER_LEGACY_BUNDLE_SHA256 <-
  "dce312128e4392aaecdba535bc92be5d9ae8f1dc0e938745b530bc2a4f7f0868"

# v4 admits only the 25 exact identities (26 source rows) exposed by the
# failed-safe full fetch below. The signed replay was independently rehashed
# and deterministically reproduced before these count-bounded quarantines were
# added; numeric values remain untouched and the rows remain excluded.
WATER_UNIT_POLICY_V4_EVIDENCE <- list(
  workflow_run = "30852990426",
  source_sha = "60aca33c36ced33e6f381ee58a551e379acdaf26",
  review_schema = "water-refresh-review-v1",
  prior_unit_policy =
    "explicit-targets-audited-exclusions-value-invariant-v3",
  refresh_review_receipt_sha256 =
    "0f533784f24408a938324bfb0b521e11f1b023ee59527ab1c15e0f6318c8102c",
  unit_review_sha256 =
    "c18b00c39f74dae90ae7253dd2371482362b5ce354169826c50dda4281202485",
  n_new_identities = 25L,
  n_new_source_rows = 26L
)

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

# Missing labels are metadata omissions, not alternate unit claims. The signed
# v4 replay binds every eligible missing-label row to EcoCore_CSU. Fill only
# these exact analyte/target pairs when that source provenance is exact and a
# row carrying the target label is present anywhere in the same build. The
# target-support row is deliberately global; it need not come from EcoCore_CSU.
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
  required_laboratory = rep("EcoCore_CSU", 12L),
  stringsAsFactors = FALSE
)

# Non-missing mismatches are excluded, never relabelled. Six concentration
# pairs are the audited WALK-2019 label defect. All 53 of those exact identities
# in the signed v4 replay carry Florida International University provenance, so
# a missing or different laboratory fails closed. The residual TPC/TPN
# `milligram` identities conflict with NEON's current product change log, which
# says EcoCore particulate C/N was converted to microgramsPerLiter. They are
# therefore unresolved legacy unit anomalies: quarantine them conservatively
# until their source history is reconciled rather than guessing at a conversion.
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
  required_laboratory = c(rep("Florida International University", 6L),
                          "EcoCore_CSU", "EcoCore_CSU"),
  stringsAsFactors = FALSE
)

.water_exclusion_identity_rows <- c(
  "WALK\t2019-06-18\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-01\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-22\tNH4 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-08-13\tNH4 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-09-04\tNH4 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-09-16\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-10-07\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-11-04\tNH4 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-12-02\tNH4 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-06-18\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-07-01\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-07-22\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-08-13\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-09-04\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-09-16\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-10-07\tNO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-11-04\tNO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tNO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-06-18\tNO3+NO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-07-01\tNO3+NO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-22\tNO3+NO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-08-13\tNO3+NO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-09-04\tNO3+NO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-09-16\tNO3+NO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-10-07\tNO3+NO2 - N\tmicrogramsPerLiter\t2",
  "WALK\t2019-11-04\tNO3+NO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-12-02\tNO3+NO2 - N\tmicrogramsPerLiter\t1",
  "WALK\t2019-06-18\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-01\tOrtho - P\tmicrogramsPerLiter\t2",
  # Added for the v5 producer (2026-10-26): a Florida International University
  # non-detect with a blank concentration ("ND"), the same audited WALK-2019
  # microgramsPerLiter label defect. The v4 producer dropped blank values, so
  # this identity first reached the unit review in full-fetch run 40351031862
  # (12180 mismatch identities, this one the only one needing review). The
  # EcoCore milligramsPerLiter replicate for the same date (0.036) is kept.
  "WALK\t2019-07-22\tOrtho - P\tmicrogramsPerLiter\t1",
  "WALK\t2019-08-13\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-09-04\tOrtho - P\tmicrogramsPerLiter\t3",
  "WALK\t2019-09-16\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-10-07\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-11-04\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tOrtho - P\tmicrogramsPerLiter\t2",
  "WALK\t2019-06-18\tTDP\tmicrogramsPerLiter\t1",
  "WALK\t2019-07-01\tTDP\tmicrogramsPerLiter\t1",
  "WALK\t2019-07-22\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-08-13\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-09-04\tTDP\tmicrogramsPerLiter\t1",
  "WALK\t2019-09-16\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-10-07\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-11-04\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tTDP\tmicrogramsPerLiter\t2",
  "WALK\t2019-06-18\tTP\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-01\tTP\tmicrogramsPerLiter\t2",
  "WALK\t2019-07-22\tTP\tmicrogramsPerLiter\t1",
  "WALK\t2019-08-13\tTP\tmicrogramsPerLiter\t2",
  "WALK\t2019-09-04\tTP\tmicrogramsPerLiter\t4",
  "WALK\t2019-09-16\tTP\tmicrogramsPerLiter\t1",
  "WALK\t2019-10-07\tTP\tmicrogramsPerLiter\t1",
  "WALK\t2019-11-04\tTP\tmicrogramsPerLiter\t2",
  "WALK\t2019-12-02\tTP\tmicrogramsPerLiter\t2",
  "CRAM\t2017-05-02\tTPC\tmilligram\t1",
  "CRAM\t2017-08-29\tTPC\tmilligram\t12",
  "CUPE\t2017-07-11\tTPC\tmilligram\t2",
  "LECO\t2017-11-27\tTPC\tmilligram\t1",
  "LEWI\t2017-12-13\tTPC\tmilligram\t2",
  "LIRO\t2017-05-01\tTPC\tmilligram\t1",
  "OKSR\t2017-05-28\tTPC\tmilligram\t2",
  "REDB\t2017-04-18\tTPC\tmilligram\t2",
  "REDB\t2017-04-25\tTPC\tmilligram\t2",
  "WALK\t2017-11-20\tTPC\tmilligram\t1",
  "CRAM\t2017-05-02\tTPN\tmilligram\t1",
  "CRAM\t2017-08-29\tTPN\tmilligram\t2",
  "CUPE\t2017-07-11\tTPN\tmilligram\t1",
  "LECO\t2017-11-27\tTPN\tmilligram\t1",
  "LEWI\t2017-12-13\tTPN\tmilligram\t1",
  "LIRO\t2017-05-01\tTPN\tmilligram\t1",
  "OKSR\t2017-05-28\tTPN\tmilligram\t2",
  "REDB\t2017-04-18\tTPN\tmilligram\t1",
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

# ---- Identity-keyed value quarantine (v5) ------------------------------------
# NEON's DP1.20093.001 change log (2026-04-21) states that iron and manganese
# analysed by USGS_NWQA at ARIK, BARC, PRLA, PRPO, REDB and SUGG from
# 2012-01-31 through 2016-06-19 are in microgramsPerLiter but carry the label
# milligramsPerLiter; NEON will republish corrected values in RELEASE-2027.
# The label already equals the established target, so the unit-mismatch rules
# above cannot see these rows. They are excluded with a receipt and are NEVER
# rescaled: no value is divided by 1000. The quarantine fails closed when the
# laboratory receipt is wrong, when an unlisted identity appears in the window,
# when a source-row count grows, when a site/analyte median falls below 1 mg/L
# (NEON has already corrected the values, so the quarantine would drop good
# data), or when a matched row is tagged RELEASE-2027 or later.
.water_quarantine_sites <- c("ARIK", "BARC", "PRLA", "PRPO", "REDB", "SUGG")
WATER_VALUE_QUARANTINE_RULES <- data.frame(
  site = rep(.water_quarantine_sites, times = 2L),
  analyte = rep(c("Fe", "Mn"), each = length(.water_quarantine_sites)),
  unit = "milligramsPerLiter",
  date_start = "2012-01-31",
  date_end = "2016-06-19",
  required_laboratory = "USGS_NWQA",
  reason = "neon-changelog-2026-04-21-usgs-nwqa-ugL-labelled-mgL",
  min_group_median_value = 1,
  stringsAsFactors = FALSE
)
rm(.water_quarantine_sites)
# Every genuine mislabelled value is a microgram count read as mg/L; the
# smallest committed one is PRLA 2014-06-19 Mn at 0.897. A per-row floor of
# 0.1 mg/L is an extra guard against a partially corrected group whose median
# still clears 1 mg/L.
WATER_VALUE_QUARANTINE_MIN_ROW_VALUE <- 0.1
# The first release in which NEON says corrected values will appear.
WATER_VALUE_QUARANTINE_CORRECTED_RELEASE_YEAR <- 2027L

# The exact 102 collapsed identities (110 source rows) in the committed
# 2026-08-04 bundle. site, collectDate, analyte, unit, max_source_rows.
.water_quarantine_identity_rows <- c(
  "ARIK\t2012-01-31\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2012-03-07\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2012-03-14\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2012-03-26\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2012-12-04\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-01-03\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-01-29\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-02-12\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-02-26\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-03-12\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-07-09\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-07-23\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-08-06\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-10-22\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-11-05\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-11-19\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-12-17\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2013-12-31\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-01-14\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-01-28\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-02-11\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-02-25\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-03-11\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-03-25\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-04-08\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-04-22\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-05-06\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-05-20\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-06-03\tFe\tmilligramsPerLiter\t1",
  "ARIK\t2014-06-17\tFe\tmilligramsPerLiter\t1",
  "BARC\t2014-01-15\tFe\tmilligramsPerLiter\t1",
  "BARC\t2014-02-11\tFe\tmilligramsPerLiter\t1",
  "BARC\t2014-03-11\tFe\tmilligramsPerLiter\t1",
  "BARC\t2014-04-15\tFe\tmilligramsPerLiter\t1",
  "BARC\t2014-05-13\tFe\tmilligramsPerLiter\t1",
  "BARC\t2014-06-03\tFe\tmilligramsPerLiter\t3",
  "PRLA\t2014-05-14\tFe\tmilligramsPerLiter\t1",
  "PRLA\t2014-06-19\tFe\tmilligramsPerLiter\t1",
  "PRPO\t2014-05-13\tFe\tmilligramsPerLiter\t1",
  "PRPO\t2014-06-17\tFe\tmilligramsPerLiter\t1",
  "REDB\t2012-02-03\tFe\tmilligramsPerLiter\t1",
  "REDB\t2012-02-16\tFe\tmilligramsPerLiter\t1",
  "REDB\t2012-02-23\tFe\tmilligramsPerLiter\t1",
  "REDB\t2012-03-06\tFe\tmilligramsPerLiter\t1",
  "REDB\t2012-03-13\tFe\tmilligramsPerLiter\t1",
  "REDB\t2012-03-30\tFe\tmilligramsPerLiter\t1",
  "SUGG\t2014-01-16\tFe\tmilligramsPerLiter\t1",
  "SUGG\t2014-02-11\tFe\tmilligramsPerLiter\t1",
  "SUGG\t2014-03-11\tFe\tmilligramsPerLiter\t1",
  "SUGG\t2014-05-13\tFe\tmilligramsPerLiter\t1",
  "SUGG\t2014-06-10\tFe\tmilligramsPerLiter\t3",
  "ARIK\t2012-01-31\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2012-03-07\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2012-03-14\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2012-03-26\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2012-12-04\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-01-03\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-01-29\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-02-12\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-02-26\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-03-12\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-07-09\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-07-23\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-08-06\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-10-22\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-11-05\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-11-19\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-12-17\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2013-12-31\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-01-14\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-01-28\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-02-11\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-02-25\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-03-11\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-03-25\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-04-08\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-04-22\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-05-06\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-05-20\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-06-03\tMn\tmilligramsPerLiter\t1",
  "ARIK\t2014-06-17\tMn\tmilligramsPerLiter\t1",
  "BARC\t2014-01-15\tMn\tmilligramsPerLiter\t1",
  "BARC\t2014-02-11\tMn\tmilligramsPerLiter\t1",
  "BARC\t2014-03-11\tMn\tmilligramsPerLiter\t1",
  "BARC\t2014-04-15\tMn\tmilligramsPerLiter\t1",
  "BARC\t2014-05-13\tMn\tmilligramsPerLiter\t1",
  "BARC\t2014-06-03\tMn\tmilligramsPerLiter\t3",
  "PRLA\t2014-05-14\tMn\tmilligramsPerLiter\t1",
  "PRLA\t2014-06-19\tMn\tmilligramsPerLiter\t1",
  "PRPO\t2014-05-13\tMn\tmilligramsPerLiter\t1",
  "PRPO\t2014-06-17\tMn\tmilligramsPerLiter\t1",
  "REDB\t2012-02-03\tMn\tmilligramsPerLiter\t1",
  "REDB\t2012-02-16\tMn\tmilligramsPerLiter\t1",
  "REDB\t2012-02-23\tMn\tmilligramsPerLiter\t1",
  "REDB\t2012-03-06\tMn\tmilligramsPerLiter\t1",
  "REDB\t2012-03-13\tMn\tmilligramsPerLiter\t1",
  "REDB\t2012-03-30\tMn\tmilligramsPerLiter\t1",
  "SUGG\t2014-01-16\tMn\tmilligramsPerLiter\t1",
  "SUGG\t2014-02-11\tMn\tmilligramsPerLiter\t1",
  "SUGG\t2014-03-11\tMn\tmilligramsPerLiter\t1",
  "SUGG\t2014-05-13\tMn\tmilligramsPerLiter\t1",
  "SUGG\t2014-06-10\tMn\tmilligramsPerLiter\t3"
)
.water_quarantine_parts <- strsplit(.water_quarantine_identity_rows, "\t",
                                    fixed = TRUE)
WATER_VALUE_QUARANTINE_IDENTITIES <- data.frame(
  site = vapply(.water_quarantine_parts, `[[`, character(1), 1L),
  collectDate = vapply(.water_quarantine_parts, `[[`, character(1), 2L),
  analyte = vapply(.water_quarantine_parts, `[[`, character(1), 3L),
  unit = vapply(.water_quarantine_parts, `[[`, character(1), 4L),
  max_source_rows = as.integer(vapply(.water_quarantine_parts, `[[`,
                                      character(1), 5L)),
  stringsAsFactors = FALSE
)
rm(.water_quarantine_parts, .water_quarantine_identity_rows)

# Separate from WATER_UNIT_RECEIPT_FIELDS so a v4 producer bundle (which has the
# unit receipt and no quarantine receipt) stays distinguishable from v5.
WATER_VALUE_QUARANTINE_RECEIPT_FIELDS <- c(
  "n_value_rows_quarantined", "value_quarantine_receipt",
  "value_quarantine_receipt_sha256"
)
WATER_VALUE_QUARANTINE_RECEIPT_COLUMNS <- c(
  "site", "collectDate", "analyte", "unit", "reason", "max_source_rows",
  "n_excluded"
)

# Identities the v5 producer added to the audited list. A v4 producer bundle's
# own receipt was written before them, so its receipt is checked against the
# list without them; the runtime canonicalizer always uses the full list.
WATER_UNIT_V5_ADDED_IDENTITY_KEYS <- c(
  "WALK\r2019-07-22\rOrtho - P\rmicrogramsPerLiter"
)
water_unit_identities_for_policy <- function(policy) {
  ids <- WATER_UNIT_EXCLUSION_IDENTITIES
  if (identical(policy, WATER_UNIT_PRIOR_POLICY)) {
    keys <- paste(ids$site, ids$collectDate, ids$analyte, ids$from_unit, sep = "\r")
    ids <- ids[!keys %in% WATER_UNIT_V5_ADDED_IDENTITY_KEYS, , drop = FALSE]
    rownames(ids) <- NULL
  }
  ids
}

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

# Index of the value-quarantine rule whose site, analyte, unit label, and
# inclusive collection-date window cover each row (NA when none does).
water_value_quarantine_rule_index <- function(site, collectDate, analyte,
                                              unit) {
  rules <- WATER_VALUE_QUARANTINE_RULES
  date <- suppressWarnings(as.Date(substr(as.character(collectDate), 1L, 10L)))
  rule_index <- match(
    water_unit_rule_key(paste(site, analyte, sep = "\r"), unit),
    water_unit_rule_key(paste(rules$site, rules$analyte, sep = "\r"),
                        rules$unit)
  )
  in_window <- !is.na(rule_index) & !is.na(date) &
    date >= as.Date(rules$date_start[rule_index]) &
    date <= as.Date(rules$date_end[rule_index])
  rule_index[!in_window] <- NA_integer_
  rule_index
}

# Fail-closed value checks shared by the producer (source rows) and the runtime
# (collapsed rows). Matched values must look like microgram counts read as mg/L:
# every (site, analyte) group median of the finite values must clear the rule
# floor, and no finite value may fall below the per-row floor.
water_value_quarantine_check_values <- function(rule_index, value, context) {
  rules <- WATER_VALUE_QUARANTINE_RULES
  matched <- which(!is.na(rule_index))
  if (!length(matched)) return(invisible(TRUE))
  value <- as.numeric(value)
  groups <- split(matched, rule_index[matched])
  for (g in names(groups)) {
    i <- as.integer(g)
    finite <- value[groups[[g]]][is.finite(value[groups[[g]]])]
    label <- paste(rules$site[[i]], rules$analyte[[i]])
    if (!length(finite)) {
      stop(sprintf(paste0(
        "%s value quarantine cannot verify %s: no finite matched values."
      ), context, label), call. = FALSE)
    }
    if (stats::median(finite) < rules$min_group_median_value[[i]]) {
      stop(sprintf(paste0(
        "%s value quarantine median for %s is below %g mg/L; NEON may have ",
        "corrected these values, so the quarantine requires review."
      ), context, label, rules$min_group_median_value[[i]]), call. = FALSE)
    }
    if (any(finite < WATER_VALUE_QUARANTINE_MIN_ROW_VALUE)) {
      stop(sprintf(paste0(
        "%s value quarantine for %s holds a value below %g mg/L and ",
        "requires review."
      ), context, label, WATER_VALUE_QUARANTINE_MIN_ROW_VALUE), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# 4-digit year of a RELEASE-YYYY tag; NA for PROVISIONAL or a missing tag.
# Any other non-empty tag fails closed.
water_release_year <- function(release) {
  release <- trimws(as.character(release))
  out <- rep(NA_integer_, length(release))
  tagged <- !is.na(release) & nzchar(release) &
    toupper(release) != "PROVISIONAL"
  parsed <- grepl("^RELEASE-[0-9]{4}([^0-9].*)?$", release, ignore.case = TRUE)
  if (any(tagged & !parsed)) {
    stop(sprintf("Unrecognized NEON release tag on a quarantined row: %s",
                 paste(unique(release[tagged & !parsed]), collapse = ", ")),
         call. = FALSE)
  }
  out[tagged] <- as.integer(substr(release[tagged], 9L, 12L))
  out
}

# Producer quarantine on SOURCE rows, before the replicate collapse, so n_reps,
# means, and spreads never include a quarantined replicate. Returns the keep
# mask and a receipt over every policy identity (zero counts included).
apply_water_value_quarantine <- function(site, collectDate, analyte, value,
                                         units, laboratoryName = NULL,
                                         release = NULL) {
  validate_water_unit_policy()
  n <- length(analyte)
  stopifnot(length(site) == n, length(collectDate) == n,
            length(value) == n, length(units) == n)
  if (is.null(laboratoryName)) laboratoryName <- rep(NA_character_, n)
  stopifnot(length(laboratoryName) == n)
  if (!is.null(release)) stopifnot(length(release) == n)

  site <- as.character(site)
  collect_date <- as.character(as.Date(substr(as.character(collectDate), 1L,
                                              10L)))
  analyte <- as.character(analyte)
  source_labels <- water_unit_source_label(units)
  laboratoryName <- as.character(laboratoryName)

  rules <- WATER_VALUE_QUARANTINE_RULES
  identities <- WATER_VALUE_QUARANTINE_IDENTITIES
  rule_index <- water_value_quarantine_rule_index(
    site, collect_date, analyte, source_labels
  )
  matched <- !is.na(rule_index)

  required_lab <- rules$required_laboratory[rule_index]
  if (any(matched & (is.na(laboratoryName) | laboratoryName != required_lab))) {
    stop(paste0(
      "Value-quarantined row lacks its required laboratory provenance ",
      "receipt (USGS_NWQA)."
    ), call. = FALSE)
  }

  identity_keys <- water_unit_identity_key(
    identities$site, identities$collectDate, identities$analyte,
    identities$unit
  )
  row_keys <- water_unit_identity_key(site, collect_date, analyte,
                                      source_labels)
  identity_index <- match(row_keys, identity_keys)
  if (any(matched & is.na(identity_index))) {
    bad <- unique(row_keys[matched & is.na(identity_index)])
    stop(sprintf(
      "Unapproved value-quarantine identity requires review: %s",
      paste(gsub("\r", " ", bad, fixed = TRUE), collapse = ", ")
    ), call. = FALSE)
  }
  counts <- tabulate(identity_index[matched], nbins = nrow(identities))
  too_many <- counts > identities$max_source_rows
  if (any(too_many)) {
    stop(sprintf(
      "Value-quarantine source-row count increased and requires review: %s",
      paste(gsub("\r", " ", identity_keys[too_many], fixed = TRUE),
            collapse = ", ")
    ), call. = FALSE)
  }

  water_value_quarantine_check_values(rule_index, value, "Producer")

  if (!is.null(release) && any(matched)) {
    years <- water_release_year(release[matched])
    # A matched row with no release tag cannot prove it predates RELEASE-2027,
    # so it fails closed too (a cache month fetched before release capture:
    # delete its <site>_<month>_release.txt sibling and re-pull that month).
    if (anyNA(years) && any(is.na(years) &
                            !toupper(trimws(as.character(release[matched]))) %in%
                              "PROVISIONAL")) {
      stop(paste0(
        "Value-quarantined rows carry no NEON release tag; the RELEASE-2027 ",
        "check cannot run, so the quarantine requires review."
      ), call. = FALSE)
    }
    if (any(!is.na(years) &
            years >= WATER_VALUE_QUARANTINE_CORRECTED_RELEASE_YEAR)) {
      stop(sprintf(paste0(
        "Value-quarantined rows carry %s or later, the release NEON named ",
        "for corrected values; retire or revise the quarantine."
      ), paste0("RELEASE-", WATER_VALUE_QUARANTINE_CORRECTED_RELEASE_YEAR)),
      call. = FALSE)
    }
  }

  identity_rule <- water_value_quarantine_rule_index(
    identities$site, identities$collectDate, identities$analyte,
    identities$unit
  )
  receipt <- data.frame(
    site = identities$site,
    collectDate = identities$collectDate,
    analyte = identities$analyte,
    unit = identities$unit,
    reason = rules$reason[identity_rule],
    max_source_rows = identities$max_source_rows,
    n_excluded = as.integer(counts),
    stringsAsFactors = FALSE
  )
  receipt <- receipt[order(receipt$analyte, receipt$site,
                           receipt$collectDate), , drop = FALSE]
  rownames(receipt) <- NULL

  list(
    keep = !matched,
    receipt = receipt,
    receipt_sha256 = water_unit_receipt_sha256(receipt),
    n_rows_quarantined = as.integer(sum(matched))
  )
}

# Problems with a producer bundle's value-quarantine receipt, as a character
# vector (empty when valid). v5 bundles must carry the receipt; bundles from the
# prior v4 producer must not (their runtime view applies the quarantine).
water_value_quarantine_receipt_problems <- function(built) {
  present <- WATER_VALUE_QUARANTINE_RECEIPT_FIELDS %in% names(built)
  policy <- built$unit_policy
  if (identical(policy, WATER_UNIT_PRIOR_POLICY)) {
    if (any(present)) {
      return("a v4 producer bundle carries a value-quarantine receipt")
    }
    return(character())
  }
  if (!identical(policy, WATER_UNIT_POLICY)) {
    return("unexpected unit policy")
  }
  if (!all(present)) {
    return(sprintf("missing value-quarantine receipt field(s): %s",
                   paste(WATER_VALUE_QUARANTINE_RECEIPT_FIELDS[!present],
                         collapse = ", ")))
  }
  receipt <- built$value_quarantine_receipt
  problems <- character()
  if (!is.data.frame(receipt) ||
      !identical(names(receipt), WATER_VALUE_QUARANTINE_RECEIPT_COLUMNS)) {
    return("value-quarantine receipt schema changed")
  }
  ids <- WATER_VALUE_QUARANTINE_IDENTITIES
  expected <- data.frame(
    site = ids$site, collectDate = ids$collectDate, analyte = ids$analyte,
    unit = ids$unit,
    reason = WATER_VALUE_QUARANTINE_RULES$reason[
      water_value_quarantine_rule_index(ids$site, ids$collectDate,
                                        ids$analyte, ids$unit)
    ],
    max_source_rows = ids$max_source_rows,
    stringsAsFactors = FALSE
  )
  expected <- expected[order(expected$analyte, expected$site,
                             expected$collectDate), , drop = FALSE]
  rownames(expected) <- NULL
  observed <- receipt[seq_len(6L)]
  rownames(observed) <- NULL
  if (!identical(observed, expected)) {
    problems <- c(problems, "value-quarantine identities differ from policy")
  }
  n_excluded <- receipt$n_excluded
  if (!is.integer(n_excluded) || anyNA(n_excluded) || any(n_excluded < 0L) ||
      any(n_excluded > receipt$max_source_rows)) {
    problems <- c(problems,
                  "value-quarantine counts violate audited identity bounds")
  }
  total <- built$n_value_rows_quarantined
  if (!is.integer(total) || length(total) != 1L || is.na(total) ||
      !identical(total, as.integer(sum(n_excluded)))) {
    problems <- c(problems,
                  "value-quarantine total disagrees with identity receipt")
  }
  if (!identical(built$value_quarantine_receipt_sha256,
                 water_unit_receipt_sha256(receipt))) {
    problems <- c(problems, "value-quarantine receipt SHA-256 mismatch")
  }
  problems
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

  # Identity-keyed value quarantine. A collapsed bundle carries no laboratory
  # provenance, so the runtime binds each covered row to an exact listed
  # identity and its audited source-row bound instead.
  quarantine_rules <- WATER_VALUE_QUARANTINE_RULES
  quarantine_identities <- WATER_VALUE_QUARANTINE_IDENTITIES
  quarantine_rule <- water_value_quarantine_rule_index(
    swc_long$site, swc_long$collectDate, analyte, source_labels
  )
  quarantined <- !is.na(quarantine_rule)
  quarantine_index <- match(
    row_keys,
    water_unit_identity_key(
      quarantine_identities$site, quarantine_identities$collectDate,
      quarantine_identities$analyte, quarantine_identities$unit
    )
  )
  if (any(quarantined & is.na(quarantine_index))) {
    bad <- unique(row_keys[quarantined & is.na(quarantine_index)])
    stop(sprintf(
      "Unapproved runtime value-quarantine identity requires review: %s",
      paste(gsub("\r", " ", bad, fixed = TRUE), collapse = ", ")
    ), call. = FALSE)
  }
  if (anyDuplicated(row_keys[quarantined])) {
    stop("Runtime value-quarantine identities are not at the collapsed grain.",
         call. = FALSE)
  }
  quarantine_over <- quarantined &
    swc_long$n_reps > quarantine_identities$max_source_rows[quarantine_index]
  if (any(quarantine_over, na.rm = TRUE)) {
    stop(sprintf(
      "Runtime value-quarantine count increased and requires review: %s",
      paste(gsub("\r", " ", unique(row_keys[which(quarantine_over)]),
                 fixed = TRUE), collapse = ", ")
    ), call. = FALSE)
  }
  water_value_quarantine_check_values(quarantine_rule, swc_long$value,
                                      "Runtime")

  excluded_rows <- mismatch | quarantined
  keep <- !excluded_rows
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

  # One receipt in source-row order; `reason` tells a unit-mismatch exclusion
  # from a value-quarantine exclusion.
  mismatch_reason <- WATER_UNIT_EXCLUSION_RULES$reason[match(
    water_unit_rule_key(analyte, source_labels),
    water_unit_rule_key(WATER_UNIT_EXCLUSION_RULES$analyte,
                        WATER_UNIT_EXCLUSION_RULES$from_unit)
  )]
  reason <- ifelse(mismatch, mismatch_reason,
                   quarantine_rules$reason[quarantine_rule])
  excluded <- data.frame(
    site = as.character(swc_long$site[excluded_rows]),
    collectDate = as.character(swc_long$collectDate[excluded_rows]),
    analyte = analyte[excluded_rows],
    from_unit = source_labels[excluded_rows],
    n_source_rows = swc_long$n_reps[excluded_rows],
    reason = as.character(reason[excluded_rows]),
    stringsAsFactors = FALSE
  )
  list(
    data = retained,
    excluded = excluded,
    n_collapsed_rows_excluded = as.integer(nrow(excluded)),
    n_source_rows_excluded = as.integer(sum(excluded$n_source_rows)),
    n_missing_labels_rewritten = as.integer(sum(missing)),
    n_value_quarantine_rows_excluded = as.integer(sum(quarantined)),
    n_value_quarantine_source_rows_excluded =
      as.integer(sum(swc_long$n_reps[quarantined]))
  )
}

validate_water_unit_policy <- function() {
  targets <- WATER_ESTABLISHED_UNIT_TARGETS
  rewrites <- WATER_UNIT_LABEL_REWRITES
  exclusions <- WATER_UNIT_EXCLUSION_RULES
  identities <- WATER_UNIT_EXCLUSION_IDENTITIES
  evidence <- WATER_UNIT_POLICY_V4_EVIDENCE
  stopifnot(
    identical(names(evidence), c(
      "workflow_run", "source_sha", "review_schema", "prior_unit_policy",
      "refresh_review_receipt_sha256", "unit_review_sha256",
      "n_new_identities", "n_new_source_rows"
    )),
    grepl("^[0-9]+$", evidence$workflow_run),
    grepl("^[0-9a-f]{40}$", evidence$source_sha),
    identical(evidence$review_schema, "water-refresh-review-v1"),
    identical(evidence$prior_unit_policy,
              "explicit-targets-audited-exclusions-value-invariant-v3"),
    grepl("^[0-9a-f]{64}$", evidence$refresh_review_receipt_sha256),
    grepl("^[0-9a-f]{64}$", evidence$unit_review_sha256),
    identical(evidence$n_new_identities, 25L),
    identical(evidence$n_new_source_rows, 26L),
    is.character(targets), length(targets) == 34L,
    !is.null(names(targets)), !anyDuplicated(names(targets)),
    all(nzchar(names(targets))), all(nzchar(targets)),
    identical(names(rewrites), c(
      "analyte", "from_unit", "to_unit", "required_laboratory"
    )),
    all(rewrites$from_unit == WATER_MISSING_UNIT),
    identical(rewrites$required_laboratory, rep("EcoCore_CSU", 12L)),
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
    identical(
      exclusions$required_laboratory[
        exclusions$reason == "audited-legacy-mislabeled-concentration"
      ],
      rep("Florida International University", 6L)
    ),
    identical(
      exclusions$required_laboratory[
        exclusions$reason == "unresolved-legacy-particulate-unit-anomaly"
      ],
      rep("EcoCore_CSU", 2L)
    ),
    identical(names(identities), c("site", "collectDate", "analyte",
                                    "from_unit", "max_source_rows")),
    nrow(identities) == 74L,
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

  q_rules <- WATER_VALUE_QUARANTINE_RULES
  q_ids <- WATER_VALUE_QUARANTINE_IDENTITIES
  q_rule_of_id <- water_value_quarantine_rule_index(
    q_ids$site, q_ids$collectDate, q_ids$analyte, q_ids$unit
  )
  stopifnot(
    identical(WATER_UNIT_ACCEPTED_PRODUCER_POLICIES,
              c(WATER_UNIT_POLICY, WATER_UNIT_PRIOR_POLICY)),
    !identical(WATER_UNIT_POLICY, WATER_UNIT_PRIOR_POLICY),
    identical(names(q_rules), c(
      "site", "analyte", "unit", "date_start", "date_end",
      "required_laboratory", "reason", "min_group_median_value"
    )),
    nrow(q_rules) == 12L,
    identical(sort(unique(q_rules$site)),
              c("ARIK", "BARC", "PRLA", "PRPO", "REDB", "SUGG")),
    identical(sort(unique(q_rules$analyte)), c("Fe", "Mn")),
    !anyDuplicated(paste(q_rules$site, q_rules$analyte, sep = "\r")),
    all(q_rules$analyte %in% names(targets)),
    identical(q_rules$unit, unname(targets[q_rules$analyte])),
    all(q_rules$unit == "milligramsPerLiter"),
    all(q_rules$date_start == "2012-01-31"),
    all(q_rules$date_end == "2016-06-19"),
    all(q_rules$required_laboratory == "USGS_NWQA"),
    all(q_rules$reason ==
          "neon-changelog-2026-04-21-usgs-nwqa-ugL-labelled-mgL"),
    is.double(q_rules$min_group_median_value),
    all(q_rules$min_group_median_value == 1),
    identical(WATER_VALUE_QUARANTINE_MIN_ROW_VALUE, 0.1),
    identical(WATER_VALUE_QUARANTINE_CORRECTED_RELEASE_YEAR, 2027L),
    identical(names(q_ids), c("site", "collectDate", "analyte", "unit",
                              "max_source_rows")),
    nrow(q_ids) == 102L,
    !anyDuplicated(water_unit_identity_key(
      q_ids$site, q_ids$collectDate, q_ids$analyte, q_ids$unit
    )),
    identical(as.character(as.Date(q_ids$collectDate)), q_ids$collectDate),
    !anyNA(q_rule_of_id),
    is.integer(q_ids$max_source_rows),
    all(!is.na(q_ids$max_source_rows) & q_ids$max_source_rows > 0L),
    identical(sum(q_ids$max_source_rows), 110L),
    identical(WATER_VALUE_QUARANTINE_RECEIPT_FIELDS, c(
      "n_value_rows_quarantined", "value_quarantine_receipt",
      "value_quarantine_receipt_sha256"
    )),
    !any(WATER_VALUE_QUARANTINE_RECEIPT_FIELDS %in% WATER_UNIT_RECEIPT_FIELDS)
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
    stop("Audited unit anomaly lacks its required laboratory provenance receipt.",
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
  rewrite_required_lab <- rewrite_rules$required_laboratory[rewrite_index]
  provenance_bound_rewrite <- missing & !is.na(rewrite_required_lab)
  if (any(provenance_bound_rewrite &
          (is.na(laboratoryName[keep]) |
             laboratoryName[keep] != rewrite_required_lab))) {
    stop(paste0(
      "Missing-label repair lacks its required laboratory provenance ",
      "receipt."
    ), call. = FALSE)
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
    required_laboratory = rewrite_rules$required_laboratory,
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
