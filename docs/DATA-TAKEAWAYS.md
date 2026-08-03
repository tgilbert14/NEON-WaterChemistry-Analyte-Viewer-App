# NEON Surface Water Chemistry — Analyte Viewer — Data Takeaways & Critical Review
_Historical production-baseline audit — June 2026. NEON DP1.20093.001 (Surface Water Chemistry)._

## 2026-08-03 remediation status

This file preserves the measurements and findings from the June production
baseline. Where an old recommendation below conflicts with this section, this
section is authoritative for the release candidate:

- The shared producer/runtime unit policy pins 34 presentation targets. The 31
  external-lab analytes are guarded by row-level source labels; the three field
  analytes use explicit fixed extraction units because their source table has no
  per-row unit labels. Numeric values are never silently converted.
- Signed full-fetch replay run 30852990426 expanded the count-bounded quarantine
  from the original 48 identities to 73: 17 additional WALK-2019
  alternate-label identities and eight additional EcoCore_CSU TPC/TPN
  `milligram` identities. The latter remain unresolved legacy anomalies: the
  current [revision-H guide](https://data.neonscience.org/api/v0/documents/NEON_waterChem_userGuide_vH)
  supersedes F.1, while the [official product change log](https://data.neonscience.org/api/v0/products/DP1.20093.001)
  says EcoCore particulate C/N data were converted to `microgramsPerLiter`.
  Policy v4 infers no conversion and changes no numeric value. The replay's 73
  identities represent 75 source rows; the deployed legacy bundle still contains
  only the original 48 collapsed groups (99 represented source rows), which its
  runtime filter removes from the app/index.
- The tracked static `data/codebook.csv` is regenerated from the fail-closed
  effective runtime view. Both build paths refresh it, and the independent
  verifier reconciles its version, provenance, complete text contract, roster,
  units, counts, and below-detection metadata to the bundle.
- The app now has a site-aware plausibility gate/audit surface, >25% BDL
  down-weighting, and legacy/format-change export disclosure. Older “missing”
  findings below are retained as historical review evidence and marked resolved.

## What the data actually shows
- **Big, dense, real bundle.** `data/neon_swc.rds` holds **196,856 long-format observations** across **34 aquatic sites** and **34 analytes**, spanning **2012-01-31 → 2026-05-21** (15 calendar years), built 2026-06-13, `partial = FALSE`. The wide matrix (`swc_wide`) is **7,159 site×date rows** with a 19.1% overall cell NA-rate — i.e. a real but workable sparse analyte matrix. (`D$built`, `swc_long`, `swc_wide`.)
- **The flagship default verifies.** At **SYCA (Sycamore Creek, AZ)**, `specificConductanceField` vs `ANC` over the full record gives **Pearson r = 0.856, Spearman ρ = 0.873, n = 54 paired** — exactly the README's "r ≈ 0.86" claim. This is a genuine carbonate-terrain ionic-strength↔buffering link, not a cherry-pick.
- **Sampling is DENSE, not small-n.** Median **~21 site-visits/site/year** (max 30); at SYCA the main analyte pairs with **n = 145 (median)** across 33 other analytes — **0 of 33 fall below the n≥8 reliability floor**. ANC, the thinnest headline analyte, still has **31–117 obs/site (median 78), 0 sites under 30**. This is a fundamentally different power regime from the terrestrial cascade rungs (n=6 site-years).
- **Below-detection is real and heavily concentrated.** Overall censored rate **7.9% (15,553 / 196,856)**, but it is **wildly analyte-specific**: **Br 56.5%**, **Mn 38.8%**, **Fe 36.2%**, **F 33.9%**, **Ortho-P 26.0%**, **NH4-N 23.5%**, **NO2-N 23.3%**. Conductance/temp/DO/ANC are 0% censored. The flag is carried (`belowDetection`) but the value stored is the reported number, not an imputed half-DL.
- **Replicate handling is honest and disclosed.** **12.1% of rows (23,794)** are replicate means (`n_reps` 2–12; 173,062 are single grabs); pre-collapse spread is preserved in `value_sd` (non-NA exactly on the 12.1% multi-rep rows). One row per `site×collectDate×analyte` is enforced by `stopifnot(!anyDuplicated(...))`.
- **Units are inconsistent at the metadata layer.** **20 of 34 analytes carry >1 distinct unit string.** Two failure modes: (a) **`NA` mixed with a real unit** — `UV Absorbance (254 nm)` is **4,677 rows `NA` + 1,786 "absorbance units"**, and because `analyte_meta`/`analyte_coverage.csv` takes `dplyr::first(units)`, the **published unit for UV254 is literally `NA`**; (b) **mg/L mixed with µg/L** (NH4-N, NO2-N, NO3+NO2-N, Ortho-P, TDP, TP) and µg/L vs "milligram" (TPC, TPN).
- **The 36 collapsed WALK concentration-label groups across six analytes are label defects, not converted values.** TP "µg/L" rows have **median 0.057** vs mg/L **median 0.025** — a true µg/L total-P would be ~25–57, so these groups contain mg/L-magnitude values wearing a µg/L label. This conclusion does **not** extend to the separately quarantined TPC/TPN anomalies.
- **Historical June finding—resolved in the app gate.** `ANC` max = **927 meq/L at CARI (2017-09-25)** while that site's ANC median is **0.73** and the next-highest value is **16.96**; `Fe` max is **931 mg/L** (median 0.011). These values remain auditable but the current site-aware plausibility gate keeps them out of fits, STL, glm, and map summaries, while retaining plausible saline PRPO conductance.
- **Provenance flags are rich and exported.** 30.8% of rows carry a `labFlag`; dominant flags are **`formatChange` (60,373 rows)** and **`legacyData` (14,217)** — i.e. ~30% of the record predates a NEON method/format standardization, plus rare quality flags (`ccvAboveCriteria`, `likelySampleContamination`, `undiluted sample surpassed LDR`). The long-CSV export carries `lab_flag`, so an analyst CAN filter them downstream.

## How it's built
**Source → bundle → app.** `scripts/precompute_neon_data.R` pulls the basic-package CSVs (`swc_externalLabDataByAnalyte` + `swc_fieldSuperParent`) per site-month directly from the NEON public API (resumable, disk-cached, newest-first), for 34 hard-coded sites / 3,936 site-months. Both entry points hand off to the **single shared builder** `scripts/build_swc_bundle.R::build_swc_bundle()` (no divergent tidy logic), which: parses below-detection from NEON's string codes (`"ND"/"BDL"/"BD"/"1"`), stacks lab + field-probe rows, **collapses replicates to one row per site×date×analyte keeping `n_reps` + `value_sd` + a real `any(below)` flag**, builds `swc_wide`, derives metadata, and regenerates the versioned codebook. A `validate_bundle()` contract assertion stops schema/unit drift; `save_bundle()` uses verified backup/promotion recovery. The app and search-index builder apply the same runtime unit boundary before any display or statistic — no runtime NEON calls.

**Metric definitions the app renders:** Compare (z-normalized or dual-axis time series, open markers = below detection); Relationship (OLS on date-paired samples → R²/adj-R²/slope±SE/p + lag-1 residual ACF flag, via `fit_lm()`); Correlations (`correlation_table()` — Spearman default + Pearson, paired-n per row, n≥8 reliability gate, multiple-comparisons caveat); Seasonal (real `stats::stl()` on monthly means, interior gaps linearly interpolated with the fill count stamped on-chart, hard-blocked under 45% real or gap >12 months); Predictor (`glm` on the 3 best-Pearson-correlated analytes, circular field/lab "twin" + gravimetric TSS excluded, repeated 10-fold CV RMSE vs a mean-only baseline via `kfold_rmse()`, `set.seed(42)` → reproducible); Data (wide table + tidy-long CSV, wide CSV, data dictionary, one-click PDF report — all stamped with a provenance header line).

## Critical findings by lens

### NEONize (suite cohesion / honest machinery)
- **[low] Strong honest-stats parity.** n-on-everything, Spearman default, n≥8 gate, multiple-comparisons caveat, real STL with disclosed interpolation count, CV-RMSE vs null baseline, repeated-measures p-value ACF flag, provenance header on every export. This app is at or above suite gold standard for honesty chrome — **keep it as the template** for the other apps. → no fix, propagate the pattern.
- **[med, historical—implemented] `partial`/legacy disclosure was absent from exports.** The current provenance header reports the legacy/format-change share and the long export retains `lab_flag`.

### Ecological (Aquatics domain)
- **[high, historical—implemented] Plausibility QC was absent.** The current site-aware gate excludes flagged singletons from estimators/maps and exposes an audit surface without deleting source values.
- **[med, historical—implemented] Heavy censoring needed clearer handling.** Current correlations surface per-analyte BDL share and grey/down-weight analytes above 25%; values remain preserved and results stay exploratory.
- **[low, historical—candidate fixed] UV254/UV280 exported `NA` units.** The explicit target is now `absorbance units`; the regenerated codebook verifier rejects recurrence.

### Data science (Quinn — analysis-ready / FAIR)
- **[high, candidate fixed] Unit metadata was not analysis-ready.** The reviewed contract now pins 34 targets, fills only registered missing labels, quarantines exact audited non-missing mismatches with identity/count/SHA-256 receipts, and never guesses a conversion. TPC/TPN residual labels remain unresolved anomalies pending source reconciliation.
- **[med, candidate fixed] The shipped codebook was stale.** Both build paths now emit it and the independent verifier reconciles its schema/version, 34-analyte roster, units, counts, and provenance to the exact bundle.
- **[low] Reproducibility is good.** Single-builder contract + `stopifnot` grain check + `set.seed(42)` CV + committed `.rds` + provenance header = re-derivable. Keep it.

### Statistics
- **[low] Small-n honesty is genuinely satisfied here** — unlike the cascade rungs, within-site n is 100+; the n≥8 gate almost never bites at full record. Don't import the suite's "pooling is mandatory" reflex into this app uncritically; the per-site verdict IS defensible when n is in the hundreds. → Keep per-site, but make the n≥8 grey-out dynamic to censored-n, not just paired-n.
- **[med] Multiple comparisons in the correlation screen are caveated but not corrected.** Screening 33 analytes × 34 sites is a large family; the caveat text is honest but no FDR adjustment is offered. → Offer an optional Benjamini-Hochberg `q` column.
- **[med] Repeated-measures non-independence is flagged (lag-1 ACF) but the reported p-value is still the naive OLS p.** For high-ACF pairs the displayed p is materially optimistic. → When |ACF| ≥ 0.5, additionally report an effective-n or block-bootstrap p, not just a warning string.

## Honest-stats & caveats — what this app must NOT be read to claim
- **Not causal, not a sensor.** Correlations/regressions are co-sampled associations; both analytes routinely track a third driver (discharge, season). The glm predictor is explicitly an interpolation aid with optimistic CV RMSE (predictors chosen on the full record), not a calibrated estimate.
- **Censored-heavy analytes are exploratory only.** Any result on Br/Mn/Fe/F/Ortho-P/NH4-N/NO2-N (25–57% below DL) is dominated by detection-limit ties — hypothesis-generating, never a reported effect size.
- **STL is descriptive of the measured record, not a forecast,** and on gappy analytes the interpolated months can smooth away real variability (disclosed on-chart).
- **Do not treat the release candidate as production until exact-head review/CI and data-candidate review complete.** The candidate unit boundary is fail closed; derived unit-bearing quantities still require their own scientific review.
- **Extreme singletons remain source-auditable but are estimator-gated.** Any quoted trend should disclose the plausibility rule and review the excluded-row audit.

## Place in the cascade
This app is **climate's downstream water-chemistry fingerprint**, not a trophic rung — it sits *beside* the climate driver, not between plants and consumers. Its honest role in the integrator's bottom-up `climate → plants → consumers` story:
- **Corroborates the climate/aridity axis.** Specific conductance is a clean dissolved-load integrator: the cross-year median at the suite scale tracks wet/dry cycling (median µS/cm rose then fell 2013→2019 then stabilized), and saline endmembers (PRPO prairie potholes, 7,924 µS/cm) vs dilute montane streams (COMO, WLOU) map the same aridity gradient the desert seasonal-split story rests on. It can **anchor the precip side** of the biome-conditional `precip → greenup` prior in warm deserts.
- **Aligns with the suite's "fix the input upstream, not the stats" doctrine.** The machinery here (permutation-free but n-honest, STL valid, CV valid) is sound; the leverage is upstream — **unit canonicalization, below-detection handling, and outlier gating** — exactly mirroring the cascade lesson that desert "weak ecology" was a *method artifact* (annual aggregation), not bad statistics.
- **Offers a rare well-powered rung for method-checking.** With n in the hundreds per site, this app is the suite's best place to *demonstrate* that the honest-stats chrome behaves correctly when power is NOT the limiting factor — a useful control against the n=6 false-negative regime that haunts the terrestrial rungs.
- **Does NOT feed the lag-aware cascade directly:** sub-annual grab chemistry has no defensible lag link to greenup/consumer rungs; treat it as **descriptive climate corroboration**, the same tier the suite assigns to breeding birds.
