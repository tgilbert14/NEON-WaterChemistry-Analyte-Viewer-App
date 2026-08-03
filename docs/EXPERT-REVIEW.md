# Surface water chemistry (grab samples) — Expert Review by Brooke (NEON DP1.20093.001)
_Historical product-expert review — June 2026, with August 2026 release-candidate addendum._

## 2026-08-03 release-candidate addendum

The June prose and scorecard below are retained as the baseline review, not as a
description of the current candidate. The current state supersedes conflicting
recommendations:

- A shared fail-closed unit policy pins 34 presentation targets. Row-level unit
  transitions are observed for 31 external-lab analytes; the three field-derived
  analytes use explicit fixed extraction units because the field source table has
  no row-level unit labels.
- Modal relabelling is explicitly rejected. Registered missing labels require an
  observed target label; 36 WALK mismatch identities and 12 unresolved legacy
  TPC/TPN identities are quarantined with count/receipt bounds. The exact legacy
  runtime removes all 48 groups (99 represented source rows) before app/index use.
- The TPC/TPN identities are not asserted to be unconverted masses. The current
  [revision-H guide](https://data.neonscience.org/api/v0/documents/NEON_waterChem_userGuide_vH)
  supersedes F.1, and the [product change log](https://data.neonscience.org/api/v0/products/DP1.20093.001)
  says EcoCore particulate C/N was converted to `microgramsPerLiter`; residual
  `milligram` labels remain unresolved anomalies pending source reconciliation.
- The site-aware plausibility gate/audit surface, >25% BDL down-weighting,
  legacy/format-change export disclosure, generated static codebook, and strict
  codebook verifier are implemented. Fresh exact-head CI/review is still pending.

> **Historical June verdict:** I walked this app end-to-end against the NEON grab-sample SOP, the censored-data literature, and what an EPA NARS or USGS reviewer would actually demand — and the verdict is the one I gave the team in my brief: **the statistics chrome here is the suite's gold standard; the geochemistry going INTO it is where the app can still be challenged.** The honesty machinery is genuinely excellent — n on everything, Spearman by default, an n≥8 gate, a real `stats::stl()` (not the 2021 synthetic sine-wave), CV-RMSE against a mean-only baseline, a lag-1 ACF flag, below-detection drawn as open markers, and chemically-correct analyte names (Br = bromide, ANC in meq/L, pH unitless). The upstream gaps identified here drove the implemented controls summarized above. — Brooke

## Method fidelity (is the NEON protocol represented correctly?)

This is where the app is most quietly right, and I want it on the record so it isn't regressed.

- **Both ingest streams are correctly identified and labelled.** The bundle stacks `swc_externalLabDataByAnalyte` (the dissolved-chemistry workhorse) and `swc_fieldSuperParent` (three in-situ field analytes), tagged `source ∈ {External Lab, Field Probe}`. That is the DP1.20093.001 design—a discrete grab split/preserved/shipped to an external lab, co-located with field-probe records.
- **Cadence is described honestly.** The STL note — "NEON grab samples are roughly monthly with gaps" (`app.R:239`) — is precisely correct, and the verified ~21 site-visits/site/year (`docs/DATA-TAKEAWAYS.md:7`) confirms this is a **dozens-to-hundreds per-site regime**, NOT the n=6 terrestrial-cascade regime. The app correctly does NOT import a "pooling is mandatory" reflex; per-site verdicts ARE defensible here because the within-site n is in the hundreds.
- **The field/lab "twins" are handled correctly.** `specificConductanceField` (probe) and `specificConductance` (lab) measure the same quantity by different methods; the predictor's `TWIN` map (`app.R:896-899`) excludes the circular twin before fitting. That is the right call — a USGS reviewer would otherwise flag a model "predicting" lab conductance from field conductance as tautological. Gravimetric `TSS - Dry Mass` is also dropped (`app.R:903`). Good.
- **Below-detection arrives as strings and is parsed to a flag, value preserved.** The builder parses NEON's `"ND"/"BDL"/"BD"/"1"` codes into a `belowDetection` 0/1 flag and **keeps the reported number, not an imputed half-DL** (`docs/DATA-TAKEAWAYS.md:8,16`). That is the single most important method-fidelity decision on this product and the app got it right — substitution would have been the cardinal error (Helsel 2012).
- **Replicate provenance is preserved and disclosed.** 12.1% of rows are replicate means with `n_reps` and pre-collapse `value_sd` retained; the grain is enforced by `stopifnot(!anyDuplicated(...))` (`docs/DATA-TAKEAWAYS.md:9`). The hover even labels a multi-rep point ("(n-rep mean)", `app.R:709`). This is more careful than most published water-chemistry figures.

**Historical method-labelling gap—implemented.** The `formatChange` / `legacyData` flags show that roughly 30% of the June record predates a method/format standardization era. The long export retains `lab_flag`, and the current provenance header discloses the legacy/format-change share. A split-era sensitivity view remains a possible enhancement.

## Analysis & metrics — defensible? (with the literature)

The estimators themselves are sound and, unusually, honestly captioned. Where I push is on the **inputs each estimator silently trusts.**

**What is defensible as-is:**
- **Correlation screen** (`correlation_table()`, `helpers.R:231-264`) — Spearman default is the correct choice for skewed water chemistry; paired-n is computed per row on co-sampled dates; the n≥8 reliability gate and multiple-comparisons caveat are present (`app.R:443`). The flagship SYCA `specificConductanceField` vs `ANC` verifies at Pearson r = 0.856 / Spearman ρ = 0.873, n = 54 (`docs/DATA-TAKEAWAYS.md:6`) — a genuine carbonate-terrain ionic-strength↔buffering link (Hem 1985), not a cherry-pick.
- **Relationship** (`fit_lm()`, `helpers.R:269-283`) — OLS with R²/adj-R²/slope±SE/p **plus a lag-1 residual ACF flag** that correctly warns the repeated-measures p is optimistic (`app.R:777`). That ACF flag is more honesty than most journals enforce.
- **Seasonal** (`app.R:849-892`) — a **real STL** on monthly means, interior gaps linearly interpolated with the fill-count stamped on-chart (`app.R:889`), hard-blocked under 45% real or a gap >12 months (`app.R:862`). This is a true decomposition, disclosed as descriptive-not-forecast. Correct.
- **Predictor** (`pred_base()` + `kfold_rmse()`, `app.R:929`, `helpers.R:286`) — glm on the 3 best-Pearson analytes, twins excluded, repeated 10-fold CV-RMSE vs a mean-only baseline, `set.seed(42)`, labelled an interpolation aid with optimistic CV-RMSE because predictors are chosen on the full record (`app.R:248`). Honest framing.

**June gaps and their current status:**

1. **Censoring disclosure—minimum control implemented.** Below-detection values remain preserved rather than substituted. The current correlation surface reports per-analyte censored share and greys/down-weights analytes above 25% BDL. Heavy-censor results remain exploratory; a censored estimator such as `cenken` is still a future enhancement.

2. **Plausibility/outlier QC—implemented.** The current site-aware gate keeps flagged singletons such as ANC=927 and Fe=931 out of fits, STL, glm, and map summaries while preserving them in an explicit audit surface. Site-aware ceilings retain genuinely saline PRPO observations.

3. **Multiple comparisons caveated but not corrected; high-ACF p warned but not adjusted.** Screening 33 analytes × 34 sites is a large family; the caveat text is honest (`app.R:443`) but no FDR is offered. And when `|lag1_acf| ≥ 0.5` the app prints "the p-value above is optimistic" (`app.R:779`) but still reports the naive OLS p. **Fix:** an optional Benjamini-Hochberg `q` column on the correlation screen, and an effective-n or block-bootstrap p when `|ACF| ≥ 0.5` — not just a warning string.

## What the field would add (collection / analysis / presentation / use)

- **Collection / provenance:** USGS NWQA (National Field Manual, TWRI Book 9) builds field blanks and replicates into the QC chain. NEON's replicate spread is preserved (`value_sd`) but never *shown* as a QC signal. Presenting the replicate CV per analyte would let a user see analytical precision next to the trend — a small, honest add.
- **Analysis — the charge balance the app doesn't compute.** Hem 1985 treats the **anion–cation charge balance** as the first sanity check on a full major-ion analysis (a complete set should balance to ~5–10%). This bundle has Ca/Mg/Na/K and Cl/SO4/HCO3/ANC — the ingredients are present. A charge-balance % per site-date would be a *physically grounded* QC metric and would have caught the ANC=927 artifact automatically (it would balance to absurdity). Strong candidate for a future tab; **but do not ship it until the unit field is canonical** (below), because charge balance is unit-bearing.
- **Analysis — censored estimators.** The honest minimum is the BDL down-weighting above; the field-grade version is Kaplan-Meier summaries / ROS / Tobit means and `cenken` correlations (Helsel 2012) for the seven heavy-censor analytes.
- **Presentation — SUVA remains review-gated, but the metadata defect is fixed in the candidate.** UV254/UV280 now use the explicit `absorbance units` target and the regenerated static codebook is reconciled to the bundle. Do not add a derived SUVA or molar-ratio calculation without a separate scientific contract.
- **Use:** this app's best use in the suite is as a *well-powered control* — see below.

## Product-specific honesty & QC traps

The five traps I told the team I would not let slide, scored against this app:

1. **A non-detect is not a zero or half-DL.** ✅ Value preserved and flag carried; heavy-censor correlations are surfaced and down-weighted, while field-grade censored estimators remain future work.
2. **Units must be explicit before cross-analyte math.** ✅ The historical modal-relabel recommendation is superseded and must not be implemented. The candidate pins all 34 targets, transition-checks the 31 row-labelled lab analytes, records three fixed field extraction assumptions, and quarantines exact audited mismatches. TPC/TPN residual labels are unresolved anomalies, not presumed masses.
3. **Detect and gate physically implausible values before a fit.** ✅ Implemented with a site-aware gate and auditable exclusion surface.
4. **Index/relative vs absolute, named honestly.** ✅ The glm is labelled an interpolation aid with optimistic CV-RMSE (`app.R:248,413`); conductance is correctly framed as a within-site dissolved-load integrator. Keep saying it.
5. **pH and intensive-variable / log-scale traps.** ✅ pH is unitless in `ANALYTE_TBL` and the "Carbonate system (alkalinity ↔ pH)" preset (`helpers.R:140`) is grounded in carbonate equilibria (Stumm & Morgan). One latent caution: the STL and climatology aggregate the *main* analyte by arithmetic monthly mean (`app.R:852`) — fine for conductance/ANC, but **arithmetic-averaging pH is technically improper** (pH is log-scale/intensive). It's a minor, rarely-hit edge (pH is seldom the seasonal main analyte), but worth a note so no one quotes a "mean pH" trend as exact.

**A genuine reproducibility strength to protect:** single shared builder, grain check, seeded CV, committed bundle, and export provenance. The former FAIR gap is closed in the candidate: a versioned static `data/codebook.csv` is regenerated by both build paths and independently reconciled to the exact bundle.

## Place in the suite / cascade

This app is **climate's downstream water-chemistry fingerprint — it sits beside the climate driver, not on a trophic rung.** For Cass (the cascade synthesist), my standing guidance holds:

- **It corroborates the aridity axis, descriptively.** Specific conductance is a clean dissolved-load integrator; saline endmembers (PRPO prairie potholes, 7,924 µS/cm — plausible, not an artifact) vs dilute montane streams (COMO, WLOU) map the same aridity gradient the desert seasonal-split story rests on (`docs/DATA-TAKEAWAYS.md:50`). It can **anchor the precip side** of the biome-conditional `precip → greenup` prior in warm deserts.
- **It does NOT feed the lag-aware cascade.** Sub-annual grab chemistry has no defensible lag link to greenup or consumer rungs. Treat it as **descriptive climate corroboration — the same tier the suite assigns to breeding birds.** Don't let anyone wire conductance into a lag tally.
- **Its unique value: a well-powered method-check.** With n in the hundreds per site, this is the suite's best place to *demonstrate that the honest-stats chrome behaves when power is NOT the limiter* — a control against the n=6 false-negative regime that haunts the terrestrial rungs. Which is exactly why finding the input-QC gaps here matters: it's the app where you can't blame n.
- **It already embodies the suite doctrine.** The leverage is upstream — unit canonicalization, below-detection handling, outlier gating — mirroring the cascade's central lesson that desert "weak ecology" was a *method artifact* (annual aggregation), not bad statistics. **Fix the input, not the estimator.**

## June baseline scorecard (historical, superseded by the addendum)

| Dimension | Grade | One-line why |
|---|---|---|
| Method fidelity (DP1.20093.001 represented) | **A** | Both ingest streams, cadence, twins, replicates, BDL-as-flag all correct; only the legacy-era note is missing from the header. |
| Honest-stats machinery | **A** | n-on-everything, Spearman default, n≥8 gate, real STL, CV-vs-null, ACF flag — suite gold standard; keep as template. |
| Censoring handling | **C+** | Flagged in plots, preserved as value — but enters the math raw on analytes 26–57% below DL; needs BDL% surfacing + grey-out. |
| Outlier / plausibility QC | **D** | No gate anywhere; ANC=927 / Fe=931 corrupt OLS, STL, glm AND the map colorbar. Highest-leverage single fix. |
| Unit integrity | **C** | 20/34 analytes multi-unit; UV254/UV280 export `NA`; six µg/L mislabels (cosmetic). Axis patched, dictionary not. |
| Reproducibility / FAIR | **A−** | Single builder, grain check, seeded CV, committed bundle, provenance header — minus a static shipped codebook. |
| Suite/cascade role honesty | **A** | Correctly placed beside the climate driver, descriptive tier, no false lag link; serves as the well-powered control case. |

— Brooke
