# Build, test, and handoff record

## 2026-08-03 EDT - manifest allowlist and unit-contract follow-up / Codex

- Audited scheduled run 30821462254 at source
  `31b2e921a80aa262741c44f2282c781f394e1a90`. The NEON pull and scientific
  build completed (200,963 observations, 34 analytes, all 34 canonical sites,
  15,735 below-detection flags, through 2026-07-15, `partial = FALSE`), but the
  producer failed closed before packaging with `Manifest runtime file allowlist
  changed.` No producer artifact, review branch, or production update exists.
- Confirmed the exact manifest drift: `save_bundle()` preserves the old bundle
  as `data/neon_swc_YYYYMMDD-HHMMSS.rds`, while `write_manifest.R` globbed every
  top-level `data/*.rds`. A real full download therefore swept that one recovery
  backup into `manifest.json`; a skip-download build could not reproduce it.
  The backup is legitimate recoverability state, not contamination, and remains
  untouched and gitignored.
- Replaced the wildcard with one explicit five-file runtime contract:
  `app.R`, `helpers.R`, `data/codebook.csv`, `data/neon_swc.rds`, and
  `data/search_index.rds`. A base-R regression creates a timestamped backup,
  `analyte_coverage.csv`, and a stray `www` file and proves none can enter; it
  also proves a missing required runtime file fails closed. The independent
  verifier now prints the exact missing and unexpected sets on drift.
- Corrected a separate scientific-contract defect before allowing another full
  refresh. The audit evidence already established that the sparse
  `microgramsPerLiter` labels on otherwise-mg/L analytes are attached to
  mg/L-magnitude values (TP 0.057, not 57). The builder had nevertheless divided
  those values by 1,000. Canonicalization is now label-only, asserts the numeric
  vector is identical before/after unit cleanup, and emits
  `unit_policy = canonical-labels-value-invariant-v1`, the label rewrite count,
  and `n_unit_values_changed = 0` in the build receipt.
- The exact-source executable fixture includes TP=0.057 mislabeled as
  `microgramsPerLiter` and proves the output remains 0.057 (never 0.000057),
  while TP, TPC, and UV254 each finish with one canonical non-missing unit. The
  producer and the clean independent validator both run this fixture. Every
  real full-refresh artifact must carry the current unit receipt and pass the
  verifier's single-unit and zero-value-change gates; legacy committed bytes are
  accepted only for code-only PR validation.
- Production-baseline impact audit (the committed 196,856-observation bundle):
  36 rows across NH4-N (5), NO2-N (8), NO3+NO2-N (3), Ortho-P (8), TDP (6),
  and TP (6) match the old erroneous divide condition. The repaired policy
  changes **zero values**; the old policy would shrink each of those 36 values
  exactly 1,000-fold. Label-only canonicalization would rewrite 11,727 labels in
  the committed collapsed bundle, primarily missing UV254/UV280 labels, without
  changing any estimator input. This code-review branch intentionally changes
  no committed bundle, index, codebook, manifest, app, Pages, or production
  bytes, so its immediate production metric impact is zero.
- The failed full build was +4,107 observations and +182 below-detection flags
  versus the committed baseline, retained 34 sites/34 analytes, and advanced the
  data-through date from 2026-05-21 to 2026-07-15. Because verification stopped
  before packaging, GitHub retained no candidate artifact from which to compute
  finer per-analyte deltas. After human review and merge of this code-only
  repair, a new full refresh must produce an independently validated artifact;
  reviewers must inspect its exact unit receipt and metric deltas before merging
  the data candidate. Do not dispatch or merge automatically.
- Local repair evidence: both executable regressions pass under the pinned
  2026-07-15 R package snapshot; all changed/new R files parse; workflow YAML
  parses; all 12 embedded shell blocks pass `bash -n`; and `git diff --check`
  passes. Exact-head GitHub Actions remains the required Linux/R-4.5.2 proof.

## 2026-08-03 EDT - scheduled refresh recovery candidate / Codex

- Audited scheduled run 30736639401: the NEON pull completed with 200,963
  observations across all 34 sites and through 2026-07-15, but the derived-index
  step failed because the workflow had never installed plotly. No candidate was
  published and production remained on the prior committed bundle.
- Added the complete runtime-manifest dependency set under pinned Ubuntu 22.04,
  R 4.5.2, and the 2026-07-15 Posit snapshot, including the suite's fixed
  geospatial source closure. Manifest regeneration now records that same dated
  snapshot rather than silently rewriting package sources to moving latest.
- Replaced the legacy direct-to-main push with a read-only producer, a clean
  independent validator, and a write-scoped publisher that can update only
  automation/water-chemistry-data-refresh. It never creates a PR or writes main;
  it emits an exact branch/SHA/run/compare receipt for a repository write user.
- Pull requests now validate the immutable PR head rather than GitHub's synthetic
  merge SHA. PR runs cannot fetch NEON or enter the publisher; `NEON_TOKEN` exists
  only on the scheduled/manual full-fetch step.
- Strengthened the release boundary from a 90% floor to the exact canonical
  34-site roster, no observation shrink, no data-through regression, and
  partial == FALSE. The independent verifier also checks bundle/index schemas,
  source-derived provenance, manifest runtime allowlists, and every manifest MD5.
- Removed wall-clock churn from data/search_index.rds: its build identity now
  derives from data/neon_swc.rds, so a skip-download run is byte-stable.
- This candidate intentionally changes workflow, verifier, build guard, and
  documentation only. No bundle, index, codebook, manifest, app, Pages, or
  scientific-estimator byte is regenerated in the repair commit.
- Manifest rewriting now replaces residual `cran.rstudio.com` `RemoteRepos` as
  well as moving CRAN/RSPM `latest` URLs, and the verifier rejects any recurrence.
- Local evidence: the workflow parses as YAML; all 11 embedded run blocks pass
  bash -n; every changed R file parses; and git diff --check passes. A temporary
  candidate with source-derived index provenance and refreshed manifest MD5s
  passed the verifier at 196,856 observations, 34 analytes, and all 34 sites.
  The tracked old index/manifest are intentionally left untouched for pinned
  regeneration, and therefore remain fail-closed under the new exact verifier.
- Required next proof: exact-head CI, merge of this code-only repair, then one
  full pinned refresh whose independently validated candidate is reviewed and
  merged before production/Connect verification.
