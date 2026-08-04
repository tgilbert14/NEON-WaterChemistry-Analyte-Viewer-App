# Build, test, and handoff record

## 2026-08-03 EDT - signed refresh replay and unit policy v4 / Codex

- Full-fetch diagnostic run
  [30852990426](https://github.com/tgilbert14/NEON-WaterChemistry-Analyte-Viewer-App/actions/runs/30852990426)
  ran at exact source `60aca33c36ced33e6f381ee58a551e379acdaf26`.
  It passed the gate and six-file Connect cold start, downloaded the complete
  public product, then failed closed before candidate publication on 25
  previously unaudited unit identities. Production remained on the verified
  196,856-observation baseline.
- The retained artifact contained exactly the three canonical replay files, the
  deterministic unit review, and its receipt. Independent rehash/replay matched
  receipt SHA-256
  `0f533784f24408a938324bfb0b521e11f1b023ee59527ab1c15e0f6318c8102c`
  and review SHA-256
  `c18b00c39f74dae90ae7253dd2371482362b5ce354169826c50dda4281202485`.
  It covers 238,488 lab rows, 8,599 field rows, and the canonical 34-site
  coordinate roster.
- The replay exposed 73 non-target unit identities / 75 source rows in total.
  Forty-eight were already audited. The 25 new identities / 26 rows belong only
  to the two existing anomaly families: 17 WALK-2019 concentration-label
  identities from Florida International University and eight EcoCore_CSU
  TPC/TPN `milligram` identities. The latter remain unresolved legacy
  particulate-unit anomalies because the fetch lacks the defensible
  sample-volume evidence required for a mass-to-concentration conversion.
- Unit policy v4 adds only those exact site/date/analyte/source-label identities
  with their observed source-row counts as upper bounds. It still quarantines
  every mismatch, changes zero numeric values, and fails on a new identity,
  count increase, missing or inexact required laboratory provenance, unknown
  analyte, or unsupported missing-label repair. The six WALK concentration
  families require exact `Florida International University` provenance;
  TPC/TPN exclusions and all 12 registered missing-label rewrites require exact
  `EcoCore_CSU` provenance. Target-label support remains global and may come
  from another laboratory. Replaying the signed source under v4 yields 73 audited
  exclusions, 11,681 approved missing-label identities, and zero unapproved
  identities.
- The review receipt alone is not treated as scientific authority. Validation
  canonicalizes the stored replay, recomputes the deterministic review, and
  requires exact parsed-table and CSV-byte equality with the stored review.
  Full-fetch candidate validation also rebuilds the bundle from that replay and
  requires exact object equality after normalizing only the producer timestamp.
  These strengthened gates still require a fresh exact-head workflow result.
- Release boundary: exact-head tests, a second authenticated full fetch, clean
  independent candidate verification, reviewer-authenticated PR, merge, and
  live Pages/Connect proof are still required before the refreshed data can
  replace production.

## 2026-08-03 EDT - Connect startup recovery / Codex

- Merge `091a97c74ca287d68aeda5cf644246e058545692` passed exact-head CI and
  GitHub Pages, but the public Connect content returned Posit's `Startup Error`
  page after automatic publication. The full Water refresh was held; no new NEON
  candidate was dispatched or published against the failed runtime.
- The merged six-file runtime was independently cold-sourced with isolated
  home/cache/tmp and `LC_ALL=C`. It returned a `shiny.appobj` with the exact
  reviewed receipts: 48 collapsed / 99 represented unit rows quarantined,
  11,679 labels filled, 198 plausibility exclusions, and zero PRPO high-variance
  exclusions. This ruled out an ordinary local top-level source failure from the
  manifest file set, locale, committed data, or scientific contract; exact Ubuntu
  and real Connect proof remained required.
- The evidence isolated the leading recovery boundary to package restoration or
  package/runtime compatibility: the failed manifest retained the
  same 103 package names but changed 35 versions and encoded `classInt`, `raster`,
  `s2`, `sf`, `sp`, `terra`, `units`, and `wk` as direct URL/source records. The
  prior healthy Water manifest and healthy suite siblings used zero URL records.
- The recovery preserves all merged scientific/data logic and every data byte;
  the only app change computes and publishes readiness/receipt metadata. It
  restores the prior known-good package records from source commit
  `31b2e921a80aa262741c44f2282c781f394e1a90`, normalizes both repository fields to
  the fixed Posit jammy `2026-07-15` snapshot, and records them in
  `config/connect-manifest-packages-v1.json`. `write_manifest.R` regenerates only
  the exact six file checksums, refuses dependency-name drift, overlays the lock,
  requires R 4.5.2 / locale C / 103 standard CRAN records, forbids URL and moving
  repositories, and retains terra 1.8-50 for the documented GDAL boundary.
- The independent candidate verifier now requires semantic identity with the lock.
  Adversarial fixtures cover package removal, version mutation, URL-source drift,
  and moving repositories in addition to the existing scientific/data attacks.
- A new required `connect_cold_start` job derives all `name@version` specs from the
  lock, restores the complete graph in a clean Ubuntu 22.04/R 4.5.2 job, verifies
  every installed version, cold-sources only the six manifest files, starts a real
  localhost Shiny process, and requires `water-chemistry-v1` from HTTP. The normal
  producer also runs the six-file source/HTTP gate before packaging.
- `.github/workflows/post-deploy.yml` now waits through Connect cold start after
  every main publication, rejects `Startup Error` even when returned with HTTP
  200, requires the Water readiness marker plus an exact receipt derived from all
  six committed runtime checksums, checks Pages, and maintains a production-health
  issue on failure/recovery. Its bounded retry budget leaves explicit margin for
  outage-issue handling before the 35-minute job timeout.
- Local evidence: package-lock, allowlist, unit-contract, and backup-recovery tests
  passed; the exact six-file source and localhost HTTP boot passed with the receipts
  above; changed R and shell files parse; both workflows parse as YAML; and
  `git diff --check` passes. The full index/adversarial verifier remains intentionally
  delegated to exact-head Ubuntu CI because the promoted search-index bytes contain
  Linux/OpenBLAS floating representations that do not exact-recompute on macOS.
- Required release boundary: exact-head clean-restore CI, independent review, merge,
  real Connect semantic recovery, then—and only then—the held full NEON refresh.

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
- Replaced the wildcard with one explicit six-file runtime contract:
  `app.R`, `helpers.R`, `scripts/water_unit_contract.R`, `data/codebook.csv`,
  `data/neon_swc.rds`, and `data/search_index.rds`. A base-R regression creates a timestamped backup,
  `analyte_coverage.csv`, and a stray `www` file and proves none can enter; it
  also proves a missing required runtime file fails closed. The independent
  verifier now prints the exact missing and unexpected sets on drift.
- Corrected a separate scientific-contract defect before allowing another full
  refresh. The audit evidence established that the sparse
  `microgramsPerLiter` labels on otherwise-mg/L analytes are attached to
  mg/L-magnitude values (TP 0.057, not 57). The builder had nevertheless divided
  those values by 1,000. The revised contract pins the exact presentation target
  for all 34 established analytes. The 31 external-lab analytes carry row-level
  labels and are transition-guarded. The three field-derived analytes
  (`waterTemp`, `dissolvedOxygenField`, `specificConductanceField`) arrive as
  value columns without per-row unit labels, so their units are explicit fixed
  extraction assumptions rather than observed upstream-transition receipts.
  Only missing labels on 12 named lab analytes may be filled, and only when the
  target label is also observed. Non-missing mismatches are never relabelled or
  numerically converted.
- The 36 known WALK-2019 alternate-label groups are bounded to their exact
  site/date/analyte/source-label identities and source-row maxima, then
  quarantined with a sorted SHA-256-bound receipt. Any first sparse mismatch at
  a new identity, any count increase, any all-at-once target change, any unknown
  analyte, or any unregistered missing label fails closed under
  `unit_policy = explicit-targets-audited-exclusions-value-invariant-v3`.
- Independent review found that the 12 collapsed TPC/TPN `milligram` groups
  cannot be label-only repairs. The current [revision-H NEON guide](https://data.neonscience.org/api/v0/documents/NEON_waterChem_userGuide_vH)
  supersedes the older F.1 text, and the current [official product change log](https://data.neonscience.org/api/v0/products/DP1.20093.001)
  says all EcoCore particulate C/N data were converted from `milligrams` to
  `microgramsPerLiter`. The residual labels are therefore unresolved legacy unit
  anomalies, not proven masses. Their exact 12 identities remain conservatively
  quarantined with EcoCore_CSU provenance and count bounds pending source
  reconciliation; no conversion is inferred.
- The shared runtime boundary now protects the exact legacy bundle before new
  data lands. It removes the same 48 collapsed mismatch identities—36 WALK
  concentration-label groups plus 12 TPC/TPN groups, representing 99 source
  rows—from the app, exports, metadata, wide matrix, and search index. Unknown
  identities, duplicate collapsed identities, or replicate-count growth fail
  closed. Numeric values are unchanged. The runtime policy is itself a required
  manifest file, so app and index cannot drift onto different rules.
- The executable fixture proves audited TP and TPC mismatches are excluded rather
  than divided or relabelled, missing UV metadata is filled without value change,
  and all receipts are deterministic. It rejects the first 57-µg/L transition
  row among 1,000 mg/L rows, Ca/ANC lab-label transitions, unknown analytes,
  unaudited identities, increased identity counts, unexpected TPC/TPN laboratory
  provenance, fractional counts, and receipt tampering. A real `field_raw`
  fixture proves the three fixed field extraction units and values. Kept numeric
  values must remain identical and `n_unit_values_changed` must be exact `0L`.
- Receipt-free validation is not event-based. It is allowed only when the
  candidate SHA-256 is the exact known legacy bundle
  `dce312128e4392aaecdba535bc92be5d9ae8f1dc0e938745b530bc2a4f7f0868`
  **and** equals the bundle independently extracted from the immutable PR base or
  current `main`. Unknown verifier flags, a typoed strict flag, and extra
  positional arguments fail before candidate inspection. This closes both the
  code-PR and manual-skip legacy bypasses.
- Full fetch and cache-only builds both regenerate the versioned static
  `data/codebook.csv`. The independent verifier checks its header/version,
  exported-column section (including every exact allowed-value, definition, and
  NA-semantics string), exact analyte roster, target units, coverage counts,
  below-detection provenance, build date, and product against the candidate
  bundle; a recomputed manifest checksum cannot bless semantically stale bytes.
- The independent verifier also reconstructs both search-index tables from the
  fail-closed runtime rows and current plausibility gate, then exact-compares all
  values, statistics, years, site metadata, labels, units, counts, and row order.
  It requires all 18 directly imported or namespace-used runtime packages, not a
  representative subset. Adversarial tests prove that tampered index means and
  counts, false codebook text, and a missing direct dependency remain rejected
  even after the attacker regenerates every manifest checksum.
- Pull-request validation preserves the committed index, codebook, and manifest
  before extracting the producer artifact. After independent validation and
  artifact upload, it byte-compares those committed files with the validated
  rebuild and fails with an exact promotion instruction on any mismatch. Thus a
  PR cannot turn green by validating regenerated bytes while leaving stale bytes
  in its immutable head.
- Build receipts now require exact integer scalars for observation, site,
  analyte, below-detection, and index totals, exact logical `partial = FALSE`,
  and reconciliation against the candidate tables. Fractional/coerced counts,
  `NA`/string partial flags, and tampered totals fail independently.
- `save_bundle()` now writes and hashes pending bytes first, refuses a colliding
  timestamped backup, verifies the old-byte copy before promotion, verifies the
  promoted hash, and restores the verified backup if promotion fails. The
  executable regression proves both collision safety and forced partial-promotion
  failure: the prior bytes are restored exactly, pending bytes are cleaned up,
  and the verified recovery backup is preserved.
- Production-baseline impact audit (the committed 196,856-observation bundle):
  48 collapsed rows: 36 across NH4-N (5), NO2-N (8), NO3+NO2-N (3), Ortho-P (8),
  TDP (6), and TP (6) carry the affected alternate label. Six are singleton
  records that the old policy would definitely shrink 1,000-fold; 30 collapse
  multiple raw replicates, so their exact old-policy output effect cannot be
  reconstructed from the collapsed bundle alone. The repaired policy changes
  **zero values**. The shared runtime boundary fills 11,679 registered missing
  labels in the committed collapsed bundle, primarily UV254/UV280, and excludes
  the 48 mismatch groups. The 12 TPC/TPN groups represent 30 source rows and are
  unresolved anomalies. This repair changes app/runtime/index policy, builder,
  verifier, tests, and documentation, but does not mutate the committed source
  bundle. Production remains on its last known-good release until exact-head CI,
  review, merge, and deployment complete.
- The failed full build was +4,107 observations and +182 below-detection flags
  versus the committed baseline, retained 34 sites/34 analytes, and advanced the
  data-through date from 2026-05-21 to 2026-07-15. Because verification stopped
  before packaging, GitHub retained no candidate artifact from which to compute
  finer per-analyte deltas. After human review and merge of this code-only
  repair, a new full refresh must produce an independently validated artifact;
  reviewers must inspect its exact unit receipt and metric deltas before merging
  the data candidate. Do not dispatch or merge automatically.
- Independent review rejected initial head `a6c3f509` before merge for a modal
  transition risk, legacy receipt bypass, fail-open CLI/count parsing, unchecked
  backup creation, incomplete established-unit targets, and invalid TPC/TPN mass
  relabelling. A second pre-commit review then rejected stale codebook packaging,
  fail-open build receipts, an overstated 34-analyte transition claim, unsafe
  legacy-runtime relabelling, superseded particulate rationale, an untested
  restore branch, and contradictory docs. A third review of snapshot `ffb727f`
  rejected fail-open search-index semantics, incomplete tidy-codebook text and
  runtime-dependency validation, PR validation of regenerated rather than
  committed derived bytes, and three stale documentation statements. This
  working tree addresses all three review rounds; fresh independent review and
  exact-head CI are still required before merge.
- Local release gate passed on the complete working tree: runtime-allowlist,
  producer-unit/runtime, independent-verifier adversarial, and forced-promotion
  recovery regressions; all 16 app/helper/script/test R files parsed; workflow
  YAML parsed and all 14 embedded shell `run` blocks passed `bash -n`; and
  `git diff --check` passed. The exact verifier accepted the immutable legacy
  bundle at 196,856 observations, 34 analytes, 34 sites, through 2026-05-21,
  SHA-256 `dce312128e4392aaecdba535bc92be5d9ae8f1dc0e938745b530bc2a4f7f0868`.
  An isolated app boot produced 196,808 retained rows, 48 quarantined collapsed
  groups / 99 represented source rows, 11,679 registered missing-label rewrites,
  198 plausibility exclusions, and zero PRPO high-variance exclusions. Repeated
  index/codebook/manifest generation was byte-identical. Exact-head GitHub
  Actions remains the required Linux/R-4.5.2 proof.

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
- At that initial recovery stage, the candidate intentionally changed workflow,
  verifier, build guard, and documentation only. The later unit-contract follow-
  up above supersedes that scope and also changes runtime policy/app/index,
  source ingest, codebook generation, adversarial tests, and documentation.
- Manifest rewriting now replaces residual `cran.rstudio.com` `RemoteRepos` as
  well as moving CRAN/RSPM `latest` URLs, and the verifier rejects any recurrence.
- Local evidence: the workflow parses as YAML; all 11 embedded run blocks pass
  bash -n; every changed R file parses; and git diff --check passes. A temporary
  candidate with source-derived index provenance and refreshed manifest MD5s
  passed the verifier at 196,856 observations, 34 analytes, and all 34 sites.
  At that initial stage, the tracked old index/manifest were intentionally left
  untouched for pinned regeneration and remained fail-closed under the then-new
  exact verifier. The later unit-contract follow-up above regenerated both and
  supersedes that historical state.
- Required next proof: exact-head CI, merge of this code-only repair, then one
  full pinned refresh whose independently validated candidate is reviewed and
  merged before production/Connect verification.
