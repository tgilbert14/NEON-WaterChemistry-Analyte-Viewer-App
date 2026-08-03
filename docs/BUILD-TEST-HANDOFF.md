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
