# Build, test, and handoff record

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
