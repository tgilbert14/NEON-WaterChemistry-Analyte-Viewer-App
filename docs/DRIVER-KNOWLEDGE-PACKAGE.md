# Driver knowledge package — Surface Water Chemistry

## Decision

| Decision axis | Disposition |
|---|---|
| Scientific contract / release process | **ADOPT** audited-unit identity, signed replay, deterministic quarantine, and exact-runtime receipts |
| Ecological use | **CONTEXT** |
| Driver ingestion | **HOLD** |
| Driver artifact impact | **NO DRIVER BYTE CHANGE** |

Surface water chemistry is an aquatic **condition/context** product. It is not
discharge, a primary-production rung, or a causal trophic vote. The current
release is scientifically reviewable and production-verified, but its overlap
with the terrestrial Driver is not an eligible integration join. Driver
integration therefore remains **UNMEASURED** pending a registered aquatic-key
contract, a declared context role, an independently verified adapter, and
source-to-Driver parity evidence.

The `ADOPT` row is a reusable contract decision, not an ecological promotion.

## Product and release identity

- NEON product: `DP1.20093.001` — Surface Water Chemistry.
- Reviewed source: `e2ea753` (`e2ea753a25257492e4a9e82970c8275d898a2788`).
- Independently validated data candidate: `27512485`
  (`27512485a2252e994be501eca3e8440e7659d2c1`).
- Promotion: [PR #15](https://github.com/tgilbert14/NEON-WaterChemistry-Analyte-Viewer-App/pull/15),
  exact-head workflow
  [30876917859](https://github.com/tgilbert14/NEON-WaterChemistry-Analyte-Viewer-App/actions/runs/30876917859),
  merged as `ee95af3` (`ee95af3e270099980ea5bc98b28b549456b3f0b2`).
- Production receipts: Pages workflow
  [30878152320](https://github.com/tgilbert14/NEON-WaterChemistry-Analyte-Viewer-App/actions/runs/30878152320)
  and production check
  [30878153073](https://github.com/tgilbert14/NEON-WaterChemistry-Analyte-Viewer-App/actions/runs/30878153073).

These identities bind this package to the promoted release. A later refresh is
new evidence and must be reviewed under its own exact source, candidate, and
production receipts.

## Canonical release facts

The promoted bundle contains:

- **200,953** canonical observations;
- **34** analytes at **34** aquatic sites;
- observations from **2012-01-31 through 2026-07-15**; and
- `partial = FALSE` for product `DP1.20093.001`.

Unit policy
`explicit-targets-audited-exclusions-value-invariant-v4` rewrites **14,422**
approved missing unit labels, excludes **75** audited source rows, and changes
**0** numeric values. The 34 presentation targets and the exclusion identities
are fail-closed contracts; the exclusions are quarantined rather than converted
or silently relabelled.

At the `site × calendar year` availability grain, the canonical bundle has
**387 aquatic site-years**. All **34 sites** have at least six years (the
observed range is 9–15). This is an availability summary only. It is not an
independence claim, a minimum-sample estimator gate, or proof of a
Driver-compatible annual response.

## Driver compatibility audit

The exact Water `site × year` rows have **0** direct site-code joins to the
current terrestrial Driver. That result is expected by construction: Water
uses the aquatic site roster, while the Driver uses its registered terrestrial
site roster.

A deliberately weaker `NEON domain × calendar year` proxy finds a Driver
calendar presence for **351 of 387** Water site-years (90.7%). This is a
**proxy diagnostic, NOT an eligible join**. It does not identify a shared site,
shared sampling opportunity, hydrologic linkage, causal exposure, or comparable
estimator. It must not be used to manufacture support or cast a Driver vote.

| Driver question | Current evidence |
|---|---|
| Exact terrestrial site-code join | 0 site-years; incompatible rosters by construction |
| Same-domain/calendar proxy | 351/387 site-years; diagnostic only |
| Registered aquatic key or crosswalk | Missing |
| Registered Driver role | Missing; candidate role is context only |
| Reviewed annual adapter | Missing |
| Source-to-Driver parity receipt | Missing |
| Vote eligibility | No |

## Ecological interpretation boundary

Water chemistry can describe the aquatic expression of climate, geology,
hydrology, and biogeochemical processing. Within the app, dense repeated grab
samples support transparent descriptive associations and condition summaries.
They do not turn chemistry into flow, plant production, or a consumer response.

In particular:

- conductance is a dissolved-load condition indicator, not a precipitation or
  discharge measurement;
- sub-annual co-sampled analyte associations are not annual causal links to the
  terrestrial plant-to-consumer cascade;
- below-detection structure, replicate handling, unit quarantine, and
  site-aware plausibility rules remain part of any interpretation; and
- domain coincidence cannot substitute for an ecological site crosswalk or a
  registered mechanism.

Accordingly, this product may inform narrative context and future aquatic
design work, but it contributes no trophic edge, no mechanism tally, and no
vote to the current Driver.

## Requirements to reopen ingestion

Reconsider the HOLD only after all of the following are registered and tested:

1. an exact immutable source pin and a declared Water role that remains
   non-causal unless a separate mechanism is justified;
2. aquatic site keys or a scientifically reviewed crosswalk—never a
   domain-only substitute;
3. an analyte-specific annual estimand with sampling-opportunity, replicate,
   censoring, unit, plausibility, and coverage rules;
4. an explicit temporal alignment and multiplicity plan appropriate to that
   estimand;
5. an independently implemented adapter with source-to-output parity and
   missingness/support diagnostics; and
6. predeclared eligibility and interpretation rules before any association is
   inspected.

Until then the formal suite disposition is **CONTEXT / HOLD DRIVER INGESTION /
NO DRIVER BYTE CHANGE**.

## Reproduction notes

The release counts and unit receipts are stored in `data/neon_swc.rds` under
`built`; the product dictionary is `data/codebook.csv`; and the fail-closed unit
rules are in `scripts/water_unit_contract.R`. The site-year availability count
is the number of distinct canonical `site × year(collectDate)` pairs in
`swc_long`. Release history and verification boundaries are recorded in
`docs/BUILD-TEST-HANDOFF.md`.
