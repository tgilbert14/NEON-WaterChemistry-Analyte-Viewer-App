# Project status: NEON Water Chemistry Analyte Viewer

> Read this before flagging anything. Maintained by `triage` and by the app's owner session after every
> review or ship. The **Deliberately deferred** section is binding: a cold-boot agent does not re-flag what it
> lists. Suite doctrine: https://github.com/tgilbert14/NEON-Driver-Cascade/blob/master/docs/neonize-playbook.md
> (this repo's `docs/neonize-playbook.md` is a pointer once PR #24 merges). App authority: `docs/BUILD-TEST-HANDOFF.md`,
> `DEPLOY.md`, `docs/EXPERT-REVIEW.md`.

As of: 2026-10-26 · Version: `main` @ `87245a2` (2026-10-20) · Stage: live; the finish-round PR from
`automation/water-chemistry-data-refresh` is in review

## Release and data state

| Item | Value | How to check |
|---|---|---|
| Live app | https://019ebf59-b2ff-a20a-cb35-6d227ca6261a.share.connect.posit.cloud/ | TG-Data-Apps `monitoring/sites.tsv` |
| Cover | https://tgilbert14.github.io/NEON-WaterChemistry-Analyte-Viewer-App/ | |
| Deployed revision | `87245a2` (the live `ddl-runtime-receipt` matches it, probe 2026-10-22) | `scripts/post_deploy_smoke.sh` |
| NEON product | `DP1.20093.001` · `RELEASE-2026` (DOI https://doi.org/10.48443/7b2c-ga32) plus `PROVISIONAL` from 2025-01-06 in the review candidate; the bundle on main records no release tags | the bundle's `built$neon_releases` |
| Data through | main: 2026-07-15. Review candidate: 2026-10-07 | the landing view's "Data through" line |
| Review branch | `automation/water-chemistry-data-refresh`: code plus a validated v5 bundle, in the finish-round PR | `gh api repos/tgilbert14/NEON-WaterChemistry-Analyte-Viewer-App/compare/main...automation/water-chemistry-data-refresh` |
| Last executed refresh | full fetches dispatched on the finish branch on 2026-10-26: run 40351031862 stopped for review on one identity (now audited), run 40374201024 passed every data check and stopped on a stale manifest checksum, and the publishing run is named in the PR | TG-Data-Apps `node .claude/scripts/neon-suite-probe.mjs --app water-chem` |
| 60-day schedule cutoff | 2026-12-20 from the main head commit (probe, 2026-10-22) | the probe |
| Suite lint | 0 red, 0 yellow on the finish branch, 2026-10-26 | TG-Data-Apps `node .claude/scripts/neon-suite-lint.mjs --app water-chem --worktree` |
| Open PRs, in merge order | the finish-round PR (code and data together; normal merge commit, not squash); #24 (playbook pointer, independent of it) | `gh pr list --repo tgilbert14/NEON-WaterChemistry-Analyte-Viewer-App` |

## Now (P1: blocks a ship, a refresh, a demo or a user)

- [ ] Merge the finish-round PR with a normal merge commit, so the bot data commit keeps its validated parent · owner Tim · evidence: its exact-head CI
- [ ] After the merge, check the live app: title "NEON Water Chemistry · Analyte Viewer", "Data through 2026-10-07" on the landing view, REDB loads every tab, Change site shows the picker map · owner Tim or the owner session · evidence: `post-deploy.yml` runs the semantic and browser smokes

## Next (P2: real, not urgent)

- [ ] Set `CARTO_BASEMAP_KEY` as a Connect content variable and republish; the app serves the Esri grey fallback until then · owner Tim · evidence: `DEPLOY.md`
- [ ] Add `www/pincards.js` and `www/vendor/html-to-image-1.11.11.js` to the receipt file list for `water-chem` in TG-Data-Apps `.claude/neon/suite.json`, or the probe reports live != HEAD after the merge · owner hub session or Tim · evidence: `scripts/runtime_manifest_files.R`
- [ ] A ruleset on `main` (pull request required, 0 approvals, no force-push or deletion; required checks `ci / static` and `ci / r-tests`) · owner Tim · evidence: suite audit appendix, branch-protection finding
- [ ] Confirm the 86804 TPC/TPN window on a tokened pull with `laboratoryName` (the 108 remaining rows are single replicates) · owner brooke · evidence: `config/neon-product-watch.json`

## Later (P3: polish)

- [ ] Re-lock shiny 1.14.0 and bslib 0.11.0 (the suite versions) in a dedicated, reviewed lock update
- [ ] Censored-data estimators (Kaplan-Meier or ROS) for site medians where more than half the record is below detection; today the app reports "mostly below DL" instead of a number
- [ ] An optional "fit after the 2019 method change only" toggle on the STL view

## Deliberately deferred (DO NOT re-flag: decided, with the reason)

- **SRER as the default site**: not applicable. SRER has no aquatic site; the default is SYCA (Sycamore Creek, AZ, D14), the Arizona aquatic site. Decided by the owner session on 2026-10-26. Reopen when NEON adds an aquatic site at SRER.
- **Release-pinned fetch (`?release=RELEASE-2026`)**: deferred because pinning would drop every provisional month (all of 2025 and 2026, 37,897 rows in the review candidate) and shrink the record, which the refresh guard refuses. The app records each row's release and cites RELEASE-2026 plus the provisional data instead. Decided by the owner session on 2026-10-26. Reopen when Tim wants released data only.
- **shiny/bslib lock bump**: deferred because the repo's contract makes a dependency change a separately reviewed lock update and Connect rebuild; no defect is attributed to shiny 1.13.0 or bslib 0.10.0. Decided by the owner session on 2026-10-26. Reopen with a dedicated lock PR.
- **NEON change-log 116504 (REDB ALK/ANC 2024-08-13 to 2026-05-26 erroneous)**: no data change. The bundle's ANC is the external lab's and ends 2020-03-19; ALK is not ingested. Decided by the owner session on 2026-10-26. Reopen if the producer ever ingests domain-lab titrations.
- **Suite decisions already settled (never re-flag in this repo):** the review-branch refresh contract (a person's merge of the review branch is the deploy; no workflow pushes the watched branch); `skip_download` runs may publish to the review branch; terra pinned at 1.8-50; `neonUtilities` out of `manifest.json`; SRER as the default site unless this app documents a reason (it does, above); no visible AI-art badges; the copy voice (no em dashes, no pitch, no hype, no sincerity labels, never state what the reader can already see).

## Owner decisions pending

- The Site Explorer in the in-app registry · options: list it / leave it out · recommendation: leave it out until the roster decision; the registry lists the ten Shiny explorers · asked on 2026-10-26 in `reports/status/water-chem.md`

## Recently shipped (so cold-boot agents do not re-open it)

- 2026-10-21 · plots no longer blank after a site change; plain page title; picker map after a deep-linked start · PR #23 · merge `87245a2` · verified live (REDB every tab, 34 markers)
- 2026-10-21 · Fe/Mn value quarantine (runtime), kickMaps handler, per-site censoring, 2019 method marker, phone legend · PR #22 · merge `817bebf` · verified live together with #23, which fixed the blank plots #22 introduced
- 2026-10-21 · refresh cron and calendar gate off the midnight edge; shared token probe · PR #21

## Keeping this file true

- "As of" and "Version" come from git, never from memory or from this file.
- An item moves to Recently shipped only with a merge SHA and a live check.
- When a deferral's reopen condition is met, move it back to Now or Next with the evidence.
