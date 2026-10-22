<!-- neonize-playbook-stub: v1 -->
# NEONize playbook: pointer for Water Chemistry Analyte Viewer

The NEON suite playbook has one home, read by Claude Code and Codex alike:
https://github.com/tgilbert14/NEON-Driver-Cascade/blob/master/docs/neonize-playbook.md
(side-by-side clone: `../NEON-Driver-Cascade/docs/neonize-playbook.md`). Read it before
suite work. Fold every suite rule, pattern or gotcha into that file, never into this one.
Anything here beyond this pointer and the app facts below is a stale fork: report it.

## This app

- Repo `NEON-WaterChemistry-Analyte-Viewer-App` · watched branch `main` · product `DP1.20093.001`
- Rung specialist: `brooke`
- Refresh: `refresh-data.yml` (first-week cadence) publishes validated bytes only to the fixed review branch `automation/water-chemistry-data-refresh`, never `main`; a person's merge to `main` is the deploy.
- Local authority files: `docs/BUILD-TEST-HANDOFF.md` (commands, evidence, current release path, session log), `docs/EXPERT-REVIEW.md` (product-expert review; its 2026-08-03 addendum supersedes the June text), `docs/DRIVER-KNOWLEDGE-PACKAGE.md` (Driver disposition), `DEPLOY.md` (Connect and Pages deploy notes from the shinyapps.io migration).

## App-specific rules

- Default demo and startup site is **SYCA** (Sycamore Creek, Arizona; `DEF_SITE` in `app.R`): this is a stream product and SRER has no aquatic sampling.
- There is no `docs/SCIENCE-CONTRACT.md` yet; the scientific rules live in the expert-review addendum and the Driver knowledge package.

## Owner-settled house rules (both tools)

- Default demo, startup and picker pre-selection site: **SRER** (then JORN, SJER, MOAB, ONAQ) unless this app documents a reason.
- Copy voice on covers, share cards and in-app copy: plain words a non-expert follows; say what NEON is; no em dashes; no sales pitch (price, access, ease); no hype adjectives; no sincerity labels ("honest", "real data"); never state what the reader can already see.
- No visible "illustration" badge on poster art; the art's status lives in alt text and the image provenance record.

Cold boot: also grep the fleet's `.claude/agents/LESSONS.md` in TG-Data-Apps (Codex: the
`dev-teams` mirror) for your role name: `grep -E '^- \[[0-9-]*\] [a-z0-9+-]*<role>[ +]'`.
