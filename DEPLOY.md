# Deploy runbook

The app is live on Posit Connect Cloud:
<https://019ebf59-b2ff-a20a-cb35-6d227ca6261a.share.connect.posit.cloud/>.
The landing cover is GitHub Pages, served from `docs/`:
<https://tgilbert14.github.io/NEON-WaterChemistry-Analyte-Viewer-App/>.

```
  Posit Connect Cloud                         GitHub Pages (docs/)
  └─ runs the eight-file runtime allowlist    └─ landing cover, og:image card,
     from main, republished on every push        one link to the app
```

## Connect Cloud

Connect Cloud deploys from the public GitHub repo (`main`, primary file `app.R`) using
`manifest.json`, and republishes on every push to `main`. The runtime contract is exactly
`app.R`, `helpers.R`, `scripts/water_unit_contract.R`, `data/codebook.csv`,
`data/neon_swc.rds` and `data/search_index.rds`: no recovery backups, cache, `legacy/`,
or download-only packages.

Refresh the manifest whenever a runtime file changes:

```sh
Rscript --vanilla scripts/write_manifest.R
```

Do not hand-write an `appFiles` vector: the script and its regression share the single
runtime-file allowlist and enforce pinned package sources and checksums. Ordinary runtime and
data rebuilds keep the reviewed 103-package graph in
`config/connect-manifest-packages-v1.json`; a dependency change needs a separately reviewed
lock update.

Pull requests restore that exact graph on Ubuntu 22.04 with R 4.5.2, cold-boot the runtime files,
require the readiness marker, page title and `lang="en"` over HTTP, and drive the local app
in Chromium (`scripts/connect_runtime_smoke.sh --browser`, which runs
`scripts/browser_smoke.mjs`). After a push to `main`, `post-deploy.yml` waits for Connect
Cloud, rejects Posit host error pages, runs the same browser smoke against the live URL, and
opens or updates a production-health issue on failure.

The free tier sleeps between visits, so the first request after a quiet spell takes a few
seconds; the cover says so under its button.

### Response headers

Connect Cloud owns the HTTP response headers. The app sets no Content-Security-Policy or
HSTS header of its own and cannot. If those headers are ever required, the route is a custom
domain behind a proxy that adds them.

## Environment variables

| Variable | Where it is set | Read by | When unset |
|----------|-----------------|---------|------------|
| `CARTO_BASEMAP_KEY` | Connect Cloud content variable (optional) | `app.R`, the map basemap | The map uses the Esri grey canvas basemap; a value that does not look like a CARTO key is treated as unset. |
| `NEON_TOKEN` | GitHub Actions repository secret; `.Renviron` locally | `scripts/precompute_neon_data.R`, the workflow token probe | In CI the token probe and the fetch stop with a named error. Locally the fetch prints a note; NEON refuses anonymous data downloads. |
| `SOURCE_SHA` | Provided by the refresh workflow | `scripts/precompute_neon_data.R`, the review validators | Required (a 40-character commit id) whenever `WATER_REFRESH_REVIEW_DIR` is set; the review writer stops without it. |
| `WATER_REFRESH_REVIEW_DIR` | Provided by the refresh workflow | `scripts/precompute_neon_data.R`, `scripts/water_refresh_review.R` | No replay or unit-review evidence is written; a local refresh still builds the bundle. |
| `CI` | Provided by GitHub Actions (`true`) | `scripts/precompute_neon_data.R` | Treated as a local run: a missing `NEON_TOKEN` is a note, not an error. |
| `GITHUB_STEP_SUMMARY` | Provided by GitHub Actions | `scripts/refresh_data.R` | The run summary is printed only to the log. |

The smoke scripts also read optional tuning variables (`SMOKE_*`, `WATER_CONNECT_SMOKE_PORT`,
`BROWSER_SMOKE_URL`, `BROWSER_SMOKE_TIMEOUT_MS`); each has a default in the script.

## Data refresh

`refresh-data.yml` proposes a monthly refresh on a fixed review branch,
`automation/water-chemistry-data-refresh`; no workflow pushes `main`. When a validated
candidate changes the data, the workflow opens or comments on the issue "Water Chemistry
refresh ready for review" with the branch, head commit, date range and observation delta. A
weekly reminder comments when the branch has waited more than 14 days. Merging the PR is the
deploy; the next post-deploy run closes the issue. A weekly NEON watch compares the product's
change log and releases with `config/neon-product-watch.json` and opens one issue listing
anything not yet reviewed.

## Landing cover (GitHub Pages)

1. Repo **Settings → Pages → Source: Deploy from a branch → `main` / `/docs`**.
2. Keep the committed `docs/og-image.png` (1200×630) in sync with substantive cover changes.
3. `node scripts/check_cover.mjs` (run in CI) checks the cover's copy, title, language and its
   single link to the NEON Cross-Product Response Atlas.
