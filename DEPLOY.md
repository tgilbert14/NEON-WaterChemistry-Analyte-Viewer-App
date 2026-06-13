# Deploy & migration runbook

The app currently has a stale `rsconnect/shinyapps.io/` record. **shinyapps.io is being
retired (end of 2026)** — do not invest there. This is the plan to move it, mirroring the
Big 12 Girth Index migration (hosted app + a GitHub-Pages landing/redirect with an og card
and a cold-start pre-warm).

## The shape of the move

```
  Posit Connect Cloud (or shinylive)        GitHub Pages  (docs/)
  └─ runs app.R + helpers.R + neon_swc.rds  └─ landing page → "Launch" → app URL
                                               + og:image social card
                                               + fetch() pre-warm ping (cold-start fix)
```

The landing page already exists at [`docs/index.html`](docs/index.html). After you deploy the
app, set the app URL in **one place** (the `APP_URL` constant near the top of that file) and
publish Pages.

---

## Option A — Posit Connect Cloud (recommended first move; works today)

Connect Cloud deploys an R app straight from a public GitHub repo using `manifest.json`
(already generated, lists only `app.R`, `helpers.R`, `data/neon_swc.rds` — no legacy/, no
neonUtilities/mlr/shinydashboard).

1. Sign in at <https://connect.posit.cloud> with GitHub.
2. **New Content → Shiny → from Git**, pick `tgilbert14/NEON-WaterChemistry-Analyte-Viewer-App`,
   branch `main`, primary doc `app.R`.
3. It reads `manifest.json`, restores packages, and serves. Auto-republishes on every push.
4. Copy the published URL → set `APP_URL` in `docs/index.html`.

Regenerate the manifest whenever dependencies change:
```r
rsconnect::writeManifest(appFiles = c("app.R","helpers.R","data/neon_swc.rds"),
                         appPrimaryDoc = "app.R")
```

**Cold start:** the free tier sleeps. The landing page's pre-warm `fetch(APP_URL)` on load wakes
it while the visitor reads, so the app is usually warm by the time they click Launch.

## Option B — Shinylive / WebAssembly (best long-term: static, zero server, infinite scale)

Because the app has **no runtime dependency** (read-only bundled `.rds`, no neonUtilities), it is
an ideal shinylive candidate — it could be served entirely from GitHub Pages alongside the
landing page. **Gate: every package needs a wasm binary.** Check each at
<https://repo.r-wasm.org/> before committing: `shiny, bslib, bsicons, plotly, DT, ggplot2,
dplyr, tidyr, readr, lubridate, shinycssloaders`. These are mainstream (r-universe builds wasm
for ~all CRAN), but **verify per-package — one missing binary kills the static build**. There is
no `forecast`/`mlr` dependency to block it (STL is base `stats`).

```r
# install.packages("shinylive")
shinylive::export(".", "docs/app")   # emits a static site under docs/app/
# then APP_URL = "app/"  (relative) and you have ONE GitHub-Pages deploy: landing + app
```

If the wasm check is clean, this is the cheapest, fastest home and removes Connect Cloud's cold
start entirely. If any package fails the check, stay on Connect Cloud (Option A).

---

## Publish the landing page (GitHub Pages)

1. Repo **Settings → Pages → Source: Deploy from a branch → `main` / `/docs`**.
2. Page goes live at `https://tgilbert14.github.io/NEON-WaterChemistry-Analyte-Viewer-App/`
   (or wire a custom subdomain like the Girth Index — add a `CNAME` file in `docs/` and a DNS
   CNAME with the cloud proxy turned off for validation).
3. Drop a real `docs/og-image.png` (1200×630) so the social card renders — there's a placeholder
   note in the HTML. A screenshot of the Compare tab works well.

## Retire the old target

Once Connect Cloud (or shinylive) is live and the landing page points at it, delete the
`rsconnect/shinyapps.io/` tree (or leave it — it's inert; `deployApp()` is no longer used).
