#======================================================================
# NEON Surface Water Chemistry — Analyte Viewer  (v3, bslib rebuild)
# Timothy Gilbert · rebuilt 2026
#
# Real NEON SWC data (DP1.20093.001) is precomputed into data/neon_swc.rds
# (see scripts/precompute_neon_data.R) and loaded once below — no runtime
# neonUtilities pulls. UI: bslib. Charts: plotly. Stats: honest (n shown,
# Spearman default, CI bands, real STL seasonal decomposition).
#======================================================================
suppressWarnings(suppressMessages({
  library(shiny); library(bslib); library(bsicons)
  library(dplyr); library(tidyr); library(readr); library(lubridate)
  library(plotly); library(DT); library(ggplot2); library(shinycssloaders)
}))
source("helpers.R")
options(shiny.sanitize.errors = TRUE)                 # never leak a raw R error to the page
shinyOptions(cache = cachem::cache_disk(file.path(tempdir(), "neon-cache"), max_size = 50 * 1024^2))

## ---- Data (loaded once, shared across sessions) --------------------------
D <- readRDS("data/neon_swc.rds")

## ---- App-side input QC (expert-review findings, applied at load — NO bundle
##      rebuild) -------------------------------------------------------------
# Three upstream fixes the bundle's first()-wins shortcuts left open, applied
# once here against the in-memory long frame so EVERY downstream reactive (fits,
# STL, glm, map mean, exports, dictionary) inherits them:
#   (1) Canonical unit per analyte (modal, not first; UV gets a real unit) —
#       coerce swc_long$units to one string per analyte (LABEL only; values are
#       NOT rescaled — the µg/L mixers are mislabeled mg/L-magnitude rows).
#   (2) Plausibility gate — flag values above the per-analyte ceiling so a lone
#       artifact (ANC 927, Fe 931) never drives a fit/STL/glm/map mean.
#   (3) Censoring rate per analyte (already in analyte_meta) — surfaced via click.
CANON_MAP  <- canonical_units(D$swc_long)
CEIL_TBL   <- plausibility_ceilings(D$swc_long)
CEIL_MAP   <- ceiling_map(CEIL_TBL)

# (1) stamp the canonical unit onto every long row (so axis/hover/export agree)
D$swc_long$units <- unname(CANON_MAP[D$swc_long$analyte]) %|na|% D$swc_long$units
# (2) flag plausibility on the long frame; keep the value for the audit marker,
#     but NULL it out of the wide matrix the fits/means/STL/glm read from.
D$swc_long$implausible <- !is_plausible(D$swc_long$value, D$swc_long$analyte, CEIL_MAP,
                                        site = D$swc_long$site)
# table of excluded extremes, for the clickable audit marker (small, never inline).
# The ceiling shown is the SITE-aware one where it applies (high-variance analytes),
# so a saline site like PRPO is judged against its own range, not the pooled one.
.ceil_for <- function(an, st) {
  key <- paste(an, st, sep = "\r")
  sc  <- unname(CEIL_MAP$site[key])
  ifelse(!is.na(sc), sc, unname(CEIL_MAP$global[an]))
}
EXCLUDED_EXTREMES <- D$swc_long |>
  dplyr::filter(implausible) |>
  dplyr::transmute(site, collectDate, analyte,
                   display = analyte_display(analyte), value,
                   units = unname(CANON_MAP[analyte]) %|na|% units,
                   ceiling = .ceil_for(analyte, site)) |>
  dplyr::arrange(dplyr::desc(value))
# Surface (not hide) any high-variance-analyte rows the gate trimmed, so a
# legitimately extreme site is auditable. With the site-aware gate, PRPO's
# saline conductance now stays IN; this records what (if anything) was trimmed.
PRPO_TRIMMED <- EXCLUDED_EXTREMES |>
  dplyr::filter(site == "PRPO", analyte %in% c("specificConductanceField","specificConductance","TDS","Na","Cl","SO4"))
message(sprintf("[plausibility] %d implausible rows excluded total; %d are PRPO high-variance grabs (site-aware gate keeps genuine saline values).",
                nrow(EXCLUDED_EXTREMES), nrow(PRPO_TRIMMED)))
# (2) rebuild the wide matrix from the PLAUSIBLE long rows only — this is the
#     single chokepoint every stats tab + the map mean pivot through, so one
#     edit here gates fit_lm / stl / glm / the choropleth colorbar at once.
D$swc_wide <- D$swc_long |>
  dplyr::filter(!implausible) |>
  dplyr::select(site, collectDate, analyte, value) |>
  tidyr::pivot_wider(names_from = analyte, values_from = value)
# (1)+(3) refresh analyte_meta so the dictionary export carries the canonical
#     unit (no more NA for UV254/UV280) and a real censored fraction.
D$analyte_meta <- D$analyte_meta |>
  dplyr::mutate(units = unname(CANON_MAP[analyte]) %|na|% units,
                pct_below = ifelse(is.finite(n) & n > 0, n_below / n, NA_real_))
# (3) per-analyte below-detection fraction, for the correlation down-weighting +
#     the clickable censoring note (Br 57%, Mn/Fe/F ~34-39% below DL).
CENSOR_MAP <- setNames(D$analyte_meta$pct_below, D$analyte_meta$analyte)

# Site choices: rich labels, only sites that actually have data
site_tbl   <- D$sites_meta |> arrange(siteName)
SITE_CHO   <- setNames(site_tbl$site, paste0(site_tbl$site, " · ", site_tbl$siteName))
DEF_SITE   <- if ("SYCA" %in% site_tbl$site) "SYCA" else site_tbl$site[1]

site_present <- function(s) sort(unique(D$swc_long$analyte[D$swc_long$site == s]))
site_span    <- function(s) {
  r <- D$sites_meta[D$sites_meta$site == s, c("first","last")]
  if (!nrow(r) || is.na(r$first)) range(D$swc_long$collectDate) else c(r$first, r$last)
}
pick_default_analytes <- function(s) {
  p <- site_present(s)
  a <- if ("specificConductanceField" %in% p) "specificConductanceField" else p[1]
  b <- if ("ANC" %in% p) "ANC" else setdiff(p, a)[1]
  c(a, b)
}
DEF_A <- pick_default_analytes(DEF_SITE); DEF_SPAN <- site_span(DEF_SITE)
DEF_SITE_B <- (setdiff(site_tbl$site, DEF_SITE))[1]   # a different site for the two-site compare

## ---- Theme ---------------------------------------------------------------
aqua_theme <- bs_theme(
  version = 5,
  # Light "desert-day" base (the app DEFAULTS to light); the brand teal #0E7C9B is
  # kept as primary for chart/cover cohesion. The dark-mode system + the dark
  # command-band / value-box info-boxes are carried in the bs_add_rules() CSS below.
  bg = "#FBFDFE", fg = "#16243a",
  primary = "#0E7C9B", secondary = "#4A6575",   # darkened for >=4.5:1 contrast on white (.scope-note token)
  success = "#3f9a52", info = "#2f8fc4", warning = "#d6a31c", danger = "#e0685a",
  base_font = font_google("Inter"), heading_font = font_google("Inter Tight"),
  "border-radius" = "0.6rem"
) |>
  bs_add_rules("
    .value-box { box-shadow: 0 1px 5px rgba(11,42,58,.08); }
    .card { box-shadow: 0 1px 5px rgba(11,42,58,.06); }
    .vb-door { cursor: pointer; transition: transform .12s ease; height:100%; }
    .vb-door > * { height:100%; }
    .vb-door:hover { transform: translateY(-2px); }
    .value-box .value-box-title { font-size:.8rem; opacity:.9; }
    .value-box .value-box-value { white-space: nowrap; }
    .info-link { color: var(--bs-secondary); opacity:.65; text-decoration:none; font-size:1rem;
                 display:inline-flex; align-items:center; padding:.15rem .3rem; line-height:1; }
    .info-link:hover { opacity:1; }
    .help-q { opacity:.55; cursor:pointer; font-size:.82em; vertical-align:middle; }
    .help-q:hover { opacity:1; }
    .value-box .help-q { color: currentColor; }
    .card-header { font-weight:600; }
    .scope-note { color: var(--bs-secondary); font-style: italic; font-size:.82rem; }
    .preset-reason { background: rgba(14,124,155,.06); border-left:3px solid var(--bs-primary);
                     padding:.5rem .75rem; border-radius:.3rem; font-size:.86rem; margin-bottom:.4rem; }

    /* --- interaction feedback (the front half of the loading loop) --- */
    /* every tappable control acknowledges the press instantly, before the server replies */
    .btn:active, .vb-door:active { transform: translateY(1px) scale(.985); filter: brightness(.96); }
    /* full-screen 'working' veil raised CLIENT-SIDE on heavy interactions (site/analyte/map…) */
    #wcOverlay { display:none; position:fixed; inset:0; z-index:6000; background:rgba(251,253,254,.84);
                 align-items:center; justify-content:center; flex-direction:column; gap:.7rem; }
    #wcOverlay .wc-spin { width:46px; height:46px; border:4px solid rgba(14,124,155,.2); border-top-color:#0E7C9B;
                          border-radius:50%; animation:wcspin .8s linear infinite; }
    /* mascot variant of the loader: drop the circle-border spinner, let the droplet bob */
    #wcOverlay .wc-spin.mascot-spin { border:none; border-radius:0; width:92px; height:auto; animation:none; }
    #wcOverlay .wc-msg { font-weight:600; color:#0E7C9B; letter-spacing:.2px; }
    @keyframes wcspin { to { transform:rotate(360deg); } }
    @media (prefers-reduced-motion: reduce) { #wcOverlay .wc-spin { animation:none; } }
    /* =====================================================================
       v2 FLOW chrome — the sidebar is gone. A slim TOP BAR (theme + How-it-
       works) sits above the summary strip; a sticky CONTEXT BAND below it
       carries the current site, a Change-site link (back to the picker map)
       and a Report download; and a SELECT PANEL on the Explore tab holds the
       relocated site/date/analyte controls beside the map.
       ===================================================================== */
    /* ---- top bar ---- */
    .top-bar { display:flex; align-items:center; justify-content:space-between;
               gap:12px; flex-wrap:wrap; padding:8px 4px 6px; margin:0 0 6px;
               border-bottom:1px solid var(--line); }
    .top-bar-brand { display:flex; align-items:center; gap:9px; min-width:0; }
    .top-bar-brand .tb-mark { font-size:20px; line-height:1; flex:none; color:var(--pine2); }
    .top-bar-brand .tb-mark .bi { color:var(--pine2); }
    .top-bar-brand .tb-title { font-weight:800; font-size:15px; color:var(--pine2);
                               letter-spacing:.2px; white-space:nowrap; }
    .top-bar-actions { display:flex; align-items:center; gap:12px; }
    .top-bar-actions .tb-help { min-height:44px; display:inline-flex; align-items:center; font-weight:700; }
    .tb-theme { display:flex; align-items:center; gap:6px; }
    .tb-theme-lab { color:var(--muted); font-size:15px; display:inline-flex; align-items:center; }
    .tb-theme .form-check, .tb-theme bslib-input-dark-mode { min-height:44px; display:inline-flex; align-items:center; }
    [data-bs-theme='dark'] .top-bar-brand .tb-title,
    [data-bs-theme='dark'] .top-bar-brand .tb-mark,
    [data-bs-theme='dark'] .top-bar-brand .tb-mark .bi { color:var(--pine); }
    @media (max-width:560px) {
      .top-bar { padding:7px 2px; }
      .top-bar-brand .tb-title { font-size:13.5px; white-space:normal; }
      .top-bar-actions .tb-help .bi + * { display:none; } /* icon-only help on tiny screens */
    }

    /* ---- context band (sticky 'what am I looking at + how do I switch') ---- */
    .context-band { position:sticky; top:0; z-index:20; background:#e7f1f5;
                    border:1px solid rgba(14,124,155,.16); border-radius:.5rem; padding:.4rem .75rem;
                    margin:.1rem 0 .7rem; font-size:.85rem; color:#0a5f78; font-weight:600;
                    display:flex; align-items:center; flex-wrap:wrap; gap:.35rem .9rem;
                    box-shadow:0 2px 6px rgba(0,0,0,.05); }
    .context-band .bi { color:#0E7C9B; flex:none; }
    .context-band .cb-site { display:inline-flex; align-items:center; gap:.35rem; }
    .context-band .cb-site-name { font-weight:800; }
    .context-band .cb-site-code { font-weight:600; opacity:.7; }
    .context-band .cb-armed { display:inline-flex; align-items:center; gap:.3rem; font-weight:600; opacity:.92; }
    .context-band .cb-actions { margin-left:auto; display:inline-flex; align-items:center; gap:.2rem; flex:none; }
    .cb-change, .cb-report { font-size:.86rem; font-weight:700; text-decoration:none;
                             color:#0E7C9B; padding:.3rem .55rem; border-radius:.4rem;
                             min-height:38px; display:inline-flex; align-items:center; gap:.25rem; }
    .cb-change:hover, .cb-report:hover { background:rgba(14,124,155,.12); color:#0a5f78; text-decoration:none; }
    .cb-report { border-left:1px solid rgba(14,124,155,.22); border-radius:0 .4rem .4rem 0; }
    [data-bs-theme='dark'] .context-band { background:#0c3350; border-color:rgba(52,198,216,.30); color:#6ee6f0; }
    [data-bs-theme='dark'] .context-band .bi { color:#34c6d8; }
    [data-bs-theme='dark'] .cb-change, [data-bs-theme='dark'] .cb-report { color:#6ee6f0; }
    [data-bs-theme='dark'] .cb-change:hover, [data-bs-theme='dark'] .cb-report:hover {
                            background:rgba(52,198,216,.16); color:#bdf2f7; }
    @media (max-width:560px) {
      .context-band .cb-armed { flex-basis:100%; order:3; }
      .context-band .cb-actions { margin-left:0; }
    }

    /* ---- relocated site-select panel (was the sidebar) ---- */
    .select-panel { max-width:980px; margin:6px auto 14px; text-align:left;
                    background:var(--bg); border:1px solid var(--line);
                    border-left:4px solid var(--pine2); border-radius:14px;
                    padding:16px 18px; box-shadow:0 4px 16px var(--shadow); }
    [data-bs-theme='dark'] .select-panel { background:linear-gradient(180deg,#0c3350 0%,var(--paper) 100%); }
    .select-panel .sp-head { font-weight:700; color:var(--pine2); font-size:13.5px;
                             display:flex; align-items:center; gap:7px; margin-bottom:12px; }
    [data-bs-theme='dark'] .select-panel .sp-head { color:var(--pine); }
    .select-panel .sp-row { display:flex; gap:16px; flex-wrap:wrap; align-items:flex-end; margin-bottom:4px; }
    .select-panel .sp-row-analytes { align-items:flex-end; }
    .select-panel .sp-field { flex:1 1 230px; min-width:190px; }
    .select-panel .sp-field-date { flex:1 1 250px; min-width:220px; }
    .select-panel .sp-field .form-group,
    .select-panel .sp-field .shiny-input-container { margin-bottom:0; width:100%; }
    .select-panel label, .select-panel .control-label { color:var(--ink); font-weight:600; font-size:13px; }
    .select-panel .sp-field .form-control,
    .select-panel .sp-field .form-select,
    .select-panel .sp-field .selectize-input { min-height:44px; }
    .select-panel .input-daterange { display:flex; gap:6px; }
    .select-panel .input-daterange .form-control { min-height:44px; }
    .select-panel .sp-date-sub { display:flex; align-items:center; flex-wrap:wrap; gap:.4rem .7rem; margin-top:.3rem; }
    .select-panel .sp-swap { flex:0 0 auto; padding-bottom:.35rem; }
    .select-panel .sp-swap .btn { min-height:44px; }
    .select-panel .sp-row-preset { align-items:flex-end; }
    .select-panel .sp-preset-note { flex:1 1 220px; padding-bottom:.55rem; }
    .select-panel .sp-armed { margin-top:.6rem; display:flex; align-items:center; flex-wrap:wrap; gap:.35rem; }
    @media (max-width:640px) {
      .select-panel { padding:14px 13px; }
      .select-panel .sp-row { gap:10px; }
      .select-panel .sp-field, .select-panel .sp-field-date { flex:1 1 100%; min-width:0; }
      .select-panel .sp-swap { width:100%; text-align:center; }
      .select-panel .sp-swap .btn { width:100%; }
    }

    /* small clickable QC pill: a flagged extreme was excluded — click to audit */
    .qc-pill { margin-left:auto; flex:none; display:inline-flex; align-items:center; gap:.3rem;
               background:rgba(214,163,28,.16); color:#9a7314; border:1px solid rgba(214,163,28,.4);
               border-radius:1rem; padding:.05rem .55rem; font-size:.76rem; font-weight:600;
               cursor:pointer; text-decoration:none; line-height:1.5; }
    .qc-pill:hover { background:rgba(214,163,28,.28); color:#7a5a10; }
    .qc-pill .bi { color:#b8860b !important; }
    [data-bs-theme='dark'] .qc-pill { background:rgba(110,230,196,.14); color:#6ee6c4;
               border-color:rgba(110,230,196,.4); }
    [data-bs-theme='dark'] .qc-pill .bi { color:#6ee6c4 !important; }

    /* (the navbar command band + value-box dark info-box treatment now live in
       the DESERT-NIGHT block below, replacing the old water-flow gradient) */

    /* =====================================================================
       DESERT-NIGHT design tokens. The app DEFAULTS to light (:root = desert-
       day), but the prominent INFO BOXES — the command-band navbar + the
       value boxes — wear the dark desert-night scheme in BOTH modes (the
       'light page, dark hero' cascade look). Toggling dark swaps the tokens.
       ===================================================================== */
    :root {
      --pine: #1a9fb0; --pine2: #0e7c9b; --terra: #2f8fc4; --cardinal: #4aa3e0;
      --gold: #c79a1c; --gold-ink: #9a7314; --sky: #2f8fc4; --green: #3f9aa0;
      --ink: #122636; --muted: #5a6a82; --bg: #eef5fb; --paper: #ffffff;
      --line: #d6e0ee; --shadow: rgba(52,198,216,.10);
    }
    [data-bs-theme='dark'] {
      --pine: #34c6d8; --pine2: #6ee6f0; --terra: #4aa3e0; --cardinal: #4aa3e0;
      --gold: #6ee6c4; --gold-ink: #6ee6c4; --sky: #4aa3e0; --green: #6ee6c4;
      --ink: #e6f4fb; --muted: #9fc2d6; --bg: #04111e; --paper: #0c3350;
      --line: rgba(255,255,255,.12); --shadow: rgba(0,0,0,.5);
    }
    /* body aurora gradients (aqua/blue), both modes */
    body {
      background:
        radial-gradient(1100px 520px at 8% -8%, rgba(52,198,216,.05), transparent 60%),
        radial-gradient(900px 460px at 102% 2%, rgba(74,163,224,.04), transparent 55%),
        linear-gradient(180deg, #eef5fb 0%, #e6eef8 100%);
      background-attachment: fixed;
    }
    [data-bs-theme='dark'] body {
      background:
        radial-gradient(1100px 520px at 8% -8%, rgba(52,198,216,.12), transparent 60%),
        radial-gradient(900px 460px at 102% 2%, rgba(74,163,224,.10), transparent 55%),
        radial-gradient(900px 600px at 50% 120%, rgba(110,230,196,.06), transparent 60%),
        linear-gradient(180deg, #04111e 0%, #020a13 100%);
    }

    /* ====================================================================== *
     *  PREMIUM DESERT-NIGHT — the navbar becomes a dark COMMAND BAND with a   *
     *  drifting starfield + teal/coral/gold nebula, in BOTH modes. Every      *
     *  animation is prefers-reduced-motion gated at the bottom.               *
     * ====================================================================== */
    @keyframes wcStars { to { background-position: 0 220px, 0 220px, 0 220px, 0 220px; } }
    @keyframes wcSheen { 0% { left:-65%; opacity:0; } 12% { opacity:.85; } 100% { left:135%; opacity:0; } }
    @keyframes wcFloat { 0%,100% { transform: translateY(0); } 50% { transform: translateY(-3px); } }

    .navbar, .bslib-page-navbar > .navbar {
      position: relative; overflow: hidden;
      background-image: radial-gradient(125% 150% at 16% -12%, #0e4a66 0%, #08233a 46%, #04111e 100%) !important;
      background-size: auto !important; animation: none !important;        /* swap the old water-flow for the command band */
      box-shadow: 0 14px 40px rgba(3,8,20,.42), inset 0 -1px 0 rgba(52,198,216,.20) !important;
    }
    .navbar > * { position: relative; z-index: 1; }
    .navbar::before {                /* drifting starfield */
      content:''; position:absolute; inset:0; z-index:0; pointer-events:none; opacity:.55;
      background-repeat: repeat; background-size: 210px 220px,210px 220px,210px 220px,210px 220px;
      background-image:
        radial-gradient(1.4px 1.4px at 30px 40px, rgba(255,255,255,.6), transparent),
        radial-gradient(1.2px 1.2px at 150px 90px, rgba(150,230,255,.5), transparent),
        radial-gradient(1.5px 1.5px at 90px 175px, rgba(74,163,224,.4), transparent),
        radial-gradient(1.1px 1.1px at 175px 28px, rgba(110,230,196,.5), transparent);
      animation: wcStars 120s linear infinite;
    }
    .navbar::after {                 /* aqua/blue/mint nebula */
      content:''; position:absolute; inset:0; z-index:0; pointer-events:none; opacity:.6;
      background:
        radial-gradient(120px 120px at 8% 40%, rgba(52,198,216,.16), transparent 70%),
        radial-gradient(150px 150px at 88% 24%, rgba(74,163,224,.12), transparent 70%),
        radial-gradient(120px 120px at 60% 96%, rgba(110,230,196,.10), transparent 70%);
    }
    .navbar .navbar-brand, .navbar .navbar-brand * { color: #6ee6f0 !important; }
    .navbar .navbar-brand .bi { color: #34c6d8 !important;
      display:inline-block; animation: wcFloat 4.5s ease-in-out infinite; }

    /* ---- DARK 'info-box' value boxes on the LIGHT page (the cascade look) ---- *
     * bslib value_box() renders as .value-box with a bg-{theme} utility. We
     * override ALL of them to the dark navy-glass command scheme with light text
     * + a per-theme accent rail, in BOTH modes, then add a sheen-sweep + lift. */
    .value-box, .bslib-value-box {
      position: relative; overflow: hidden; isolation: isolate;
      background: linear-gradient(180deg, #0c3350 0%, #04111e 100%) !important;
      border: 1px solid rgba(255,255,255,.10) !important;
      border-top: 3px solid var(--vb-accent, #34c6d8) !important;
      box-shadow: 0 12px 28px -12px rgba(3,8,20,.6) !important;
      transition: transform .2s cubic-bezier(.22,1,.36,1), box-shadow .28s ease;
    }
    /* per-theme accent rail, mapped from bslib's bg-{theme} class on the box */
    .value-box.bg-primary,   .bslib-value-box.bg-primary,   .bslib-value-box.text-bg-primary   { --vb-accent: #34c6d8; }
    .value-box.bg-secondary, .bslib-value-box.bg-secondary, .bslib-value-box.text-bg-secondary { --vb-accent: #9fc2d6; }
    .value-box.bg-success,   .bslib-value-box.bg-success,   .bslib-value-box.text-bg-success   { --vb-accent: #6ee6c4; }
    .value-box.bg-warning,   .bslib-value-box.bg-warning,   .bslib-value-box.text-bg-warning   { --vb-accent: #6ee6f0; }
    .value-box.bg-info,      .bslib-value-box.bg-info,      .bslib-value-box.text-bg-info      { --vb-accent: #4aa3e0; }
    .value-box.bg-danger,    .bslib-value-box.bg-danger,    .bslib-value-box.text-bg-danger    { --vb-accent: #fb8a7e; }
    .value-box.bg-dark,      .bslib-value-box.bg-dark,      .bslib-value-box.text-bg-dark      { --vb-accent: #6ee6f0; }
    /* force light text inside every value box (bslib's text-bg-* utilities vary) */
    .value-box, .value-box *, .bslib-value-box, .bslib-value-box * { color: #ffffff !important; }
    .value-box .value-box-title, .bslib-value-box .value-box-title { color: #9fc2d6 !important; font-weight: 600; }
    .value-box .value-box-value, .bslib-value-box .value-box-value { color: #ffffff !important; }
    .value-box .help-q, .bslib-value-box .help-q { color: #9fc2d6 !important; }
    .value-box::after {              /* keep the soft top sheen */
      content:''; position:absolute; inset:0; pointer-events:none; z-index:0;
      background: radial-gradient(130% 90% at 12% -10%, rgba(255,255,255,.10), transparent 55%); }
    .value-box > * { position: relative; z-index: 1; }
    /* sheen-sweep on hover for the clickable value-box 'doors' */
    .vb-door .value-box::before {
      content:''; position:absolute; top:0; left:-65%; width:55%; height:100%; z-index:2;
      background: linear-gradient(100deg, transparent, rgba(255,255,255,.45), transparent);
      transform: skewX(-18deg); pointer-events:none; opacity:0; }
    .vb-door:hover .value-box::before { animation: wcSheen .85s ease; }
    .value-box:hover {
      box-shadow: 0 18px 36px rgba(0,0,0,.5), 0 0 0 1px rgba(52,198,216,.32) !important; }

    @media (prefers-reduced-motion: reduce) {
      .navbar::before, .vb-door .value-box::before { animation: none !important; }
      .navbar .navbar-brand .bi { animation: none !important; }
      .value-box:hover, .vb-door:hover { transform: none; }
    }

    /* ============ in-app mascot — loader · splash guide · celebration ============ */
    .mascot { display:block; width:100%; height:auto; overflow:visible; }
    .mascot-ear-l, .mascot-ear-r { transform-box:fill-box; transform-origin:50% 88%; }
    .mascot-eyes { transform-box:fill-box; transform-origin:center; }
    @keyframes mascotBob { 0%,100% { transform: translateY(0); } 50% { transform: translateY(-5px); } }
    @keyframes mascotEarL { 0%,100% { transform: rotate(0); } 50% { transform: rotate(-9deg); } }
    @keyframes mascotEarR { 0%,100% { transform: rotate(0); } 50% { transform: rotate(9deg); } }
    @keyframes mascotBlink { 0%,92%,100% { transform: scaleY(1); } 96% { transform: scaleY(.1); } }
    /* loader: the droplet bobs + blinks while data loads */
    .load-spin.mascot-spin { font-size:0; width:92px; height:auto; margin:0 auto 6px; animation:none; }
    .mascot-spin .mascot { animation: mascotBob 1.5s ease-in-out infinite; }
    .mascot-spin .mascot-ear-l { animation: mascotEarL 1.5s ease-in-out infinite; }
    .mascot-spin .mascot-ear-r { animation: mascotEarR 1.5s ease-in-out infinite; }
    .mascot-spin .mascot-eyes { animation: mascotBlink 3.4s ease-in-out infinite; }
    /* splash guide: a friendly nudge in the corner while no site is loaded */
    .splash-guide { position:fixed; right:18px; bottom:16px; z-index:30; display:flex; align-items:flex-end; gap:6px; pointer-events:none; }
    .splash-guide .sg-mascot { width:74px; flex:none; }
    .splash-guide .sg-mascot .mascot { animation: mascotBob 2.6s ease-in-out infinite; }
    .splash-guide .sg-mascot .mascot-ear-l { animation: mascotEarL 2.6s ease-in-out infinite; }
    .splash-guide .sg-mascot .mascot-ear-r { animation: mascotEarR 2.6s ease-in-out infinite; }
    .splash-guide .sg-mascot .mascot-eyes { animation: mascotBlink 4.2s ease-in-out infinite; }
    .splash-guide .sg-bubble { margin-bottom:34px; background:var(--paper); border:1px solid var(--line); color:var(--pine);
      font-size:12.5px; font-weight:700; padding:6px 11px; border-radius:12px 12px 2px 12px;
      box-shadow:0 8px 20px -8px var(--shadow); white-space:nowrap; }
    .splash-guide.wave .sg-mascot { transform-box:fill-box; transform-origin:50% 90%; animation: mascotWave 1s ease 3; }
    @keyframes mascotWave { 0%,100% { transform: rotate(0); } 25% { transform: rotate(-10deg); } 75% { transform: rotate(10deg); } }
    /* celebration: the droplet hops up + fades on a notable find */
    .mascot-cheer { position:fixed; left:50%; bottom:7%; width:118px; z-index:5000; pointer-events:none;
      transform:translateX(-50%); animation: mascotCheerPop 1.7s ease forwards; }
    @keyframes mascotCheerPop {
      0% { opacity:0; transform:translate(-50%,42px) scale(.6); }
      20% { opacity:1; transform:translate(-50%,-12px) scale(1.06); }
      45% { transform:translate(-50%,-34px) scale(1); }
      72% { transform:translate(-50%,-24px) scale(1); }
      100% { opacity:0; transform:translate(-50%,-66px) scale(.9); }
    }
    @media (max-width:640px) { .splash-guide { display:none; } }
    @media (prefers-reduced-motion: reduce) {
      .mascot-spin .mascot, .mascot-spin .mascot-ear-l, .mascot-spin .mascot-ear-r, .mascot-spin .mascot-eyes,
      .splash-guide .sg-mascot .mascot, .splash-guide .sg-mascot .mascot-ear-l, .splash-guide .sg-mascot .mascot-ear-r,
      .splash-guide .sg-mascot .mascot-eyes, .splash-guide.wave .sg-mascot { animation:none !important; }
      .mascot-cheer { display:none; }
    }

    /* ====================================================================== *
     *  'Browse all 46 sites' — a CLOSED-by-default collapsible site list      *
     *  under the Explore map. Matches the canonical sibling pattern (Small    *
     *  Mammal .picker-list / Birds .site-browse) but wears THIS app's aqua    *
     *  accent (--pine2 #0E7C9B / --pine #1a9fb0 from the desert-night tokens).*
     * ====================================================================== */
    .site-browse { margin: 12px 0 2px; text-align: left; }
    /* kill the native disclosure triangle */
    .site-browse > summary { list-style: none; }
    .site-browse > summary::-webkit-details-marker { display: none; }
    .site-browse > summary:focus-visible { outline: none; }
    .site-browse-summary {
      display: flex; align-items: center; gap: .55rem; min-height: 40px;
      padding: .5rem .85rem; cursor: pointer; user-select: none;
      background: var(--paper); border: 1px solid var(--line); border-radius: .6rem;
      color: var(--pine2); font-weight: 600; font-size: .92rem;
      box-shadow: 0 1px 4px var(--shadow); transition: background .15s ease, border-color .15s ease;
    }
    .site-browse-summary:hover { background: var(--bg); border-color: var(--pine); }
    .site-browse > summary:focus-visible .site-browse-summary {
      border-color: var(--pine); box-shadow: 0 0 0 3px rgba(14,124,155,.25); }
    .site-browse-summary .bi { color: var(--pine2); }
    .sb-chevron { display: inline-flex; margin-left: auto; color: var(--pine);
      opacity: .85; transition: transform .2s ease; }
    .site-browse[open] .sb-chevron { transform: rotate(180deg); }
    .site-browse-grid {
      margin-top: 8px; max-height: 340px; overflow-y: auto;
      display: grid; grid-template-columns: repeat(3, 1fr); gap: 4px 10px;
      padding: 8px; border: 1px solid var(--line); border-radius: .6rem;
      background: var(--paper);
    }
    .site-browse-link {
      display: block; padding: 6px 9px; border-radius: 8px; min-height: 38px;
      color: var(--ink); font-size: 13px; text-decoration: none; }
    .site-browse-link:hover { background: var(--bg); color: var(--pine2); }
    .site-browse-link b { color: var(--pine2); }
    .sb-meta { color: var(--muted); font-size: 11.5px; }
    @media (max-width: 900px) { .site-browse-grid { grid-template-columns: repeat(2, 1fr); } }
    @media (max-width: 560px) { .site-browse-grid { grid-template-columns: 1fr; } }

    /* closed-by-default 'Show all analytes' wrapper for the full correlation DT */
    .cor-table-details { margin: 6px 0 2px; }
    .cor-table-details > summary { list-style: none; }
    .cor-table-details > summary::-webkit-details-marker { display: none; }
    .cor-table-details[open] .sb-chevron { transform: rotate(180deg); }

    /* in-app sibling links (About modal) */
    .sibling-chip { display: inline-flex; align-items: center; padding: .2rem .6rem;
      border: 1px solid var(--line); border-radius: 1rem; font-size: .82rem;
      color: var(--pine2); text-decoration: none; background: var(--paper); }
    .sibling-chip:hover { background: var(--bg); border-color: var(--pine); color: var(--pine2); }
  ")

## ---- Per-tab info-modal content (progressive disclosure) -----------------
INFO <- list(
  compare = list("How the time series works", HTML(
    "Both analytes are matched on collection date and drawn on a shared time axis. <b>Normalized</b>
     mode standardizes each series to mean 0, SD 1 so you compare <i>shape</i> honestly even when the
     units differ. <b>Dual axis (raw)</b> shows the original values on two independent scales, which is useful,
     but the proximity of two lines on independent axes does <i>not</i> by itself imply correlation
     (use the Relationship tab to test that). Open markers are below the analytical detection limit.
     Source: NEON SWC DP1.20093.001 (external lab + field probe).")),
  relationship = list("How the regression works", HTML(
    "An ordinary least-squares line fit to date-paired samples. <b>R²</b> is the share of variance in the
     secondary analyte explained by the main one; the <b>p-value</b> tests whether the slope differs from
     zero; the shaded band is the 95% confidence interval. These are repeated measures through time, so
     observations are not independent, so temporal autocorrelation makes the p-value optimistic (a lag-1
     autocorrelation flag is shown). Correlation is not causation: both analytes may track a third driver
     such as discharge or season.")),
  correlations = list("How the correlation screen works", HTML(
    "Each coefficient is computed only on dates where both analytes were sampled (n shown per row).
     <b>Spearman ρ</b> is the headline because water-chemistry data are typically skewed and relationships
     are often monotonic-but-nonlinear; Pearson r (linear) is shown alongside. This screens ~34 analytes
     at once, so some low p-values will appear by chance (multiple comparisons); treat results as
     hypothesis-generating, never confirmatory. Rows with fewer than 8 paired samples are flagged
     unreliable and greyed.")),
  seasonal = list("How the seasonal view works", HTML(
    "The main analyte is aggregated to monthly means of the <i>real</i> measured series, then split by STL
     (Seasonal-Trend decomposition using Loess) into a smooth <b>trend</b>, a repeating <b>seasonal</b>
     cycle, and a <b>remainder</b>. Requires ≥ 24 months across ≥ 2 years. NEON grab samples are roughly
     monthly with gaps, so <b>interior months with no sample are linearly interpolated before decomposing</b>
     (the count is stamped on the chart); long gaps appear as smooth ramps and can understate real
     variability. This is descriptive of the record, not a calibrated forecast.")),
  predictor = list("How the predictor works", HTML(
    "A generalized linear model (<code>glm</code>) trained on this site's bundled records, estimating the
     main analyte from its three most-correlated analytes. Validated with repeated k-fold cross-validation;
     RMSE (root-mean-square error, in the analyte's units) is the typical out-of-sample miss, shown next
     to a mean-only baseline so you can judge whether the model adds anything. Because the 3 predictors are
     chosen on the full record, the cross-validated RMSE is mildly <b>optimistic</b>; treat it as a floor
     on the true error. Move the sliders within each predictor's observed range for a live estimate. This
     is an interpolation aid, not a calibrated sensor.")),
  data = list("About this table", HTML(
    "The wide table underlying every tab: one row per collection date for the selected site and range,
     one column per analyte. Units come from NEON; blank cells mean that analyte was not measured on that
     date. Use the buttons to download exactly the current site and range."))
)

info_link <- function(id) actionLink(id, bs_icon("info-circle"), class = "info-link",
                                      title = "How is this computed?")

## ---- Plain-language glossary + a (?) popover helper for jargon ------------
GLOSSARY <- list(
  spearman = HTML("<b>Spearman's rho (ρ)</b> measures whether two analytes <b>rise and fall together</b>,
    even if not in a straight line. <b>+1</b> = move together perfectly, <b>0</b> = no relationship,
    <b>−1</b> = one goes up as the other goes down. We default to it (over Pearson) because water-chemistry
    values are usually skewed."),
  pearson  = HTML("<b>Pearson's r</b> measures how close two analytes are to a <b>straight-line</b>
    relationship, on the same −1 to +1 scale as Spearman, but it assumes the link is linear."),
  n        = HTML("<b>n</b> is the number of dates where <b>both</b> analytes were measured, the sample
    size behind the number. Bigger n = more trustworthy."),
  r2       = HTML("<b>R²</b> is the share of one analyte's variation explained by the other (0–100%).
    Higher = the line fits the points better."),
  pval     = HTML("<b>p-value</b> is roughly the chance you'd see a relationship this strong if there were
    really none. Small (e.g. &lt; 0.05) suggests it isn't a fluke, though it's optimistic for samples
    repeated through time."),
  rmse     = HTML("<b>RMSE</b> is the model's typical miss, in the analyte's units. <b>Skill</b> compares it
    to simply guessing the average: <b>+50%</b> means the model's error is half that naive baseline (and a
    negative skill means it's worse than guessing the average)."),
  stl      = HTML("<b>STL</b> splits the monthly series into a slow <b>trend</b>, a repeating <b>seasonal</b>
    cycle, and leftover noise, computed from the real measured data."),
  censor   = HTML("<b>Below detection (BDL)</b> means the lab couldn't measure the analyte above its
    detection limit. We keep the reported number (drawn as an <b>open marker</b>) and never substitute a
    zero. But when a large share of an analyte's samples are below detection, a correlation is dominated by
    detection-limit <b>ties</b>, not real chemistry, so we <b>grey out</b> any analyte with &gt; 25% below
    detection and treat it as <b>exploratory only</b> (Helsel 2012).")
)
# small (?) icon that pops a plain-language explanation on click/hover
help_pop <- function(key, ttl = NULL)
  popover(bs_icon("question-circle", class = "help-q"), GLOSSARY[[key]], title = ttl, placement = "top")

## ---- JS: onboarding localStorage + phone width ---------------------------
APP_JS <- HTML("
$(document).on('shiny:connected', function(){
  try { Shiny.setInputValue('welcome_seen', localStorage.getItem('neon_seen'), {priority:'event'}); }
  catch(e){ Shiny.setInputValue('welcome_seen', null, {priority:'event'}); }
  Shiny.setInputValue('client_w', window.innerWidth, {priority:'event'});
});
Shiny.addCustomMessageHandler('neon_set_seen', function(x){ try{ localStorage.setItem('neon_seen','1'); }catch(e){} });

/* ---- kickMaps: re-measure the plotly site map after 'Change site' --------
   'Change site' navigates back to the Explore tab; the map was rendered while
   that tab may have been hidden (0px wide), so dispatch a window 'resize' across
   several frames so Plotly re-fits the settled width instead of painting half-
   width. Mirrors the flagship Small Mammal kickMaps handler (leaflet there). */
Shiny.addCustomMessageHandler('kickMaps', function(){
  var kick = function(){ try { window.dispatchEvent(new Event('resize')); } catch(e){} };
  if (window.requestAnimationFrame) requestAnimationFrame(kick);
  [80, 250, 500, 900].forEach(function(t){ setTimeout(kick, t); });
});
window.addEventListener('resize', function(){ clearTimeout(window.__rt);
  window.__rt=setTimeout(function(){ Shiny.setInputValue('client_w', window.innerWidth, {priority:'event'}); },250); });

/* ---- client-side 'working' veil ----------------------------------------
   A site/analyte/date/map change kicks off recompute on the worker; the
   per-output withSpinner can't paint until that work starts, so raise a veil
   on the GESTURE and clear it when Shiny goes idle (work finished). */
var wcSafety=null;
function wcVeilOn(){ var o=document.getElementById('wcOverlay'); if(!o) return;
  o.style.display='flex';
  if(navigator.vibrate){ try{ navigator.vibrate(12); }catch(e){} }
  clearTimeout(wcSafety); wcSafety=setTimeout(wcVeilOff, 30000); }   /* never stick */
function wcVeilOff(){ var o=document.getElementById('wcOverlay'); if(o) o.style.display='none'; clearTimeout(wcSafety); }
/* color_mode (the dark-mode toggle) is intentionally NOT here: it only re-themes
   the page, it never re-binds data, so raising the working veil on it was a false
   load. The veil now fires only on genuinely data-bound changes. */
var WC_HEAVY=['site','analyte_main','analyte_secondary','dates','preset','cor_method','ts_mode','site_b','swap','full_range'];
$(document).on('shiny:inputchanged', function(e){ if(WC_HEAVY.indexOf(e.name)>=0) wcVeilOn(); });
/* a map-dot tap now opens a lightweight Explore|About choice card (no recompute),
   so the veil is raised by the actual load instead (input$site is in WC_HEAVY). */
$(document).on('shiny:idle', function(){ wcVeilOff(); });

/* ---- mascot celebration: the droplet hops up + fades on a notable find ----
   This app has no confetti yet, so mascotCheer() is wired but unused; it clones
   the loader mascot into a transient .mascot-cheer, ready for any future call. */
function mascotCheer(big){
  try {
    if (window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;
    var src = document.querySelector('#wcOverlay .mascot');
    if (!src) return;
    var wrap = document.createElement('div');
    wrap.className = 'mascot-cheer';
    wrap.appendChild(src.cloneNode(true));
    document.body.appendChild(wrap);
    setTimeout(function(){ if (wrap.parentNode) wrap.parentNode.removeChild(wrap); }, 1700);
  } catch (e) {}
}

/* ---- first-visit: the splash mascot waves hello once (localStorage-gated) ---- */
document.addEventListener('DOMContentLoaded', function(){
  try {
    if (localStorage.getItem('smtMascotSeen') === '1') return;
    var g = document.querySelector('.splash-guide');
    if (g) {
      g.classList.add('wave');
      localStorage.setItem('smtMascotSeen', '1');
      setTimeout(function(){ g.classList.remove('wave'); }, 3300);
    }
  } catch (e) {}
});
/* dismiss the corner nudge once the user actually engages a site (so it isn't permanent) */
$(document).on('shiny:inputchanged', function(e){
  if (e.name === 'site' || e.name === 'preset') {
    var g = document.getElementById('splashGuide'); if (g) g.style.display = 'none';
  }
});
$(document).on('mousedown', '#map', function(){
  var g = document.getElementById('splashGuide'); if (g) g.style.display = 'none';
});
")

# The app mascot — a flat (no-gradient, no-id so it's safely reusable) cute water
# droplet in the aqua accent. Used as the loading spinner, the splash guide, and
# the celebration hop. The eyes are classed so the CSS can blink them.
MASCOT_CRITTER <- htmltools::HTML(paste0(
  '<svg class="mascot" viewBox="0 0 120 120" aria-hidden="true">',
  '<path d="M60,16 C36,52 28,72 60,98 C92,72 84,52 60,16 Z" fill="#46c6da"/>',
  '<ellipse cx="48" cy="54" rx="6" ry="11" fill="#ffffff" opacity=".4" transform="rotate(-18 48 54)"/>',
  '<g class="mascot-eyes"><circle cx="50" cy="64" r="6.5" fill="#0a2230"/><circle cx="70" cy="64" r="6.5" fill="#0a2230"/>',
  '<circle cx="48" cy="61.5" r="2.4" fill="#ffffff"/><circle cx="68" cy="61.5" r="2.4" fill="#ffffff"/></g>',
  '</svg>'))

#======================================================================
# UI
#======================================================================
ui <- page_fillable(
  title = tagList(bs_icon("droplet-half"), " NEON Water Chemistry · Analyte Viewer"),
  theme = aqua_theme,
  window_title = "NEON Analyte Viewer",
  # Flow content naturally instead of filling the viewport. A fillable page collapses
  # its card bodies to ~0 height when served in an indefinite-height container (Connect
  # Cloud / iframe), which hid every chart on the deployed host. Natural flow + each
  # chart's explicit height = charts always render and the page just scrolls.
  fillable = FALSE,
  tags$head(tags$script(APP_JS),
            tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),

  # client-side 'working' overlay (shown on heavy interactions, hidden on idle)
  tags$div(id = "wcOverlay", tags$div(class = "wc-spin mascot-spin", MASCOT_CRITTER), tags$div(class = "wc-msg", "Updating…")),

  # in-app mascot splash guide — a friendly corner nudge; first-visit wave + dismiss on first interaction (JS below)
  tags$div(id = "splashGuide", class = "splash-guide",
           tags$div(class = "sg-bubble", "Pick a site to start"),
           tags$div(class = "sg-mascot", MASCOT_CRITTER)),

  # ---- persistent top control bar (theme + about/help) -------------------
  # v2 flow: the sidebar is gone. The picker map on the Explore tab IS how you
  # select a site (its select panel carries the same site/date/analyte ids).
  # The two controls that must stay reachable everywhere — the theme toggle and
  # the About/How-it-works dialog — live in this slim top-right bar.
  div(class = "top-bar",
    div(class = "top-bar-brand",
      tags$span(class = "tb-mark", bs_icon("droplet-half")),
      tags$span(class = "tb-title", "Water Chemistry · Analyte Viewer")),
    div(class = "top-bar-actions",
      actionButton("about", tagList(bs_icon("question-circle"), " How it works"),
                   class = "btn-outline-secondary btn-sm tb-help"),
      div(class = "tb-theme",
        tags$span(class = "tb-theme-lab", bs_icon("circle-half")),
        input_dark_mode(id = "color_mode", mode = "light")))   # DEFAULT LIGHT (dark info-boxes on a light page)
  ),

  # Summary strip
  uiOutput("summary_strip"),

  # server-rendered context band: current site + analytes, with a "Change site"
  # control (jumps back to the Explore picker map) and a "Report" download. This
  # replaces the old sidebar's role of "what am I looking at + how do I switch".
  uiOutput("context_band"),

  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      "Explore", icon = bs_icon("geo-alt"),
      # ---- relocated select panel (was the sidebar) ----------------------
      # Same input ids the server's selection / load / preset logic depend on
      # (site, dates, full_range, analyte_main, swap, analyte_secondary, preset).
      # Tapping a map dot is the primary path; this panel is the by-name path and
      # the place to set the date window + analyte pair. Same ids => server is
      # untouched.
      div(class = "select-panel",
        div(class = "sp-head", bs_icon("sliders"),
            " Pick a site by name, set the date window, and choose two analytes"),
        div(class = "sp-row",
          div(class = "sp-field",
            selectizeInput("site", label = tagList(bs_icon("pin-map-fill"), " Field site"),
                           choices = SITE_CHO, selected = DEF_SITE, width = "100%",
                           options = list(placeholder = "Type to search sites…"))),
          div(class = "sp-field sp-field-date",
            dateRangeInput("dates", label = tagList(bs_icon("calendar3"), " Date range"),
                           format = "yyyy-mm", startview = "year",
                           start = DEF_SPAN[1], end = DEF_SPAN[2]),
            div(class = "sp-date-sub",
              actionLink("full_range", "Use full record", class = "info-link"),
              span(class = "scope-note", textOutput("date_hint", inline = TRUE))))),
        div(class = "sp-row sp-row-analytes",
          div(class = "sp-field",
            selectizeInput("analyte_main", label = tagList(bs_icon("droplet"), " Main analyte"),
                           choices = analyte_choices(site_present(DEF_SITE)),
                           selected = DEF_A[1], width = "100%")),
          div(class = "sp-swap",
            actionButton("swap", tagList(bs_icon("arrow-left-right"), " swap"),
                         class = "btn-sm btn-outline-secondary")),
          div(class = "sp-field",
            selectizeInput("analyte_secondary", label = tagList(bs_icon("droplet-half"), " Compare against"),
                           choices = analyte_choices(site_present(DEF_SITE)),
                           selected = DEF_A[2], width = "100%"))),
        div(class = "sp-row sp-row-preset",
          div(class = "sp-field",
            selectInput("preset", label = tagList(bs_icon("lightbulb"), " Jump to a preset comparison"),
                        choices = c("Custom…" = "", setNames(names(PRESETS), names(PRESETS))),
                        selected = names(PRESETS)[1], width = "100%")),
          div(class = "sp-preset-note scope-note", "Sets both analytes to a meaningful pair.")),
        div(class = "sp-armed scope-note", bs_icon("crosshair"), " ", textOutput("armed", inline = TRUE),
            uiOutput("qc_chip", inline = TRUE))),

      card(full_screen = TRUE,
        card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                        span("Pick a site to explore"),
                        span(class = "scope-note d-none d-md-inline",
                             "Markers are coloured by the main analyte's site average. Tap any marker"))),
        withSpinner(plotlyOutput("map", height = 540), type = 8, color = "#0E7C9B", hide.ui = TRUE),
        # Browse-all-sites — a CLOSED-by-default collapsible list under the map.
        # Each row sets input$pickFromList = <site code>; the server observer
        # (same path as the map click) selects that site and jumps to Compare.
        tags$details(class = "site-browse",
          tags$summary(
            tags$span(class = "site-browse-summary",
              bs_icon("list-ul"),
              tags$span(paste0("Browse all ", nrow(site_tbl), " sites")),
              tags$span(class = "sb-chevron", bs_icon("chevron-down")))),
          div(class = "site-browse-grid",
            lapply(seq_len(nrow(site_tbl)), function(i)
              tags$a(class = "site-browse-link", href = "#",
                onclick = sprintf(
                  "if(window.wcVeilOn)wcVeilOn();Shiny.setInputValue('pickFromList','%s',{priority:'event'});return false;",
                  site_tbl$site[i]),
                tags$b(site_tbl$site[i]),
                sprintf(" · %s ", site_tbl$siteName[i]),
                tags$span(class = "sb-meta",
                  paste0(site_tbl$state[i] %|na|% site_tbl$domain[i] %|na|% "NEON",
                         " · ", format(ifelse(is.na(site_tbl$n_obs[i]), 0L, site_tbl$n_obs[i]),
                                       big.mark = ","), " obs")))))),
        card_footer(class = "scope-note", uiOutput("map_footer")))
    ),
    nav_panel(
      "Compare", icon = bs_icon("graph-up"),
      card(full_screen = TRUE,
        card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                        span("Two analytes through time"),
                        div(class = "d-flex align-items-center gap-2",
                            radioButtons("ts_mode", NULL, inline = TRUE,
                              choices = c("Normalized" = "norm", "Dual axis (raw)" = "dual"),
                              selected = "norm"), info_link("info_compare")))),
        uiOutput("preset_reason"),
        uiOutput("ts_note"),
        withSpinner(plotlyOutput("ts", height = 440), type = 8, color = "#0E7C9B", hide.ui = TRUE),
        card_footer(class = "scope-note",
          span(textOutput("ts_footer", inline = TRUE)), " · ",
          actionLink("goto_seasonal", "see its seasonal pattern →", class = "info-link")))
    ),
    nav_panel(
      "Relationship", icon = bs_icon("rulers"),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(7, 5)),
        card(full_screen = TRUE,
          card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                          span("Regression"), info_link("info_relationship"))),
          withSpinner(plotlyOutput("reg", height = 420), type = 8, color = "#0E7C9B", hide.ui = TRUE)),
        card(card_header("Fit statistics"),
          uiOutput("reg_stats"),
          card_footer(class = "scope-note",
            "OLS on date-paired samples. Correlation ≠ causation; repeated-measures p-values are optimistic.")))
    ),
    nav_panel(
      "Correlations", icon = bs_icon("bar-chart-steps"),
      card(full_screen = TRUE,
        card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                        span("Main analyte vs every other analyte"),
                        div(class = "d-flex align-items-center gap-2",
                            checkboxInput("cor_bh", "BH q", value = FALSE, width = "auto"),
                            radioButtons("cor_method", NULL, inline = TRUE,
                              choices = c("Spearman" = "spearman", "Pearson" = "pearson"),
                              selected = "spearman"),
                            help_pop("spearman", "Spearman vs Pearson"),
                            help_pop("censor", "Below-detection handling"),
                            info_link("info_correlations")))),
        withSpinner(plotlyOutput("cor_lolli", height = 540), type = 8, color = "#0E7C9B", hide.ui = TRUE),
        tags$details(class = "cor-table-details",
          tags$summary(class = "site-browse-summary",
            bs_icon("table"), tags$span("Show all analytes"),
            tags$span(class = "sb-chevron", bs_icon("chevron-down"))),
          div(style = "min-height:340px; overflow-x:auto",
              withSpinner(DTOutput("cor_table"), type = 8, color = "#0E7C9B", hide.ui = TRUE))),
        card_footer(class = "scope-note", HTML(
          "Top 18 shown above (open <b>Show all analytes</b> for the full table). <b style='color:#0E7C9B'>Teal</b> = reliable,
           <b style='color:#9aa0a6'>grey</b> = under 8 paired samples or &gt; 25% below detection. Computed on
           co-sampled dates only; screening many analytes at once inflates chance findings, so treat them as hypothesis-generating,
           not confirmatory. Toggle <b>BH q</b> for Benjamini-Hochberg false-discovery-adjusted p-values.")))
    ),
    nav_panel(
      "Seasonal pattern", icon = bs_icon("calendar3"),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
        card(full_screen = TRUE,
          card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                          span("Monthly climatology"), info_link("info_seasonal"))),
          withSpinner(plotlyOutput("clim", height = 380), type = 8, color = "#0E7C9B", hide.ui = TRUE)),
        card(full_screen = TRUE,
          card_header("Seasonal-trend decomposition (STL)"),
          withSpinner(plotlyOutput("stl", height = 430), type = 8, color = "#0E7C9B", hide.ui = TRUE),
          card_footer(class = "scope-note",
            "Descriptive of the real monthly record, not a calibrated forecast. Needs ≥ 24 months.")))
    ),
    nav_panel(
      "Predictor", icon = bs_icon("cpu"),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(5, 7)),
        card(card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                             span("Estimate the main analyte"), info_link("info_predictor"))),
          uiOutput("pred_intro"), uiOutput("pred_sliders")),
        card(card_header("Prediction"),
          uiOutput("pred_out"),
          withSpinner(plotlyOutput("pred_ctx", height = 240), type = 8, color = "#0E7C9B", hide.ui = TRUE),
          card_footer(class = "scope-note",
            "glm on the 3 best-correlated analytes; cross-validated RMSE shown. Interpolation aid, not a sensor.")))
    ),
    nav_panel(
      "Two sites", icon = bs_icon("signpost-split"),
      card(full_screen = TRUE,
        card_header(div(class = "d-flex justify-content-between align-items-center gap-3 flex-wrap",
          span("Same analyte, two sites"),
          div(class = "d-flex align-items-center gap-2",
              span(class = "scope-note", "vs"),
              selectizeInput("site_b", NULL, choices = SITE_CHO, selected = DEF_SITE_B, width = "240px")))),
        uiOutput("two_sites_note"),
        withSpinner(plotlyOutput("two_sites", height = 480), type = 8, color = "#0E7C9B", hide.ui = TRUE),
        card_footer(class = "scope-note", "The main analyte at the selected site vs a second site, over the same date window."))
    ),
    nav_panel(
      "Data", icon = bs_icon("table"),
      card(full_screen = TRUE,
        card_header(div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          span("Data table & downloads"),
          div(class = "btn-group btn-group-sm",
              downloadButton("dl_report", "PDF report", class = "btn-sm btn-primary"),
              downloadButton("dl_long", "Tidy CSV", class = "btn-sm btn-outline-primary"),
              downloadButton("dl_wide", "Wide CSV", class = "btn-sm btn-outline-secondary"),
              downloadButton("dl_dict", "Dictionary", class = "btn-sm btn-outline-secondary"),
              downloadButton("dl_codebook", "Codebook", class = "btn-sm btn-outline-secondary"),
              info_link("info_data")))),
        withSpinner(DTOutput("data_table"), type = 8, color = "#0E7C9B", hide.ui = TRUE))
    )
  )
)

#======================================================================
# SERVER
#======================================================================
server <- function(input, output, session) {
  mode    <- reactive(input$color_mode %||% "light")
  narrow  <- reactive(isTRUE((input$client_w %||% 1200) < 700))   # phone -> bigger chart text
  dates_d <- reactive(input$dates)        # dateRangeInput fires on calendar close; no debounce needed
  main_a  <- reactive(input$analyte_main)
  sec_a   <- reactive(input$analyte_secondary)

  ## ---- Onboarding modal (once) ----
  observeEvent(input$welcome_seen, once = TRUE, ignoreNULL = FALSE, {
    if (!identical(input$welcome_seen, "1")) {
      showModal(modalDialog(
        title = "Welcome to the NEON Analyte Viewer", easyClose = TRUE,
        tags$p("Compare two water-chemistry analytes at any NEON aquatic field site, then explore how they relate over time."),
        tags$ol(
          tags$li(HTML("<b>Tap a site on the map</b> (coloured by the analyte), or pick one by name in the panel above it, to begin.")),
          tags$li(HTML("Choose a <b>main analyte</b> and one to <b>compare</b> it against, or start from a preset.")),
          tags$li(HTML("Explore the tabs: time series, seasonal pattern, a predictor, relationships, correlations, and two-site comparisons. Use <b>Change site</b> in the band up top to return to the map at any time."))),
        tags$p(HTML("Loaded with <b>real NEON Surface Water Chemistry data</b> (product DP1.20093.001), bundled with the app so it is ready to use.")),
        checkboxInput("dont_show", "Don't show this again", FALSE),
        footer = actionButton("start", "Start", class = "btn-primary")))
    }
  })
  observeEvent(input$start, {
    if (isTRUE(input$dont_show)) session$sendCustomMessage("neon_set_seen", TRUE)
    removeModal()
  })

  ## ---- Info modals ----
  lapply(names(INFO), function(k) {
    observeEvent(input[[paste0("info_", k)]], {
      showModal(modalDialog(title = INFO[[k]][[1]], INFO[[k]][[2]], easyClose = TRUE,
                            footer = modalButton("Close")))
    })
  })
  observeEvent(input$about, {
    showModal(modalDialog(title = "About this app & data", easyClose = TRUE, size = "l",
      footer = modalButton("Close"),
      HTML(sprintf(
        "<p>This app compares <b>analyte concentrations</b> from the NEON
         <a href='https://data.neonscience.org/data-products/DP1.20093.001' target='_blank'>Surface Water
         Chemistry</a> product across aquatic field sites. Pick a site, a date range, and two analytes;
         then explore the time series, their relationship, a correlation screen, seasonality, and a
         glm-based predictor.</p>
         <p><b>Data:</b> %s observations · %s sites · %s analytes, pulled directly from the NEON API and
         bundled as a read-only dataset (built %s, data through %s). The app reads this bundled data rather than downloading from NEON at runtime.</p>
         %s
         <p class='scope-note'>Honesty notes: every statistic shows its sample size; correlations default to
         Spearman with an n ≥ 8 reliability flag; the seasonal view is a real STL decomposition of the
         monthly record (not a fabricated forecast); below-detection values are flagged, not hidden.</p>",
        format(D$built$n_obs %||% 0, big.mark = ","), D$built$n_sites, D$built$n_analytes,
        substr(D$built$when %||% "", 1, 10), D$built$data_through %||% "—",
        if (isTRUE(D$built$partial)) "<p class='scope-note' style='color:#C98A1E'><b>⚠ Partial dataset:</b> built from cached sites only while the full 34-site pull was in progress; counts reflect available data, not the complete product.</p>" else "")),
      sibling_block(),
      tags$hr(),
      tags$p(class = "scope-note", style = "margin-bottom:.2rem",
        HTML("Built by <b>Desert Data Labs</b> · Tucson, Arizona · ",
             "<a href='mailto:desertdatalabs@gmail.com'>desertdatalabs@gmail.com</a>")),
      tags$p(class = "scope-note", style = "font-size:.78rem; opacity:.85",
        "Not affiliated with NEON, Battelle, or the National Science Foundation. NEON data are provided under the NEON data policy.")))
  })

  ## ---- Preset -> analytes (only members the site actually has) ----
  observeEvent(input$preset, {
    if (nzchar(input$preset %||% "")) {
      pr <- PRESETS[[input$preset]]; pres <- site_present(input$site)
      if (pr[1] %in% pres) updateSelectizeInput(session, "analyte_main", selected = pr[1])
      if (pr[2] %in% pres) updateSelectizeInput(session, "analyte_secondary", selected = pr[2])
    }
  })

  ## ---- Site change -> analyte choices + date span ----
  observeEvent(input$site, {
    p <- site_present(input$site)
    ch <- analyte_choices(p)
    keep_main <- if (main_a() %in% p) main_a() else pick_default_analytes(input$site)[1]
    keep_sec  <- if (sec_a()  %in% p) sec_a()  else pick_default_analytes(input$site)[2]
    updateSelectizeInput(session, "analyte_main", choices = ch, selected = keep_main)
    updateSelectizeInput(session, "analyte_secondary", choices = ch, selected = keep_sec)
    sp <- site_span(input$site)
    updateDateRangeInput(session, "dates", start = sp[1], end = sp[2])
  }, ignoreInit = TRUE)

  ## ---- Keep the preset dropdown honest: blank it when the analytes no longer match ----
  observeEvent(list(main_a(), sec_a()), {
    if (nzchar(input$preset %||% "") && !identical(c(main_a(), sec_a()), unname(PRESETS[[input$preset]])))
      updateSelectInput(session, "preset", selected = "")
  }, ignoreInit = TRUE)

  ## ---- Swap main <-> secondary ----
  observeEvent(input$swap, {
    a <- main_a(); b <- sec_a()
    updateSelectizeInput(session, "analyte_main", selected = b)
    updateSelectizeInput(session, "analyte_secondary", selected = a)
  })
  output$date_hint <- renderText({
    sp <- site_span(input$site)
    paste0(input$site, " record: ", format(sp[1], "%Y-%m"), " to ", format(sp[2], "%Y-%m"))
  })

  observeEvent(input$full_range, {
    sp <- site_span(input$site)
    updateDateRangeInput(session, "dates", start = sp[1], end = sp[2])
  })

  ## ---- Core reactives ----
  # All raw-series reactives drop implausible extremes (the gate) so no single
  # artifact blows out a z-score, an axis, a monthly mean, or the STL trend; the
  # excluded values are surfaced via the QC chip, never silently — and stay in
  # the raw long-CSV export (implausible_extreme flag) for audit.
  sel_long <- reactive({
    req(input$site, main_a(), sec_a()); d <- dates_d(); req(length(d) == 2)
    D$swc_long |> filter(site == input$site, analyte %in% c(main_a(), sec_a()),
                         collectDate >= d[1], collectDate <= d[2], !implausible)
  }) |> bindCache(input$site, main_a(), sec_a(), dates_d())

  wide_site <- reactive({
    req(input$site); d <- dates_d(); req(length(d) == 2)
    D$swc_wide |> filter(site == input$site, collectDate >= d[1], collectDate <= d[2])
  }) |> bindCache(input$site, dates_d())

  sel_pair <- reactive({
    w <- wide_site(); req(main_a() %in% names(w), sec_a() %in% names(w))
    if (identical(main_a(), sec_a())) return(tibble(collectDate = as.Date(character()), x = numeric(), y = numeric()))
    tibble(collectDate = w$collectDate,
           x = suppressWarnings(as.numeric(w[[main_a()]])),
           y = suppressWarnings(as.numeric(w[[sec_a()]]))) |>
      filter(is.finite(x), is.finite(y))
  })

  # one Pearson correlation table per (site, main, range) — reused by the table and the predictor
  cor_base <- reactive({
    correlation_table(wide_site() |> select(-collectDate, -dplyr::any_of("site")),
                      main_a(), method = "pearson", min_n = 8, censor_map = CENSOR_MAP)
  }) |> bindCache(input$site, main_a(), dates_d())

  preset_match <- reactive({
    cur <- c(main_a(), sec_a())
    hit <- which(vapply(PRESETS, function(p) all(p == cur), logical(1)))
    if (length(hit)) names(PRESETS)[hit[1]] else NA_character_
  })

  armed_txt <- reactive({
    sp <- dates_d()
    paste0(input$site, " · ", format(sp[1], "%Y"), "–", format(sp[2], "%Y"), " · ",
           analyte_display(main_a()), " vs ", analyte_display(sec_a()))
  })
  output$armed     <- renderText(armed_txt())
  output$armed_top <- renderText(armed_txt())

  ## ---- Context band: current site + analytes + Change-site + Report -------
  # Replaces the old sidebar's "what am I looking at / how do I switch" role.
  # "Change site" returns to the Explore picker map; "Report" streams the PDF
  # site report (same content as the Data-tab download, via output$report_band).
  output$context_band <- renderUI({
    sm <- D$sites_meta[D$sites_meta$site == input$site, ]
    div(class = "context-band",
      div(class = "cb-site",
        bs_icon("geo-alt-fill"),
        tags$span(class = "cb-site-name", sm$siteName %||% input$site),
        tags$span(class = "cb-site-code", paste0(" (", input$site, ")"))),
      div(class = "cb-armed", bs_icon("crosshair"), " ", textOutput("armed_top", inline = TRUE)),
      div(class = "cb-actions",
        actionLink("changeSite", tagList(bs_icon("arrow-left-circle"), " Change site"),
                   class = "cb-change"),
        downloadLink("report_band", tagList(bs_icon("file-earmark-arrow-down"), " Report"),
                     class = "cb-report")),
      # clickable QC marker — only when this site/analyte owns an excluded extreme
      uiOutput("qc_chip_band", inline = TRUE))
  })

  # "Change site" -> jump to the Explore tab (the picker map + select panel),
  # then nudge the plotly map to re-measure into the now-visible width so it
  # never paints half-width (the flagship kickMaps multi-frame resize fix).
  observeEvent(input$changeSite, {
    nav_select("main_tabs", "Explore")
    session$sendCustomMessage("kickMaps", list())
  })

  ## ---- Plausibility audit: clickable chip + modal (no inline caveat text) ----
  # Relevant when the current site, or either selected analyte, owns an excluded
  # extreme — i.e. when a gate is actually affecting what the user is viewing.
  qc_relevant <- reactive({
    if (!nrow(EXCLUDED_EXTREMES)) return(EXCLUDED_EXTREMES[0, ])
    EXCLUDED_EXTREMES |>
      dplyr::filter(site == input$site | analyte %in% c(main_a(), sec_a()))
  })
  qc_pill_ui <- function(link_id) {
    rel <- qc_relevant(); if (!nrow(rel)) return(NULL)
    n <- nrow(rel)
    actionLink(link_id, class = "qc-pill",
      title = "An implausible extreme was excluded from the fits, the seasonal trend, the predictor and the map. Click to audit",
      tagList(bs_icon("exclamation-triangle-fill"),
              sprintf("%d extreme value%s excluded", n, if (n > 1) "s" else "")))
  }
  output$qc_chip      <- renderUI(qc_pill_ui("qc_audit"))
  output$qc_chip_band <- renderUI(qc_pill_ui("qc_audit_band"))
  qc_audit_modal <- function() {
    rel <- qc_relevant(); all_n <- nrow(EXCLUDED_EXTREMES)
    tbl <- rel |>
      dplyr::transmute(Site = site, Date = format(collectDate, "%Y-%m-%d"),
                       Analyte = display,
                       Value = paste0(signif(value, 4), ifelse(nzchar(pretty_unit(units, analyte)), paste0(" ", pretty_unit(units, analyte)), "")),
                       `Plausible ceiling` = signif(ceiling, 4))
    showModal(modalDialog(
      title = tagList(bs_icon("shield-check"), " Excluded extreme values"),
      easyClose = TRUE, size = "l", footer = modalButton("Close"),
      tags$p(HTML(
        "These values exceed a per-analyte plausibility ceiling (above the 99.9th percentile <i>and</i>
         50× the analyte median), almost certainly unit or decimal artifacts, not real chemistry.
         They are <b>excluded from every fit, the seasonal decomposition, the predictor, and the map's
         colour scale</b>, but kept here for audit. The reported number is preserved in the raw record.")),
      DT::datatable(tbl, rownames = FALSE,
                    options = list(dom = "t", pageLength = 25, ordering = FALSE)),
      if (all_n > nrow(rel)) tags$p(class = "scope-note",
        sprintf("Showing the %d relevant to your current site/analytes; %d total excluded across the dataset.",
                nrow(rel), all_n))))
  }
  observeEvent(input$qc_audit,      qc_audit_modal())
  observeEvent(input$qc_audit_band, qc_audit_modal())

  output$preset_reason <- renderUI({
    pm <- preset_match(); if (is.na(pm)) return(NULL)
    div(class = "preset-reason", bs_icon("lightbulb"), " ", PRESET_REASON[[pm]])
  })

  output$ts_note <- renderUI({
    txt <- if (identical(input$ts_mode, "dual"))
      "Two independent y-axes (left = main, right = secondary, colour-matched). Lines tracking each other does not by itself imply correlation; test it on the Relationship tab."
    else
      "Each series is standardized to mean 0, SD 1 so you compare shape, not magnitude. Raw values are on the Data tab (or hover/tap a point)."
    div(class = "scope-note", style = "margin:.1rem 0 .4rem", txt)
  })

  ## ---- Summary strip (analyte sparklines + correlation + site) ----
  output$summary_strip <- renderUI({
    sl <- sel_long(); p <- sel_pair(); n <- nrow(p)
    A <- main_a(); B <- sec_a()
    ma <- sl |> filter(analyte == A) |> arrange(collectDate)
    sb <- sl |> filter(analyte == B) |> arrange(collectDate)
    unitA <- pretty_unit(ma$units[1] %||% NA, A); unitB <- pretty_unit(sb$units[1] %||% NA, B)
    latestA <- if (nrow(ma)) paste0(signif(tail(ma$value, 1), 4), if (nzchar(unitA)) paste0(" ", unitA) else "") else "—"
    latestB <- if (nrow(sb)) paste0(signif(tail(sb$value, 1), 4), if (nzchar(unitB)) paste0(" ", unitB) else "") else "—"
    r <- if (n >= 8) suppressWarnings(stats::cor(p$x, p$y, method = "spearman")) else NA_real_
    sm <- D$sites_meta[D$sites_meta$site == input$site, ]
    r_theme <- if (is.na(r)) "secondary" else if (abs(r) >= 0.7) "success" else if (abs(r) >= 0.4) "warning" else "secondary"
    layout_columns(
      col_widths = breakpoints(sm = 6, lg = 3), fill = FALSE,
      value_box(analyte_display(A), latestA, paste0("latest of ", nrow(ma), " samples"),
                showcase = spark(ma$collectDate, ma$value, "#6ee6f0"),
                showcase_layout = "bottom", theme = "primary"),
      value_box(analyte_display(B), latestB, paste0("latest of ", nrow(sb), " samples"),
                showcase = spark(sb$collectDate, sb$value, "#f0c08a"),
                showcase_layout = "bottom", theme = "secondary"),
      div(class = "vb-door", role = "button", tabindex = "0",
          `aria-label` = "Open the Relationship tab for this analyte pair",
          onclick = "Shiny.setInputValue('goto_rel', Math.random(), {priority:'event'})",
          onkeydown = "if(event.key==='Enter'||event.key===' '){event.preventDefault();Shiny.setInputValue('goto_rel', Math.random(), {priority:'event'})}",
          value_box(tagList("Correlation · Spearman ρ ",
                            tags$span(onclick = "event.stopPropagation()", help_pop("spearman", "Spearman ρ"))),
                    ifelse(is.na(r), "—", sprintf("%.2f", r)),
                    if (n > 0) paste0("n = ", n, " paired") else "select two analytes", theme = r_theme)),
      value_box("Field site", input$site, sm$siteName %||% input$site, theme = "dark")
    )
  })
  observeEvent(input$goto_rel, nav_select("main_tabs", "Relationship"))
  observeEvent(input$goto_seasonal, nav_select("main_tabs", "Seasonal pattern"))

  ## ---- Compare: time series ----
  output$ts <- renderPlotly({ safe_plotly({
    df <- sel_long()
    if (!nrow(df)) return(plotly_message("No matched samples in this window. Try 'Use full record' on the Explore tab.", mode()))
    A <- main_a(); B <- sec_a()
    unitA <- df$units[df$analyte == A][1]; unitB <- df$units[df$analyte == B][1]

    if (identical(input$ts_mode, "dual")) {
      dA <- df |> filter(analyte == A) |> arrange(collectDate)
      dB <- df |> filter(analyte == B) |> arrange(collectDate)
      p <- plot_ly() |>
        add_trace(data = dA, x = ~collectDate, y = ~value, type = "scatter", mode = "markers+lines",
                  name = analyte_display(A), line = list(color = COL$main, width = 2),
                  marker = list(color = COL$main, size = 7),
                  hovertemplate = paste0("<b>", analyte_display(A), "</b><br>%{x|%b %d, %Y}<br>%{y:.4g} ",
                                         pretty_unit(unitA, A), "<extra></extra>")) |>
        add_trace(data = dB, x = ~collectDate, y = ~value, yaxis = "y2", type = "scatter", mode = "markers+lines",
                  name = analyte_display(B), line = list(color = COL$secondary, width = 2),
                  marker = list(color = COL$secondary, size = 7),
                  hovertemplate = paste0("<b>", analyte_display(B), "</b><br>%{x|%b %d, %Y}<br>%{y:.4g} ",
                                         pretty_unit(unitB, B), "<extra></extra>")) |>
        layout(
          hovermode = "x unified",
          xaxis = list(title = "", type = "date", showspikes = TRUE, spikemode = "across",
                       spikethickness = 1, spikecolor = "#9aa7b0", spikedash = "dot"),
          yaxis  = list(title = list(text = axis_title(A, unitA), font = list(color = COL$main)),
                        tickfont = list(color = COL$main)),
          yaxis2 = list(title = list(text = axis_title(B, unitB), font = list(color = COL$secondary)),
                        tickfont = list(color = COL$secondary), overlaying = "y", side = "right", showgrid = FALSE),
          legend = list(orientation = "h", x = 0, y = 1.1), margin = list(t = 34, r = 72, b = 40))
    } else {
      df2 <- df |> group_by(analyte) |> arrange(collectDate) |>
        mutate(sd_ = sd(value, na.rm = TRUE), mu_ = mean(value, na.rm = TRUE),
               z = ifelse(is.finite(sd_) & sd_ > 0, (value - mu_) / sd_, 0),
               role = ifelse(analyte == A, "Main", "Secondary"),
               disp = analyte_display(analyte), below = belowDetection == 1) |> ungroup()
      pal <- c(Main = COL$main, Secondary = COL$secondary)
      p <- plot_ly()
      for (rl in c("Main", "Secondary")) {
        dd <- df2 |> filter(role == rl)
        if (!nrow(dd)) next
        p <- add_trace(p, data = dd, x = ~collectDate, y = ~z, type = "scatter", mode = "markers+lines",
                       name = dd$disp[1], legendgroup = rl,
                       line = list(color = pal[[rl]], width = 2),
                       marker = list(color = pal[[rl]], size = 7, line = list(color = "rgba(255,255,255,.8)", width = 1)),
                       text = ~paste0("<b>", disp, "</b><br>%{x}"),
                       hovertemplate = ~paste0("<b>", disp, "</b><br>%{x|%b %d, %Y}<br>value: ",
                                               signif(value, 4), " ", pretty_unit(units, analyte),
                                               ifelse(n_reps > 1, paste0(" (", n_reps, "-rep mean)"), ""),
                                               "<br>z: %{y:.2f}<extra></extra>"))
      }
      bd <- df2 |> filter(below)
      if (nrow(bd)) p <- add_trace(p, data = bd, x = ~collectDate, y = ~z, type = "scatter", mode = "markers",
        name = "below detection", marker = list(symbol = "circle-open", size = 10, color = "#444", line = list(width = 1.5)),
        hovertemplate = "below detection limit<extra></extra>")
      p <- p |> layout(
        hovermode = "x unified",
        yaxis = list(title = "Standardized value (z-score)", zeroline = TRUE),
        xaxis = list(title = "", type = "date"),
        legend = list(orientation = "h", x = 0, y = 1.1), margin = list(t = 34, b = 40))
    }
    p |> plotly_theme(mode(), narrow()) |> plotly_clean(paste0(input$site, "_", A, "_vs_", B, "_timeseries"))
  }, mode()) })

  output$ts_footer <- renderText({
    df <- sel_long(); sp <- dates_d()
    sm <- D$sites_meta[D$sites_meta$site == input$site, ]
    paste0(sm$siteName %||% input$site, " · ", nrow(df), " samples · ",
           format(sp[1], "%b %Y"), "–", format(sp[2], "%b %Y"), " · source: NEON SWC DP1.20093.001")
  })

  ## ---- Relationship ----
  output$reg <- renderPlotly({ safe_plotly({
    df <- sel_pair()
    if (nrow(df) < 3) return(plotly_message(
      sprintf("Not enough shared dates to fit a line (n = %d; need ≥ 3).", nrow(df)), mode()))
    df <- df |> mutate(season = season_of(collectDate), year = year(collectDate))
    A <- main_a(); B <- sec_a()
    w <- wide_site(); unitA <- D$analyte_meta$units[D$analyte_meta$analyte == A][1]
    unitB <- D$analyte_meta$units[D$analyte_meta$analyte == B][1]
    f <- fit_lm(df)
    sub <- sprintf("OLS · R² = %.3f · p %s · n = %d", f$r2,
                   ifelse(f$p < .001, "< 0.001", paste0("= ", signif(f$p, 2))), f$n)
    pp <- ggplot(df, aes(x, y)) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = COL$main, fill = COL$main,
                  alpha = .15, linewidth = .9) +
      geom_point(aes(fill = season,
                     text = paste0(format(collectDate, "%b %d, %Y"), "<br>",
                                   analyte_display(A), ": ", signif(x, 4), "<br>",
                                   analyte_display(B), ": ", signif(y, 4), "<br>", season, " ", year)),
                 shape = 21, size = 2.7, stroke = .3, color = "white", alpha = .9) +
      scale_fill_manual(values = SEASON_PAL, name = NULL) +
      labs(x = axis_title(A, unitA), y = axis_title(B, unitB),
           title = paste0(analyte_display(B), " vs ", analyte_display(A)), subtitle = sub) +
      theme_neon()
    ggplotly(pp, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.2)) |>
      plotly_theme(mode(), narrow()) |> plotly_clean(paste0(input$site, "_", A, "_vs_", B, "_regression"))
  }, mode()) })

  output$reg_stats <- renderUI({
    df <- sel_pair()
    if (nrow(df) < 3) return(div(class = "text-muted p-3",
      sprintf("This pair has %d matched samples; the relationship view needs at least 3.", nrow(df))))
    f <- fit_lm(df)
    ac <- f$lag1_acf
    ac_flag <- if (is.na(ac)) "—" else if (abs(ac) >= 0.5) "high" else if (abs(ac) >= 0.25) "moderate" else "low"
    r2_theme <- if (f$r2 >= 0.7) "success" else if (f$r2 >= 0.4) "warning" else "secondary"
    tagList(
      layout_columns(col_widths = 6, fill = FALSE,
        value_box(tagList("R² ", help_pop("r2", "R-squared")), sprintf("%.3f", f$r2), "variance explained", theme = r2_theme),
        value_box("Adjusted R²", sprintf("%.3f", f$adj_r2), "penalized for predictors", theme = "secondary"),
        value_box("Slope", sprintf("%.3g", f$slope), sprintf("± %.2g (SE)", f$slope_se), theme = "secondary"),
        value_box(tagList("n ", help_pop("n", "Sample size")), f$n, "paired samples", theme = "primary")),
      div(class = "px-2 pb-2",
        tags$p(HTML(sprintf("<b>p-value (slope):</b> %s ",
                            ifelse(f$p < .001, "&lt; 0.001", signif(f$p, 3)))), help_pop("pval", "p-value")),
        tags$p(HTML(sprintf("<b>Temporal autocorrelation (lag-1):</b> %s (%s). %s",
                            ifelse(is.na(ac), "—", sprintf("%.2f", ac)), ac_flag,
                            "High values mean the p-value above is optimistic.")))),
      # clickable censoring badge — only when a fitted analyte is heavily below DL;
      # one short clause + a (?) that opens the existing explanation (no wall of text)
      { cen <- CENSOR_MAP[c(main_a(), sec_a())]; cen <- cen[is.finite(cen) & cen >= 0.25]
        if (length(cen)) div(class = "px-2 pb-2 scope-note",
          bs_icon("exclamation-triangle"),
          sprintf(" %s heavily below detection, exploratory only ",
                  paste(analyte_display(names(cen)), collapse = " & ")),
          help_pop("censor", "Below-detection handling")) }
    )
  })

  ## ---- Correlations ----
  cor_tbl <- reactive({
    correlation_table(wide_site() |> select(-collectDate, -dplyr::any_of("site")),
                      main_a(), method = input$cor_method, min_n = 8, censor_map = CENSOR_MAP)
  }) |> bindCache(input$site, main_a(), input$cor_method, dates_d())

  output$cor_lolli <- renderPlotly({ safe_plotly({
    ct <- cor_tbl()
    if (is.null(ct) || !nrow(ct)) return(plotly_message("No co-sampled analytes in this window.", mode()))
    # top 18 by |coef| keeps the labels legible; the full set is in the table below
    ct <- head(ct, 18) |> mutate(display = factor(display, levels = rev(display)))
    pp <- ggplot(ct, aes(x = coef, y = display)) +
      geom_vline(xintercept = 0, color = "rgba(0,0,0,.3)") +
      geom_segment(aes(x = 0, xend = coef, yend = display, color = reliable), linewidth = .9) +
      geom_point(aes(color = reliable, text = paste0(display, "<br>", input$cor_method, " = ", signif(coef, 3),
                     "<br>n = ", n,
                     ifelse(heavy_censor, sprintf("<br>⚠ %.0f%% below detection, exploratory only", 100 * pct_below), ""),
                     ifelse(!reliable & !heavy_censor, "<br>⚠ low n, interpret with care", ""))), size = 3.2) +
      scale_color_manual(values = c(`TRUE` = COL$main, `FALSE` = "#BBBBBB")) +
      scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .5)) +
      labs(x = paste0(tools::toTitleCase(input$cor_method), " correlation with ", analyte_display(main_a())), y = NULL) +
      theme_neon() + theme(legend.position = "none", axis.text.y = element_text(size = 10.5))
    ggplotly(pp, tooltip = "text") |> layout(margin = list(l = 10)) |>
      plotly_theme(mode(), narrow()) |> plotly_clean(paste0(input$site, "_correlations"))
  }, mode()) })

  output$cor_table <- renderDT({
    ct <- cor_tbl()
    if (is.null(ct) || !nrow(ct))
      return(datatable(data.frame(Note = "No co-sampled analytes in this window. Widen the date range."),
                       rownames = FALSE, options = list(dom = "t")))
    fmtp <- function(p) ifelse(is.na(p), "—", ifelse(p < 1e-4, "<0.0001", formatC(p, format = "g", digits = 2)))
    fmtbdl <- function(x) ifelse(is.na(x) | x < 0.005, "—", sprintf("%.0f%%", 100 * x))
    show <- ct |> transmute(Analyte = display, n,
                            `Spearman ρ` = round(spearman, 3), `p (ρ)` = fmtp(p_spearman),
                            `Pearson r` = round(pearson, 3),  `p (r)` = fmtp(p_pearson),
                            BDL = fmtbdl(pct_below),
                            Reading = ifelse(ties, paste0(flag, " †"), flag))
    # Optional multiple-comparison + autocorrelation columns (review finding #6):
    # BH false-discovery q on the active method's p, and an effective-n-adjusted p
    # for the rows whose lag-1 |ACF| >= 0.5 (a dash where no adjustment applies).
    cap_extra <- ""
    if (isTRUE(input$cor_bh)) {
      show$`BH q` <- fmtp(ct$q_bh)
      show$`p (n_eff)` <- ifelse(is.na(ct$p_eff), "—", fmtp(ct$p_eff))
      cap_extra <- " <b>BH q</b> = Benjamini-Hochberg false-discovery-adjusted p across these analytes. <b>p (n_eff)</b> = p re-tested on the autocorrelation-deflated effective n (shown only where lag-1 |ACF| ≥ 0.5)."
    }
    datatable(show, rownames = FALSE, options = list(pageLength = 8, dom = "tip"),
              caption = htmltools::tags$caption(style = "caption-side:top",
                htmltools::HTML(paste0("Each p-value pairs with the coefficient in the same colour group. † = tied ranks, Spearman p is approximate. <b>BDL</b> = % below detection; rows over 25% are exploratory only.", cap_extra)))) |>
      formatStyle("Reading", target = "cell",
                  backgroundColor = styleEqual(
                    c("strong +", "strong −", "moderate", "strong + †", "strong − †", "moderate †"),
                    c("#dff0e8", "#fde7e3", "#fdf3e0", "#dff0e8", "#fde7e3", "#fdf3e0")))
  })

  ## ---- Seasonal: climatology + STL ----
  main_monthly <- reactive({
    d <- dates_d()
    D$swc_long |> filter(site == input$site, analyte == main_a(),
                         collectDate >= d[1], collectDate <= d[2], !implausible)
  })

  output$clim <- renderPlotly({ safe_plotly({
    df <- main_monthly()
    if (nrow(df) < 6) return(plotly_message("Not enough samples for a monthly climatology.", mode()))
    df <- df |> mutate(month = month(collectDate, label = TRUE), year = year(collectDate)) |>
      add_count(month, name = "n_month")
    unit <- df$units[1]
    pp <- ggplot(df, aes(month, value)) +
      geom_boxplot(outlier.shape = NA, fill = COL$main, alpha = .18, color = COL$main, linewidth = .5) +
      geom_jitter(aes(text = paste0(format(collectDate, "%b %Y"), "<br>", signif(value, 4), " ",
                                    pretty_unit(units, analyte), "<br>", month, ": n = ", n_month)),
                  width = .15, height = 0, size = 1.8, color = COL$main, alpha = .55) +
      labs(x = NULL, y = axis_title(main_a(), unit),
           title = paste0("Monthly climatology · ", analyte_display(main_a()))) +
      theme_neon()
    ggplotly(pp, tooltip = "text") |> plotly_theme(mode(), narrow()) |> plotly_clean(paste0(input$site, "_climatology"))
  }, mode()) })

  output$stl <- renderPlotly({ safe_plotly({
    df <- main_monthly()
    ms <- df |> mutate(ym = floor_date(collectDate, "month")) |>
      group_by(ym) |> summarise(v = mean(value, na.rm = TRUE), .groups = "drop") |> arrange(ym)
    if (nrow(ms) < 24 || as.numeric(diff(range(ms$ym))) < 365 * 2)
      return(plotly_message("Need ≥ 24 months across ≥ 2 years for an STL decomposition.\nWiden the date range.", mode()))
    full <- tibble(ym = seq(min(ms$ym), max(ms$ym), by = "month")) |> left_join(ms, by = "ym")
    v <- full$v
    # Guard: refuse only when interpolation would dominate (a gap longer than a full
    # seasonal cycle, or under ~45% real). Otherwise show it and disclose the count in
    # the title — the disclosure, not a hard block, is the honesty lever (Quinn/Don/Aaron).
    n_real <- sum(!is.na(v)); n_tot <- length(v); n_fill <- n_tot - n_real
    gap_runs <- rle(is.na(v)); max_gap <- if (any(gap_runs$values)) max(gap_runs$lengths[gap_runs$values]) else 0
    if (n_real / n_tot < 0.45 || max_gap > 12)
      return(plotly_message(sprintf(
        "Too sparse for an honest decomposition\n(%d of %d months sampled; longest gap %d months).\nWiden the range or pick a better-sampled analyte.",
        n_real, n_tot, max_gap), mode()))
    # linear-interpolate interior gaps; carry ends
    idx <- which(!is.na(v))
    if (length(idx) >= 2) v <- approx(idx, v[idx], xout = seq_along(v), rule = 2)$y
    tsv <- ts(v, frequency = 12, start = c(year(min(full$ym)), month(min(full$ym))))
    dec <- stats::stl(tsv, s.window = "periodic", robust = TRUE)
    cmp <- as.data.frame(dec$time.series)
    out <- tibble(ym = full$ym, observed = full$v,
                  trend = cmp$trend, seasonal = cmp$seasonal, remainder = cmp$remainder)
    unit <- pretty_unit(df$units[1], main_a())
    p1 <- plot_ly(out, x = ~ym) |>
      add_trace(y = ~observed, type = "scatter", mode = "markers", name = "observed",
                marker = list(color = "rgba(120,130,140,.6)", size = 5),
                hovertemplate = paste0("%{x|%b %Y}<br>%{y:.3g} ", unit, "<extra></extra>")) |>
      add_trace(y = ~trend, type = "scatter", mode = "lines", name = "trend",
                line = list(color = COL$main, width = 3),
                hovertemplate = "trend %{y:.3g}<extra></extra>") |>
      add_trace(y = ~(trend + seasonal), type = "scatter", mode = "lines", name = "trend + seasonal",
                line = list(color = COL$secondary, width = 1.4, dash = "dot"), opacity = .8,
                hovertemplate = "%{y:.3g}<extra></extra>") |>
      layout(yaxis = list(title = axis_title(main_a(), df$units[1])), xaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.18, x = 0), margin = list(t = 46, b = 64),
             title = list(
               text = paste0("<span style='font-size:11.5px;color:#9aa4ad;font-weight:400'>",
                             n_real, " of ", n_tot, " months real · ", n_fill, " interpolated before decomposition</span>"),
               x = 0, xanchor = "left", y = 0.99, yanchor = "top"))
    p1 |> plotly_theme(mode(), narrow()) |> plotly_clean(paste0(input$site, "_STL"))
  }, mode()) })

  ## ---- Predictor ----
  # field/lab measurements of the same property — exclude as predictors (circular)
  TWIN <- c(specificConductanceField = "specificConductance",
            specificConductance = "specificConductanceField",
            dissolvedOxygenField = "dissolvedOxygen",
            dissolvedOxygen = "dissolvedOxygenField")
  pred_predictors <- reactive({
    ct <- cor_base()                                    # shared Pearson table (cached)
    if (is.null(ct) || !nrow(ct)) return(character(0))
    drop <- c(TWIN[[main_a()]] %||% "", "TSS - Dry Mass")  # exclude circular twin + gravimetric mass
    head(ct$code[ct$reliable & !(ct$code %in% drop)], 3)
  })

  output$pred_intro <- renderUI({
    preds <- pred_predictors()
    if (!length(preds)) return(div(class = "text-muted",
      "Not enough reliably co-correlated analytes at this site/range to build a predictor. Widen the date range or pick another main analyte."))
    div(tags$p(HTML(sprintf("Estimate <b>%s</b> from its %d best-correlated analytes:",
                            analyte_display(main_a()), length(preds)))),
        tags$ul(lapply(preds, function(p) tags$li(analyte_display(p)))))
  })

  output$pred_sliders <- renderUI({
    preds <- pred_predictors(); w <- wide_site(); req(length(preds) > 0)
    lapply(preds, function(p) {
      v <- suppressWarnings(as.numeric(w[[p]])); v <- v[is.finite(v)]
      if (!length(v)) return(NULL)
      sliderInput(paste0("pred_", p), analyte_display(p),
                  min = round(min(v), 3), max = round(max(v), 3), value = round(stats::median(v), 3))
    })
  })

  # Model + cross-validation depend on site / range / main analyte / predictors —
  # NOT on the sliders. Splitting it out means a slider drag no longer re-runs the
  # (reps × k = 50-fit) cross-validation; only the cheap predict() re-fires.
  pred_base <- reactive({
    preds <- pred_predictors(); w <- wide_site()
    if (!length(preds)) return(list(status = "none"))
    d <- w[, c(main_a(), preds), drop = FALSE]; names(d)[1] <- "target"
    d <- d[stats::complete.cases(d), , drop = FALSE]
    if (nrow(d) < (length(preds) + 5)) return(list(status = "few"))
    form <- stats::as.formula(paste0("target ~ ", paste(sprintf("`%s`", preds), collapse = " + ")))
    fit <- tryCatch(stats::glm(form, data = d), error = function(e) NULL)
    if (is.null(fit)) return(list(status = "fit"))
    obs <- suppressWarnings(as.numeric(w[[main_a()]])); obs <- obs[is.finite(obs)]
    list(status = "ok", fit = fit, preds = preds, cv = kfold_rmse(w, main_a(), preds),
         observed = obs,
         unit = pretty_unit(D$analyte_meta$units[D$analyte_meta$analyte == main_a()][1], main_a()))
  })

  # the only slider-dependent piece: the live point estimate
  pred_yhat <- reactive({
    b <- pred_base(); if (b$status != "ok") return(NA_real_)
    newd <- as.data.frame(lapply(b$preds, function(p) input[[paste0("pred_", p)]]))
    names(newd) <- b$preds
    if (any(vapply(newd, function(x) is.null(x) || !is.finite(x), logical(1)))) return(NA_real_)
    tryCatch(as.numeric(stats::predict(b$fit, newd)), error = function(e) NA_real_)
  })

  output$pred_out <- renderUI({
    b <- pred_base()
    if (b$status == "none") return(div(class = "text-muted p-2", "—"))
    if (b$status == "few")  return(div(class = "text-muted p-2",
      "Too few complete records to fit the model here. Widen the date range."))
    if (b$status == "fit")  return(div(class = "text-danger p-2", "Model could not be fit for this selection."))
    yhat  <- pred_yhat()
    skill <- if (is.na(b$cv$rmse) || is.na(b$cv$null_rmse) || b$cv$null_rmse == 0) NA else 1 - b$cv$rmse / b$cv$null_rmse
    tagList(
      value_box(paste0("Predicted ", analyte_display(main_a())),
                ifelse(is.na(yhat), "—", paste0(signif(yhat, 4), " ", b$unit)),
                "from the slider values", theme = "primary"),
      div(class = "px-2 pt-2 scope-note",
          HTML(sprintf("Cross-validated RMSE ≈ %s %s vs %s %s for a mean-only baseline (skill %s) on %d records. ",
                  ifelse(is.na(b$cv$rmse), "—", signif(b$cv$rmse, 3)), b$unit,
                  ifelse(is.na(b$cv$null_rmse), "—", signif(b$cv$null_rmse, 3)), b$unit,
                  ifelse(is.na(skill), "—", sprintf("%+.0f%%", 100 * skill)), b$cv$n)),
          help_pop("rmse", "RMSE & skill")))
  })

  # fills the Prediction card: where the slider-driven estimate lands within the
  # site's own observed distribution for the main analyte (typical vs extreme?)
  output$pred_ctx <- renderPlotly({ safe_plotly({
    b <- pred_base()
    if (b$status != "ok" || !length(b$observed))
      return(plotly_message("Pick a site and main analyte to see where the estimate lands.", mode()))
    yhat <- pred_yhat()
    pp <- ggplot(data.frame(value = b$observed), aes(value)) +
      geom_histogram(bins = 30, fill = COL$main, alpha = .30, color = COL$main, linewidth = .2) +
      labs(x = axis_title(main_a(), b$unit), y = "samples",
           title = paste0("Your estimate (dashed) vs observed ", analyte_display(main_a()))) +
      theme_neon()
    if (!is.na(yhat)) pp <- pp +
      geom_vline(xintercept = yhat, color = COL$secondary, linewidth = 1, linetype = "dashed")
    ggplotly(pp) |> plotly_theme(mode(), narrow()) |> plotly_clean(paste0(input$site, "_prediction_context"))
  }, mode()) })

  ## ---- Data table + downloads ----
  # tidy long slice of the current site/range (the analysis-ready export)
  long_slice <- reactive({
    d <- dates_d()
    D$swc_long |>
      filter(site == input$site, collectDate >= d[1], collectDate <= d[2]) |>
      transmute(site, collectDate, analyte, analyte_label = analyte_display(analyte),
                value, units, n_reps, value_sd, below_detection = belowDetection == 1,
                implausible_extreme = implausible,   # kept in raw export, excluded from fits/maps
                lab_flag = labFlag, source, product = D$built$product) |>
      arrange(collectDate, analyte)
  })

  output$data_table <- renderDT({
    w <- wide_site()
    if (!nrow(w))
      return(datatable(data.frame(Note = "No samples in this window. Widen the date range."),
                       rownames = FALSE, options = list(dom = "t")))
    keep <- c("collectDate", intersect(c(main_a(), sec_a()), names(w)),
              setdiff(names(w), c("site", "collectDate", main_a(), sec_a())))
    show <- w[, intersect(keep, names(w)), drop = FALSE] |> arrange(collectDate)
    datatable(show, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
  })

  fn_base <- reactive({ sp <- dates_d(); paste0(input$site, "-SWC-", format(sp[1], "%Y"), "-", format(sp[2], "%Y")) })
  # legacy/format-change fraction of the exported site-window slice (~30% of the
  # record predates a NEON method/format standardization — a method boundary an
  # analyst comparing pre/post trends should be told about, in the export header).
  pct_legacy_slice <- function() {
    d <- dates_d()
    s <- D$swc_long |> filter(site == input$site, collectDate >= d[1], collectDate <= d[2])
    if (!nrow(s)) return(0)
    mean(grepl("formatChange|legacyData", s$labFlag), na.rm = TRUE)
  }
  provenance <- function() sprintf("# NEON Surface Water Chemistry %s | site %s | built %s | data through %s | values are replicate means (n_reps); below_detection = reported below the analytical detection limit | implausible extremes (> per-analyte p99.9 & 50x median) excluded from fits/maps | %.0f%% of rows carry a legacy/formatChange method-standardization flag",
                                   D$built$product, input$site, substr(D$built$when, 1, 10), D$built$data_through, 100 * pct_legacy_slice())

  output$dl_long <- downloadHandler(
    filename = function() paste0(fn_base(), "-long.csv"),
    content = function(file) {
      writeLines(provenance(), file)
      readr::write_excel_csv(long_slice(), file, append = TRUE, col_names = TRUE)  # write_excel_csv => UTF-8 BOM, clean in Excel
    })
  output$dl_wide <- downloadHandler(
    filename = function() paste0(fn_base(), "-wide.csv"),
    content = function(file) {
      w <- wide_site()
      writeLines(provenance(), file)
      readr::write_excel_csv(w, file, append = TRUE, col_names = TRUE)
    })
  output$dl_dict <- downloadHandler(
    filename = function() "NEON-SWC-data-dictionary.csv",
    content = function(file) {
      # Canonical units now flow from D$analyte_meta (modal, UV no longer NA);
      # carry the below-detection fraction + the plausibility ceiling so the
      # exported codebook is self-describing (the FAIR codebook gap).
      dict <- ANALYTE_TBL |>
        left_join(D$analyte_meta |> select(code = analyte, units, n, n_sites, n_below, pct_below, source), by = "code") |>
        left_join(CEIL_TBL$global |> select(code = analyte, plausibility_ceiling = ceiling), by = "code") |>
        transmute(code, display, group, indicator, units, n_obs = n, n_sites,
                  n_below, pct_below = round(pct_below, 4), plausibility_ceiling = signif(plausibility_ceiling, 4),
                  source)
      # provenance header documents the below-detection convention + the gate
      writeLines(c(
        "# NEON Surface Water Chemistry data dictionary (DP1.20093.001)",
        "# units = canonical (modal) unit per analyte; below_detection rows keep the reported number (never substituted)",
        "# n_below / pct_below = count / fraction of below-detection samples; plausibility_ceiling = per-analyte max(p99.9, 50x median) above which a value is excluded as an artifact"),
        file)
      # write_csv (no BOM) for the appended block: write_excel_csv would emit a
      # second UTF-8 BOM mid-file, right before the header row, breaking strict CSV readers
      readr::write_csv(dict, file, append = TRUE, col_names = TRUE)
    })

  # versioned column-level codebook shipped beside the bundle (the keep-vector
  # codebook); served as-is so the in-app download matches the committed file.
  output$dl_codebook <- downloadHandler(
    filename = function() "NEON-SWC-codebook.csv",
    content = function(file) {
      src <- "data/codebook.csv"
      if (file.exists(src)) file.copy(src, file, overwrite = TRUE)
      else writeLines("# codebook.csv not bundled in this build", file)
    })

  ## ---- One-click PDF site report (self-contained, base pdf() + ggplot) ----
  # Shared by the Data-tab "PDF report" button AND the context-band "Report" link
  # (output$dl_report + output$report_band both call write_site_report).
  write_site_report <- function(file) {
      d <- dates_d(); A <- main_a(); B <- sec_a(); st <- input$site
      sm <- D$sites_meta[D$sites_meta$site == st, ]
      uA <- D$analyte_meta$units[D$analyte_meta$analyte == A][1]
      uB <- D$analyte_meta$units[D$analyte_meta$analyte == B][1]
      wide <- D$swc_wide |> filter(site == st, collectDate >= d[1], collectDate <= d[2])
      pair <- tibble(collectDate = wide$collectDate,
                     x = suppressWarnings(as.numeric(wide[[A]])),
                     y = suppressWarnings(as.numeric(wide[[B]]))) |> filter(is.finite(x), is.finite(y))
      f <- if (nrow(pair) >= 3) fit_lm(pair) else NULL
      rho <- if (nrow(pair) >= 8) suppressWarnings(stats::cor(pair$x, pair$y, method = "spearman")) else NA

      grDevices::pdf(file, width = 8.5, height = 11); on.exit(grDevices::dev.off())
      # PDF-safe theme (base pdf() device can't use the web "Inter" font)
      th <- ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(plot.title = element_text(face = "bold", size = 15),
                       plot.subtitle = element_text(color = "grey40"),
                       plot.title.position = "plot",
                       panel.grid.minor = element_blank(), legend.position = "bottom")

      # Page 1 — cover + headline stats
      lines <- c(
        sprintf("Site: %s (%s · %s)", sm$siteName %||% st, st, sm$domain %||% ""),
        sprintf("Window: %s to %s", d[1], d[2]),
        sprintf("Analytes: %s  vs  %s", analyte_display(A), analyte_display(B)),
        sprintf("Paired samples: %d", nrow(pair)),
        if (!is.na(rho)) sprintf("Spearman rho: %.2f", rho) else "Spearman rho: insufficient data (n < 8)",
        if (!is.null(f)) sprintf("OLS R-squared: %.3f   ·   p %s", f$r2, ifelse(f$p < .001, "< 0.001", paste0("= ", signif(f$p, 2)))) else "OLS: insufficient data",
        sprintf("Source: NEON Surface Water Chemistry %s · data through %s", D$built$product, D$built$data_through))
      cover <- ggplot() + xlim(0, 1) + ylim(0, 1) + theme_void() +
        annotate("text", x = 0.04, y = 0.92, hjust = 0, fontface = "bold", size = 8, color = PG$main,
                 label = "NEON Water Chemistry") +
        annotate("text", x = 0.04, y = 0.85, hjust = 0, size = 6, label = "Site report") +
        annotate("text", x = 0.04, y = seq(0.72, 0.72 - 0.06 * (length(lines) - 1), by = -0.06),
                 hjust = 0, size = 4.2, label = lines) +
        annotate("text", x = 0.04, y = 0.06, hjust = 0, size = 3, color = "grey50",
                 label = paste0("Generated ", substr(D$built$when, 1, 10), " from a precomputed read-only dataset (no live API)."))
      print(cover)

      # Page 2 — time series (both analytes, faceted, real units); drop implausible extremes
      sl <- D$swc_long |> filter(site == st, analyte %in% c(A, B), collectDate >= d[1], collectDate <= d[2], !implausible) |>
        mutate(lab = factor(ifelse(analyte == A, axis_title(A, uA), axis_title(B, uB)),
                            levels = c(axis_title(A, uA), axis_title(B, uB))))
      if (nrow(sl)) print(
        ggplot(sl, aes(collectDate, value)) +
          geom_line(aes(color = lab), linewidth = .6) + geom_point(aes(color = lab), size = 1.3) +
          scale_color_manual(values = setNames(c(PG$main, PG$secondary), levels(sl$lab)), guide = "none") +
          facet_wrap(~lab, ncol = 1, scales = "free_y", strip.position = "left") +
          labs(x = NULL, y = NULL, title = paste0(sm$siteName %||% st, " · analytes through time")) +
          th + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90)))

      # Page 3 — regression
      if (!is.null(f)) {
        sub <- sprintf("OLS · R² = %.3f · p %s · n = %d", f$r2,
                       ifelse(f$p < .001, "< 0.001", paste0("= ", signif(f$p, 2))), nrow(pair))
        print(ggplot(pair, aes(x, y)) +
          geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = PG$main, fill = PG$main, alpha = .15) +
          geom_point(color = PG$secondary, alpha = .8, size = 2) +
          labs(x = axis_title(A, uA), y = axis_title(B, uB),
               title = paste0(analyte_display(B), " vs ", analyte_display(A)), subtitle = sub) + th)
      }

      # Page 4 — seasonal climatology of the main analyte; drop implausible extremes
      cl <- D$swc_long |> filter(site == st, analyte == A, collectDate >= d[1], collectDate <= d[2], !implausible) |>
        mutate(month = lubridate::month(collectDate, label = TRUE))
      if (nrow(cl) >= 6) print(
        ggplot(cl, aes(month, value)) +
          geom_boxplot(outlier.shape = NA, fill = PG$main, alpha = .18, color = PG$main) +
          geom_jitter(width = .15, height = 0, color = PG$main, alpha = .5, size = 1.4) +
          labs(x = NULL, y = axis_title(A, uA), title = paste0("Monthly climatology · ", analyte_display(A))) + th)
  }
  output$dl_report <- downloadHandler(
    filename = function() paste0(fn_base(), "-report.pdf"),
    content  = write_site_report)
  # context-band "Report" link — same PDF, reachable from every tab
  output$report_band <- downloadHandler(
    filename = function() paste0(fn_base(), "-report.pdf"),
    content  = write_site_report)

  ## ---- Two sites: same analyte at site A vs site B ----
  output$two_sites_note <- renderUI({
    s2name <- D$sites_meta$siteName[match(input$site_b, D$sites_meta$site)]
    div(class = "scope-note", style = "margin:.1rem 0 .4rem",
        sprintf("Comparing %s at %s vs %s.", analyte_display(main_a()),
                D$sites_meta$siteName[match(input$site, D$sites_meta$site)] %||% input$site,
                s2name %||% input$site_b))
  })
  output$two_sites <- renderPlotly({ safe_plotly({
    d <- dates_d(); A <- main_a(); s1 <- input$site; s2 <- input$site_b %||% DEF_SITE_B
    df <- D$swc_long |> filter(analyte == A, site %in% c(s1, s2),
                               collectDate >= d[1], collectDate <= d[2], !implausible) |> arrange(collectDate)
    if (!nrow(df)) return(plotly_message("No data for this analyte at these sites in this window.", mode()))
    unit <- df$units[1]
    lab <- function(s) paste0(s, " · ", D$sites_meta$siteName[match(s, D$sites_meta$site)])
    df <- df |> mutate(siteLab = ifelse(site == s1, lab(s1), lab(s2)))
    pal <- setNames(c(COL$main, COL$secondary), c(lab(s1), lab(s2)))
    plot_ly(df, x = ~collectDate, y = ~value, color = ~siteLab, colors = pal,
            type = "scatter", mode = "markers+lines", marker = list(size = 6), line = list(width = 2),
            hovertemplate = ~paste0("<b>", siteLab, "</b><br>%{x|%b %d, %Y}<br>%{y:.4g} ", pretty_unit(unit, A), "<extra></extra>")) |>
      layout(hovermode = "x unified",
             yaxis = list(title = axis_title(A, unit)),
             xaxis = list(title = "", type = "date", showspikes = TRUE, spikemode = "across",
                          spikethickness = 1, spikecolor = "#9aa7b0", spikedash = "dot"),
             legend = list(orientation = "h", y = -0.22, x = 0), margin = list(t = 48, b = 74),
             title = list(text = paste0(analyte_display(A), " · two sites"), font = list(size = 14),
                          x = 0, xanchor = "left", y = 0.98, yanchor = "top")) |>
      plotly_theme(mode(), narrow()) |> plotly_clean(paste0(s1, "_vs_", s2, "_", A))
  }, mode()) })

  ## ---- Explore map: markers coloured by the main analyte; click -> select + Compare ----
  output$map <- renderPlotly({ safe_plotly({
    d <- dates_d(); ana <- main_a()
    # exclude implausible singletons so one artifact (ANC 927) can't blow out the
    # continent-wide YlGnBu colorbar and floor every other site
    site_avg <- D$swc_long |>
      filter(analyte == ana, collectDate >= d[1], collectDate <= d[2], !implausible) |>
      group_by(site) |> summarise(avg = mean(value, na.rm = TRUE), nobs = dplyr::n(), .groups = "drop")
    m <- D$sites_meta |> filter(is.finite(lat), is.finite(long)) |>
      left_join(site_avg, by = "site") |> mutate(sel = site == input$site)
    if (!nrow(m)) return(plotly_message("No site coordinates available.", mode()))
    unit  <- pretty_unit(D$analyte_meta$units[D$analyte_meta$analyte == ana][1], ana)
    has_v <- m |> filter(is.finite(avg)); no_v <- m |> filter(!is.finite(avg))
    # heavy-tail-safe colour channel: log10(site avg) for skewed analytes, clamped
    # to p5/p95, colorbar ticking in TRUE units (so PRPO can't wash the rest out)
    csc <- if (nrow(has_v)) map_colour_scale(has_v$avg, ana) else NULL

    geo <- list(scope = "north america", lataxis = list(range = c(15, 72)), lonaxis = list(range = c(-162, -60)),
                showland = TRUE, landcolor = if (identical(mode(), "dark")) "rgba(40,46,54,1)" else "rgba(243,245,247,1)",
                subunitcolor = "rgba(180,190,200,1)", countrycolor = "rgba(180,190,200,1)", bgcolor = "rgba(0,0,0,0)")
    p <- plot_ly(source = "sitemap")
    if (nrow(no_v))
      p <- add_trace(p, data = no_v, type = "scattergeo", mode = "markers", lat = ~lat, lon = ~long,
                     customdata = ~site, showlegend = FALSE,
                     marker = list(size = ifelse(no_v$sel, 14, 7), color = "#cdd5da",
                                   line = list(width = ifelse(no_v$sel, 2.2, .4),
                                               color = ifelse(no_v$sel, COL$secondary, "white"))),
                     text = ~paste0("<b>", siteName, "</b><br>", site, "<br>no ", analyte_display(ana),
                                    " in this window<br><i>tap to select &amp; compare</i>"), hoverinfo = "text")
    if (nrow(has_v))
      p <- add_trace(p, data = has_v, type = "scattergeo", mode = "markers", lat = ~lat, lon = ~long,
                     customdata = ~site, showlegend = FALSE,
                     marker = list(size = ifelse(has_v$sel, 17, 11), color = csc$z,
                                   cmin = csc$cmin, cmax = csc$cmax,
                                   colorscale = "YlGnBu", reversescale = TRUE, showscale = TRUE,
                                   colorbar = list(title = list(
                                                     text = paste0(analyte_display(ana), "<br>", unit,
                                                                   if (isTRUE(csc$log)) "<br><span style='font-size:9px'>(log scale)</span>" else ""),
                                                     font = list(size = 10)),
                                                   tickvals = csc$tickvals, ticktext = csc$ticktext,
                                                   thickness = 12, len = .68, x = 1),
                                   line = list(width = ifelse(has_v$sel, 2.4, .5),
                                               color = ifelse(has_v$sel, COL$secondary, "white"))),
                     text = ~paste0("<b>", siteName, "</b><br>", site, " · ", domain %||% "", "<br>",
                                    analyte_display(ana), ": ", signif(avg, 3), " ", unit, " (avg, n=", nobs, ")",
                                    "<br><i>tap to select &amp; compare</i>"), hoverinfo = "text")
    p |> layout(geo = geo, margin = list(t = 0, b = 0, l = 0, r = 0)) |>
      event_register("plotly_click") |> plotly_theme(mode(), narrow()) |> plotly_clean("neon_sites_map")
  }, mode()) })

  output$map_footer <- renderUI({
    HTML(sprintf("Markers coloured by the average <b>%s</b> at each site over the selected window (darker = higher);
                  grey = not measured there. The selected site is ringed. <b>Tap any marker</b> to choose it and
                  jump to the comparison.", analyte_display(main_a())))
  })

  # ---- Map site-picker: tap a dot -> a choice card (Explore | About) ----------
  # Matches the flagship Small Mammal / Ground Beetle picker: tapping a marker no
  # longer auto-jumps. It opens a small modal offering a CLEAR choice: "Explore
  # this site" (load it and go to Compare) or "About this site" (an instant info
  # card, no load). The Explore button sets input$mapExplore; About sets
  # input$mapInfo. This app has a SINGLE site selectize (no state cascade), so
  # selecting the site already syncs the one sidebar selector, so there is no live
  # sidebar mismatch to fix here. rv$pendingSite is kept for parity with the
  # cascade apps and to document the shared-load pattern.
  rv <- reactiveValues(pendingSite = NULL)

  # the shared load path used by the map choice, the About modal footer, and the
  # browse list: select the site (syncs the sidebar) and jump to Compare.
  load_site <- function(site) {
    if (is.null(site) || length(site) != 1 || !(site %in% D$sites_meta$site)) return(invisible())
    updateSelectizeInput(session, "site", selected = site)
    nav_select("main_tabs", "Compare")
  }

  site_choice_modal <- function(code) {
    sm <- D$sites_meta[D$sites_meta$site == code, ]
    if (!nrow(sm)) return(invisible())
    where <- paste(stats::na.omit(c(as.character(sm$state[1]),
      if (!is.na(sm$domain[1])) paste("NEON", sm$domain[1]) else NA)), collapse = " · ")
    showModal(modalDialog(
      title = tagList(bs_icon("geo-alt-fill"), " ", sm$siteName[1] %||% code,
                      span(class = "scope-note", paste0(" (", code, ")"))),
      easyClose = TRUE, size = "m",
      footer = tagList(
        actionButton("mapInfo_btn", "About this site", class = "btn-outline-secondary",
          onclick = sprintf("Shiny.setInputValue('mapInfo','%s',{priority:'event'});", code)),
        actionButton("mapExplore_btn", tagList("Explore this site ", bs_icon("arrow-right")),
          class = "btn-primary",
          onclick = sprintf("Shiny.setInputValue('mapExplore','%s',{priority:'event'});", code))),
      tags$p(class = "scope-note", style = "margin-bottom:.4rem", where),
      tags$p(HTML(sprintf(
        "<b>%s</b> observations · <b>%s</b> analytes, %s to %s.",
        format(ifelse(is.na(sm$n_obs[1]), 0L, sm$n_obs[1]), big.mark = ","),
        ifelse(is.na(sm$n_analytes[1]), 0L, sm$n_analytes[1]),
        if (!is.na(sm$first[1])) format(sm$first[1], "%b %Y") else "—",
        if (!is.na(sm$last[1])) format(sm$last[1], "%b %Y") else "—"))),
      tags$p(class = "scope-note", "Load this site to compare its analytes, or open the details first.")))
  }

  site_info_modal <- function(code) {
    sm <- D$sites_meta[D$sites_meta$site == code, ]
    if (!nrow(sm)) return(modalDialog(title = "Site info", easyClose = TRUE,
      footer = modalButton("Close"), p("No details are available for this site.")))
    dash <- function(x) if (length(x) == 0 || is.na(x) || !nzchar(as.character(x))) "—" else as.character(x)
    coords <- if (!is.na(sm$lat[1]) && !is.na(sm$long[1])) sprintf("%.3f, %.3f", sm$lat[1], sm$long[1]) else "—"
    modalDialog(
      title = tagList(bs_icon("geo-alt-fill"), " ", sm$siteName[1] %||% code,
                      span(class = "scope-note", paste0(" (", code, ")"))),
      easyClose = TRUE, size = "m",
      footer = tagList(
        modalButton("Close"),
        actionButton("mapExplore_btn2", tagList("Explore this site ", bs_icon("arrow-right")),
          class = "btn-primary",
          onclick = sprintf("Shiny.setInputValue('mapExplore','%s',{priority:'event'});", code))),
      tags$dl(class = "row mb-0",
        tags$dt(class = "col-5", "Where"),
        tags$dd(class = "col-7", paste0(dash(sm$state[1]), " · NEON ", dash(sm$domain[1]))),
        tags$dt(class = "col-5", "Coordinates"),
        tags$dd(class = "col-7", coords),
        tags$dt(class = "col-5", "Site type"),
        tags$dd(class = "col-7", dash(sm$siteType[1])),
        tags$dt(class = "col-5", "Records"),
        tags$dd(class = "col-7", sprintf("%s observations across %s analytes",
          format(ifelse(is.na(sm$n_obs[1]), 0L, sm$n_obs[1]), big.mark = ","),
          ifelse(is.na(sm$n_analytes[1]), 0L, sm$n_analytes[1]))),
        tags$dt(class = "col-5", "Coverage"),
        tags$dd(class = "col-7", sprintf("%s to %s",
          if (!is.na(sm$first[1])) format(sm$first[1], "%b %Y") else "—",
          if (!is.na(sm$last[1])) format(sm$last[1], "%b %Y") else "—"))))
  }

  # Clicking a site on the map opens the Explore | About choice card.
  observeEvent(event_data("plotly_click", source = "sitemap"), {
    ev <- event_data("plotly_click", source = "sitemap")
    site <- ev$customdata
    if (!is.null(site) && length(site) == 1 && site %in% D$sites_meta$site)
      site_choice_modal(site)
  })

  # "Explore this site" (choice card OR About modal footer) -> load it.
  observeEvent(input$mapExplore, {
    removeModal()
    load_site(input$mapExplore)
  })
  # "About this site" -> instant info card (no load).
  observeEvent(input$mapInfo, {
    s <- input$mapInfo
    if (!is.null(s) && nzchar(s)) showModal(site_info_modal(s))
  })

  # Browse-all-sites list (under the Explore map) -> same shared load path:
  # select the site (syncs the sidebar selectize) and jump to Compare.
  observeEvent(input$pickFromList, {
    site <- input$pickFromList
    if (!is.null(site) && length(site) == 1 && site %in% D$sites_meta$site) {
      rv$pendingSite <- site   # documents the shared-pick pattern; single selector syncs directly
      load_site(site)
    }
  })
}

shinyApp(ui, server)
