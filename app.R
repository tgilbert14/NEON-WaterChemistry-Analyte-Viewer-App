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

# Site choices: rich labels, only sites that actually have data
site_tbl   <- D$sites_meta |> arrange(siteName)
SITE_CHO   <- setNames(site_tbl$site, paste0(site_tbl$site, " — ", site_tbl$siteName))
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
  bg = "#FBFDFE", fg = "#13242c",
  primary = "#0E7C9B", secondary = "#5B7A8C",
  success = "#2E8B6F", info = "#3E92CC", warning = "#C98A1E", danger = "#C44536",
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

    /* --- subtle water motion (tasteful, not gaudy) --- */
    .navbar, .bslib-page-navbar > .navbar {
      background-image: linear-gradient(110deg,#0a5f78 0%,#0E7C9B 28%,#1aa0c0 50%,#0E7C9B 72%,#0a5f78 100%) !important;
      background-size: 280% 100%; animation: waterflow 22s ease-in-out infinite;
    }
    @keyframes waterflow { 0%,100% { background-position:0% 50%; } 50% { background-position:100% 50%; } }
    /* soft light-on-water sheen across the coloured value boxes */
    .value-box { position:relative; overflow:hidden; }
    .value-box::after { content:''; position:absolute; inset:0; pointer-events:none;
      background: radial-gradient(130% 90% at 12% -10%, rgba(255,255,255,.14), transparent 55%); }
    /* respect reduced-motion preference */
    @media (prefers-reduced-motion: reduce) { .navbar { animation:none !important; } }
  ")

## ---- Per-tab info-modal content (progressive disclosure) -----------------
INFO <- list(
  compare = list("How the time series works", HTML(
    "Both analytes are matched on collection date and drawn on a shared time axis. <b>Normalized</b>
     mode standardizes each series to mean 0, SD 1 so you compare <i>shape</i> honestly even when the
     units differ. <b>Dual axis (raw)</b> shows the original values on two independent scales — useful,
     but the proximity of two lines on independent axes does <i>not</i> by itself imply correlation
     (use the Relationship tab to test that). Open markers are below the analytical detection limit.
     Source: NEON SWC DP1.20093.001 (external lab + field probe).")),
  relationship = list("How the regression works", HTML(
    "An ordinary least-squares line fit to date-paired samples. <b>R²</b> is the share of variance in the
     secondary analyte explained by the main one; the <b>p-value</b> tests whether the slope differs from
     zero; the shaded band is the 95% confidence interval. These are repeated measures through time, so
     observations are not independent — temporal autocorrelation makes the p-value optimistic (a lag-1
     autocorrelation flag is shown). Correlation is not causation: both analytes may track a third driver
     such as discharge or season.")),
  correlations = list("How the correlation screen works", HTML(
    "Each coefficient is computed only on dates where both analytes were sampled (n shown per row).
     <b>Spearman ρ</b> is the headline because water-chemistry data are typically skewed and relationships
     are often monotonic-but-nonlinear; Pearson r (linear) is shown alongside. This screens ~34 analytes
     at once, so some low p-values will appear by chance (multiple comparisons) — treat results as
     hypothesis-generating, never confirmatory. Rows with fewer than 8 paired samples are flagged
     unreliable and greyed.")),
  seasonal = list("How the seasonal view works", HTML(
    "The main analyte is aggregated to monthly means of the <i>real</i> measured series, then split by STL
     (Seasonal-Trend decomposition using Loess) into a smooth <b>trend</b>, a repeating <b>seasonal</b>
     cycle, and a <b>remainder</b>. Requires ≥ 24 months across ≥ 2 years. NEON grab samples are roughly
     monthly with gaps, so <b>interior months with no sample are linearly interpolated before decomposing</b>
     (the count is stamped on the chart); long gaps appear as smooth ramps and can understate real
     variability. This is descriptive of the record — not a calibrated forecast.")),
  predictor = list("How the predictor works", HTML(
    "A generalized linear model (<code>glm</code>) trained on this site's bundled records, estimating the
     main analyte from its three most-correlated analytes. Validated with repeated k-fold cross-validation;
     RMSE (root-mean-square error, in the analyte's units) is the typical out-of-sample miss, shown next
     to a mean-only baseline so you can judge whether the model adds anything. Because the 3 predictors are
     chosen on the full record, the cross-validated RMSE is mildly <b>optimistic</b> — treat it as a floor
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
    relationship, on the same −1 to +1 scale as Spearman — but it assumes the link is linear."),
  n        = HTML("<b>n</b> is the number of dates where <b>both</b> analytes were measured — the sample
    size behind the number. Bigger n = more trustworthy."),
  r2       = HTML("<b>R²</b> is the share of one analyte's variation explained by the other (0–100%).
    Higher = the line fits the points better."),
  pval     = HTML("<b>p-value</b> is roughly the chance you'd see a relationship this strong if there were
    really none. Small (e.g. &lt; 0.05) suggests it isn't a fluke — though it's optimistic for samples
    repeated through time."),
  rmse     = HTML("<b>RMSE</b> is the model's typical miss, in the analyte's units. <b>Skill</b> compares it
    to simply guessing the average: <b>+50%</b> means the model's error is half that naive baseline (and a
    negative skill means it's worse than guessing the average)."),
  stl      = HTML("<b>STL</b> splits the monthly series into a slow <b>trend</b>, a repeating <b>seasonal</b>
    cycle, and leftover noise — computed from the real measured data.")
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
window.addEventListener('resize', function(){ clearTimeout(window.__rt);
  window.__rt=setTimeout(function(){ Shiny.setInputValue('client_w', window.innerWidth, {priority:'event'}); },250); });
")

#======================================================================
# UI
#======================================================================
ui <- page_sidebar(
  title = tagList(bs_icon("droplet-half"), " NEON Water Chemistry — Analyte Viewer"),
  theme = aqua_theme,
  window_title = "NEON Analyte Viewer",
  # Flow content naturally instead of filling the viewport. A fillable page collapses
  # its card bodies to ~0 height when served in an indefinite-height container (Connect
  # Cloud / iframe), which hid every chart on the deployed host. Natural flow + each
  # chart's explicit height = charts always render and the page just scrolls.
  fillable = FALSE,
  tags$head(tags$script(APP_JS),
            tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),

  sidebar = sidebar(
    title = "Controls", width = 340, open = "desktop",
    selectizeInput("site", "Field site", choices = SITE_CHO, selected = DEF_SITE,
                   options = list(placeholder = "Type to search sites…")),
    div(dateRangeInput("dates", "Date range", format = "yyyy-mm", startview = "year",
                       start = DEF_SPAN[1], end = DEF_SPAN[2]),
        actionLink("full_range", "Use full record", class = "info-link"),
        div(class = "scope-note", textOutput("date_hint", inline = TRUE))),
    selectizeInput("analyte_main", "Main analyte", choices = analyte_choices(site_present(DEF_SITE)),
                   selected = DEF_A[1]),
    div(class = "text-center my-1",
        actionButton("swap", tagList(bs_icon("arrow-down-up"), " swap"),
                     class = "btn-sm btn-outline-secondary")),
    selectizeInput("analyte_secondary", "Compare against",
                   choices = analyte_choices(site_present(DEF_SITE)), selected = DEF_A[2]),
    div(class = "scope-note", textOutput("armed", inline = TRUE)),
    hr(),
    selectInput("preset", "Jump to a preset comparison",
                choices = c("Custom…" = "", setNames(names(PRESETS), names(PRESETS))),
                selected = names(PRESETS)[1]),
    div(class = "scope-note", style = "margin-top:-.4rem", "Sets both analytes to a meaningful pair."),
    hr(),
    div(class = "d-flex justify-content-between align-items-center",
        input_dark_mode(id = "color_mode"),
        actionLink("about", "About & data", class = "info-link"))
  ),

  # Summary strip
  uiOutput("summary_strip"),

  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      "Explore", icon = bs_icon("geo-alt"),
      card(full_screen = TRUE,
        card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                        span("Pick a site to explore"),
                        span(class = "scope-note d-none d-md-inline",
                             "Markers are coloured by the main analyte's site average — click any marker"))),
        withSpinner(plotlyOutput("map", height = 540), type = 8, color = "#0E7C9B", hide.ui = TRUE),
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
      "Seasonal pattern", icon = bs_icon("calendar3"),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
        card(full_screen = TRUE,
          card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                          span("Monthly climatology"), info_link("info_seasonal"))),
          withSpinner(plotlyOutput("clim", height = 380), type = 8, color = "#0E7C9B", hide.ui = TRUE)),
        card(full_screen = TRUE,
          card_header("Seasonal-trend decomposition (STL)"),
          withSpinner(plotlyOutput("stl", height = 380), type = 8, color = "#0E7C9B", hide.ui = TRUE),
          card_footer(class = "scope-note",
            "Descriptive of the real monthly record — not a calibrated forecast. Needs ≥ 24 months.")))
    ),
    nav_panel(
      "Predictor", icon = bs_icon("cpu"),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(5, 7)),
        card(card_header(div(class = "d-flex justify-content-between align-items-center gap-3",
                             span("Estimate the main analyte"), info_link("info_predictor"))),
          uiOutput("pred_intro"), uiOutput("pred_sliders")),
        card(card_header("Prediction"),
          uiOutput("pred_out"),
          card_footer(class = "scope-note",
            "glm on the 3 best-correlated analytes; cross-validated RMSE shown. Interpolation aid, not a sensor.")))
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
                            radioButtons("cor_method", NULL, inline = TRUE,
                              choices = c("Spearman" = "spearman", "Pearson" = "pearson"),
                              selected = "spearman"),
                            help_pop("spearman", "Spearman vs Pearson"), info_link("info_correlations")))),
        withSpinner(plotlyOutput("cor_lolli", height = 540), type = 8, color = "#0E7C9B", hide.ui = TRUE),
        div(style = "min-height:340px; overflow-x:auto", DTOutput("cor_table")),
        card_footer(class = "scope-note", HTML(
          "Top 18 shown above (full list in the table). <b style='color:#0E7C9B'>Teal</b> = reliable (n ≥ 8),
           <b style='color:#9aa0a6'>grey</b> = fewer than 8 paired samples. Computed on co-sampled dates only;
           screening many analytes at once inflates chance findings — hypothesis-generating, not confirmatory.")))
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
        withSpinner(plotlyOutput("two_sites", height = 440), type = 8, color = "#0E7C9B", hide.ui = TRUE),
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
          tags$li(HTML("<b>Click a site on the map</b> (coloured by the analyte) — or use the sidebar — to begin.")),
          tags$li(HTML("Choose a <b>main analyte</b> and one to <b>compare</b> it against — or start from a preset.")),
          tags$li(HTML("Explore the tabs: time series, seasonal pattern, a predictor, relationships, correlations, and two-site comparisons."))),
        tags$p(HTML("Loaded with <b>real NEON Surface Water Chemistry data</b> (product DP1.20093.001), bundled for instant results.")),
        checkboxInput("dont_show", "Don't show this again", FALSE),
        footer = actionButton("start", "Start exploring", class = "btn-primary")))
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
         bundled as a read-only dataset (built %s, data through %s). No live downloads — results are instant.</p>
         %s
         <p class='scope-note'>Honesty notes: every statistic shows its sample size; correlations default to
         Spearman with an n ≥ 8 reliability flag; the seasonal view is a real STL decomposition of the
         monthly record (not a fabricated forecast); below-detection values are flagged, not hidden.</p>",
        format(D$built$n_obs %||% 0, big.mark = ","), D$built$n_sites, D$built$n_analytes,
        substr(D$built$when %||% "", 1, 10), D$built$data_through %||% "—",
        if (isTRUE(D$built$partial)) "<p class='scope-note' style='color:#C98A1E'><b>⚠ Partial dataset:</b> built from cached sites only while the full 34-site pull was in progress; counts reflect available data, not the complete product.</p>" else ""))))
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
  sel_long <- reactive({
    req(input$site, main_a(), sec_a()); d <- dates_d(); req(length(d) == 2)
    D$swc_long |> filter(site == input$site, analyte %in% c(main_a(), sec_a()),
                         collectDate >= d[1], collectDate <= d[2])
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
                      main_a(), method = "pearson", min_n = 8)
  }) |> bindCache(input$site, main_a(), dates_d())

  preset_match <- reactive({
    cur <- c(main_a(), sec_a())
    hit <- which(vapply(PRESETS, function(p) all(p == cur), logical(1)))
    if (length(hit)) names(PRESETS)[hit[1]] else NA_character_
  })

  output$armed <- renderText({
    sp <- dates_d()
    paste0(input$site, " · ", format(sp[1], "%Y"), "–", format(sp[2], "%Y"), " · ",
           analyte_display(main_a()), " vs ", analyte_display(sec_a()))
  })

  output$preset_reason <- renderUI({
    pm <- preset_match(); if (is.na(pm)) return(NULL)
    div(class = "preset-reason", bs_icon("lightbulb"), " ", PRESET_REASON[[pm]])
  })

  output$ts_note <- renderUI({
    txt <- if (identical(input$ts_mode, "dual"))
      "Two independent y-axes (left = main, right = secondary, colour-matched). Lines tracking each other does not by itself imply correlation — test it on the Relationship tab."
    else
      "Each series is standardized to mean 0, SD 1 so you compare shape, not magnitude. Raw values are in the hover and on the Data tab."
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
                showcase = spark(ma$collectDate, ma$value, "#9fd6e6"),
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
    if (!nrow(df)) return(plotly_message("No matched samples in this window. Try 'Use full record' in the sidebar.", mode()))
    A <- main_a(); B <- sec_a()
    unitA <- df$units[df$analyte == A][1]; unitB <- df$units[df$analyte == B][1]

    if (identical(input$ts_mode, "dual")) {
      dA <- df |> filter(analyte == A) |> arrange(collectDate)
      dB <- df |> filter(analyte == B) |> arrange(collectDate)
      p <- plot_ly() |>
        add_trace(data = dA, x = ~collectDate, y = ~value, type = "scatter", mode = "markers+lines",
                  name = analyte_display(A), line = list(color = COL$main, width = 2),
                  marker = list(color = COL$main, size = 7),
                  hovertemplate = paste0("<b>", analyte_display(A), "</b><br>%{x|%b %d, %Y}<br>%{y} ",
                                         pretty_unit(unitA, A), "<extra></extra>")) |>
        add_trace(data = dB, x = ~collectDate, y = ~value, yaxis = "y2", type = "scatter", mode = "markers+lines",
                  name = analyte_display(B), line = list(color = COL$secondary, width = 2),
                  marker = list(color = COL$secondary, size = 7),
                  hovertemplate = paste0("<b>", analyte_display(B), "</b><br>%{x|%b %d, %Y}<br>%{y} ",
                                         pretty_unit(unitB, B), "<extra></extra>")) |>
        layout(
          xaxis = list(title = "", type = "date"),
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
        tags$p(HTML(sprintf("<b>Temporal autocorrelation (lag-1):</b> %s (%s) — %s",
                            ifelse(is.na(ac), "—", sprintf("%.2f", ac)), ac_flag,
                            "high values mean the p-value above is optimistic."))))
    )
  })

  ## ---- Correlations ----
  cor_tbl <- reactive({
    correlation_table(wide_site() |> select(-collectDate, -dplyr::any_of("site")),
                      main_a(), method = input$cor_method, min_n = 8)
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
                     "<br>n = ", n, ifelse(reliable, "", "<br>⚠ low n — interpret with care"))), size = 3.2) +
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
      return(datatable(data.frame(Note = "No co-sampled analytes in this window — widen the date range."),
                       rownames = FALSE, options = list(dom = "t")))
    fmtp <- function(p) ifelse(is.na(p), "—", ifelse(p < 1e-4, "<0.0001", formatC(p, format = "g", digits = 2)))
    show <- ct |> transmute(Analyte = display, n,
                            `Spearman ρ` = round(spearman, 3), `p (ρ)` = fmtp(p_spearman),
                            `Pearson r` = round(pearson, 3),  `p (r)` = fmtp(p_pearson),
                            Reading = ifelse(ties, paste0(flag, " †"), flag))
    datatable(show, rownames = FALSE, options = list(pageLength = 8, dom = "tip"),
              caption = htmltools::tags$caption(style = "caption-side:top",
                "Each p-value pairs with the coefficient in the same colour group. † = tied ranks, Spearman p is approximate.")) |>
      formatStyle("Reading", target = "cell",
                  backgroundColor = styleEqual(c("strong +", "strong −", "moderate", "strong + †", "strong − †", "moderate †"),
                                               c("#dff0e8", "#fde7e3", "#fdf3e0", "#dff0e8", "#fde7e3", "#fdf3e0")))
  })

  ## ---- Seasonal: climatology + STL ----
  main_monthly <- reactive({
    d <- dates_d()
    D$swc_long |> filter(site == input$site, analyte == main_a(),
                         collectDate >= d[1], collectDate <= d[2])
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
           title = paste0("Monthly climatology — ", analyte_display(main_a()))) +
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
             legend = list(orientation = "h", y = -0.16, x = 0), margin = list(t = 74, b = 64),
             title = list(
               text = paste0("STL — ", analyte_display(main_a()),
                             "<br><span style='font-size:11px;color:#9aa4ad'>",
                             n_real, " of ", n_tot, " months real · ", n_fill, " interpolated before decomposition</span>"),
               font = list(size = 14), x = 0, xanchor = "left", y = 0.98, yanchor = "top"))
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

  output$pred_out <- renderUI({ tryCatch({
    preds <- pred_predictors(); w <- wide_site()
    if (!length(preds)) return(div(class = "text-muted p-2", "—"))
    d <- w[, c(main_a(), preds), drop = FALSE]; names(d)[1] <- "target"
    d <- d[stats::complete.cases(d), , drop = FALSE]
    if (nrow(d) < (length(preds) + 5)) return(div(class = "text-muted p-2",
      "Too few complete records to fit the model here. Widen the date range."))
    form <- stats::as.formula(paste0("target ~ ", paste(sprintf("`%s`", preds), collapse = " + ")))
    fit <- tryCatch(stats::glm(form, data = d), error = function(e) NULL)
    if (is.null(fit)) return(div(class = "text-danger p-2", "Model could not be fit for this selection."))
    newd <- as.data.frame(lapply(preds, function(p) input[[paste0("pred_", p)]]))
    names(newd) <- preds
    if (any(vapply(newd, function(x) is.null(x) || !is.finite(x), logical(1))))
      return(div(class = "text-muted p-2", "Adjusting to the new selection…"))
    yhat <- tryCatch(as.numeric(stats::predict(fit, newd)), error = function(e) NA_real_)
    cv <- kfold_rmse(w, main_a(), preds)
    skill <- if (is.na(cv$rmse) || is.na(cv$null_rmse) || cv$null_rmse == 0) NA else 1 - cv$rmse / cv$null_rmse
    unit <- pretty_unit(D$analyte_meta$units[D$analyte_meta$analyte == main_a()][1], main_a())
    tagList(
      value_box(paste0("Predicted ", analyte_display(main_a())),
                ifelse(is.na(yhat), "—", paste0(signif(yhat, 4), " ", unit)),
                "from the slider values", theme = "primary"),
      div(class = "px-2 pt-2 scope-note",
          HTML(sprintf("Cross-validated RMSE ≈ %s %s vs %s %s for a mean-only baseline (skill %s) on %d records. ",
                  ifelse(is.na(cv$rmse), "—", signif(cv$rmse, 3)), unit,
                  ifelse(is.na(cv$null_rmse), "—", signif(cv$null_rmse, 3)), unit,
                  ifelse(is.na(skill), "—", sprintf("%+.0f%%", 100 * skill)), cv$n)),
          help_pop("rmse", "RMSE & skill"))
    )
  }, error = function(e) div(class = "text-muted p-2", "Adjusting…")) })

  ## ---- Data table + downloads ----
  # tidy long slice of the current site/range (the analysis-ready export)
  long_slice <- reactive({
    d <- dates_d()
    D$swc_long |>
      filter(site == input$site, collectDate >= d[1], collectDate <= d[2]) |>
      transmute(site, collectDate, analyte, analyte_label = analyte_display(analyte),
                value, units, n_reps, value_sd, below_detection = belowDetection == 1,
                lab_flag = labFlag, source, product = D$built$product) |>
      arrange(collectDate, analyte)
  })

  output$data_table <- renderDT({
    w <- wide_site()
    if (!nrow(w))
      return(datatable(data.frame(Note = "No samples in this window — widen the date range."),
                       rownames = FALSE, options = list(dom = "t")))
    keep <- c("collectDate", intersect(c(main_a(), sec_a()), names(w)),
              setdiff(names(w), c("site", "collectDate", main_a(), sec_a())))
    show <- w[, intersect(keep, names(w)), drop = FALSE] |> arrange(collectDate)
    datatable(show, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"))
  })

  fn_base <- reactive({ sp <- dates_d(); paste0(input$site, "-SWC-", format(sp[1], "%Y"), "-", format(sp[2], "%Y")) })
  provenance <- function() sprintf("# NEON Surface Water Chemistry %s | site %s | built %s | data through %s | values are replicate means (n_reps); below_detection = reported below the analytical detection limit",
                                   D$built$product, input$site, substr(D$built$when, 1, 10), D$built$data_through)

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
      dict <- ANALYTE_TBL |>
        left_join(D$analyte_meta |> select(code = analyte, units, n, n_sites, n_below, source), by = "code") |>
        transmute(code, display, group, indicator, units, n_obs = n, n_sites, n_below, source)
      readr::write_excel_csv(dict, file)
    })

  ## ---- One-click PDF site report (self-contained, base pdf() + ggplot) ----
  output$dl_report <- downloadHandler(
    filename = function() paste0(fn_base(), "-report.pdf"),
    content = function(file) {
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
        annotate("text", x = 0.04, y = 0.92, hjust = 0, fontface = "bold", size = 8, color = COL$main,
                 label = "NEON Water Chemistry") +
        annotate("text", x = 0.04, y = 0.85, hjust = 0, size = 6, label = "Site report") +
        annotate("text", x = 0.04, y = seq(0.72, 0.72 - 0.06 * (length(lines) - 1), by = -0.06),
                 hjust = 0, size = 4.2, label = lines) +
        annotate("text", x = 0.04, y = 0.06, hjust = 0, size = 3, color = "grey50",
                 label = paste0("Generated ", substr(D$built$when, 1, 10), " from a precomputed read-only dataset (no live API)."))
      print(cover)

      # Page 2 — time series (both analytes, faceted, real units)
      sl <- D$swc_long |> filter(site == st, analyte %in% c(A, B), collectDate >= d[1], collectDate <= d[2]) |>
        mutate(lab = factor(ifelse(analyte == A, axis_title(A, uA), axis_title(B, uB)),
                            levels = c(axis_title(A, uA), axis_title(B, uB))))
      if (nrow(sl)) print(
        ggplot(sl, aes(collectDate, value)) +
          geom_line(aes(color = lab), linewidth = .6) + geom_point(aes(color = lab), size = 1.3) +
          scale_color_manual(values = setNames(c(COL$main, COL$secondary), levels(sl$lab)), guide = "none") +
          facet_wrap(~lab, ncol = 1, scales = "free_y", strip.position = "left") +
          labs(x = NULL, y = NULL, title = paste0(sm$siteName %||% st, " — analytes through time")) +
          th + theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90)))

      # Page 3 — regression
      if (!is.null(f)) {
        sub <- sprintf("OLS · R² = %.3f · p %s · n = %d", f$r2,
                       ifelse(f$p < .001, "< 0.001", paste0("= ", signif(f$p, 2))), nrow(pair))
        print(ggplot(pair, aes(x, y)) +
          geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = COL$main, fill = COL$main, alpha = .15) +
          geom_point(color = COL$secondary, alpha = .8, size = 2) +
          labs(x = axis_title(A, uA), y = axis_title(B, uB),
               title = paste0(analyte_display(B), " vs ", analyte_display(A)), subtitle = sub) + th)
      }

      # Page 4 — seasonal climatology of the main analyte
      cl <- D$swc_long |> filter(site == st, analyte == A, collectDate >= d[1], collectDate <= d[2]) |>
        mutate(month = lubridate::month(collectDate, label = TRUE))
      if (nrow(cl) >= 6) print(
        ggplot(cl, aes(month, value)) +
          geom_boxplot(outlier.shape = NA, fill = COL$main, alpha = .18, color = COL$main) +
          geom_jitter(width = .15, height = 0, color = COL$main, alpha = .5, size = 1.4) +
          labs(x = NULL, y = axis_title(A, uA), title = paste0("Monthly climatology — ", analyte_display(A))) + th)
    })

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
                               collectDate >= d[1], collectDate <= d[2]) |> arrange(collectDate)
    if (!nrow(df)) return(plotly_message("No data for this analyte at these sites in this window.", mode()))
    unit <- df$units[1]
    lab <- function(s) paste0(s, " — ", D$sites_meta$siteName[match(s, D$sites_meta$site)])
    df <- df |> mutate(siteLab = ifelse(site == s1, lab(s1), lab(s2)))
    pal <- setNames(c(COL$main, COL$secondary), c(lab(s1), lab(s2)))
    plot_ly(df, x = ~collectDate, y = ~value, color = ~siteLab, colors = pal,
            type = "scatter", mode = "markers+lines", marker = list(size = 6), line = list(width = 2),
            hovertemplate = ~paste0("<b>", siteLab, "</b><br>%{x|%b %d, %Y}<br>%{y} ", pretty_unit(unit, A), "<extra></extra>")) |>
      layout(yaxis = list(title = axis_title(A, unit)), xaxis = list(title = "", type = "date"),
             legend = list(orientation = "h", y = 1.1), margin = list(t = 30),
             title = list(text = paste0(analyte_display(A), " — two sites"), font = list(size = 14), x = 0, xanchor = "left")) |>
      plotly_theme(mode(), narrow()) |> plotly_clean(paste0(s1, "_vs_", s2, "_", A))
  }, mode()) })

  ## ---- Explore map: markers coloured by the main analyte; click -> select + Compare ----
  output$map <- renderPlotly({ safe_plotly({
    d <- dates_d(); ana <- main_a()
    site_avg <- D$swc_long |>
      filter(analyte == ana, collectDate >= d[1], collectDate <= d[2]) |>
      group_by(site) |> summarise(avg = mean(value, na.rm = TRUE), nobs = dplyr::n(), .groups = "drop")
    m <- D$sites_meta |> filter(is.finite(lat), is.finite(long)) |>
      left_join(site_avg, by = "site") |> mutate(sel = site == input$site)
    if (!nrow(m)) return(plotly_message("No site coordinates available.", mode()))
    unit  <- pretty_unit(D$analyte_meta$units[D$analyte_meta$analyte == ana][1], ana)
    has_v <- m |> filter(is.finite(avg)); no_v <- m |> filter(!is.finite(avg))

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
                                    " in this window<br><i>click to select</i>"), hoverinfo = "text")
    if (nrow(has_v))
      p <- add_trace(p, data = has_v, type = "scattergeo", mode = "markers", lat = ~lat, lon = ~long,
                     customdata = ~site, showlegend = FALSE,
                     marker = list(size = ifelse(has_v$sel, 17, 11), color = ~avg,
                                   colorscale = "YlGnBu", reversescale = TRUE, showscale = TRUE,
                                   colorbar = list(title = list(text = paste0(analyte_display(ana), "<br>", unit),
                                                                font = list(size = 10)), thickness = 12, len = .68, x = 1),
                                   line = list(width = ifelse(has_v$sel, 2.4, .5),
                                               color = ifelse(has_v$sel, COL$secondary, "white"))),
                     text = ~paste0("<b>", siteName, "</b><br>", site, " · ", domain %||% "", "<br>",
                                    analyte_display(ana), ": ", signif(avg, 3), " ", unit, " (avg, n=", nobs, ")",
                                    "<br><i>click to select</i>"), hoverinfo = "text")
    p |> layout(geo = geo, margin = list(t = 0, b = 0, l = 0, r = 0)) |>
      event_register("plotly_click") |> plotly_theme(mode(), narrow()) |> plotly_clean("neon_sites_map")
  }, mode()) })

  output$map_footer <- renderUI({
    HTML(sprintf("Markers coloured by the average <b>%s</b> at each site over the selected window (darker = higher);
                  grey = not measured there. The selected site is ringed. <b>Click any marker</b> to choose it and
                  jump to the comparison.", analyte_display(main_a())))
  })

  # Clicking a site on the map selects it and takes the user to the comparison
  observeEvent(event_data("plotly_click", source = "sitemap"), {
    ev <- event_data("plotly_click", source = "sitemap")
    site <- ev$customdata
    if (!is.null(site) && length(site) == 1 && site %in% D$sites_meta$site) {
      updateSelectizeInput(session, "site", selected = site)
      nav_select("main_tabs", "Compare")
    }
  })
}

shinyApp(ui, server)
