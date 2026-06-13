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
    .vb-door { cursor: pointer; transition: transform .12s ease; }
    .vb-door:hover { transform: translateY(-2px); }
    .info-link { color: var(--bs-secondary); opacity:.7; text-decoration:none; font-size:.85rem; }
    .info-link:hover { opacity:1; }
    .scope-note { color: var(--bs-secondary); font-style: italic; font-size:.82rem; }
    .preset-reason { background: rgba(14,124,155,.06); border-left:3px solid var(--bs-primary);
                     padding:.5rem .75rem; border-radius:.3rem; font-size:.86rem; margin-bottom:.4rem; }
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
    selectInput("preset", tagList("Preset comparison ", info_link("info_preset")),
                choices = c("Custom…" = "", setNames(names(PRESETS), names(PRESETS))),
                selected = names(PRESETS)[1]),
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
    input_dark_mode(id = "color_mode"),
    actionLink("about", tagList(bs_icon("info-circle"), " About this app & data"), class = "info-link")
  ),

  # Summary strip
  uiOutput("summary_strip"),

  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      "Compare", icon = bs_icon("graph-up"),
      card(full_screen = TRUE,
        card_header(div(class = "d-flex justify-content-between align-items-center",
                        span("Two analytes through time"),
                        div(radioButtons("ts_mode", NULL, inline = TRUE,
                              choices = c("Normalized" = "norm", "Dual axis (raw)" = "dual"),
                              selected = "norm"), info_link("info_compare")))),
        uiOutput("preset_reason"),
        uiOutput("ts_note"),
        withSpinner(plotlyOutput("ts", height = 440), type = 8, color = "#0E7C9B", hide.ui = TRUE),
        card_footer(class = "scope-note", textOutput("ts_footer", inline = TRUE)))
    ),
    nav_panel(
      "Relationship", icon = bs_icon("rulers"),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(7, 5)),
        card(full_screen = TRUE,
          card_header(div(class = "d-flex justify-content-between",
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
        card_header(div(class = "d-flex justify-content-between align-items-center",
                        span("Main analyte vs every other analyte"),
                        div(radioButtons("cor_method", NULL, inline = TRUE,
                              choices = c("Spearman" = "spearman", "Pearson" = "pearson"),
                              selected = "spearman"), info_link("info_correlations")))),
        withSpinner(plotlyOutput("cor_lolli", height = 420), type = 8, color = "#0E7C9B", hide.ui = TRUE),
        div(style = "min-height:340px; overflow-x:auto", DTOutput("cor_table")),
        card_footer(class = "scope-note", HTML(
          "Computed on co-sampled dates only (n per row). Screening many analytes at once inflates chance
           findings — hypothesis-generating, not confirmatory. Rows with n &lt; 8 are flagged.")))
    ),
    nav_panel(
      "Seasonal", icon = bs_icon("calendar3"),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)),
        card(full_screen = TRUE,
          card_header(div(class = "d-flex justify-content-between",
                          span("Monthly climatology"), info_link("info_seasonal"))),
          withSpinner(plotlyOutput("clim", height = 380), type = 8, color = "#0E7C9B", hide.ui = TRUE)),
        card(full_screen = TRUE,
          card_header("Seasonal-trend decomposition (STL)"),
          withSpinner(plotlyOutput("stl", height = 380), type = 8, color = "#0E7C9B", hide.ui = TRUE),
          card_footer(class = "scope-note",
            "Descriptive of the real monthly record — not a calibrated forecast. Needs ≥ 24 months."))),
    ),
    nav_panel(
      "Predictor", icon = bs_icon("cpu"),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(5, 7)),
        card(card_header(div(class = "d-flex justify-content-between",
                             span("Estimate the main analyte"), info_link("info_predictor"))),
          uiOutput("pred_intro"), uiOutput("pred_sliders")),
        card(card_header("Prediction"),
          uiOutput("pred_out"),
          card_footer(class = "scope-note",
            "glm on the 3 best-correlated analytes; cross-validated RMSE shown. Interpolation aid, not a sensor.")))
    ),
    nav_panel(
      "Data", icon = bs_icon("table"),
      card(full_screen = TRUE,
        card_header(div(class = "d-flex justify-content-between align-items-center",
          span("Data table"),
          div(class = "btn-group btn-group-sm",
              downloadButton("dl_long", "Tidy CSV", class = "btn-sm btn-outline-primary"),
              downloadButton("dl_wide", "Wide CSV", class = "btn-sm btn-outline-secondary"),
              downloadButton("dl_dict", "Data dictionary", class = "btn-sm btn-outline-secondary"),
              info_link("info_data")))),
        withSpinner(DTOutput("data_table"), type = 8, color = "#0E7C9B", hide.ui = TRUE))
    ),
    nav_panel(
      "Site map", icon = bs_icon("geo-alt"),
      card(full_screen = TRUE,
        card_header("NEON aquatic sites — selected site highlighted"),
        withSpinner(plotlyOutput("map", height = 500), type = 8, color = "#0E7C9B", hide.ui = TRUE),
        card_footer(class = "scope-note", "Hover a site for its record length and analyte coverage."))
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
          tags$li(HTML("Pick a <b>field site</b> and a <b>date range</b> in the sidebar.")),
          tags$li(HTML("Choose a <b>main analyte</b> and one to <b>compare</b> it against — or start from a preset.")),
          tags$li(HTML("Move through the tabs: time series, relationship, correlations, seasonality, and a quick predictor."))),
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
  observeEvent(input$info_preset, {
    showModal(modalDialog(title = "Preset comparisons", easyClose = TRUE, footer = modalButton("Close"),
      HTML("Each preset sets both analytes to a scientifically meaningful pair (e.g. specific conductance vs ANC,
            or total vs bioavailable phosphorus). Pick <b>Custom…</b> to choose any two analytes yourself.")))
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

  ## ---- Summary strip ----
  output$summary_strip <- renderUI({
    p <- sel_pair(); n <- nrow(p)
    # Spearman, n>=8 to color "strong" — consistent with the Correlations tab default
    r <- if (n >= 8) suppressWarnings(stats::cor(p$x, p$y, method = "spearman")) else NA_real_
    sm <- D$sites_meta[D$sites_meta$site == input$site, ]
    sp <- dates_d()
    yrs <- round(as.numeric(difftime(sp[2], sp[1], units = "days")) / 365.25, 1)
    r_theme <- if (is.na(r)) "secondary" else if (abs(r) >= 0.7) "success" else if (abs(r) >= 0.4) "warning" else "secondary"
    layout_columns(
      col_widths = breakpoints(sm = 6, lg = 3), fill = FALSE,
      value_box("Paired samples", n, "matched collection dates",
                showcase = bs_icon("droplet-half"), theme = "primary"),
      value_box("Date span", paste0(format(sp[1], "%Y"), "–", format(sp[2], "%Y")),
                paste0(yrs, " yrs of record"), showcase = bs_icon("calendar-range"), theme = "secondary"),
      div(class = "vb-door", role = "button", tabindex = "0",
          `aria-label` = "Open the Relationship tab for this analyte pair",
          onclick = "Shiny.setInputValue('goto_rel', Math.random(), {priority:'event'})",
          onkeydown = "if(event.key==='Enter'||event.key===' '){event.preventDefault();Shiny.setInputValue('goto_rel', Math.random(), {priority:'event'})}",
          value_box("Correlation (Spearman ρ)",
                    ifelse(is.na(r), "—", sprintf("%.2f", r)),
                    paste0(analyte_display(main_a()), " vs ", analyte_display(sec_a()),
                           if (n > 0) paste0(" · n = ", n) else ""),
                    showcase = bs_icon("graph-up"), theme = r_theme)),
      value_box(input$site, sm$siteName %||% input$site,
                paste0(sm$domain %||% "", if (!is.na(sm$state %||% NA)) paste0(" · ", sm$state) else ""),
                showcase = bs_icon("geo-alt"), theme = "dark")
    )
  })
  observeEvent(input$goto_rel, nav_select("main_tabs", "Relationship"))

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
        value_box("R²", sprintf("%.3f", f$r2), "variance explained", theme = r2_theme),
        value_box("Adjusted R²", sprintf("%.3f", f$adj_r2), "penalized for predictors", theme = "secondary"),
        value_box("Slope", sprintf("%.3g", f$slope), sprintf("± %.2g (SE)", f$slope_se), theme = "secondary"),
        value_box("n", f$n, "paired samples", theme = "primary")),
      div(class = "px-2 pb-2",
        tags$p(HTML(sprintf("<b>p-value (slope):</b> %s",
                            ifelse(f$p < .001, "&lt; 0.001", signif(f$p, 3))))),
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
    ct <- ct |> mutate(display = factor(display, levels = rev(display)))
    pp <- ggplot(ct, aes(x = coef, y = display)) +
      geom_vline(xintercept = 0, color = "rgba(0,0,0,.3)") +
      geom_segment(aes(x = 0, xend = coef, yend = display, color = reliable), linewidth = .9) +
      geom_point(aes(color = reliable, text = paste0(display, "<br>", input$cor_method, " = ", signif(coef, 3),
                     "<br>n = ", n, ifelse(reliable, "", "<br>⚠ low n — interpret with care"))), size = 3) +
      scale_color_manual(values = c(`TRUE` = COL$main, `FALSE` = "#BBBBBB"),
                         labels = c(`TRUE` = "n ≥ 8", `FALSE` = "n < 8"), name = NULL) +
      scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .5)) +
      labs(x = paste0(tools::toTitleCase(input$cor_method), " correlation with ", analyte_display(main_a())), y = NULL) +
      theme_neon()
    ggplotly(pp, tooltip = "text") |> plotly_theme(mode(), narrow()) |> plotly_clean(paste0(input$site, "_correlations"))
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
             legend = list(orientation = "h", y = 1.08), margin = list(t = 56, b = 40),
             title = list(
               text = paste0("STL — ", analyte_display(main_a()),
                             "<br><span style='font-size:11px;color:#9aa4ad'>",
                             n_real, " of ", n_tot, " months real · ", n_fill, " interpolated before decomposition</span>"),
               font = list(size = 14), x = 0, xanchor = "left"))
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
                "from the slider values", showcase = bs_icon("magic"), theme = "primary"),
      div(class = "px-2 pt-2 scope-note",
          sprintf("Cross-validated RMSE ≈ %s %s vs %s %s for a mean-only baseline (skill %s) on %d records.",
                  ifelse(is.na(cv$rmse), "—", signif(cv$rmse, 3)), unit,
                  ifelse(is.na(cv$null_rmse), "—", signif(cv$null_rmse, 3)), unit,
                  ifelse(is.na(skill), "—", sprintf("%+.0f%%", 100 * skill)), cv$n))
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

  ## ---- Site map ----
  output$map <- renderPlotly({ safe_plotly({
    m <- D$sites_meta |> mutate(sel = site == input$site) |> filter(is.finite(lat), is.finite(long))
    if (!nrow(m)) return(plotly_message("No site coordinates available.", mode()))
    plot_ly(m, type = "scattergeo", mode = "markers", lat = ~lat, lon = ~long,
            marker = list(size = ~ifelse(sel, 14, 7),
                          color = ~ifelse(sel, COL$secondary, "#9aa0a6"),
                          line = list(width = ~ifelse(sel, 1.6, .4), color = "white")),
            text = ~paste0("<b>", siteName, "</b><br>", site, " · ", domain %||% "", "<br>",
                           n_obs %||% 0, " samples · ", n_analytes %||% 0, " analytes<br>", first, " – ", last),
            hoverinfo = "text") |>
      layout(geo = list(scope = "north america",
                        lataxis = list(range = c(15, 72)), lonaxis = list(range = c(-162, -60)),
                        showland = TRUE,
                        landcolor = if (identical(mode(), "dark")) "rgba(40,46,54,1)" else "rgba(243,245,247,1)",
                        subunitcolor = "rgba(180,190,200,1)", countrycolor = "rgba(180,190,200,1)",
                        bgcolor = "rgba(0,0,0,0)"),
             margin = list(t = 0, b = 0, l = 0, r = 0)) |>
      plotly_theme(mode(), narrow()) |> plotly_clean("neon_sites_map")
  }, mode()) })
}

shinyApp(ui, server)
