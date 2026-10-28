args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
src <- paste(readLines(file.path(repo_root, "app.R"), warn = FALSE,
                       encoding = "UTF-8"), collapse = "\n")

# 2026-10-20 production regression: freezeReactiveValue() on the analyte or
# date inputs cancels every reactive that reads them for the flush, and the
# Shiny client never resends an input whose value did not change, so a site
# change that kept the same analytes left every plot blank until a manual
# nudge. Site-dependent inputs are re-set by update* calls instead; they must
# never be frozen.
frozen <- regmatches(src, gregexpr(
  "freezeReactiveValue\\s*\\(\\s*input\\s*,\\s*['\"](dates|analyte_main|analyte_secondary|site)['\"]",
  src, perl = TRUE))[[1]]
if (length(frozen)) {
  stop("app.R freezes a site-dependent input (plots stay blank after a site change): ",
       paste(frozen, collapse = "; "), call. = FALSE)
}

# The browser tab must carry a plain-string title and a page language
# (audit item 21): a tagList title renders as raw '<svg ...' in the tab, and
# window_title is not a page_fillable() argument.
page_call <- regmatches(src, regexpr("page_fillable\\((.|\n){0,600}", src, perl = TRUE))
stopifnot(length(page_call) == 1L)
stopifnot(grepl("title\\s*=\\s*\"[^\"]+\"", page_call, perl = TRUE))
stopifnot(grepl("lang\\s*=\\s*\"en\"", page_call, perl = TRUE))
stopifnot(!grepl("window_title\\s*=", page_call, perl = TRUE))

cat("Site-change guard passed: no frozen site-dependent inputs; plain page title and lang set.\n")

# The picker map's first render lands behind a hidden splash when the startup
# resolver auto-enters a deep-linked or resumed site; the leaflet binding keeps it
# as pendingRenderData until the widget's own resize() runs with a real size.
# kickMaps must call that resize() (via HTMLWidgets.find('#map')), and the server
# must not force a second render: two map instances race each other's pending
# Leaflet events and throw "_leaflet_pos" page errors.
kick <- regmatches(src, regexpr("addCustomMessageHandler\\('kickMaps'(.|\n){0,900}", src, perl = TRUE))
stopifnot(length(kick) == 1L,
          grepl("HTMLWidgets.find\\('#map'\\)", kick, perl = TRUE),
          grepl("\\.resize\\(m\\.offsetWidth, m\\.offsetHeight\\)", kick, perl = TRUE))
stopifnot(!grepl("map_render_nonce", src, fixed = TRUE))
cat("Picker-map pending-render guard passed.\n")
