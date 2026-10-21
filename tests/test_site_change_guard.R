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
# resolver auto-enters a deep-linked or resumed site; a leaflet widget rendered
# into display:none never gets its view or layers. The map must re-render once
# on "Change site": renderLeaflet reads map_render_nonce, the resolver marks the
# hidden render, and the changeSite observer bumps the nonce.
stopifnot(grepl("renderLeaflet\\(\\{\\s*\\n\\s*map_render_nonce\\(\\)", src, perl = TRUE))
stopifnot(grepl("map_hidden_render\\(TRUE\\)", src, perl = TRUE))
change_site <- regmatches(src, regexpr("observeEvent\\(input\\$changeSite(.|\n){0,900}", src, perl = TRUE))
stopifnot(length(change_site) == 1L,
          grepl("map_render_nonce\\(map_render_nonce\\(\\) \\+ 1L\\)", change_site, perl = TRUE))
cat("Picker-map re-render guard passed.\n")
