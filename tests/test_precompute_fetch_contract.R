# Fetch-side contract of the refresh: the NEON token goes only to NEON hosts,
# a CI run without the token stops, blank-like lab rows are counted (never
# dropped), the fetch receipt round-trips, and the miss-rate gate warns on any
# miss and stops above 1%. The pure helpers are loaded by name from the scripts
# so nothing here touches the network.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}

load_functions <- function(path, names, envir) {
  exprs <- parse(path, keep.source = FALSE)
  found <- character()
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% names) {
      eval(e, envir)
      found <- c(found, as.character(e[[2]]))
    }
  }
  missing <- setdiff(names, found)
  if (length(missing)) stop("Not found in ", path, ": ", paste(missing, collapse = ", "))
  invisible(envir)
}

env <- new.env(parent = globalenv())
assign("%||%", function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}, envir = env)
load_functions(file.path(repo_root, "scripts", "precompute_neon_data.R"),
               c("water_url_host", "water_neon_token_host_ok",
                 "water_neon_headers", "water_require_neon_token",
                 "water_blank_like_rows", "water_fetch_receipt"), env)
load_functions(file.path(repo_root, "scripts", "refresh_data.R"),
               c("water_read_fetch_receipt", "water_fetch_receipt_gate",
                 "water_refresh_summary_lines", "water_write_step_summary"), env)

expect_error <- function(expr, pattern) {
  msg <- tryCatch({ force(expr); "" }, error = conditionMessage)
  if (!nzchar(msg) || !grepl(pattern, msg, fixed = TRUE)) {
    stop(sprintf("Expected an error matching '%s', got '%s'", pattern, msg))
  }
  invisible(msg)
}

with(env, {
  # 1) Token host rule.
  ok <- c(
    "https://data.neonscience.org/api/v0/data/DP1.20093.001/SYCA/2024-05",
    "https://DATA.neonscience.org:443/api/v0/sites/SYCA",
    "https://storage.neonscience.org/neon-publication/x.csv?sig=abc"
  )
  bad <- c(
    "http://data.neonscience.org/api/v0/sites/SYCA",          # not https
    "https://storage.googleapis.com/neon-publication/x.csv?X-Goog-Signature=1",
    "https://neonscience.org.evil.example/x",                 # suffix trick
    "https://evilneonscience.org/x",                          # no dot boundary
    "https://data.neonscience.org@evil.example/x",            # userinfo
    "https://neonscience.org/x",                              # bare apex
    "ftp://data.neonscience.org/x", "", NA
  )
  stopifnot(
    all(water_neon_token_host_ok(ok)),
    !any(water_neon_token_host_ok(bad)),
    identical(water_url_host("https://Data.NeonScience.org:8443/a?b=c"),
              "data.neonscience.org"),
    identical(water_url_host("https://user@host.example/a"), ""),
    identical(water_neon_headers(ok[[1]], "tok"), c("X-API-Token" = "tok")),
    is.null(water_neon_headers(bad[[2]], "tok")),
    is.null(water_neon_headers(ok[[1]], ""))
  )

  # 2) CI without a token stops and names the secret; local runs only note it.
  expect_error(water_require_neon_token("true", ""), "NEON_TOKEN repository secret")
  stopifnot(isTRUE(water_require_neon_token("true", "tok")),
            isTRUE(water_require_neon_token("", "")),
            isTRUE(water_require_neon_token("false", "")))

  # 3) Blank-like rows: whichever identifier columns exist, case-insensitive.
  lab <- data.frame(
    sampleID = c("SYCA.20240501.BLANK.1", "SYCA.20240501", NA, "x"),
    namedLocation = c("SYCA.AOS.S2", "SYCA.AOS.fieldBlank", "y", NA),
    analyte = "Cl", stringsAsFactors = FALSE
  )
  stopifnot(
    identical(water_blank_like_rows(lab), 2L),
    identical(water_blank_like_rows(lab[, "analyte", drop = FALSE]), 0L),
    identical(water_blank_like_rows(NULL), 0L),
    identical(water_blank_like_rows(lab[0, ]), 0L)
  )

  # 4) Receipt: hosts only (no query string), misses capped at 100, JSON
  #    round trip keeps arrays even of length 0 or 1.
  misses <- sprintf("SYCA 20%02d-01 data-api", 1:150 %% 100)
  r <- water_fetch_receipt(
    1000, 990, misses,
    c("https://storage.googleapis.com/a.csv?X-Goog-Signature=SECRET",
      "https://data.neonscience.org/b.csv", NA),
    c("siteID", "analyte"), 3L
  )
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(r, path, auto_unbox = TRUE, pretty = TRUE)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  back <- water_read_fetch_receipt(path)
  stopifnot(
    !grepl("SECRET", text, fixed = TRUE), !grepl("?", text, fixed = TRUE),
    identical(back$file_hosts, c("data.neonscience.org", "storage.googleapis.com")),
    identical(back$n_miss, 150L), length(back$misses) == 100L,
    identical(back$lab_columns, c("siteID", "analyte")),
    identical(back$n_blank_like_rows, 3L)
  )
  empty <- water_fetch_receipt(10, 10, character(), character(), character(), 0L)
  jsonlite::write_json(empty, path, auto_unbox = TRUE)
  back_empty <- water_read_fetch_receipt(path)
  stopifnot(identical(back_empty$misses, character()),
            identical(back_empty$file_hosts, character()),
            identical(back_empty$n_miss, 0L))
  unlink(path)
  stopifnot(is.null(water_read_fetch_receipt(path)))

  # 5) Gate: no miss = silent; any miss = warning; > 1% = error.
  gate <- function(n_sm, n_miss) water_fetch_receipt_gate(list(
    n_site_months = n_sm, n_miss = n_miss,
    misses = rep("SYCA 2024-01 data-api", n_miss)
  ))
  g0 <- gate(1000L, 0L); g1 <- gate(1000L, 10L); g2 <- gate(1000L, 11L)
  stopifnot(
    is.null(g0$warning), is.null(g0$error),
    grepl("10 of 1000", g1$warning, fixed = TRUE), is.null(g1$error),
    !is.null(g2$warning), grepl("more than 1%", g2$error, fixed = TRUE),
    !is.null(water_fetch_receipt_gate(NULL)$error),
    !is.null(water_fetch_receipt_gate(list(n_site_months = NA, n_miss = 0))$error)
  )

  # 6) Job summary: lines go to GITHUB_STEP_SUMMARY only when it is set.
  lines <- water_refresh_summary_lines(
    r, list(n_obs = 100L, data_through = "2026-07-01"),
    list(n_obs = 120L, data_through = "2026-09-01",
         n_value_rows_quarantined = 110L,
         value_quarantine_receipt = data.frame(n_excluded = c(1L, 0L, 2L)))
  )
  stopifnot(
    any(grepl("Observations: 100 to 120", lines, fixed = TRUE)),
    any(grepl("Data through: 2026-07-01 to 2026-09-01", lines, fixed = TRUE)),
    any(grepl("110 source rows over 2 identities", lines, fixed = TRUE)),
    any(grepl("Site-months missed: 150", lines, fixed = TRUE)),
    any(grepl("Blank-like lab rows", lines, fixed = TRUE)),
    !any(grepl("—", lines, fixed = TRUE))
  )
  summary_path <- tempfile()
  old <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = NA)
  Sys.setenv(GITHUB_STEP_SUMMARY = summary_path)
  water_write_step_summary(lines)
  stopifnot(identical(readLines(summary_path), lines))
  Sys.unsetenv("GITHUB_STEP_SUMMARY")
  stopifnot(!water_write_step_summary(lines))
  if (!is.na(old)) Sys.setenv(GITHUB_STEP_SUMMARY = old)
  unlink(summary_path)
  # A run that stopped before the build still gets readable lines.
  stopifnot(length(water_refresh_summary_lines(list(), NULL, list())) > 5L)
})

cat("Precompute fetch contract (token host rule, CI token, blank-like rows, receipt, miss gate, summary) passed.\n")
