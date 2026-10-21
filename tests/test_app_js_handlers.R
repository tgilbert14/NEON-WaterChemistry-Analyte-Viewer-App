args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}

# Optional override (first trailing arg) so the guard can be exercised against a
# deliberately broken copy of app.R.
trailing <- commandArgs(trailingOnly = TRUE)
app_path <- if (length(trailing)) trailing[[1]] else file.path(repo_root, "app.R")
src <- paste(readLines(app_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# 1. Shiny's addCustomMessageHandler throws "handler must be a function that
#    takes one argument" for a zero-argument function literal, and everything
#    after it in the same <script> never binds.
handler_re <- "addCustomMessageHandler\\s*\\(\\s*(['\"])([^'\"]+)\\1\\s*,\\s*function\\s*\\(([^)]*)\\)"
calls <- regmatches(src, gregexpr(handler_re, src, perl = TRUE))[[1]]
stopifnot(length(calls) > 0)
handler_names <- sub(handler_re, "\\2", calls, perl = TRUE)
handler_params <- trimws(sub(handler_re, "\\3", calls, perl = TRUE))
empty <- handler_names[!nzchar(handler_params)]
if (length(empty)) {
  stop("addCustomMessageHandler with a zero-argument function: ",
       paste(empty, collapse = ", "), call. = FALSE)
}

# Every addCustomMessageHandler( call must have been matched (no handler passed
# as a variable or in a form the regex above misses).
n_calls <- lengths(regmatches(src, gregexpr("addCustomMessageHandler\\s*\\(", src, perl = TRUE)))
stopifnot(identical(n_calls, length(calls)))

# 2. Every message the server sends must have a client handler.
send_re <- "sendCustomMessage\\s*\\(\\s*(['\"])([^'\"]+)\\1"
sends <- regmatches(src, gregexpr(send_re, src, perl = TRUE))[[1]]
sent_names <- unique(sub(send_re, "\\2", sends, perl = TRUE))
missing <- setdiff(sent_names, handler_names)
if (length(missing)) {
  stop("sendCustomMessage without a matching addCustomMessageHandler: ",
       paste(missing, collapse = ", "), call. = FALSE)
}

cat("JS custom-message handler guard passed (", length(handler_names), " handlers, ",
    length(sent_names), " sent names).\n", sep = "")
