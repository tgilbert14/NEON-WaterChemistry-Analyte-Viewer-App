args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
source(file.path(repo_root, "scripts", "build_swc_bundle.R"))

root <- tempfile("water-save-bundle-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE), add = TRUE)
out <- file.path(root, "neon_swc.rds")

old <- list(version = "old", values = 1:3)
new <- list(version = "new", values = 4:6)
stale <- list(version = "stale-collision")
saveRDS(old, out)
old_md5 <- unname(tools::md5sum(out))

collision_time <- as.POSIXct("2026-08-03 12:34:56", tz = "UTC")
collision_backup <- sub(
  "[.]rds$", format(collision_time, "_%Y%m%d-%H%M%S.rds"), out
)
saveRDS(stale, collision_backup)
stale_md5 <- unname(tools::md5sum(collision_backup))

collision_error <- tryCatch({
  save_bundle(new, out, backup_time = collision_time)
  ""
}, error = conditionMessage)
stopifnot(nzchar(collision_error))
stopifnot(identical(unname(tools::md5sum(out)), old_md5))
stopifnot(identical(unname(tools::md5sum(collision_backup)), stale_md5))
stopifnot(identical(readRDS(out), old))

success_time <- as.POSIXct("2026-08-03 12:34:57", tz = "UTC")
success_backup <- sub(
  "[.]rds$", format(success_time, "_%Y%m%d-%H%M%S.rds"), out
)
save_bundle(new, out, backup_time = success_time)
stopifnot(file.exists(success_backup))
stopifnot(identical(readRDS(success_backup), old))
stopifnot(identical(readRDS(out), new))
stopifnot(!length(list.files(root, pattern = "[.]pending-", full.names = TRUE)))

# Force the promotion copy to leave corrupt bytes and report failure. The same
# injected copy function then permits the recovery copy, proving that the exact
# prior bytes are restored before save_bundle() reports the failed promotion.
saveRDS(old, out)
old_md5 <- unname(tools::md5sum(out))
recovery_time <- as.POSIXct("2026-08-03 12:34:58", tz = "UTC")
recovery_backup <- sub(
  "[.]rds$", format(recovery_time, "_%Y%m%d-%H%M%S.rds"), out
)
copy_calls <- 0L
fail_promotion_copy <- function(from, to, overwrite = FALSE) {
  copy_calls <<- copy_calls + 1L
  if (copy_calls == 2L) {
    writeBin(charToRaw("intentionally incomplete promotion"), to)
    return(FALSE)
  }
  file.copy(from, to, overwrite = overwrite)
}
recovery_error <- tryCatch({
  save_bundle(
    new, out, backup_time = recovery_time,
    copy_file = fail_promotion_copy
  )
  ""
}, error = conditionMessage)
stopifnot(grepl("prior bytes restored: yes", recovery_error, fixed = TRUE))
stopifnot(identical(copy_calls, 3L))
stopifnot(file.exists(recovery_backup))
stopifnot(identical(unname(tools::md5sum(recovery_backup)), old_md5))
stopifnot(identical(unname(tools::md5sum(out)), old_md5))
stopifnot(identical(readRDS(out), old))
stopifnot(!length(list.files(root, pattern = "[.]pending-", full.names = TRUE)))

cat(paste0(
  "Bundle backup collision, exact promotion, and forced recovery-path ",
  "regressions passed.\n"
))
