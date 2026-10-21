# Regression: the app's data path must tolerate NA-valued non-detects
# (NEON changeLog 2022-08-18: a non-detect may be reported with a blank
# concentration; the producer keeps it as value = NA, belowDetection = 1).
# Mutates the committed bundle in memory (50 below-detection rows -> NA) and runs
# the same load gate as app.R, then the per-site correlation table, the pair
# censoring helpers, and fit_lm. Offline; helpers.R only loads dplyr + tidyr.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
repo_root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg[[1]]), ".."), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}
old_wd <- setwd(repo_root)
on.exit(setwd(old_wd), add = TRUE)
suppressMessages({ library(dplyr); library(tidyr) })
source(file.path("scripts", "water_unit_contract.R"))
source("helpers.R")

D <- readRDS(file.path("data", "neon_swc.rds"))
site <- "ARIK"
cand <- which(D$swc_long$site == site & D$swc_long$belowDetection == 1 &
              D$swc_long$analyte %in% c("Br", "NO3+NO2 - N", "Fe", "Mn", "TDP"))
stopifnot(length(cand) >= 50)
set.seed(1)
hit <- sample(cand, 50)
D$swc_long$value[hit] <- NA_real_

D <- apply_runtime_water_unit_contract(D)
stopifnot(sum(is.na(D$swc_long$value)) == 50)   # NA non-detects survive the contract

# same gate as app.R data-load block
CANON_MAP <- canonical_units(D$swc_long)
CEIL_MAP  <- ceiling_map(plausibility_ceilings(D$swc_long))
stopifnot(all(is.finite(CEIL_MAP$global)))
D$swc_long$implausible <- !is_plausible(D$swc_long$value, D$swc_long$analyte, CEIL_MAP,
                                        site = D$swc_long$site)
stopifnot(!any(D$swc_long$implausible[is.na(D$swc_long$value)]))  # NA kept, not gated
wide <- D$swc_long |> filter(!implausible) |>
  select(site, collectDate, analyte, value) |>
  pivot_wider(names_from = analyte, values_from = value)
bdl <- D$swc_long |> filter(!implausible) |>
  select(site, collectDate, analyte, belowDetection) |>
  pivot_wider(names_from = analyte, values_from = belowDetection, values_fn = max)
stopifnot(all(vapply(wide[, -(1:2)], is.numeric, logical(1))))

w <- wide |> filter(site == !!site) |> select(-site)
b <- bdl  |> filter(site == !!site)
for (main in c("Br", "NO3+NO2 - N", "Ca")) {
  ct <- correlation_table(w, main, method = "spearman", bdl_site = b)
  stopifnot(nrow(ct) > 0, all(is.finite(ct$coef)), all(ct$n >= 4),
            all(is.finite(ct$pct_below)), all(ct$pct_below >= 0 & ct$pct_below <= 1),
            all(is.finite(ct$pct_below_main)), all(is.finite(ct$pct_below_partner)),
            all(!is.na(ct$flag)))
}

sl <- D$swc_long |> filter(site == !!site, analyte %in% c("Br", "Cl"), !implausible)
pc <- pair_censor_share(sl, "Br", "Cl")
stopifnot(pc$n > 0, is.finite(pc$share))
# ARIK Br: ~0% below DL before 2016, ~100% 2016-2019, ~4% after -> mixed eras
stopifnot(isTRUE(bdl_mixes_eras(sl$collectDate[sl$analyte == "Br"],
                                sl$belowDetection[sl$analyte == "Br"])))

pair <- tibble(collectDate = w$collectDate,
               x = suppressWarnings(as.numeric(w[["Br"]])),
               y = suppressWarnings(as.numeric(w[["Cl"]])))
stopifnot(anyNA(pair$x))                # the injected NA really reaches the pair
f <- fit_lm(pair)                       # fit_lm keeps complete cases itself
stopifnot(!is.null(f), is.finite(f$r2), is.finite(f$slope),
          f$n == sum(is.finite(pair$x) & is.finite(pair$y)))

cv <- kfold_rmse(as.data.frame(w), "Cl", c("Br", "Na"), k = 5, reps = 1)
stopifnot(is.finite(cv$rmse), cv$n > 0)

cat("test_na_nondetect_tolerance: OK\n")
