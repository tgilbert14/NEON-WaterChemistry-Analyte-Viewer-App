# Regression: per-site censoring on the committed bundle (audit 2026-10-18).
# Runs the committed bundle through the runtime unit contract and the app's
# plausibility gate (app.R data-load block), then correlation_table() with the
# per-site below-detection matrix. A site that is clean for an analyte the
# network censors heavily must not be greyed, and a heavily censored pair must
# be. Also unit-checks the Spearman interval and the effective n on a toy series.
# Offline; helpers.R only loads dplyr + tidyr.
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

D <- apply_runtime_water_unit_contract(readRDS(file.path("data", "neon_swc.rds")))

# same gate as the app.R data-load block
CEIL_MAP <- ceiling_map(plausibility_ceilings(D$swc_long))
D$swc_long$implausible <- !is_plausible(D$swc_long$value, D$swc_long$analyte, CEIL_MAP,
                                        site = D$swc_long$site)
wide <- D$swc_long |> filter(!implausible) |>
  select(site, collectDate, analyte, value) |>
  pivot_wider(names_from = analyte, values_from = value)
bdl <- D$swc_long |> filter(!implausible) |>
  select(site, collectDate, analyte, belowDetection) |>
  pivot_wider(names_from = analyte, values_from = belowDetection, values_fn = max)

row_for <- function(site, partner, main = "specificConductanceField") {
  w <- wide |> filter(site == !!site) |> select(-site)
  b <- bdl  |> filter(site == !!site)
  ct <- correlation_table(w, main, method = "spearman", min_n = 8, bdl_site = b)
  r <- ct[ct$code == partner, , drop = FALSE]
  if (nrow(r) != 1L) stop(sprintf("%s: no %s row against %s", site, partner, main), call. = FALSE)
  r
}

# HOPB iron is mostly detected at this site, though Fe is ~35% below DL network-wide
hopb <- row_for("HOPB", "Fe")
stopifnot(!hopb$heavy_censor)

# TOOK nitrate + nitrite sits near its detection limit: the pair is censored
took <- row_for("TOOK", "NO3+NO2 - N")
stopifnot(took$heavy_censor, !took$reliable, grepl("^censored", took$flag))

# COMO conductance vs iron: enough clean pairs to be a reliable row
como <- row_for("COMO", "Fe")
if (como$n >= 8) stopifnot(como$reliable, !como$heavy_censor)

# CUPE ammonium: censored
cupe <- row_for("CUPE", "NH4 - N")
stopifnot(cupe$heavy_censor, grepl("^censored", cupe$flag))

# every finite interval brackets its rho and is NA below the n floor
for (r in list(hopb, took, como, cupe)) {
  if (is.finite(r$rho_lo)) stopifnot(r$rho_lo <= r$spearman, r$spearman <= r$rho_hi,
                                     r$rho_lo >= -1, r$rho_hi <= 1, r$n >= 8)
}

# ---- toy checks: effective n and the Spearman interval ----------------------
# n_eff = n (1 - rx ry) / (1 + rx ry) only when rx ry > 0
stopifnot(isTRUE(all.equal(effective_n(100, 0.5, 0.5), 100 * 0.75 / 1.25)))
stopifnot(isTRUE(all.equal(effective_n(100, -0.4, -0.5), 100 * 0.8 / 1.2)))
stopifnot(is.na(effective_n(100, 0.5, -0.5)), is.na(effective_n(100, 0, 0.9)),
          is.na(effective_n(100, NA, 0.5)))

# Fisher z with the Bonett-Wright se, on n when n_eff is NA
ci <- spearman_ci(0.6, 50, NA_real_)
se <- sqrt((1 + 0.6^2 / 2) / (50 - 3))
stopifnot(isTRUE(all.equal(ci$lo, tanh(atanh(0.6) - qnorm(0.975) * se))),
          isTRUE(all.equal(ci$hi, tanh(atanh(0.6) + qnorm(0.975) * se))))
# on n_eff when finite: a smaller n_eff gives a wider interval
ci_eff <- spearman_ci(0.6, 50, 20)
stopifnot(ci_eff$lo < ci$lo, ci_eff$hi > ci$hi, ci_eff$n_use == 20)
# NA under the n floor or when n_use <= 3
stopifnot(is.na(spearman_ci(0.6, 7, NA_real_)$lo), is.na(spearman_ci(0.6, 50, 3)$lo))

# an AR(1) pair through correlation_table: n_eff set, below n, interval present
set.seed(7)
nn <- 120
e <- as.numeric(stats::arima.sim(list(ar = 0.7), nn))
toy <- tibble(collectDate = as.Date("2015-01-01") + 30 * seq_len(nn),
              a = e, b = e + as.numeric(stats::arima.sim(list(ar = 0.7), nn)))
ct <- correlation_table(toy[sample(nn), ], "a", method = "spearman")  # row order shuffled
stopifnot(nrow(ct) == 1L, is.finite(ct$acf_main), ct$acf_main > 0.4, ct$acf_partner > 0.4,
          is.finite(ct$n_eff), ct$n_eff < nn,
          isTRUE(all.equal(ct$n_eff, effective_n(nn, ct$acf_main, ct$acf_partner))),
          is.finite(ct$rho_lo), ct$rho_lo < ct$spearman, ct$spearman < ct$rho_hi)

cat("test_censoring_population: OK\n")
