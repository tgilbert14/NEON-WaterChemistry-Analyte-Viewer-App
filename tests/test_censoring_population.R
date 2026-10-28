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

# The shares must come from the site's own matched samples (a date counts when
# both analytes were sampled, with a value or a below-detection flag; it is
# censored when either one is below detection), never from the network-wide
# rate. The facts move with each refresh (a v5 bundle restores the blank
# non-detects the v4 producer dropped, so HOPB iron is no longer "clean"), so the
# test recomputes the expected share independently instead of pinning a site.
site_share <- function(site, partner, main = "specificConductanceField") {
  rows <- D$swc_long |> filter(!implausible, site == !!site, analyte %in% c(main, partner))
  sampled <- rows |> filter(is.finite(value) | belowDetection == 1)
  a <- sampled |> filter(analyte == main) |> group_by(collectDate) |>
    summarise(ba = max(belowDetection), .groups = "drop")
  b <- sampled |> filter(analyte == partner) |> group_by(collectDate) |>
    summarise(bb = max(belowDetection), .groups = "drop")
  j <- inner_join(a, b, by = "collectDate")
  if (!nrow(j)) return(NA_real_)
  mean(j$ba == 1 | j$bb == 1)
}
network_share <- function(partner) mean(D$swc_long$belowDetection[D$swc_long$analyte == partner] == 1)

cases <- list(c("HOPB", "Fe"), c("TOOK", "NO3+NO2 - N"), c("COMO", "Fe"), c("CUPE", "NH4 - N"),
              c("SYCA", "Ortho - P"), c("PRPO", "Fe"), c("ARIK", "Br"))
rows <- list(); disagree <- 0L
for (cs in cases) {
  r <- row_for(cs[1], cs[2]); expect <- site_share(cs[1], cs[2])
  stopifnot(is.finite(expect), isTRUE(all.equal(r$pct_below, expect)),
            identical(isTRUE(r$heavy_censor), expect >= 0.25))
  if (isTRUE(r$heavy_censor)) stopifnot(!r$reliable, grepl("^censored", r$flag))
  if ((expect >= 0.25) != (network_share(cs[2]) >= 0.25)) disagree <- disagree + 1L
  rows[[length(rows) + 1L]] <- r
}
# the audit's defect: at least one of these pairs is on the other side of the
# 25% line from the network rate, and the table follows the site, not the network
stopifnot(disagree >= 1L)

# every finite interval brackets its rho and is NA below the n floor
for (r in rows) {
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
