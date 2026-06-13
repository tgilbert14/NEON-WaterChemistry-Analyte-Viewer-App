#----------------------------------------------------------------------
# make_og_image.R — draws docs/og-image.png (1200x630) for the landing card.
# Self-contained base-R graphics; matches the app's aquatic teal identity.
#----------------------------------------------------------------------
ROOT <- getwd()
out  <- file.path(ROOT, "docs", "og-image.png")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

teal <- "#0E7C9B"; teal_d <- "#0a5f78"; teal_l <- "#1597b8"; ink <- "#0B2A3A"

png(out, width = 1200, height = 630, res = 144)
op <- par(mar = c(0, 0, 0, 0), bg = teal); on.exit({ par(op); dev.off() })
plot.new(); plot.window(xlim = c(0, 1200), ylim = c(0, 630), xaxs = "i", yaxs = "i")

# background: teal with a soft top-left glow + faint bubbles
rect(0, 0, 1200, 630, col = teal, border = NA)
for (i in seq(0, 1, length.out = 60)) {           # radial-ish glow top-left
  col <- grDevices::adjustcolor(teal_l, alpha.f = 0.012)
  symbols(180, 560, circles = 30 + i * 720, inches = FALSE, add = TRUE, bg = col, fg = NA)
}
set.seed(7)
for (k in 1:14) {                                  # faint bubbles
  symbols(runif(1, 60, 1140), runif(1, 60, 560), circles = runif(1, 8, 34),
          inches = FALSE, add = TRUE,
          bg = grDevices::adjustcolor("white", alpha.f = runif(1, .02, .05)), fg = NA)
}

# badge
text(70, 552, "NEON · SURFACE WATER CHEMISTRY · DP1.20093.001",
     col = grDevices::adjustcolor("white", .85), cex = .92, font = 2, adj = 0)

# title
text(68, 470, "NEON Water Chemistry", col = "white", cex = 3.6, font = 2, adj = 0)
text(68, 392, "Analyte Viewer",        col = "white", cex = 3.6, font = 2, adj = 0)

# subtitle
text(70, 318, "Compare analytes across 34 NEON aquatic sites — relationships,",
     col = grDevices::adjustcolor("white", .92), cex = 1.18, adj = 0)
text(70, 286, "correlations, seasonality. Real data, statistics that show their work.",
     col = grDevices::adjustcolor("white", .92), cex = 1.18, adj = 0)

# stat chips
chips <- list(c("34", "sites"), c("~197k", "observations"), c("34", "analytes"), c("0", "API waits"))
x0 <- 70; gap <- 14; w <- 250; h <- 96; y1 <- 70
for (i in seq_along(chips)) {
  xl <- x0 + (i - 1) * (w + gap)
  rect(xl, y1, xl + w, y1 + h, col = grDevices::adjustcolor("white", .12), border = NA)
  text(xl + 18, y1 + 64, chips[[i]][1], col = "white", cex = 2.0, font = 2, adj = 0)
  text(xl + 18, y1 + 28, chips[[i]][2], col = grDevices::adjustcolor("white", .85), cex = 1.0, adj = 0)
}
cat("wrote", out, "\n")
