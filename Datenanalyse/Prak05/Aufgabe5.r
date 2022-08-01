seq <- c(10, 20, 30, 40, 50)

for (n in seq) {
  zaehle_ppcc <- 0
  zaehle_SW <- 0
  zaehle_SF <- 0
  zaehle_KS <- 0
  for (i in 1:1000) {
    data <- rt(n, 3)
    #PPCC Verfahren
    xs <- sort(data)
    pp <- ppoints(xs, a = 0.5)
    qq <- qnorm(pp)
    rs <- cor(qq, xs)
    nt <- c(5, 10, 15, 20, 30, 40, 50, 60, 100)
    rk <- c(0.903, 0.934, 0.951, 0.960, 0.971, 0.977, 0.981, 0.984, 0.989)
    r_krit <- matrix(c(nt, rk), dimnames = list(c(nt), c("n", "r_0.1")), nrow = length(rk), ncol = 2, byrow = FALSE)
    if (rs >= r_krit[as.character(n), 2]) {
      zaehle_ppcc <- zaehle_ppcc + 1
    }
    # Shapiro-Wilk Test
    SW <- shapiro.test(data)
    p.SW <- SW$p.value
    if (p.SW >= 0.1) {
      zaehle_SW <- zaehle_SW + 1
    }
    # Shapiro-Francia Test
    u <- log(n)
    v <- log(u)
    a <- -1.2725 + 1.0531 * (v - u)
    b <- 1.0308 - 0.26758 * (v + 2 / u)
    W <- rs ^ 2
    Z <- (log(1 - W) - a) / b
    p.SF <- pnorm(Z, lower.tail = F)
    if (p.SF >= 0.1) {
      zaehle_SF <- zaehle_SF + 1
    }
    # Kolmogoroff-Smirnoff Test
    KS <- ks.test(data, "pnorm")
    p.KS <- KS$p.value
    if (p.KS >= 0.1) {
      zaehle_KS <- zaehle_KS + 1
    }
  }
  sink("Aufgabe5_Teststärke_df_<3>_Chaudhary_Ghazlan.txt", append = TRUE)
  cat(paste("======================================================================================================================"))
  cat("\n")
  cat(paste("Prozentualen Anteil der Fälle bei denen die Null-Hypothese fälschlicherweise nicht abgelehnt wird für n = ", n))
  cat("\n")
  cat(paste("PPCC =", round((zaehle_ppcc / 10), digits = 3), "%"))
  cat("\n")
  cat(paste("Shapiro-Wilk =", round(zaehle_SW / 10, digits = 3), "%"))
  cat("\n")
  cat(paste("Shapiro-Francia =", round(zaehle_SF / 10, digits = 3), "%"))
  cat("\n")
  cat(paste("Kolmogoroff-Smirnoff =", round(zaehle_KS / 10, digits = 3), "%"))
  cat("\n")
  cat(paste("======================================================================================================================"))
  cat("\n")
  sink()
}