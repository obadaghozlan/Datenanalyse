# Datein einlesen
file_arkona <- "Arkona.txt"
file_norderney <- "Norderney.txt"

data_arkona <- read.table(file = file_arkona, header = TRUE, dec = ".", sep = "", strip.white = TRUE, na.strings = "-999")
data_norderney <- read.table(file = file_norderney, header = TRUE, dec = ".", sep = "", strip.white = TRUE, na.strings = "-999")

# Aufgabe1
monats_summe_arkona <- vector(mode = "numeric", length = 12)
monats_summe_norderney <- vector(mode = "numeric", length = 12)

monats_summe_arkona <- aggregate(x = data_arkona$RSK, by = list(monat = data_arkona$Monat), FUN = sum, na.rm = TRUE)
monats_summe_norderney <- aggregate(x = data_norderney$RSK, by = list(monat = data_norderney$Monat), FUN = sum, na.rm = TRUE)

# Aufgabe2
row_names = c("Monat", "Arkona", "Norderney")
col_names = c("JAN", "FEB", "MAR", "APR", "MAI", "JUN", "JUL", "AUG", "SEP", "OKT", "NOV", "DEZ")

monats_summe_arkona_norderney <- data.frame(monat = col_names,
                                            arkona = monats_summe_arkona$x,
                                            norderney = monats_summe_norderney$x)

print("Die akkumilierte Niederschlagsumme (Monatssumme)")
print(monats_summe_arkona_norderney)

jpeg("Aufgabe2_Barplot_Chaudhary_Ghazlan.jpg", width = 1600, height = 960, units = "px", pointsize = 14, quality = 100, bg = "white", res = 144)
barplot(t(as.matrix(monats_summe_arkona_norderney[, 2:3])),
        beside = TRUE,
        legend.text = TRUE,
        names.arg = col_names,
        ylim = c(0, 140),
        main = "Balkendiagramm zum vergleich der Niederschlagssumme zweier Datensätze pro Monat",
        ylab = "Niederschlagssumme in mm",
        xlab = "Monat",
        col = c("darkblue", "red"))
dev.off()

# Aufgabe3
jpeg("Aufgabe3_Boxplot_Chaudhary_Ghazlan.jpg", width = 1200, height = 960, units = "px", pointsize = 14, quality = 100, bg = "white", res = 144)
boxplot((as.matrix(monats_summe_arkona_norderney[, 2:3])),
        main = "Boxplot der 12 Monatssummen der beiden Datensätze",
        ylab = "Niederschlagsumme in mm")
dev.off()

# Aufgabe4
cor_test_kendall <- cor.test(monats_summe_arkona$x, monats_summe_norderney$x, method = "kendall", alternative = "two.sided")
if (cor_test_kendall$p.value < 0.1) {
  print(paste("Nach Rangkorrelationstest nach Kendall ist das p-wert = ", round(cor_test_kendall$p.value, 4), "und da p-wert(2*p) < 0.1 True ist kann die Nullhypotheses abgelehnt werden. Das heißt, die beiden 12-monatigen Datensätze weisen signifikante Korrelation auf"))

  # Aufgabe5
  paardifferenz <- monats_summe_arkona$x - monats_summe_norderney$x
  nor_test_paardifferenz <- shapiro.test(paardifferenz)
  if (nor_test_paardifferenz$p.value < 0.1) {
    print(paste("Nach Shapiro-Wilk Test ist das p-wert = ", round(nor_test_paardifferenz$p.value, 4), "und da p-wert < 0.1 True ist kann die Nullhypotheses abgelehnt werden. Das heißt, dass die daten nicht normalverteilt sind."))

    # Aufgabe6
    wilcox_vorzeichen_rangtest <- wilcox.test(monats_summe_arkona$x, monats_summe_norderney$x, alternative = "two.sided", paired = TRUE)
    if (wilcox_vorzeichen_rangtest$p.value < 0.05) {
      print(paste("Nach Wilcox vorzeichen rangtest ist das p-wert = ", round(wilcox_vorzeichen_rangtest$p.value, 4), "und da p-wert < 0.05 True ist kann die Nullhypotheses abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen signifikanten Unterschied."))
    } else {
      print(paste("Nach Wilcox vorzeichen rangtest ist das p-wert = ", round(wilcox_vorzeichen_rangtest$p.value, 4), "und da p-wert < 0.05 False ist kann die Nullhypotheses nicht abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen keinen signifikanten Unterschied."))
    }

  } else {
    print(paste("Nach Shapiro-Wilk Test ist das p-wert = ", round(nor_test_paardifferenz$p.value, 4), "und da p-wert < 0.1 False ist kann die Nullhypotheses nicht abgelehnt werden. Das heißt, wir können annehmen dass die daten normalverteilt sind."))

    # Aufgabe6
    ttest_gepaartestichproben <- t.test(monats_summe_arkona$x, monats_summe_norderney$x, alternative = "two.sided", paired = TRUE)
    if (ttest_gepaartestichproben$p.value < 0.05) {
      print(paste("Nach t-Test für gepaarte stichproben ist das p-wert = ", round(ttest_gepaartestichproben$p.value, 4), "und da p-wert < 0.05 True ist kann die Nullhypotheses abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen signifikanten Unterschied."))
    } else {
      print(paste("Nach t-Test für gepaarte stichproben ist das p-wert = ", round(ttest_gepaartestichproben$p.value, 4), "und da p-wert < 0.05 False ist kann die Nullhypotheses nicht abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen keinen signifikanten Unterschied."))
    }
  }

} else {
  print(paste("Nach Rangkorrelationstest nach Kendall ist das p-wert = ", round(cor_test_kendall$p.value, 4), "und da p-wert(2*p) < 0.1 False ist kann die Nullhypotheses nicht abgelehnt werden. Das heißt, die beiden 12-monatigen Datensätze weisen keine signifikante Korrelation auf"))

  # Aufgabe5
  nor_test_arkona <- shapiro.test(monats_summe_arkona$x)
  nor_test_norderney <- shapiro.test(monats_summe_norderney$x)
  if (nor_test_arkona$p.value < 0.1 || nor_test_norderney$p.value < 0.1) {
    print(paste("Nach Shapiro-Wilk Test ist das p-wert für arkone = ", round(nor_test_arkona$p.value, 4), "und das p-wert für norderney =", round(nor_test_norderney$p.value, 4), "kann die Nullhypotheses abgelehnt werden. Das heißt, dass die daten nicht normalverteilt sind."))

    # Aufgabe6
    wilcoxon_rangsummentest <- wilcox.test(monats_summe_arkona$x, monats_summe_norderney$x, paired = FALSE, alternative = "two.sided")
    if (wilcoxon_rangsummentest$p.value < 0.05) {
      print(paste("Nach Wilcox rangsummentest ist das p-wert = ", round(wilcoxon_rangsummentest$p.value, 4), "und da p-wert < 0.05 True ist kann die Nullhypotheses abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen signifikanten Unterschied."))
    } else {
      print(paste("Nach Wilcox rangsummentest ist das p-wert = ", round(wilcoxon_rangsummentest$p.value, 4), "und da p-wert < 0.05 False ist kann die Nullhypotheses nicht abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen keinen signifikanten Unterschied."))
    }

  } else {
    print(paste("Nach Shapiro-Wilk Test ist das p-wert für arkone = ", round(nor_test_arkona$p.value, 4), "und das p-wert für norderney =", round(nor_test_norderney$p.value, 4), "kann die Nullhypotheses nicht abgelehnt werden. Das heißt, wir können annehmen dass die daten normalverteilt sind."))

    # Aufgabe6
    var_test <- var.test(monats_summe_arkona$x, monats_summe_norderney$x, alternative = "two.sided")
    if (var_test$p.value < 0.05) {
      # signifikat unterschied zwischen varianz
      ttest_zwei_stichproben_unterschiedlich_varianz <- t.test(monats_summe_arkona$x, monats_summe_norderney$x, var.equal = FALSE, alternative = "two.sided")
      if (ttest_zwei_stichproben_unterschiedlich_varianz$p.value < 0.05) {
        print(paste("Nach t-Test für zwei stichproben ist das p-wert = ", round(ttest_zwei_stichproben_unterschiedlich_varianz$p.value, 4), "und da p-wert < 0.05 True ist kann die Nullhypotheses abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen signifikanten Unterschied."))
      } else {
        print(paste("Nach t-Test für gepaarte stichproben ist das p-wert = ", round(ttest_zwei_stichproben_unterschiedlich_varianz$p.value, 4), "und da p-wert < 0.05 False ist kann die Nullhypotheses nicht abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen keinen signifikanten Unterschied."))
      }
    } else {
      # kein signifikat unterschied zwischen varianz
      ttest_zwei_stichproben_gleicher_varianz <- t.test(monats_summe_arkona$x, monats_summe_norderney$x, var.equal = TRUE, alternative = "two.sided")
      if (ttest_zwei_stichproben_gleicher_varianz$p.value < 0.05) {
        print(paste("Nach t-Test für zwei stichproben ist das p-wert = ", round(ttest_zwei_stichproben_gleicher_varianz$p.value, 4), "und da p-wert < 0.05 True ist kann die Nullhypotheses abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen signifikanten Unterschied."))
      } else {
        print(paste("Nach t-Test für gepaarte stichproben ist das p-wert = ", round(ttest_zwei_stichproben_gleicher_varianz$p.value, 4), "und da p-wert < 0.05 False ist kann die Nullhypotheses nicht abgelehnt werden. Das heißt, die Monatsniederschläge der beiden Stationen zeigen keinen signifikanten Unterschied."))
      }
    }
  }
}