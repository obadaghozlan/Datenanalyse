# Hausaufgabe 1 (benotet)
# 
# Verwenden sie die beiden nachfolgenden Befehle zur Erzeugung eines Datensatzes,
# dessen Werte Temperaturen dastellenn sollen,
# und erweitern dann das Programm entsprechend den nachfolgenden Anweisungen.
 ta<- rep(seq(-5,35,0.1),50)    
 Temp<- sample(ta,100,replace=TRUE)
 
 # Erweitern sie das Programm, indem sie zus�tzlich die folgende Ma�e f�r den
 # Datensatz Temp bestimmen:
 # - Mittelwert
 mean_Temp <- mean(Temp)

 # - Median
 median_Temp <- median(Temp)

 # - kleinster und gr��ter Wert
 min_Temp <- min(Temp)
 max_Temp <- max(Temp)

 # - Spannweite
 spannweite_Temp <- max_Temp - min_Temp

 # - Standardabweichung
 standardabweichung_Temp <- sd(Temp)

 # - Inter Quartile Range (IQR)
 InterQuartileRange_Temp <- IQR(Temp)

 # - Median Absolute Deviation (MAD)
 MedianAbsoluteDeviation_Temp <- mad(Temp)

 # - die p-Quantile f�r p= 0.01,0.1,0.25,0.5,0.75,0.9,0.99
 quantile_Temp <- quantile(Temp, probs=c(0.01,0.1,0.25,0.5,0.75,0.9,0.99))

 # - die "normale" Schiefe und die Quartil-Schiefe entsprechend den Definitionen
 #   aus der Vorlesung (bitte keine zus�tzlichen Libraries verwenden)
 laenge_Temp <- length(Temp)
 normaleSchiefe_Temp <- ((laenge_Temp / ((laenge_Temp-1) * (laenge_Temp-2))) * ((sum((Temp - mean_Temp)^3)) / standardabweichung_Temp^3))
 quartilSchiefe_Temp <- (((quantile_Temp["75%"] - quantile_Temp["50%"]) - (quantile_Temp["50%"] - quantile_Temp["25%"])) / (quantile_Temp["75%"] - quantile_Temp["25%"]))

 # Geben Sie die Ergebnisse ihrer Berechnungen in �bersichtlicher Weise am 
 # Bildschirm (also in der R-Konsole) aus.
 print(paste("Mittelwert =", mean_Temp))
 print(paste("Median=", median_Temp))
 print(paste("kleinster Wert=", min_Temp))
 print(paste("grosster Wert=", max_Temp))
 print(paste("Spannweite=", spannweite_Temp))
 print(paste("Standardabweichung=", standardabweichung_Temp))
 print(paste("Inter Quartile Range (IQR)=", InterQuartileRange_Temp))
 print(paste("Median Absolute Deviation (MAD)=", MedianAbsoluteDeviation_Temp))
 print(paste("die p-Quantile fuer p= 0.01,0.1,0.25,0.5,0.75,0.9,0.99=", quantile_Temp))
 print(paste("normale Schiefe=", normaleSchiefe_Temp))
 print(paste("Quartil Schiefe=", quartilSchiefe_Temp))
 
 # Erzeugen sie aus dem Programm heraus ein Histogramm mit den absoluten 
 # H�ufigkeiten der Werte des Datensatzes Temp mit einer von ihnen vorgegebenen
 # Klasseneinteilung. Achten Sie dabei auf eine ad�quate Zahl von Klassen, 
 # Abbildungs- und Achsenbeschriftungen und ein ansprechendes Layout mit 
 # aufeinander abgestimmten Schriftg��en. Details zum Layout �ber zus�tzliche 
 # Attribute im hist() Befehl liefert Ihnen der help(hist) Befehl
 hist(Temp, main="Haeufigkeitsverteilung", xlab="Temprature", ylab="Haeufigkeit", breaks=8)
 
 # Speichern Sie das fertige R-Programm unter folgendem Dateinamen
 #
 # Aufgabe_1_Name1_Name2.r
 #
 # Ersetzen sie dabei Name1 und Name2 durch die Nachnamen der jeweiligen
 # Gruppenmitgliieder.
 # Laden Sie diese Datei mit dem R_Programm ins Moodle
 #
 # F�hren Sie das Programm 10 x nacheinander aus und tragen Sie die Werte f�r 
 # Mittelwert, Median, Standardabweichung sowie das 1. und 3. Quartil in eine
 # �bersichtliche Tabelle ein. Benutzen Sie dazu ein Textverarbeitungsprogramm.
 # Erkennen Sie die Auswirkungen des statistischen Zufalls?
 # Beschreiben und interpretieren sie kurz und quantitativ ihre Ergebnisse. 
 # Wie stark variieren die einzelnen Ma�zahlen?
 # Laden sie die Tabelle zusammen mir ihrer textlichen Analyse als pdf-Dokument
 # ebenfalls ins Moodle hoch.
 # Verwenden Sie folgenden Dateiname f�r das pdf-Dokument:
 # Aufgabe_1_Name1_Name2_Tabelle.pdf
 #
 # Letzter Abgabetermin:   4.5.2020 24 Uhr        !!!!
 

