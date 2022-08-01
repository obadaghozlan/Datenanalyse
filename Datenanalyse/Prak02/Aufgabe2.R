
#Aufgabe01
#Datei Lesen
Datei <- "Daten/Hamburg_Gruppe_FÜS3.txt"
data <- read.table(file=Datei,header=TRUE,dec=".",sep="",strip.white=TRUE)

#Aufgabe02
anzRows <- nrow(data)
cat("Ist laenge einglesenen Datensatzes gleich 366? ","\nAntwort ", anzRows==366, "\n")

#Aufgabe03
tmk <- data[,"TMK"]
mittelwert <- mean(tmk)
median <- median(tmk)
stab <- sd(tmk)
quantil_1 <- quantile(tmk, 0.25)
quantil_3 <- quantile(tmk, 0.75)
cat("Mittelwert = ", mittelwert, " +/- ", stab, "\n") 
cat("Median = ", median, " - ", "[", quantil_1, ",", quantil_3, "]", "\n")

#Aufgabe04
jpeg("Aufgabe2_Ghazlan_Chaudhary_Zeitreihe.jpg",width=960, height=960, units = "px", pointsize=14, quality=100, bg="white",res=144)
plot(1:366, tmk, xlab="Tagesnummer", ylab="Tagesmitteltemperaturen(\u00b0c)", main= "Zeitreihe")
dev.off()

#Aufgabe05
hist<-hist(tmk, breaks=10, plot=FALSE)
jpeg("Aufgabe2_Ghazlan_Chaudhary_Haeufigkeiten.jpg",width=960, height=960, units = "px", pointsize=14, quality=100, bg="white",res=144)
barplot(hist$counts, names.arg=hist$mid, xlab="Tagesmitteltemperaturen (\u00b0c)", ylab="Häufigkeiten", main = "Häufigkeitsverteilung")
dev.off()

#Aufgabe6
jpeg("Aufgabe2_Ghazlan_Chaudhary_Boxplot.jpg", width=960, height=960, units = "px", pointsize=14, quality=100, bg="white",res=144)
boxplot(tmk, range=1.5, ylab= "Temperaturen (\u00b0c)" ,main = "Box und Whisker plot fuer Tagesmitteltemperaturen")
dev.off()

#Aufgabe7
jpeg("Aufgabe2_Ghazlan_Chaudhary_QQplot.jpg",width=960, height=960, units = "px", pointsize=14, quality=100, bg="white",res=144)
n <- length(tmk)
y <- rnorm(10000)
tmkSort <- sort(tmk)
p_x <- ppoints(tmkSort)
q_y <- qnorm(p_x)
plot(q_y,tmkSort, xlab = "Werte der Standard-Normalverteilung",ylab = "Temperaturen (\u00b0c)", main = "QQPlot")
qqline(tmk, probs= c(0.25,0.75))
dev.off()



