Datei <- "Hamburg_Gruppe_FÜS3.txt"
data <- read.table(file=Datei,header=TRUE,dec=".",sep="",strip.white=TRUE)
if(sum(is.na(data))==0){
    print("kein fehlwert gefunden")
}else {
    print("datei enthalt fehlwerte")
}

mittelwert <-vector(mode="numeric",length=12)
mittelwert <- aggregate(x=data$TMK, by=list(monat=data$Monat),FUN=mean)
jpeg("Aufgabe4_Boxplot_Chaudhary_Ghazlan.jpg",width=960, height=960, units = "px", pointsize=14, quality=100, bg="white",res=144)
boxplot(TMK~Monat, data=data, main="Box and Whisker Plots TMK für jeden Monat", ylab="Tagesmittel der Lufttemperatur", xlab="Monat")
stripchart(mittelwert$x~mittelwert$monat, vertical=TRUE, pch=20, add=TRUE, col="red")
dev.off()

alpha = 0.95
anzh_monaten=12
row_names=c("FEB", "MAR", "APR", "MAI", "JUN", "JUL", "AUG", "SEP", "OKT", "NOV", "DEZ")
col_names=c("JAN", "FEB", "MAR", "APR", "MAI", "JUN", "JUL", "AUG", "SEP", "OKT", "NOV")
mat=matrix(NA, nrow = 11, ncol = 11, dimnames= list(row_names, col_names))
for(i in 1:(anzh_monaten-1)){
    x <- data[data[, "Monat"]==i,]
    for(j in (i+1):anzh_monaten){
        y <- data[data[, "Monat"]==j,]
        tmp=wilcox.test(x$TMK, y$TMK, alternative="two.sided", exact=FALSE, paired=FALSE)
        mat[j-1, i]=tmp$p.value
    }
}
for(row in 1:nrow(mat)) {
    for(col in 1:ncol(mat)) {
        if(is.na(mat[row, col])){
            mat[row, col] = "NA"
        }
        else if(mat[row, col] > alpha){
            mat[row, col] = "signifikant voneinander unterschiedlich"
        }
        else if(mat[row, col] < alpha){
            mat[row, col] = "nicht signifikant voneinander unterschiedlich"
        }
    }
}
write.table(mat, file="Aufgabe4_Signifikanz_Chaudhary_Ghazlan.txt", row.names=TRUE, col.names=TRUE)