

#Aufgabe1
dfs <- c(30, 10, 3)
sds <- sqrt(dfs / (dfs-2))
lim <- 6

jpeg("Aufgabe3_Dichtefunktion_Ghazlan_Chaudhary.jpg",width=960, height=960, units = "px", pointsize=14, quality=100, bg="white",res=144)
curve(dt(x,30), from = -lim, to = lim, main = "Verteilungsfunktion für die Freiheitsgraden", xlab = "X_Werte", ylab= "Wahrscheinlichkeitendichte")
curve(dt(x, 10), from = -lim, to = lim, add = TRUE, col = 'red')
curve(dt(x, 3), from = -lim, to = lim, add = TRUE, col = 'blue')

legend('topleft', 
       col = c('black', 'red', 'blue'),
       lty = 1,
       legend = c('Freiheit 30', 'Freiheit 10', 'Freiheit 3')
)
dev.off()

#Aufgabe02
z <- (-6:6)
jpeg("Aufgabe3_Verteilungsfunktionfunktion_Chaudhary_Ghazlan.jpg",width=960, height=960, units = "px", pointsize=14, quality=100, bg="white",res=144)
plot(z, dt(z, 30),  type='l', main = "Verteilungsfunktion für die Freiheitsgraden", xlab = "X_Werte", ylab = "Wahrscheinlichkeitsdichte")
lines(z, dt(z, 10), col = 'red', lty=1)
lines(z, dt(z, 3), col = 'blue', lty=1)
legend('topleft', 
       col = c('black', 'red', 'blue'),
       lty = 1,
       legend = c('Freiheit 30', 'Freiheit 10', 'Freiheit 3')
       )
dev.off()

#Aufgabe3

Q1_30<-qt(c(0.001,0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99,0.999), df=30)
Q2_10<-qt(c(0.001,0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99,0.999), df=10)
Q3_3<-qt(c(0.001,0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99,0.999), df=3)
dat <- c(Q3_3, Q2_10, Q1_30)
rnames <- c("q_0.001", "q_0.01", "q_0.025", "q_0.05", "q_0.1", "q_0.9", "q_0.95", "q_0.975", "q_0.99", "q_0.999")
cnames <- c("Freiheitsgrad 30", "Freiheitsgrad 10", "Freiheitsgrad 3")
mat <- matrix(data = dat, nrow = 10, ncol = 3, dimnames=list(rnames, cnames))
write.table(mat, file="Aufgabe3_Q-Werte_Ghazlan_Chaudhary.txt", row.names=TRUE, col.names=TRUE)



#Aufgabe4
wahrscheinlichKeitGrad30 <- sum(pt(x < 1, 30) - pt(x< -1, 30))
wahrscheinlichKeitGrad10 <- sum(pt(x < 1, 10) - pt(x< -1, 10))
wahrscheinlichKeitGrad3 <- sum(pt(x < 1, 3) - pt(x< -1, 3))

print(paste("Wahrscheinlichkeit fuer die Freiheitsgrad (30) ist ", wahrscheinlichKeitGrad30))
print(paste("Wahrscheinlichkeit fuer die Freiheitsgrad (10) ist ", wahrscheinlichKeitGrad10))
print(paste("Wahrscheinlichkeit fuer die Freiheitsgrad (3) ist ", wahrscheinlichKeitGrad3))



#Aufgabe5
par(mfrow = c(2,2), mar = c(2, 4, 1, 1))
curve(rt(x,3))
curve(dt(x,3), -3, 3)
curve(pt(x,3), -3, 3)
curve(qt(x,3))