seq = read.table("sequential.txt")
par = read.table("parallel.txt")

png("Times.png")
boxplot(seq[, 1], par[, 1], xlab = "Tipo de corrida", ylab = "Tiempo (s)", xant = "n")
axis(1, at=c(1, 2), labels = c("Secuencial", "Paralelo"))
graphics.off()

png("Correct.png")
boxplot(seq[, 2] / 10000, par[, 2] / 10000, xlab = "Tipo de corrida", ylab = "Porcentaje de correctos", xant = "n")
axis(1, at=c(1, 2), labels = c("Secuencial", "Paralelo"))
graphics.off()

wilcox.test(seq[,1], par[,1], mu = 3.6, conf.int = TRUE)
