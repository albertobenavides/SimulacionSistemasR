inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
png("p5f.png") # dibujamos f(x) para ver como es
plot(x,  (2/pi) * (1/(exp(x)+exp(-x))))
lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
graphics.off()
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
muestra <- generador(100) # sacamos una muestra
png("p5m.png") # validamos con un dibujo
hist(muestra, freq=F, breaks=50,
     main="Histograma de g(x) comparado con g(x)",
     xlim=c(inicio, final), ylim=c(0, 0.4))
lines(x, g(x), col="red") # dibujamos g(x) encima del histograma
graphics.off()

desde <- 3
hasta <- 7
cuantos <- 500
parte <- function() {
    valores <- generador(pedazo)
    return(sum(valores >= desde & valores <= hasta))
}

minIntegrales <- data.frame()
iterator = 1

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
for (i in c(10, 100, 1000)) {
  integrales <- numeric()
  times <- numeric()
  for (i in seq(i, i * 10, i)) {
    pedazo <- i
    t <- system.time(
      montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
    )[3]
    stopImplicitCluster()
    integral <- sum(montecarlo) / (cuantos * pedazo) # promedio de todas
    val <- ((pi / 2) * integral)
    dif <- abs(0.048834 - val)

    integrales <- c(integrales, dif)
    times <- c(times, t)
  }
  png(paste(i, "diff.png", sep =""))
  plot(integrales, xlab = "Valores aleatorios", ylab = "Diferencia con Wolfram", xaxt='n')
  axis(1, at=1:10, labels = seq(i, i * 10, i))
  graphics.off()

  png(paste(i, "times.png", sep=""))
  plot(times, xlab = "Valores aleatorios", ylab = "Tiempo", xaxt='n')
  axis(1, at=1:10, labels = seq(i, i * 10, i))
  graphics.off()

  print(min(integrales))
  minIntegrales <- rbind(minIntegrales, c(iterator, i, min(integrales)))
  iterator <- iterator + 1
}

png("minIntegrales.png")
plot(minIntegrales[,1], minIntegrales[,3], xlab = "Valores aleatorios", ylab = "Tiempo", xaxt='n')
axis(1, at=1:3, labels = minIntegrales[, 2], )
graphics.off()
