suppressMessages(library(doParallel))

registerDoParallel(makeCluster(detectCores() - 1))

points <- 100000
totalPiValues <- numeric()
totalTimes <- numeric()

circle <- function(){
  xs <- runif(points,min=-0.5,max=0.5)
  ys <- runif(points,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/points)*4
  return (mc.pi)
}

rep <- 10000

for (r in seq(rep, rep * 10, rep)) {
  times <- system.time(
   piValues <- foreach(i = 1:r, .combine=c) %dopar% circle()
  )[3]
  stopImplicitCluster()
  dif <- pi - median(piValues)
  totalPiValues <- c(totalPiValues, abs(dif))
  totalTimes <- c(totalTimes, times)
  print(r)
}

png("PiValues.png")
plot(totalPiValues, xlab = "Repeticiones", ylab = "Aproximaciones a PI", xaxt='n')
axis(1, at=1:10, labels = seq(rep, rep * 10, rep) )
graphics.off()

png("times.png")
plot(totalTimes, xlab = "Repeticiones", ylab = "Tiempo", xaxt='n')
axis(1, at=1:10, labels = seq(rep, rep * 10, rep))
graphics.off()
