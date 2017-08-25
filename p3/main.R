suppressMessages(library(doParallel))
source("prime.R")

from <- 1000
to <-  3000
original <- from:to
reversed <- to:from
samples <- 10
times <- data.frame()
cores <- detectCores()

for (core in 1:cores) {
  cluster <- makeCluster(core)
  registerDoParallel(cluster)
  originalTime <-  numeric()
  reversedTime <-  numeric()
  randomTime <-  numeric()
  for (r in 1:samples) {
    originalTime <- c(
      originalTime,
      system.time(foreach(n = original, .combine=c) %dopar% prime(n))[3]
      )
    reversedTime <- c(
      reversedTime,
      system.time(foreach(n = reversed, .combine=c) %dopar% prime(n))[3]
      )
    randomTime <- c(
      randomTime,
      system.time(foreach(n = sample(original), .combine=c) %dopar% prime(n))[3]
      )
  }
  stopImplicitCluster()
  stopCluster(cluster)

  times <- rbind(times, c(originalTime, core))
  times <- rbind(times, c(reversedTime, core))
  times <- rbind(times, c(randomTime, core))
}

times[, "mean"] <- apply(times[, 1:10], 1, mean)
png("MeanTime_Cluster.png")

plot(
  times[, 12],
  xlab = "ClusterArreglo",
  ylab = "Tiempo (s)",
  main = NULL,
  ylim = c(min(times[, 12]), max(times[, 12])),
  xaxt='n'
  )
  axis(1, at=1:12, labels=c(
    "1o", "1r", "1a",
    "2o", "2r", "2a",
    "3o", "3r", "3a",
    "4o", "4r", "4a"
    ))
graphics.off()

d <- density(c(t(times[, 1:10]))) # returns the density data
png("Density.png")
plot(d) # plots the results
graphics.off()

a <- c(t(times[1, 1:10]), t(times[2, 1:10]), t(times[3, 1:10]))
b <- c(t(times[4, 1:10]), t(times[5, 1:10]), t(times[6, 1:10]))
c <- c(t(times[7, 1:10]), t(times[8, 1:10]), t(times[9, 1:10]))
d <- c(t(times[10, 1:10]), t(times[11, 1:10]), t(times[12, 1:10]))

dati = list(n1=a, n2=b, n3=c, n4=d)
kruskal.test(dati)
