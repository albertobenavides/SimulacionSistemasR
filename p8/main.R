library(parallel)
source("experiment.R")

k <- 10000
n <- 1000000
debug = FALSE
paralelizar = FALSE
unlink("img/*.png")

if(paralelizar){
  times <- data.frame()

  for(c in 1:2){
    cluster <- makeCluster(c)
    for (p in 3:6) {
      k <- 10 ^ p
      n = 30 * k
      t <- system.time(experiment(50))[3]
      times <- rbind(times, c(c, t, k))
    }
    stopCluster(cluster)
  }
  write.csv(times, file = "results.csv")
} else{
  times <- data.frame()
  for (c in 1:detectCores()) {
    cluster <- makeCluster(c)
    t <- system.time(experiment(50))[3]
    times <- rbind(times, c(c, t))
    stopCluster(cluster)
  }
  colnames(times) <- c("Procesadores", "Tiempo(s)")
  png("Times_Cores.png")
  plot(times, xaxt="n")
  axis(1, at=times$Procesadores)
  graphics.off()
}
