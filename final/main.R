library(parallel)
source("experiment.R")

k <- 1024
n <- 30 * k
debug = T
paralelizar = T

jarTest = read.csv(file = "JarTest.dat", sep = " ", header = FALSE)
jarTest[, seq(3, 15, 2)] <- NULL
colnames(jarTest) <- c("particles", "Cr20rpm", "Cr40rpm", "Fe20rpm", "Fe40rpm", "Ni20rpm", "Ni40rpm", "Zn20rpm", "Zn40rpm")
jarTest[is.na(jarTest)] <- 0

for (i in 2:ncol(jarTest)) {
  png(paste("img/jarTestHist", colnames(jarTest[i]), ".png", sep = ""), width = 640, height = 480)
  barplot(
    log(jarTest[, i]),
    ylim = c(0, max(log(jarTest))),
    names.arg = seq(10, 300, 5)
  )
  graphics.off()
}

if(paralelizar){
  times <- data.frame()

  c <- detectCores(logic = F)
  cluster <- makeCluster(c)
  for (dTemp in seq(0.1, 0.9, 0.1)) {
    unlink("img/p8*.png")
    d <- dTemp
    clusterExport(cluster, "d")
    val <- experiment(100)

    png(paste("plot", d, ".png", sep = ""), width = 800, height = 480)
    plot(val, type = "l")
    graphics.off()
    #alarm()
  }
  stopCluster(cluster)

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
