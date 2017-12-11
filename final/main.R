library(parallel)
source("experiment.R")

k <- 1024
n <- 30 * k
debug = F
paralelizar = T

if (debug){
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
}

if(paralelizar){


  c <- detectCores(logic = F)
  cluster <- makeCluster(c)
  for (dTemp in seq(0.1, 0.9, 0.1)) {
    for (cTemp in seq(0.1, 0.9, 0.1)) {
      for (k in seq(1000, 10000, 1000)) {
        frecuencia <- data.frame()
        for (repetitions in 1:5) {
          n <- 30 * k
          d <- dTemp

          clusterExport(cluster, "d")
          val <- experiment(10)
          frecuencia <- rbind(frecuencia, val)

          if(debug){
            png(paste("plot", d, ".png", sep = ""), width = 800, height = 480)
            plot(val, type = "l")
            graphics.off()
            #alarm()
          }
        }
        png(paste("img/d", dTemp, "c", cTemp, "k", k, ".png", sep = ""), width = 800, height = 480)
        boxplot(
          frecuencia[, seq(1, 10, 1)],
          main = paste("d = ", dTemp, "; c = ", cTemp, "; k = ", k, sep = ""),
          xlab = "Pasos",
          ylab = "C\u{FA}mulos \u{2265} 50 part\u{ED}culas", xaxt = "n")
          axis(1, at=1:10, labels=seq(1, 10, 1)
        )
        graphics.off()
      }
    }
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
