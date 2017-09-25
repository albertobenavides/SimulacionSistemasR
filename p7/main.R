library(parallel)
library(lattice)
source("g.R")
source("experiment.R")
source("challenge1.R")
source("challenge2.R")

low <- -4
high <- 4
step <- 0.2
repetitions <- 100
debug <- FALSE
#WolframAlpha
maxZ = 0.0666822
maxZ.at <- c(-0.333023, -0.333023)

x <- seq(low, high, step)
y <- x
z <- outer(x, y, "g")
d = expand.grid(list(x = x, y = y))

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "low")
clusterExport(cluster, "high")
clusterExport(cluster, "step")
clusterExport(cluster, "g")

experimentElapsed <- numeric()
e1 <- data.frame()
for (p in 3:3) {
  maxTime <- 10 ^ p
  clusterExport(cluster, "maxTime")
  experimentElapsed <- c(
    experimentElapsed, system.time(
      results <- parSapply(cluster, 1:repetitions, experiment)
    )[3]
  )
  results <- rbind(results,
    g(results[1, ], results[2, ]), maxTime
  )
  results <- t(results)
  e1 <- rbind(e1, results)

  if(debug){
    png(paste("Experiment", 10^p, ".png", sep = ""))
    plot.new()
    print(
      levelplot(z ~ x * y, data = d,
        panel = function(...) {
          panel.levelplot(...)
          panel.xyplot(results[, 1], results[, 2], pch = 4, col = "black")
          panel.abline(v = maxZ.at[1], h = maxZ.at[2], col = "red", lty = 3)
        }
      )
    )
    graphics.off()
  }
}
colnames(e1) <- c("x", "y", "z", "maxTime")

options(digits=15)
tapply(e1$z, e1$maxTime, summary)

if(debug){
  png("ExperimentElapsed.png")
  plot(experimentElapsed, ylab = "Tiempo(s)", xlab = "Repeticiones", xaxt = "n")
  axis(1, at = 1:4, labels = c("100", "1000", "10^4", "10^5"))
  lines(experimentElapsed)
  graphics.off()
}

if (debug){
  unlink("img/*.png")

  challenge1()

  system("magick -delay 20 img/0*.png a.gif")
}

e2 <- data.frame()
e3 <- data.frame()
e4 <- data.frame()
e2Times <- numeric()
for(t in seq(0.995, 0, -0.1)){
  maxTime <- 1000
  clusterExport(cluster, "maxTime")
  clusterExport(cluster, "t")
  e2Times <- c(e2Times, system.time(results2 <- parSapply(cluster, 1:repetitions, challenge2))[3])
  results2 <- t(results2)
  e2 <- rbind(e2, results2)

  maxTime <- 10000
  clusterExport(cluster, "maxTime")
  results3 <- parSapply(cluster, 1:repetitions, challenge2)
  results3 <- t(results3)
  e3 <- rbind(e3, results3)

  maxTime <- 100
  clusterExport(cluster, "maxTime")
  results4 <- parSapply(cluster, 1:repetitions, challenge2)
  results4 <- t(results4)
  e4 <- rbind(e4, results4)

  if(debug){
    png(paste("Experiment2", t, ".png", sep = ""))
    plot.new()
    print(
      levelplot(z ~ x * y, data = d,
        panel = function(...) {
          panel.levelplot(...)
          panel.xyplot(results2[, 1], results2[, 2], pch = 4, col = "black")
          panel.abline(v = maxZ.at[1], h = maxZ.at[2], col = "red", lty = 3)
        }
      )
    )
    graphics.off()
  }
}
colnames(e2) <- c("x", "y", "z", "t")
colnames(e3) <- c("x", "y", "z", "t")
colnames(e4) <- c("x", "y", "z", "t")

png("Experiment2Max.png", width = 600, height = 300)
plot(tapply(e2$z, e2$t, max), type = "l", ylab = "g(x, y)", xlab ="T inicial", xaxt = "n", ylim = c(min(tapply(e4$z, e4$t, max)), maxZ))
lines(tapply(e3$z, e3$t, max), col = "blue")
lines(tapply(e4$z, e4$t, max), col = "red")
axis(1, at = 1:10, labels = seq(0.995, 0, -0.1))
abline(h = maxZ, col = "red", lty = 3)
legend("bottom", c("100 pasos", "1000 pasos", "10000 pasos", "max(g(x, y))"), cex=0.8, col=c("red", "black", "blue", "red"), lty= c(1, 1, 1, 3), horiz=TRUE, lwd = 2)
graphics.off()

summary(e2Times)

stopCluster(cluster)
