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

x <- seq(low, high, step)
y <- x
z <- outer(x, y, "g")
d = expand.grid(list(x = x, y = y))

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "low")
clusterExport(cluster, "high")
clusterExport(cluster, "step")
clusterExport(cluster, "maxTime")
clusterExport(cluster, "g")
system.time(
  results <- parSapply(cluster, 1:repetitions, experiment)
)[3]
system.time(
  results2 <- parSapply(cluster, seq(0.999, 0.9, -0.001), challenge2)
)[3]
stopCluster(cluster)

#WolframAlpha
maxZ = 0.0666822
maxZ.at <- c(-0.333023, -0.333023)

values <- g(results[1, ], results[2, ])

best <- which.max(values)
print(
  paste(
    "Mejor coordenada: ",
    "(", results[1, best], ", ", results[2, best], ")",
    sep = ""
  )
)
print(
  paste(
    "g(x, y) = ", values[best],
    sep = ""
  )
)
png("BestBoxplot.png")
boxplot(values, xlab = "Experimento", ylab = "g(x, y)", ylim = c(min(values), maxZ))
abline(h = maxZ, col = "red", lwd = 3)
abline(h = values[best], col = "blue", lty = 3, lwd = 2)
graphics.off()

values2 <- g(results2[1, ], results2[2, ])
best2 <- which.max(values2)
print(
  paste(
    "Mejor coordenada Reto 2: ",
    "(", results2[1, best2], ", ", results2[2, best2], ")",
    sep = ""
  )
)
print(
  paste(
    "g(x, y) = ", values2[best2],
    sep = ""
  )
)
print(
  paste(
    "Mejor t = ", seq(0.999, 0.9, -0.001)[best],
    sep = ""
  )
)

colnames(results2) <- seq(0.999, 0.9, -0.001)
rownames(results2) <- c("x", "y")

if (debug){
  unlink("img/*.png")

  png("img/experiment.png")
  levelplot(z ~ x * y, data = d,
    panel = function(...) {
      panel.levelplot(...)
      panel.xyplot(results[1, ], results[2, ], pch = 20, col = "red")
      panel.xyplot(results[1, best], results[2, best], pch = 15, col = "black")
    }
  )
  graphics.off()

  png("img/experiment2.png")
  levelplot(z ~ x * y, data = d,
    panel = function(...) {
      panel.levelplot(...)
      panel.xyplot(results2[1, ], results2[2, ], pch = 20, col = "red")
      panel.xyplot(results2[1, best2], results2[2, best2], pch = 15, col = "black")
    }
  )
  graphics.off()

  challenge1()

  system("magick -delay 20 img/0*.png a.gif")
}
