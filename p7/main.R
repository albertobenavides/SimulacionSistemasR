library(parallel)
library(lattice)
source("g.R")
source("experiment.R")

unlink("img/*.png")

low <- -4
high <- 4
step <- 0.2
repetitions <- 100
maxTime <- 100

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
  resultados <- parSapply(cluster, 1:repetitions, experiment)
)[3]
stopCluster(cluster)

#WolframAlpha
maxZ = 0.0666822
maxZ.at <- c(-0.333023, -0.333023)

valores <- g(resultados[1, ], resultados[2, ])
mejor <- which.max(valores)
print(resultados[, mejor])
print(valores[mejor])

png("img/lattice.png")
levelplot(z ~ x * y, data = d,
  panel = function(...) {
    panel.levelplot(...)
    panel.xyplot(resultados[1, ], resultados[2, ], pch = 20, col = "red")
    panel.xyplot(resultados[1, mejor], resultados[2, mejor], pch = 15, col = "black")
  }
)
graphics.off()
