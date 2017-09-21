
library(parallel)
library(lattice)

g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

experiment <- function(t) {
  curr <- c(
    runif(1, low, high), runif(1, low, high)
  )
  for (i in 1:tmax) {
    delta <- runif(1, 0, step)
    left <- curr[1] - delta
    right <- curr[1] + delta
    bottom <- curr[2] - delta
    top <- curr[2] + delta
    if(left < low | right > high | bottom < low | top > high){
      break
    }
    if (g(left, bottom) > g(curr[1], curr[2])) {
      curr <- c(left, bottom)
    }
    if (g(left, top) > g(curr[1], curr[2])) {
      curr <- c(left, top)
    }
    if (g(right, top) > g(curr[1], curr[2])) {
      curr <- c(right, top)
    }
    if (g(right, bottom) > g(curr[1], curr[2])) {
      curr <- c(right, bottom)
    }
  }
  return(curr)
}

low <- -5
high <- 5
step <- 0.25
repetitions <- 100
tmax <- 10000

x <- seq(low, high, step)
y <- x
z <- outer(x, y, "g")
dimnames(z) <- list(x, y)
d = expand.grid(list(x = x, y = y))

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "low")
clusterExport(cluster, "high")
clusterExport(cluster, "step")
clusterExport(cluster, "tmax")
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
