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

example <- function() {
  curr <- c(
    runif(1, low, high), runif(1, low, high)
  )
  best <- curr
  todos <- data.frame()
  for (i in 1:100) {
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

    png(paste("img/", sprintf("%03d", i), ".png", sep = ""))
    plot.new()
    print(
      levelplot(z ~ x * y, data = d,
        panel = function(...) {
          panel.levelplot(...)
          panel.xyplot(curr[1], curr[2], pch = 15, col = "black")
        }
      )
    )
    graphics.off()

    if(curr[1] == best[1] & curr[2] == best[2]){
      break
    } else {
      best <- curr
    }
  }
}

example()

system("magick -delay 20 img/0*.png a.gif")
