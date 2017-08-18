library(parallel)
suppressMessages(library("sna"))
dimension <- 100
size <- dimension ^ 2

experiment <- function(position){
  row <- floor((position - 1) / dimension) + 1
  col <- ((position - 1) %% dimension) + 1
  neighborhood <- current[
    max(row - 1, 1) : min(row + 1, dimension),
    max(col - 1, 1) : min(col + 1, dimension)
  ]
  return(1 * ((sum(neighborhood) - current[row, col]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dimension")
for (i in seq(0, 1, 0.1)) {
  current <- matrix(runif(size), nrow = dimension, ncol = dimension)
  current <- (current < i) * 1
  generation <- 0
  output = paste("p", i * 10, "g", formatC(generation, width = 2, format = "d", flag = "0"), ".png", sep = "")
  elapsed = paste("Porcentaje", i ,"Paso", generation)
  if(sum(current) > 0 && sum(current) < size){
    png(output)
    plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
    graphics.off()
  }
}
stopCluster(cluster)
