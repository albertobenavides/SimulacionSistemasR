library(parallel)
suppressMessages(library("sna"))
unlink("img/g*.png")
dimension <- 100
size <- dimension ^ 2
seeds <- 20
seedsPosition <- sample(1:size, seeds)
current <- matrix(0, nrow = dimension, ncol = dimension)

for (seed in 1:seeds) {
  current[seedsPosition[seed]] = seed / seeds
}

experiment <- function(position){
  row <- floor((position - 1) / dimension) + 1
  col <- ((position - 1) %% dimension) + 1
  if(current[row, col] > 0){
    return(current[row, col])
  }
  if (current[row, col] == 0){
    neighborhood <- current[
      max(row - 1, 1) : min(row + 1, dimension),
      max(col - 1, 1) : min(col + 1, dimension)
    ]
    if(sum(neighborhood) == 0){
      return(0)
    } else{
      neighborhood <- setdiff(neighborhood, 0)
      ux <- unique(neighborhood)
      val <- ux[which.max(tabulate(match(neighborhood, ux)))]
      return(val)
    }
  }
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dimension")
generation <- 0
while (any(current == 0)) {
  clusterExport(cluster, "current")
  output = paste("img/g", sprintf("%02d", generation), ".png", sep = "")
  elapsed = paste("Paso", generation)
  png(output)
  plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
  graphics.off()

  nextMatrix <- parSapply(cluster, 1:size, experiment)
  current <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)
  generation <- generation + 1
}
current[1] = 0
output = paste("img/g", sprintf("%02d", generation), ".png", sep = "")
elapsed = paste("Paso", generation)
png(output)
plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
graphics.off()

border <- c(1:dimension,
  (size - dimension + 1):size,
  seq(dimension + 1, size - 2 * dimension + 1, dimension),
  seq(2 * dimension, size - dimension, dimension)
)

borderValues <- 0
for (i in border) {
  borderValues <- c(borderValues, current[i])
}

borderValues <- unique(b)

noBorders <- current[! current %in% borderValues]

png("noBorders.png")
hist(noBorders, main = NULL, xlab = "Semilla", ylab = "Frecuencia")
graphics.off()

stopCluster(cluster)
