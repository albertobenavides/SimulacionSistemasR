library(parallel)
suppressMessages(library("sna"))
unlink("img/c*.png")
dimension <- 100
size <- dimension ^ 2
seeds <- 20
seed <- 1
seedPosition <- sample(1:size, 1)
current <- matrix(0, nrow = dimension, ncol = dimension)
current[seedPosition] = 1 - seed / seeds

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
  output = paste("img/c", sprintf("%02d", generation), ".png", sep = "")
  elapsed = paste("Paso", generation)
  png(output)
  plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
  graphics.off()

  nextMatrix <- parSapply(cluster, 1:size, experiment)
  current <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)
  generation <- generation + 1
  if(seed < seeds){
    seed <- seed + 1
    seedPosition <- round(runif(1, min = 1, max = 10000))
    while(current[seedPosition] != 0){
      seedPosition <- round(runif(1, min = 1, max = 10000))
    }
    current[seedPosition] = 1 - seed / seeds
  }
}
output = paste("img/c", sprintf("%02d", generation), ".png", sep = "")
elapsed = paste("Paso", generation)
png(output)
plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
graphics.off()

stopCluster(cluster)
