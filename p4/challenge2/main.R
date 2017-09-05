suppressMessages(library(doParallel))
dimension <- 64
size <- dimension * dimension
seedArea <- data.frame()
unlink("img/c*.png")

Rotate <- function(x) t(apply(x, 2, rev))

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
      if(runif(1) < 0.5){
        neighborhood <- setdiff(neighborhood, 0)
        ux <- unique(neighborhood)
        val <- ux[which.max(tabulate(match(neighborhood, ux)))]
        return(val)
      } else {
        return(0)
      }
    }
  }
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dimension")
seed <- 1
seedPosition <- sample(1:size, 1)
current <- matrix(0, nrow = dimension, ncol = dimension)
current[seedPosition] = seed
generation <- 0
while (any(current == 0)) {
  clusterExport(cluster, "current")
  output = paste("img/c", sprintf("%03d", generation), ".png", sep = "")
  elapsed = paste("Paso", generation)
  png(output)
  par(mar = c(0,0,0,0))
  image(Rotate(current), col=c(colors()[50], rev(terrain.colors(seed))))
  graphics.off()

  nextMatrix <- parSapply(cluster, 1:size, experiment)
  current <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)
  generation <- generation + 1

  seed <- seed + 1
  if(any(current == 0)){
    seedPosition <- sample(which(current == 0), 1)
    current[seedPosition] = seed
  }
}


current[1] = 0
output = paste("img/c", sprintf("%02d", generation), ".png", sep = "")
elapsed = paste("Paso", generation)
png(output)
par(mar = c(0,0,0,0))
image(Rotate(current), col=c(colors()[50], rev(terrain.colors(seed))))
graphics.off()

stopCluster(cluster)

system("magick -delay 10 img/c*.png p.gif")
