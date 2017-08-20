library(parallel)
suppressMessages(library("sna"))
unlink("img/g*.png")
dimension <- 100
size <- dimension ^ 2
seeds <- 20
noBordersCount <- vector(mode="numeric", length=0)
seedArea <- data.frame()

border <- c(1:dimension,
  (size - dimension + 1):size,
  seq(dimension + 1, size - 2 * dimension + 1, dimension),
  seq(2 * dimension, size - dimension, dimension)
)

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

for (i in 1:10) {
  current <- matrix(0, nrow = dimension, ncol = dimension)
  seedsPosition <- sample(1:size, seeds)
  for (seed in 1:seeds) {
    current[seedsPosition[seed]] = seed / seeds
  }

  generation <- 0

  while (any(current == 0)) {
    clusterExport(cluster, "current")
    if(i == 1){
      output = paste("img/g", sprintf("%02d", generation), ".png", sep = "")
      elapsed = paste("Paso", generation)
      png(output)
      plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
      graphics.off()
    }

    nextMatrix <- parSapply(cluster, 1:size, experiment)
    current <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)
    generation <- generation + 1
  }

  seedArea <- rbind(seedArea, as.vector(table(current)))

  if(i == 1){
    current[1] = 0
    output = paste("img/g", sprintf("%02d", generation), ".png", sep = "")
    elapsed = paste("Paso", generation)
    png(output)
    plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
    graphics.off()
  }

  borderValues <- 0
  for (i in border) {
    borderValues <- c(borderValues, current[i])
  }

  borderValues <- unique(borderValues)

  noBorders <- current[! current %in% borderValues]

  noBordersCount <- c(noBordersCount, (length(noBorders) / size))
}

png("seedArea.png")
boxplot(data.matrix(seedArea), xlab = "Semilla", ylab = "Frecuencia", main = NULL, use.cols=FALSE)
graphics.off()

png("noBorders.png")
boxplot(noBordersCount, main = NULL, xlab = "Semilla", ylab = "Extensi\u{F3}n")
graphics.off()

stopCluster(cluster)
