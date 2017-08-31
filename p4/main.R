library(parallel)
suppressMessages(library("sna"))
source("voronoi.R")

dimension <- 10
size <- dimension * dimension
voronoiMaterial <- matrix(rep(0, size), ncol = dimension, nrow = dimension)
seeds <- 12
seedPositions <- sample(1 : size, seeds)

for (i in 1:seeds) {
  voronoiMaterial[seedPositions[i]] = i;
}

cores <- detectCores()
cluster <- makeCluster(cores)
clusterExport(cluster, "dimension")
clusterExport(cluster, "size")
clusterExport(cluster, "voronoiMaterial")
clusterExport(cluster, "seeds")
clusterExport(cluster, "seedPositions")

nextMatrix <- parSapply(cluster, 1:size, voronoi)
voronoiMaterial <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)

stopCluster(cluster)

print(voronoiMaterial)
