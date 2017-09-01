suppressMessages(library(doParallel))
source("voronoi.R")
source("makeCrack.R")
source("rotate.R")
unlink("C*.png")

dimension <- 512
size <- dimension * dimension
voronoiMaterial <- matrix(rep(0, size), ncol = dimension, nrow = dimension)
seeds <- 500
seedPositions <- sample(1 : size, seeds)

for (i in 1:seeds) {
  voronoiMaterial[seedPositions[i]] = i;
}

png("VoronoiInitial.png")
par(mar = c(0,0,0,0))
image(Rotate(voronoiMaterial), col=c(colors()[24], terrain.colors(seeds + 1)))
graphics.off()

cores <- detectCores()
cluster <- makeCluster(cores - 1)
registerDoParallel(cluster)

nextMatrix <- foreach(r = 1:size, .combine=c) %dopar% Voronoi(r)
stopImplicitCluster()

voronoiMaterial <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)

voronoiMaterial[1] <- 0
png("VoronoiFinal.png")
par(mar = c(0,0,0,0))
image(Rotate(voronoiMaterial), col=c(colors()[24], terrain.colors(seeds + 1)))
graphics.off()

StartCrack <- function() {
  crackStart <- sample(1 : 4, 1)
  if(crackStart == 1) {
    crackStart <- c(1, sample(1: dimension, 1))
  } else if (crackStart == 2) {
    crackStart <- c(dimension, sample(1: dimension, 1))
  } else if (crackStart == 3) {
  crackStart <- c(sample(1: dimension, 1), 1)
  } else if (crackStart == 4) {
    crackStart <- c(sample(1: dimension, 1), dimension)
  }
  return(crackStart)
}

neighborhood <- data.frame(numeric(), numeric())
for (x in -1:1) {
  for (y in -1:1) {
    if(x != 0 | y != 0) {
      neighborhood <- rbind(neighborhood, c(x, y))
    }
  }
}

names(neighborhood) <- c("x", "y")

totalNeighbors <- dim(neighborhood)
totalNeighbors <- totalNeighbors[1]

lengths <- foreach(r = 1:1000, .combine=c) %dopar% MakeCrack(r)
stopImplicitCluster()
summary(lengths)
