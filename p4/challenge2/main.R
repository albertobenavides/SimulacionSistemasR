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
  image(Rotate(current), col=c(colors()[24], terrain.colors(seed)))
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
image(Rotate(current), col=c(colors()[24], terrain.colors(seed)))
graphics.off()

stopCluster(cluster)

system("magick -delay 10 img/c*.png p.gif")

MakeCrack <- function(r) {
  probability <- 1
  voronoiCracked <- current
  isCracking <- TRUE
  crackStart <- StartCrack()
  yCrack <- crackStart[1]
  xCrack <- crackStart[2]
  crackLength <- 0

  while(isCracking) {
    crackLength <- crackLength + 1
    interior <- numeric()
    border <- numeric()
    for (n in 1:totalNeighbors) {
      neighbor <- neighborhood[n, ]
      xNeighbor <- xCrack + neighbor$x
      yNeighbor <- yCrack + neighbor$y

      if(xNeighbor > 0 & xNeighbor <= dimension & yNeighbor > 0 & yNeighbor <= dimension & xCrack > 0 & xCrack <= dimension & yCrack > 0 & yCrack <= dimension){
        voronoiCracked[yCrack, xCrack] = 0
        if(voronoiCracked[yNeighbor, xNeighbor] > 0) {
          print(paste(yCrack, xCrack, "-", yNeighbor, xNeighbor))
          if (current[yNeighbor, xNeighbor] == current[yCrack, xCrack]) {
            interior <- c(interior, n)
          } else {
            border <- c(border, n)
          }
        }
      }
    }

    nextCrack <- 0
    if (length(border) > 0) {
      nextCrack <- sample(border, 1)
      probability <- 1
    } else if (length(interior) > 0) {
      if (runif(1) < probability) {
        nextCrack <- sample(interior, 1)
        probability <- probability * 0.99
      }
    }
    if (nextCrack > 0) {
      neighbor <- neighborhood[nextCrack, ]
      xCrack <- xCrack + neighbor$x
      yCrack <- yCrack + neighbor$y
    } else {
      isCracking = FALSE
    }
  }

  f <- list.files(path = ".", pattern="Crack*")
  if(crackLength >= 50 & length(f) <= 5) {
    png(paste("Crack", r, ".png", sep=""))
    par(mar = c(0,0,0,0))
    image(Rotate(voronoiCracked), col=c(colors()[24], terrain.colors(seed)))
    graphics.off()
  }

  return(crackLength)
}

cores <- detectCores()
cluster <- makeCluster(cores - 1)
registerDoParallel(cluster)

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

lengths <- foreach(r = 1 : 500, .combine=c) %dopar% MakeCrack(r)
stopImplicitCluster()
stopCluster(cluster)
summary(lengths)

png("Summary.png")
boxplot(lengths, xlab = "Semillas", ylab = "Distancia", main = NULL, use.cols=FALSE, labels = NULL, xaxt='n')
graphics.off()
