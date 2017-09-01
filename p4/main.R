suppressMessages(library(doParallel))
unlink("C*.png")

Experiment <- function(d, s) {
  Voronoi <- function(r) {
    if(voronoiMaterial[r] > 0) {
      return(voronoiMaterial[r])
    } else {
      y <- floor((r - 1) / dimension) + 1
      x <- ((r - 1) %% dimension) + 1
      minDistance <- size
      selectedSeed <- NULL
      for (i in 1:seeds) {
        ySeed <- floor((seedPositions[i] - 1) / dimension) + 1
        xSeed <- ((seedPositions[i] - 1) %% dimension) + 1
        distanceToSeed <- sqrt(
          (x - xSeed) * (x - xSeed) + (y - ySeed) * (y - ySeed)
        )
        if(distanceToSeed < minDistance) {
          minDistance <- distanceToSeed
          selectedSeed <- voronoiMaterial[seedPositions[i]]
        }
      }
      return(selectedSeed)
    }
  }

  MakeCrack <- function(r) {
    probability <- 1
    voronoiCracked <- voronoiMaterial
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
            if (voronoiMaterial[yNeighbor, xNeighbor] == voronoiMaterial[yCrack, xCrack]) {
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

    if(crackLength >= 100 & length(f) <= 5 & dimension == 64 & seeds == 48) {
      png(paste("Crack", r, ".png", sep=""))
      par(mar = c(0,0,0,0))
      image(Rotate(voronoiCracked), col=c(colors()[24], terrain.colors(seeds + 1)))
      graphics.off()
    }

    return(crackLength)
  }

  Rotate <- function(x) t(apply(x, 2, rev))

  dimension <- d
  size <- dimension * dimension
  voronoiMaterial <- matrix(rep(0, size), ncol = dimension, nrow = dimension)
  seeds <- s
  seedPositions <- sample(1 : size, seeds)

  for (i in 1:seeds) {
    voronoiMaterial[seedPositions[i]] = i;
  }

  if(dimension == 64 & seeds == 16){
    png("VoronoiInitial.png")
    par(mar = c(0,0,0,0))
    image(Rotate(voronoiMaterial), col=c(colors()[24], terrain.colors(seeds + 1)))
    graphics.off()
  }

  cores <- detectCores()
  cluster <- makeCluster(cores - 1)
  registerDoParallel(cluster)

  nextMatrix <- foreach(r = 1:size, .combine=c) %dopar% Voronoi(r)
  stopImplicitCluster()

  voronoiMaterial <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)

  if(dimension == 64 & seeds == 16){
    voronoiMaterial[1] <- 0
    png("VoronoiFinal.png")
    par(mar = c(0,0,0,0))
    image(Rotate(voronoiMaterial), col=c(colors()[24], terrain.colors(seeds + 1)))
    graphics.off()
  }

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

  lengths <- foreach(r = 1 : repetitions, .combine=c) %dopar% MakeCrack(r)
  stopImplicitCluster()
  stopCluster(cluster)
  summary(lengths)
  return(lengths)
}

repetitions <- 1000

totalLengths <- data.frame()

for (i in c(1, 2, 4, 8, 16)) {
  for (j in 1: 3) {
    print(paste(i, j))
    totalLengths <- rbind(
      totalLengths, Experiment(16 * i, 16 * i * j / 4)
    )
  }
}

png("Lengths.png")
boxplot(data.matrix(totalLengths), xlab = "\u{CD}ndice de experimento", ylab = "Distancia", main = NULL, use.cols=FALSE)
axis(1, at=1:15, labels=1:15)
graphics.off()

den <- density(c(t(totalLengths)))
png("Density.png")
plot(den, xlab = "\u{CD}ndice de experimento", ylab = "Distancia", main = NULL) # plots the results
graphics.off()
