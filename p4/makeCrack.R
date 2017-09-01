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
  if(crackLength >= 50 & length(f) <= 5) {
    png(paste("Crack", r, ".png", sep=""))
    par(mar = c(0,0,0,0))
    image(Rotate(voronoiCracked), col=c(colors()[24], terrain.colors(seeds + 1)))
    graphics.off()
  }

  return(crackLength)
}
