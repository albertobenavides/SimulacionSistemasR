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
