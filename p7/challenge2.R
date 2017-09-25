challenge2 <- function(r) {
  originalT <- t
  curr <- c(
    runif(1, low, high), runif(1, low, high)
  )
  for (i in 1:maxTime) {
    other <- c(
      curr[1] + runif(1, -step, step),
      curr[2] + runif(1, -step, step)
    )
    if (other[1] < low){
      other[1] <- other[1] + high
    }
    if (other[1] > high){
      other[1] <- other[1] + low
    }
    if (other[2] < low){
      other[2] <- other[2] + high
    }
    if (other[2] > high){
      other[2] <- other[2] + low
    }
    delta <- g(other[1], other[2]) - g(curr[1], curr[2])
    if (delta > 0){
      curr <- other
    } else if(delta < 0){
      if(runif(1) < exp(delta / t)){
        curr <- other
        t <- t * t
      }
    }
  }
  return(c(curr, g(curr[1], curr[2]), originalT))
}
