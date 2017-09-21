experiment <- function(t) {
  curr <- c(
    runif(1, low, high), runif(1, low, high)
  )
  for (i in 1:tmax) {
    delta <- runif(1, 0, step)
    left <- curr[1] - delta
    right <- curr[1] + delta
    bottom <- curr[2] - delta
    top <- curr[2] - delta
    if (outer(left, bottom, g) > outer(curr[1], curr[2], g)) {
      curr <- c(left, bottom)
    }
    if (outer(left, top, g) > outer(curr[1], curr[2], g)) {
      curr <- c(left, top)
    }
    if (outer(right, top, g) > outer(curr[1], curr[2], g)) {
      curr <- c(right, top)
    }
    if (outer(right, bottom, g) > outer(curr[1], curr[2], g)) {
      curr <- c(right, bottom)
    }
  }
  return(curr)
}
