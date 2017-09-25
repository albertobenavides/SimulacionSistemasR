challenge1 <- function() {
  curr <- c(
    runif(1, low, high), runif(1, low, high)
  )
  best <- curr
  todos <- data.frame()
  for (i in 1:100) {
    delta <- runif(1, 0, step)
    left <- curr[1] - delta
    right <- curr[1] + delta
    bottom <- curr[2] - delta
    top <- curr[2] + delta
    if (left < low){
      left <- left + high * 2
    }
    if (right > high){
      right <- right + low * 2
    }
    if (bottom < low){
      bottom <- bottom + high * 2
    }
    if (top > high){
      top <- top + low * 2
    }
    if (g(left, bottom) > g(curr[1], curr[2])) {
      curr <- c(left, bottom)
    }
    if (g(left, top) > g(curr[1], curr[2])) {
      curr <- c(left, top)
    }
    if (g(right, top) > g(curr[1], curr[2])) {
      curr <- c(right, top)
    }
    if (g(right, bottom) > g(curr[1], curr[2])) {
      curr <- c(right, bottom)
    }
    todos <- rbind(todos, curr)
  }
  return(todos)
}
