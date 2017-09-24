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
      left <- left + high
    }
    if (right > high){
      right <- right + low
    }
    if (bottom < low){
      bottom <- bottom + high
    }
    if (top > high){
      top <- top + low
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

    png(paste("img/", sprintf("%03d", i), ".png", sep = ""))
    plot.new()
    print(
      levelplot(z ~ x * y, data = d,
        panel = function(...) {
          panel.levelplot(...)
          panel.xyplot(curr[1], curr[2], pch = 15, col = "black")
        }
      )
    )
    title(main = i)
    graphics.off()

    if(curr[1] == best[1] & curr[2] == best[2]){
      break
    } else {
      best <- curr
    }
  }
}
