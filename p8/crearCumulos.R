crearCumulos <- function(){
  cum <- rnorm(k)
  cum <- cum - min(cum) + 1
  cum <- round(cum / sum(cum) * n)
  diferencia <- n - sum(cum)
  if(diferencia > 0){
    for (i in 1:diferencia) {
      c <- sample(1:k, 1)
      cum[c] <- cum[c] + 1
    }
  } else if(diferencia < 0){
    for (i in 1:-diferencia) {
      c <- sample(1:k, 1)
      while(cum[c] == 1){
        c <- sample(1:k, 1)
      }
      cum[c] <- cum[c] - 1
    }
  }
  return(cum)
}
