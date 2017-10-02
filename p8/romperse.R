romperse <- function(r) {
  urna <- freq[r,]
  if (urna$tam > 1) {
    rotura <- 1 / (1 + exp((c - urna$tam) / d))
    romper <- round(rotura * urna$num)
    resultado <- rep(urna$tam, urna$num - romper)
    if (romper > 0) {
      for (cumulo in 1:romper) {
        t <- 1
        if (urna$tam > 2) {
          t <- sample(1:(urna$tam-1), 1)
        }
        resultado <- c(resultado, t, urna$tam - t)
      }
    }
    assert(sum(resultado) == urna$tam * urna$num)
  } else {
    resultado <- rep(1, urna$num)
  }
  return(resultado)
}
