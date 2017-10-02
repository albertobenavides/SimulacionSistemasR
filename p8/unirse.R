unirse <- function(r) {
  urna <- freq[r,]
  union <- exp(-urna$tam / c)
  unir <- round(union * urna$num)
  if (unir > 0) {
    division <- c(rep(-urna$tam, unir), rep(urna$tam, urna$num - unir))
    assert(sum(abs(division)) == urna$tam * urna$num)
  } else {
    division <- rep(urna$tam, urna$num)
  }

  return(division)
}
