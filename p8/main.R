library(testit) # para pruebas, recuerda instalar antes de usar
k <- 10000
n <- 1000000

crearCumulos <- function(){
  cumulos <- rnorm(k)
  cumulos <- cumulos - min(cumulos) + 1
  cumulos <- round(cumulos / sum(cumulos) * n)
  diferencia <- n - sum(cumulos)
  if(diferencia > 0){
    for (i in 1:diferencia) {
      c <- sample(1:k, 1)
      cumulos[c] <- cumulos[c] + 1
    }
  } else if(diferencia < 0){
    for (i in 1:-diferencia) {
      c <- sample(1:k, 1)
      while(cumulos[c] == 1){
        c <- sample(1:k, 1)
      }
      cumulos[c] <- cumulos[c] - 1
    }
  }
  return(cumulos)
}

romperse <- function(tam, cuantos) {
  rotura <- 1 / (1 + exp((c - tam) / d))
  romper <- round(rotura * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}

unirse <- function(tam, cuantos) {
  union <- exp(-tam / c)
  unir <- round(union * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

system.time(cumulos <- crearCumulos())[3]

png("space.png", width = 1024, height = 1024)
symbols(runif(k, -min(cumulos), max(cumulos)), runif(k, -min(cumulos), max(cumulos)), circles = cumulos / 100, inches = FALSE, fg = rainbow(k), bg = rainbow(k), xlim = c(-min(cumulos), max(cumulos)), ylim = c(-min(cumulos), max(cumulos)))
graphics.off()

c <- median(cumulos)
d <- sd(cumulos) / 4

freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 50
digitos <- floor(log(duracion, 10)) + 1
for (paso in 1:duracion) {
  assert(sum(cumulos) == n)
  cumulos <- integer()
  for (i in 1:dim(freq)[1]) { # fase de rotura
    urna <- freq[i,]
    if (urna$tam > 1) { # no tiene caso romper si no se puede
      cumulos <- c(cumulos, romperse(urna$tam, urna$num))
    } else {
      cumulos <- c(cumulos, rep(1, urna$num))
    }
  }
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  freq <- as.data.frame(table(cumulos)) # actualizar urnas
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  cumulos <- integer()
  for (i in 1:dim(freq)[1]) { # fase de union
    urna <- freq[i,]
    cumulos <- c(cumulos, unirse(urna$tam, urna$num))
  }
  assert(sum(abs(cumulos)) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  juntarse <- -cumulos[cumulos < 0]
  cumulos <- cumulos[cumulos > 0]
  assert(sum(cumulos) + sum(juntarse) == n)
  nt <- length(juntarse)
  if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      for (i in 1:floor(nt / 2) ) {
        cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
      }
    }
    if (nt %% 2 == 1) {
      cumulos <- c(cumulos, juntarse[nt])
    }
  }
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  png(
    paste(
      "img/p8",
      sprintf("%03d", paso),
      ".png",
      sep=""
    ), width=300, height=300)
  tope <- 50 * ceiling(max(cumulos) / 50)
  hist(cumulos, breaks=seq(0, tope, 50),
  main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=FALSE,
  ylim=c(0, 0.05), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa")
  graphics.off()
}
system("magick -delay 20 img/p8*.png a.gif")
