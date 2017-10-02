library(testit)
source("crearCumulos.R")
source("romperse.R")
source("unirse.R")
k <- 10000
n <- 1000000
debug = FALSE

experiment <- function(){
  cumulos <- crearCumulos()

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

    cumulos <- sapply(1:nrow(freq), romperse)
    cumulos <- unlist(cumulos)

    assert(sum(cumulos) == n)
    assert(length(cumulos[cumulos == 0]) == 0)

    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)

    cumulos <- sapply(1:nrow(freq), unirse)
    cumulos <- unlist(cumulos)

    assert(sum(abs(cumulos)) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    juntarse <- -cumulos[cumulos < 0]
    cumulos <- cumulos[cumulos > 0]
    assert(sum(cumulos) + sum(juntarse) == n)
    nt <- length(juntarse)
    if (nt > 0) {
      if (nt > 1) {
        juntarse <- sample(juntarse)
        cumulosUnidos <- sapply(1:floor(nt / 2), function(i){
          return(juntarse[2*i-1] + juntarse[2*i])
        })
        cumulosUnidos <- unlist(cumulosUnidos)
        cumulos <- c(cumulos, cumulosUnidos)
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
    if(debug){
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
  }
  system("magick -delay 20 img/p8*.png a.gif")
}

system.time(experiment())[3]
