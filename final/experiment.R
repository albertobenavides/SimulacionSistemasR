experiment <- function(duracion){

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
    } else {
      resultado <- rep(1, urna$num)
    }
    return(resultado)
  }

  unirse <- function(r) {
    urna <- freq[r,]
    union <- exp(-urna$tam / c)
    unir <- round(union * urna$num)
    if (unir > 0) {
      division <- c(rep(-urna$tam, unir), rep(urna$tam, urna$num - unir))
    } else {
      division <- rep(urna$tam, urna$num)
    }

    return(division)
  }

  cumulos <- crearCumulos()

  c <- median(cumulos)
  val <- numeric()

  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  digitos <- floor(log(duracion, 10)) + 1

  for (paso in 1:duracion) {

    cumulos <- parSapply(cluster, 1:nrow(freq), romperse)
    cumulos <- unlist(cumulos)

    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]

    cumulos <- parSapply(cluster, 1:nrow(freq), unirse)
    cumulos <- unlist(cumulos)

    juntarse <- -cumulos[cumulos < 0]
    cumulos <- cumulos[cumulos > 0]
    nt <- length(juntarse)
    if (nt > 0) {
      if (nt > 1) {
        juntarse <- sample(juntarse)
        cumulosUnidos <- parSapply(cluster, 1:floor(nt / 2), function(i){
          return(juntarse[2*i-1] + juntarse[2*i])
        })
        cumulosUnidos <- unlist(cumulosUnidos)
        cumulos <- c(cumulos, cumulosUnidos)
      }
      if (nt %% 2 == 1) {
        cumulos <- c(cumulos, juntarse[nt])
      }
    }
    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    val <- c(val, sum(freq[freq$tam >= 50,2]))
    if(debug){
      png(
        paste(
          "img/p8",
          sprintf("%03d", paso),
          ".png",
          sep=""
        ), width=300, height=300)
      tope <- 50 * ceiling(max(cumulos) / 50)
      hist(cumulos, breaks=seq(0, tope, 10),
      main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=T,
      ylim=c(0, 450), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa", xlim = c(0, 100))
      abline(v = 50, col=2)
      graphics.off()
    }
  }
  if(debug){
    system(paste("magick -delay 3 img/p8*.png ", d, ".gif", sep = ""))
  }

  return(val)
}
