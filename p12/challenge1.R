binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

modelos <- read.csv("numbers.model", sep=" ", header=FALSE, stringsAsFactors=F)
sink("challenge1.txt", append = T)

tope <- 9
digitos <- 0:tope
correct <- data.frame()
for (times in 1:10) {
  temp <- data.frame()
  for(var in seq(0.01, 0.05, 0.01)){
    modelos[modelos=='n'] <- 0.995 - var
    modelos[modelos=='g'] <- 0.92 - var
    modelos[modelos=='b'] <- 0.002 + var
    r <- 5
    c <- 3
    dim <- r * c

    n <- 49
    w <- ceiling(sqrt(n))
    h <- ceiling(n / w)

    tasa <- 0.15
    tranqui <- 0.99

    k <- length(digitos)
    contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
    rownames(contadores) <- 0:tope
    colnames(contadores) <- c(0:tope, NA)

    n <- floor(log(k-1, 2)) + 1
    neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

    for (t in 1:5000) { # entrenamiento
      d <- sample(0:tope, 1)
      pixeles <- runif(dim) < modelos[d + 1,]
      correcto <- binario(d, n)
      for (i in 1:n) {
        w <- neuronas[i,]
        deseada <- correcto[i]
        resultado <- sum(w * pixeles) >= 0
        if (deseada != resultado) {
          ajuste <- tasa * (deseada - resultado)
          tasa <- tranqui * tasa
          neuronas[i,] <- w + ajuste * pixeles
        }
      }
    }

    a <- 0
    for (t in 1:10000) { # prueba
      d <- sample(0:tope, 1)
      pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
      correcto <- binario(d, n)
      salida <- rep(FALSE, n)
      for (i in 1:n) {
        w <- neuronas[i,]
        deseada <- correcto[i]
        resultado <- sum(w * pixeles) >= 0
        salida[i] <- resultado
      }
      r <- min(decimal(salida, n), k) # todos los no-existentes van al final
      contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
      if(r == correcto){
        a <- a + 1
      }
    }
    temp <- rbind(temp, a)
  }
  print(temp)
}
