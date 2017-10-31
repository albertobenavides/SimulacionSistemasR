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
modelos[modelos=='n'] <- 0.995
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002

r <- 5
c <- 3
dim <- r * c

n <- 49
w <- ceiling(sqrt(n))
h <- ceiling(n / w)

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

parallel <- T
if (length(commandArgs()) == 1) {
  parallel <- commandArgs()[1]
}
debug <- F
if (length(commandArgs()) == 2) {
  debug <- commandArgs()[2]
}

if(debug){
  png("p12g.png", width=1600, height=2000)
  par(mfrow=c(w, h), mar = c(0,0,7,0))
  suppressMessages(library("sna"))
  for (j in 1:n) {
    d <- sample(0:9, 1)
    pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
    imagen <- matrix(pixeles, nrow=r, ncol=c, byrow=TRUE)
    plot.sociomatrix(
      imagen, drawlab=FALSE, diaglab=FALSE,
      main=paste(d, ""), cex.main=5
    )
  }
  graphics.off()
}

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

if(parallel){
  library(parallel)
  cluster <- makeCluster(3)
  clusterExport(cluster, "tope")
  clusterExport(cluster, "dim")
  clusterExport(cluster, "modelos")
  clusterExport(cluster, "binario")
  clusterExport(cluster, "neuronas")
  clusterExport(cluster, "resultado")
  clusterExport(cluster, "contadores")
  clusterExport(cluster, "n")
  clusterExport(cluster, "decimal")
  clusterExport(cluster, "k")
}
neural <- function(t){
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
  return(r == correcto)
}
if(parallel){
  a <- parSapply(cluster, 1:1000, neural)
  print(sum(a))
} else{
  a <- 0
  for (t in 1:1000) { # prueba
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
  print(a)
  if(debug){
    print(contadores)
  }
}

if(parallel){
  stopCluster(cluster)
}
