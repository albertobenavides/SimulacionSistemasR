pick.one <- function(x) { # Elige un valor al azar entre una serie
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) { # Por cada termcount
    var <- pick.one(1:varcount) # toma una variable al azar
    deg <- pick.one(1:maxdeg) # en un grado al azar, hasta cierto grado
    f <-  rbind(f, c(var, runif(1), deg)) # Agrega esa variable con un coeficiente entre 0 y 1 y un grado (exponente)
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) { # Se evalúan los polinomios
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}

parallel = T
debug = F
vc <- 3
md <- 4
tc <- 5
data <- data.frame(pos=integer(), dom=integer())

k <- 2 # cuantas funciones objetivo
for(k in 2:3){
  for(n in c(200)){
    
  }
}
obj <- list()
if (parallel){
  library(parallel)
  cluster <- makeCluster(detectCores(logical=FALSE))
  maxdeg <- md
  varcount <- vc
  termcount <- tc
  clusterExport(cluster, "maxdeg")
  clusterExport(cluster, "varcount")
  clusterExport(cluster, "termcount")
  clusterExport(cluster, "pick.one")
  obj <- parLapply(cluster, 1:k, function(i){
    f <- data.frame(variable=integer(), coef=integer(), degree=integer())
    for (t in 1:termcount) { # Por cada termcount
      var <- pick.one(1:varcount) # toma una variable al azar
      deg <- pick.one(1:maxdeg) # en un grado al azar, hasta cierto grado
      f <-  rbind(f, c(var, runif(1), deg)) # Agrega esa variable con un coeficiente entre 0 y 1 y un grado (exponente)
    }
    names(f) <- c("variable", "coef", "degree")
    return(f)
  })
}else{
  for (i in 1:k) { # Se generan dos funciones objetivo
    obj[[i]] <- poli(md, vc, tc)
  }
}

minim <- (runif(k) > 0.5) # Se elige si se van a minimizar o maximizar
sign <- (1 + -2 * minim) # max = 1; min = -1
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc) # Valores aleatorios para cada variable

if(parallel){
  clusterExport(cluster, "k")
  clusterExport(cluster, "obj")
  clusterExport(cluster, "sol")
  clusterExport(cluster, "tc")
  clusterExport(cluster, "eval")
  temp <- parSapply(cluster, 1:(n*k), function(i){
    row <- floor((i - 1) / k) + 1
    col <- ((i - 1) %% k) + 1
    return(eval(obj[[col]], sol[row,], tc))
  })
  val <- matrix(temp, nrow = n, ncol = k)
}else{
  val <- matrix(rep(NA, k * n), nrow=n, ncol=k) # Valores para las soluciones
  for (i in 1:n) { # evaluamos las soluciones
    for (j in 1:k) { # para los dos objetivos
      val[i, j] <- eval(obj[[j]], sol[i,], tc) # Se guardan los valores aleatorios para cada solución
    }
  }
}

mejor1 <- which.max(sign[1] * val[,1]) #Los óptimos para cada solución
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
if(debug){
  png("p11_init.png")
  plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
  graphics.off()
  png("p11_mejores.png")
  plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
  ylab=paste(yl,"mejor con bolita naranja"),
  main="Ejemplo bidimensional")
  points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
  points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
  graphics.off()
}

no.dom <- logical()
dominadores <- integer()
if(parallel){
  clusterExport(cluster, "n")
  clusterExport(cluster, "domin.by")
  clusterExport(cluster, "val")
  clusterExport(cluster, "sign")
  dominadores <- parSapply(cluster, 1:n, function(i){
    d <- logical()
    for (j in 1:n) {
      d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
    }
    cuantos <- sum(d)
    return(cuantos)
  })
  no.dom <- c(no.dom, dominadores == 0) # nadie le domina
}else{
  for (i in 1:n) {
    d <- logical()
    for (j in 1:n) {
      d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
    }
    cuantos <- sum(d)
    dominadores <- c(dominadores, cuantos)
    no.dom <- c(no.dom, cuantos == 0) # nadie le domina
  }
}
temp <- data.frame(pos=rep(0,n), dom=dominadores)

if(parallel){
  stopCluster(cluster)
}
frente <- subset(val, no.dom) # solamente las no dominadas
png("p11_frente.png")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
ylab=paste(yl,"mejor con bolita naranja"),
main="Ejemplo bidimensional")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)
graphics.off()
library(ggplot2) # recordar instalar si hace falta
data <- rbind(data, temp)
print(data)
png("p11_violin.png")
gr <- ggplot(data, aes(x=pos, y=dom)) + geom_violin(fill="orange", color="red")
gr + geom_boxplot(width=0.2, fill="blue", color="white", lwd=2) +
xlab("") +
ylab("Frecuencia") +
ggtitle("Cantidad de soluciones dominantes")
graphics.off()
