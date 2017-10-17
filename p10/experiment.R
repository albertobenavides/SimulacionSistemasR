library(testit)
sink("myfile.txt", append=FALSE, split=FALSE)
knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) # deben ser enteros en este caso
  assert(n == length(valor))
  vt <- sum(valor) # pueden ser lo que sea
  if (pt < cap) { # cabe todo
    return(vt)
  } else {
    filas <- cap + 1 # una para cada posible peso acumulado desde cero hasta cap
    cols <- n + 1 # una para cada objeto y una extra al inicio para no llevar nada
    tabla <- matrix(rep(-Inf, filas * cols),
    nrow = filas, ncol = cols) # al inicio todo vale negativo infinito
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 # todas las filas tienen un cero al inicio (no llevar nada da cero valor)
    }
    rownames(tabla) <- 0:cap # filas corresponden a pesos acumulados posibles
    colnames(tabla) <- c(0, valor) # columnas corresponden a objetos considerados
    for (objeto in 1:n) {
      p <- peso[objeto]
      v <- valor[objeto]
      for (acum in 1:(cap+1)) {
        if (p <= acum) {
          tabla[acum, objeto + 1] <- max(valor[objeto] + tabla[acum - p, objeto], tabla[acum, objeto])
        } else {
          tabla[acum, objeto + 1] <- tabla[acum, objeto]
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

al.azar <- function(cuantos) {
  return(round(runif(cuantos)))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  if(parallel){
    a <- parSapply(cluster, 1:tam, function(i){
      round(runif(n))
    })
    pobl <- t(a)
  } else{
    for (i in 1:tam) {
      pobl[i,] <- round(runif(n))
    }
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

n <- 50
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
init <- 200

library(parallel)
parallel = T
cluster <- makeCluster(detectCores(logical=FALSE))
times <- data.frame()

for(r in 1:20){
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  pm <- 0.05
  rep <- 50
  tmax <- 50
  mejores <- double()
  t <- system.time(
    for (iter in 1:tmax) {
      p$obj <- NULL
      p$fact <- NULL
      if(parallel){
        clusterExport(cluster, "pm")
        clusterExport(cluster, "mutacion")
        clusterExport(cluster, "n")
        clusterExport(cluster, "p")
        a <- parSapply(cluster, 1:tam, function(i){
          if (runif(1) < pm) {
            mutacion(p[i,], n)
          }
        })
        a <- Filter(Negate(is.null), a)
        a <- data.frame(matrix(unlist(a), nrow=50, byrow=T))
        a <- t(a)
        p <- rbind(p, a)
      }else{
        for (i in 1:tam) {
          if (runif(1) < pm) {
            p <- rbind(p, mutacion(p[i,], n))
          }
        }
      }

      if(parallel){
        clusterExport(cluster, "p")
        clusterExport(cluster, "tam")
        clusterExport(cluster, "reproduccion")
        a <- parSapply(cluster, 1:rep, function(i){
          padres <- sample(1:tam, 2, replace=FALSE)
          hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
          hijo1 <- hijos[1:n] # primer hijo
          hijo2 <- hijos[(n+1):(2*n)] # segundo hijo
          return(rbind(hijo1, hijo2))
        })
        a <- data.frame(matrix(unlist(a), nrow=50, byrow=T))
        a <- t(a)
        p <- rbind(p, a)
        rownames(p) <- 1:nrow(p)
      } else{
        for (i in 1:rep) { # una cantidad fija de reproducciones
          padres <- sample(1:tam, 2, replace=FALSE)
          hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
          p <- rbind(p, hijos[1:n]) # primer hijo
          p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
        }
      }
      tam <- nrow(p)
      obj <- double()
      fact <- integer()
      if(parallel){
        clusterExport(cluster, "p")
        clusterExport(cluster, "obj")
        clusterExport(cluster, "fact")
        clusterExport(cluster, "objetivo")
        clusterExport(cluster, "valores")
        clusterExport(cluster, "factible")
        clusterExport(cluster, "pesos")
        clusterExport(cluster, "capacidad")
        a <- parSapply(cluster, 1:tam, function(i){
          obj <- c(obj, objetivo(unlist(p[i,]), valores))
          fact <- c(fact, factible(unlist(p[i,]), pesos, capacidad))
          return(cbind(fact, obj))
        })
        a <- t(a)
        colnames(a) <- c("fact", "obj")
        p <- cbind(p, a)
      }else{
        for (i in 1:tam) {
          obj <- c(obj, objetivo(unlist(p[i,]), valores))
          fact <- c(fact, factible(unlist(p[i,]), pesos, capacidad))
        }
        p <- cbind(p, obj)
        p <- cbind(p, fact)
      }
      p <- transform(
        p, fact = as.integer(fact),
        obj = as.double(obj)
      )
      mantener <- order(-p$fact, -p$obj)[1:init]
      p <- p[mantener,]
      rownames(p) <- 1: nrow(p)
      tam <- nrow(p)
      assert(tam == init)
      factibles <- p[p$fact == TRUE,]
      mejor <- max(factibles$obj)
      mejores <- c(mejores, mejor)
    }
  )[3]
  times <- rbind(times, c(r%%2, t))
  parallel <- !parallel
}
colnames(times) <- c("tipo", "tiempo")
print(times)

png("Times.png", width = 300, height = 600)
boxplot(times$tiempo~times$tipo, xlab = "Tipo de corrida", ylab = "Tiempo (s)", xaxt = "n")
axis(1, lab = c("No paralela", "Paralela"))
graphics.off()

stopCluster(cluster)
png("p10.png", width=600, height=300)
plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
points(1:tmax, mejores, pch=15)
abline(h=optimo, col="green", lwd=3)
graphics.off()
print(t)
