# Examina de manera sistemática los efectos de la dimensión en el número de veces que la caminata regresa al origen durante una caminata del movimiento Browniano.
#Verificar que el número de pasos de la caminata o el número de repeticiones del experimento no estén causando un efecto significativo.
#Estudiar de forma sistemática y automatizada el tiempo de ejecución de una caminata en términos del largo de la caminata y la dimensión.
#Realizar una comparación entre una implementación paralela y otra versión que no aproveche paralelismo.

library(parallel)

dimensions <- 8
repetitions <- 100
steps <- 1000

output <- data.frame()

experiment <- function(r){
  position <- rep(0, dimension)
  origin <- rep(0, dimension)
  timesReturned <- 0
  for(step in 1:steps){
    d <- sample(1:dimension, 1)
    if(runif(1) > 0.5){
      position[d] <- position[d] + 1
    } else{
      position[d] <- position[d] - 1
    }

    if(all(position == origin)){
      timesReturned <- timesReturned + 1
    }

    distanceToOrigin <- sum(abs(position - origin))
    stepsLeft <- steps - step

    if(distanceToOrigin > stepsLeft){
      break
    }
  }
  return(timesReturned)
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "steps")

for (dimension in 1:dimensions) {
  clusterExport(cluster, "dimension")
  result <- parSapply(cluster, 1:repetitions, experiment)
  output <- rbind(output, result)
}
stopCluster(cluster)

png("p1.png")
boxplot(data.matrix(output), use.cols=FALSE,
xlab="Dimensi\u{F3}n", ylab="Retornos al origen", main="Retornos al origen por dimensi\u{F3}n")
graphics.off()
