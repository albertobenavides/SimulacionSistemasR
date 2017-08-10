# Examina de manera sistemática los efectos de la dimensión en el número de veces que la caminata regresa al origen durante una caminata del movimiento Browniano.
#Verificar que el número de pasos de la caminata o el número de repeticiones del experimento no estén causando un efecto significativo.
#Estudiar de forma sistemática y automatizada el tiempo de ejecución de una caminata en términos del largo de la caminata y la dimensión.
#Realizar una comparación entre una implementación paralela y otra versión que no aproveche paralelismo.

library(parallel)

dimensions <- 8
repetitions <- 1000
steps <- 500

timesReturnedData <- data.frame()
elapsedData <- data.frame()

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

    #if(distanceToOrigin > stepsLeft){
    #  break
    #}
  }
  return(timesReturned)
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "steps")

experimentTime = NULL
maxDimension = NULL
for (dimension in 1:dimensions) {
  clusterExport(cluster, "dimension")
  experimentTime <- c(
    experimentTime,
    system.time(result <- parSapply(cluster, 1:repetitions, experiment))[3]
    )
  timesReturnedData <- rbind(timesReturnedData, result)
  elapsedData <- rbind(elapsedData, experimentTime)
  if(mean(result) < 0.5){
    maxDimension = dimension
    alert <- paste("En ", dimension, "dimensiones, la media (", mean(result), ") es inferior a 0.5, por lo que se descarta el regreso en m\u00E1s dimensiones.")
    print(paste(alert))
    break
  }
}
stopCluster(cluster)

png("TimesReturned_Dimension.png")
boxplot(data.matrix(timesReturnedData), use.cols=FALSE,
xlab="Dimensi\u{F3}n", ylab="Retornos al origen", main="Retornos")
graphics.off()

cluster <- makeCluster(detectCores() - 1)
dimension <- maxDimension
clusterExport(cluster, "dimension")
experimentTime = NULL
totalSteps <- steps
for (steps in seq(1, totalSteps, 10)) {
  clusterExport(cluster, "steps")
  experimentTime <- c(
    experimentTime,
    system.time(result <- parSapply(cluster, 1:repetitions, experiment))[3]
    )
  elapsedData <- rbind(elapsedData, experimentTime)
}
stopCluster(cluster)

png("ElapsedTime_Experiment.png")
plot(data.matrix(elapsedData), xlab="Dimensi\u{F3}n", ylab="Tiempo", main="Tiempo ejecuci\u{F3}")
graphics.off()
