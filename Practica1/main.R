library(parallel)

dimensions <- 8
repetitions <- 1000
steps <- 1000

timesReturnedData <- data.frame()
elapsedParallelData <- data.frame()
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

    if(distanceToOrigin > stepsLeft){
      break
    }
  }
  return(timesReturned)
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "steps")
maxDimension <- NULL
for (dimension in 1:dimensions) {
  clusterExport(cluster, "dimension")
  result <- parSapply(cluster, 1:repetitions, experiment)
  timesReturnedData <- rbind(timesReturnedData, result)

  if(mean(result) < 0.5){
    maxDimension <- dimension
    alert <- paste("INFO: En ", dimension, "dimensiones, la media de regresos al origen (", mean(result), ") es inferior a 0.5, por lo que se descarta el regreso en m\u00E1s dimensiones.")
    print(alert)
    break
  }
}
stopCluster(cluster)

png("TimesReturned_Dimension.png")
boxplot(data.matrix(timesReturnedData), use.cols=FALSE,
xlab="Dimensi\u{F3}n", ylab="Retornos al origen", main="Retornos")
graphics.off()
print("INFO: Imagen de Retornos al origen generada.")

cluster <- makeCluster(detectCores() - 1)
dimension <- maxDimension
clusterExport(cluster, "dimension")
experimentTime <- NULL
totalSteps <- steps
for (steps in seq(100, totalSteps, 100)) {
  clusterExport(cluster, "steps")
  experimentTime <- system.time(parSapply(cluster, 1:repetitions, experiment))[3]
  elapsedParallelData <- rbind(elapsedParallelData, experimentTime)
}
stopCluster(cluster)

png("ParallelElapsedTime_Experiment.png")
plot(data.matrix(elapsedParallelData), type="l", xlab="Pasos en cientos", ylab="Tiempo", main="Tiempo ejecuci\u{F3}n en paralelo")
graphics.off()
print("INFO: Imagen de Tiempo ejecuci\u{F3}n en paralelo generada.")

experimentTime <- NULL
for (steps in seq(100, totalSteps, 100)) {
  experimentTime <- system.time(sapply(1:repetitions, experiment))[3]
  elapsedData <- rbind(elapsedData, experimentTime)
}

png("ElapsedTime_Experiment.png")
plot(data.matrix(elapsedData), type="l", xlab="Pasos en cientos", ylab="Tiempo", main="Tiempo ejecuci\u{F3}n no paralelo")
graphics.off()
print("INFO: Imagen de Tiempo ejecuci\u{F3}n no paralelo generada")
