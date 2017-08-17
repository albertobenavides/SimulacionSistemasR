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
    alert <- paste("INFO: En", dimension,
    "dimensiones, la media de regresos al origen (",
    mean(result),
    ") es inferior a 0.5, por lo que se descarta el regreso",
    "en m\u00E1s dimensiones.")
    print(alert)
    break
  } else{
    alert <- paste("INFO: Media de dimensi\u{F3}n", dimension, ":", mean(result), ".")
    print(alert)
  }
}
stopCluster(cluster)

png("TimesReturned_Dimension.png")
boxplot(data.matrix(timesReturnedData), use.cols=FALSE,
xlab="Dimensi\u{F3}n", ylab="Retornos al origen")
graphics.off()
print("INFO: Imagen de Retornos al origen generada.")

cluster <- makeCluster(detectCores() - 1)
dimension <- maxDimension
clusterExport(cluster, "dimension")
parallelExperimentTime <- NULL
experimentTime <- NULL
totalSteps <- steps
for (steps in seq(100, totalSteps, 100)) {
  experimentTime <- system.time(sapply(1:repetitions, experiment))[3]
  elapsedData <- rbind(elapsedData, experimentTime)

  clusterExport(cluster, "steps")
  parallelExperimentTime <- system.time(
    parSapply(cluster, 1:repetitions, experiment))[3]
  elapsedParallelData <- rbind(elapsedParallelData, parallelExperimentTime)
}
stopCluster(cluster)

png("ElapsedTime_Experiment.png")
plot(data.matrix(elapsedData), xlab="Pasos", ylab="Tiempo")
points(data.matrix(elapsedParallelData), pch=15)
axis(1, at=seq(1, 10, 1), labels=seq(100, totalSteps, 100))
graphics.off()
print("INFO: Imagen de Tiempo ejecuci\u{F3}n generada.")
