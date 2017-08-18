library(parallel)
suppressMessages(library("sna"))
unlink("*.png")
dimension <- 100
size <- dimension ^ 2

elapsedGenerations <- data.frame()

experiment <- function(position){
  row <- floor((position - 1) / dimension) + 1
  col <- ((position - 1) %% dimension) + 1
  neighborhood <- current[
    max(row - 1, 1) : min(row + 1, dimension),
    max(col - 1, 1) : min(col + 1, dimension)
  ]
  return(1 * ((sum(neighborhood) - current[row, col]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dimension")
for (i in seq(0, 1, 0.1)) {
  current <- matrix(runif(size), nrow = dimension, ncol = dimension)
  current <- (current < i) * 1
  generation <- 0

  while(sum(current) > 0){
    output = paste("p", i * 10, "g", sprintf("%02d", generation), ".png", sep = "")
    elapsed = paste("Porcentaje", i ,"Paso", generation)
    if(sum(current) < size){
      png(output)
      plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
      graphics.off()
    }

    generation <- generation + 1
    initial <- sum(current)
    clusterExport(cluster, "current")
    nextMatrix <- parSapply(cluster, 1:size, experiment)
    current <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)

    if(initial == sum(current)){ # evita repeticiones en grupos que intercambian sus posiciones
      break
    }
  }
  elapsedGenerations <- rbind(elapsedGenerations, c(i, generation))
}

png("elapsedGenerations.png")
plot(elapsedGenerations[,1], elapsedGenerations[,2], xlab = "Probabilidad inicial de celda viva", ylab = "Iteraciones")
axis(1, at = elapsedGenerations[,1])
graphics.off()
stopCluster(cluster)
