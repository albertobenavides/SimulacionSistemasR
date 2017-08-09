# Examina de manera sistemática los efectos de la dimensión en el número de veces que la caminata regresa al origen durante una caminata del movimiento Browniano.
#Verificar que el número de pasos de la caminata o el número de repeticiones del experimento no estén causando un efecto significativo.
#Estudiar de forma sistemática y automatizada el tiempo de ejecución de una caminata en términos del largo de la caminata y la dimensión.
#Realizar una comparación entre una implementación paralela y otra versión que no aproveche paralelismo.

dimensions <- 1 # cantidad de dimensiones
steps <- 10000 # cantidad de pasos
position <- rep(0, dimensions)
origin <- rep(0, dimensions)
timesReturned <- 0
for(step in 1:steps){
  d <- sample(1:dimensions, 1)
  if(runif(1) > 0.5){
    position[d] <- position[d] + 1
  } else{
    position[d] <- position[d] - 1
  }

  if(all(position == origin)){
    timesReturned <- timesReturned + 1
  }

  distanceToOrigin <- sum(abs(point - origin))
  stepsLeft <- steps - step

  if(distanceToOrigin > stepsLeft){
    print("No puede regresar con los pasos restantes")
    break
  }
}
print("Regresos")
print(timesReturned)
