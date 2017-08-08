source("distance.R")
dimensions <- 8 # cantidad de dimensiones
steps <- 1000 # cantidad de pasos
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
  print(position)

  if(all(position == origin)){
    timesReturned <- timesReturned + 1
  }

  if(distanceToOrigin(position,"") > steps - step){
    print("Ya no se puede regresar")
    break
  }
}
print(timesReturned)
