times <- data.frame()

parallel <- F
debug <- F
commandArgs <- function() c(parallel, debug)

if(parallel){
  sink("parallel.txt", append = T)
}else{
  sink("sequential.txt", append = T)
}

for(i in 1:20){
  t <- system.time(
    source("experiment.R")
  )[3]
  times <- rbind(times, t)
}
print(times)
