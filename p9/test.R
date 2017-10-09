n <- 10
p <- data.frame(x = c(0.25, 0.26), y = c(0.5, 0.5), m = 5)
png("p90.png", width = 1024, height = 1024)
symbols(p$x, p$y, circles = p$m /200, inches = FALSE,
  xlim = c(0, 1), ylim = c(0, 1))
graphics.off()

library(parallel)
cluster <- makeCluster(2)
x <- rep(1, 100)
clusterExport(cluster, "x")
x <- parSapply(cluster, 1:100, function(i){
  x[i] <<- x[i] + 1
})

x <- parSapply(cluster, 1:100, function(i){
  x[i] <- x[i] + 1
})

x

stopCluster(cluster)
