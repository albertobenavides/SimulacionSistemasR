library(parallel)

a <- data.frame(x = 1:10, y = 11:20, z = 21:30)
a

cluster <- makeCluster(2)
clusterExport(cluster, "a")
b <- parSapply(cluster, 1:10, function(i){
  x <- a[i, 1]
  y <- a[i, 2]
  x <- x + 1
  y <- y + 2
  return(c(x, y))
})
a[, 1:2] <- t(b)
a
