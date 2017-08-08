distanceToOrigin <- function(point, method){
  dimensions <- length(point)
  origin <- rep(0, dimensions)
  if(method == "euclidean"){
    return(sqrt(sum((point - origin) ** 2)))
  } else{
    return(sum(abs(point - origin)))
  }
}
