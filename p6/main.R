suppressMessages(library(doParallel))
space <- 1
totalAgents <- 50
maxVelocity <- 1 / 20
infectionProbability = 0.05
recuperationProbability = 0.02
infectionRadius <- 0.1
maxTime <- 100

agents <- data.frame(
  x = runif(totalAgents),
  y = runif(totalAgents),
  dx = runif(totalAgents, -maxVelocity, maxVelocity),
  dy = runif(totalAgents, -maxVelocity, maxVelocity),
  state = sample(
    c("S", "I"),
    totalAgents,
    replace = T,
    prob = c(
      1 - infectionProbability,
      infectionProbability
    )
  )
)

levels(agents$state) <- c("I", "S", "R")

update <- function(){
  agent <- agents[i, ]
  if(agent$state == "S"){
    for (j in 1:totalAgents) {
      infectedAgent <- agents[agents$state == "I", ]
      d = sqrt(
        (agent$x - infectedAgent$x) * (agent$x - infectedAgent$x) +
        (agent$y - infectedAgent$y) * (agent$y - infectedAgent$y)
      )
      if(d < infectionRadius){
        p <- (infectionRadius - d) / infectionRadius
        if(runif(1) < p){
          agent$state <- "I"
        }
      }
    }
  } else if(agent$state == "I"){
    if(runif(1) < recuperationProbability){
      agent$state <- "R"
    }
  }
  agent$x <- agent$x + agent$dx
  agent$y <- agent$y + agent$dy
  if(agent$x > space){
    agent$x <- agent$x - space
  } else if(agent$x < 0){
    agent$x <- agent$x + space
  }
  if(agent$y > space){
    agent$y <- agent$y - space
  } else if(agent$y < 0){
    agent$y <- agent$y + space
  }
  return(agent)
}

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
for (generation in 1:maxTime) {
  clusterExport(cluster, "agents")
  nextGeneration <- foreach(i = 1:totalAgents, .combine=rbind) %dopar% update()
  agents <- nextGeneration
  stopImplicitCluster()

  png(paste("img/", sprintf("%03d", generation), ".png", sep=""))
  plot(1, type="n", main = generation, xlim = c(0, space), ylim = c(0, space), xlab = "x", ylab = "y")
  aS <- agents[agents$state == "S", ]
  aI <- agents[agents$state == "I", ]
  aR <- agents[agents$state == "R", ]
  if(nrow(aS) > 0){
    points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
  }
  if(nrow(aI) > 0){
    points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
  }
  if(nrow(aR) > 0){
    points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
  }
  graphics.off()
} # endfor generation

system("magick -delay 20 img/*.png a.gif")
unlink("img/*.png")
