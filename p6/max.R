suppressMessages(library(doParallel))
space <- 1
totalAgents <- 50
maxVelocity <- 1 / 20
recuperationProbability = 0.02
vaccineProbability = 0.03
infectionRadius <- 0.1
maxTime <- 100

maxPercentageInfections <- data.frame()
experiment <- function(){
  totalInfections <- numeric()
  for (iP in seq(0.05, 0.5, 0.05)) {
    infectionProbability = iP
    print(iP)
    agents <- data.frame(
      x = runif(totalAgents),
      y = runif(totalAgents),
      dx = runif(totalAgents, -maxVelocity, maxVelocity),
      dy = runif(totalAgents, -maxVelocity, maxVelocity),
      state = sample(
        c("S", "I", "R"),
        totalAgents,
        replace = T,
        prob = c(
          1 - infectionProbability - vaccineProbability,
          infectionProbability,
          vaccineProbability
        )
      )
    )

    aI <- agents[agents$state == "I", ]
    aR <- agents[agents$state == "R", ]

    if(nrow(aR) > 0 & nrow(aI) > 0){
      levels(agents$state) <- c("I", "R", "S")
    } else if(nrow(aR) == 0 & nrow(aI) == 0){
      levels(agents$state) <- c("S", "I", "R")
    } else if(nrow(aR) > 0){
      levels(agents$state) <- c("R", "S", "I")
    } else if(nrow(aI) > 0){
      levels(agents$state) <- c("I", "S", "R")
    }

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
    } # endFunction Update

    infected <- numeric()
    for (generation in 1:maxTime) {
      aI <- agents[agents$state == "I", ]
      infected <- c(infected, nrow(aI))
      if(nrow(aI) == 0){
        print(paste("No hay infectados en", generation))
        break
      }
      nextGeneration <- foreach(i = 1:totalAgents, .combine=rbind) %do% update()
      agents <- nextGeneration
    } # endfor generation
    infectedPercentage <- infected / totalAgents * 100
    totalInfections <- c(
      totalInfections,
      max(infectedPercentage)
    )
  } # endfor initialProbability
  return(totalInfections)
} # endFunction Experiment

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
clusterEvalQ(cluster, library(foreach))
maxPercentageInfections <- foreach(i = 1:20, .combine=rbind) %dopar% experiment()
stopImplicitCluster()

png("test.png")
boxplot(maxPercentageInfections)
graphics.off()
