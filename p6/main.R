suppressMessages(library(doParallel))
suppressMessages(library(vioplot))
space <- 1
totalAgents <- 50
maxVelocity <- 1 / 20
recuperationProbability = 0.02
vaccineProbability = 0.03
infectionRadius <- 0.1
maxTime <- 100

totalInfections <- data.frame()
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
  }

  cluster <- makeCluster(detectCores() - 1)
  registerDoParallel(cluster)

  infected <- numeric()
  for (generation in 1:maxTime) {
    aI <- agents[agents$state == "I", ]
    infected <- c(infected, nrow(aI))
    if(nrow(aI) == 0){
      print(paste("No hay infectados en", generation))
      break
    }
    clusterExport(cluster, "agents")
    nextGeneration <- foreach(i = 1:totalAgents, .combine=rbind) %dopar% update()
    agents <- nextGeneration
    stopImplicitCluster()

    if(iP == 0.05){
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
    }
  } # endfor generation
  stopCluster(cluster)
  infectedPercentage <- infected / totalAgents * 100
  totalInfections <- rbind(totalInfections, infectedPercentage)
}

png("Violin.png", width=600, height=300)
boxplot(t(totalInfections),  xlab = "Probabilidad inicial de infecci\u{F3}n", ylab = "Porcentaje de infecci\u{F3}n", xaxt='n')
axis(1, at=1:10, labels = seq(0.05, 0.5, 0.05))
graphics.off()

system("magick -delay 20 img/*.png a.gif")
unlink("img/*.png")
