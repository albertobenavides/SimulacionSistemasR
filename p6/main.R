supressMessages(library(doParallel))
space <- 1
totalAgents <- 50
maxVelocity <- 1 / 20
infectionProbability = 0.05
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
  for (generation in 1:maxTime) {
    for (i in 1:totalAgents) {
      # todo: agregar infección
      agent <- agents[i, ]
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
      agents[i, ] <- agent
    } # endfor totalAgents
    png(paste("img/", sprintf("%03d", generation), ".png", sep=""))
    plot(1, type="n", main = generation, xlim = c(0, space), ylim = c(0, space), xlab = "x", ylab = "y")
    points(agents$x, agents$y, pch=15, col="chartreuse3", bg="chartreuse3")
    graphics.off()
  } # endfor maxTime
}

update()

system("magick -delay 20 img/*.png a.gif")
unlink("img/*.png")
