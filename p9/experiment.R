library(ggtern)
library(parallel)
cluster <- makeCluster(2)

debug <- T
challenge2 <- F
n <- 100
tmax <- 100

p <- data.frame(
  x = rnorm(n), y = rnorm(n),
  c = rnorm(n), m = runif(n, 1, 5),
  vx = 0,
  vy = 0
)
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)

png("p9i.png", width = 1024, height = 1024)
symbols(p$x, p$y, circles = p$m / 200, inches = FALSE,
  fg = colores,
  bg = colores,
  xlim = c(0, 1), ylim = c(0, 1))
graphics.off()

eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  fx <- 0
  fy <- 0
  for (j in i:n) {
    if(j == i){
      return
    }
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy))
}

if(debug){
  unlink("img/p9_t*.png")
  digitos <- floor(log(tmax, 10)) + 1
  tl <- "0"
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png", sep=""))
  plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
  main="Estado inicial", xlab="X", ylab="Y")
  graphics.off()
}

clusterExport(cluster, "n")
clusterExport(cluster, "eps")

for (iter in 1:tmax) {
  clusterExport(cluster, "p")
  f <- parSapply(cluster, 1:n, fuerza)
  clusterExport(cluster, "f")
  delta <- 0.02 / max(abs(f))
  clusterExport(cluster, "delta")
  vx <- delta * f[c(TRUE, FALSE)] / p$m
  vy <- delta * f[c(FALSE, TRUE)] / p$m
  p$vx <- (p$vx + abs(vx))
  p$vy <- (p$vy + abs(vy))
  clusterExport(cluster, "p")
  clusterExport(cluster, "vx")
  clusterExport(cluster, "vy")
  pxy <- parSapply(cluster, 1:n, function(i){
    x <- max(
      min(
        p[i,]$x + vx[i], 1
      ), 0
    )
    y <- max(
      min(
        p[i,]$y + vy[i], 1
      ), 0
    )
    return(c(x, y))
  })
  p[, 1:2] <- t(pxy)

  clusterExport(cluster, "p")
  pxy <- parSapply(cluster, 1:n, function(i) {
    xi <- p[i,]$x
    yi <- p[i,]$y
    mi <- p[i,]$m
    for (j in i:n) {
      if(j == i){
        return
      }
      mj <- p[j,]$m
      r <- (mi + mj) / 200
      dx <- xi - p[j,]$x
      dy <- yi - p[j,]$y
      d <- sqrt(dx^2 + dy^2)
      if(d < r){
        xi <- xi + dx * runif(1, mj / 2, mj) / mi
        xi <- max(min(xi, 1), 0)
        yi <- yi + dy * runif(1, mj / 2, mj) / mi
        yi <- max(min(yi, 1), 0)
      }
    }
    return(c(xi, yi))
  })
  p[, 1:2] <- t(pxy)

  if(debug){
    tl <- paste(iter, "", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    png(paste("img/p9_t", tl, ".png", sep=""))
    if(challenge2){
      print(qplot(p$x, p$y, color = colores[p$g + 6], size = p$m/200, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1), main=paste("Paso", iter), xlab="X", ylab="Y"))
    }
    symbols(p$x, p$y, circles = p$m / 200, inches = FALSE,
      fg = colores[p$g+6],
      bg = colores[p$g+6],
      xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
      main=paste("Paso", iter), xlab="X", ylab="Y"
    )
    legend("topright", c("-1", "-0.8", "-0.6", "-0.4", "-0.2", "0", "0.2", "0.4", "0.6", "0.8", "1"), cex=0.8, col=colores, pch = 19, title = "Carga", horiz = F)
    graphics.off()
  }
}

p$vx <- p$vx / tmax
p$vy <- p$vy / tmax
clusterExport(cluster, "p")
p$v <- parSapply(cluster, 1:n, function(i){
  sqrt(p[i,]$vx^2 + p[i,]$vy^2)
})

a <- seq(0, max(p$v), max(p$v)/5)
a <- sprintf("%.4f",a)
png("Velocity.png")
print(
  ggtern(data=p,aes(m / 5, v / max(p$v), abs(c))) +
  geom_mask() +
  geom_point(aes(colour=m)) +
  scale_color_gradient(low='green',high='red') +
  labs(x="Masa",y="Velocidad promedio",z="abs(Carga)", color = "Masa") +
  scale_T_continuous(labels = a) +
  scale_L_continuous(labels = seq(0, 5, 1)) +
  scale_R_continuous(breaks = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
)
graphics.off()

png("Boxplot.png")
boxplot(p$v~round(p$m), xlab = "Masa", ylab = "Velocidad promedio")
graphics.off()

stopCluster(cluster)
if(debug){
  system("magick -delay 20 img/p9_t*.png p9.gif")
}

alarm()
