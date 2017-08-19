% Extinción y reproducción de autómatas celulares en R
% José Alberto Benavides Vázquez
% 17 de agosto de 2017

# Introducción

Los autómatas celulares son modelos sistemas informáticos de sistemas celulares que se simulan a través de un vector en el que sus elementos representan posiciones que pueden ocupar las diferentes células del sistema. En esencia, estos modelos se componen de 3 elementos principales[^1]:

* Una **rejilla**, generalmente bidimensional, que contiene la células.
* Un **estado** para cada célula, siendo los más usados son *viva* y *muerta*, `1` y `0` en binario.
* Un **vecindario**, definido a partir de las células adyacentes cada célula.

A partir de estos elementos, se definen **reglas** que controlan el cambio de estado entre las células a partir de los estados de su vecindario, dicho cambio que se refleja en la siguiente generación de células. Este tipo de simulaciones se pueden utilizar para estudiar turbulencias médicas, efectos de estímulos en un medio, termodinámicas y comprensión de patrones, entre otros[^2].

# Condiciones iniciales

En este reporte se parte de una **rejilla** de $100 \times 100$, con dos posibles **estados** de células: *viva* (`1`, representada por una celda negra en la gráfica) o *muerta* (`0`, representada con una celda en blanco), un **vecindario** compuesto por las $8$ células que inmediatamente rodean a cada célula[^3] y se tiene por única **regla** la siguiente: una célula estará viva en la siguiente generación sólo cuando 3 de sus vecinos lo estén en la generación actual.

Este experimento se realizará en lenguaje R con el uso de las librerías `parallel` y `sna`. Adicionalmente se usó [ImageMagick®][^4] para manipular los gráficos generados. Se llevará a cabo bajo el sistema operativo Windows 10 Home Single Language en una computadora con el siguiente procesador:	Intel(R) Core(TM) i7-7500U CPU @ 2.70GHz, 2904 Mhz de 2 procesadores principales y 4 procesadores lógicos.

# Objetivo

1. Determinar el número de iteraciones que dura la simulación sin que se mueran todas las celdas en función de la probabilidad inicial de celda viva.
2. Modificar la simulación para que modele algún tipo de crecimiento (o cristalización) en la microestructura de un material.
3. Modificar lo anterior a que nuevos núcleos puedan aparecer en momentos distintos, no únicamente al inicio, en cualquier celda que no haya sido previamente ocupado por otro núcleo.

# Simulación y resultados

## Objetivo 1

Para cumplir el objetivo descrito, se ha decidido realizar $10$ corridas del experimento, cada una con una probabilidad inicial de celda viva que va desde $0$ hasta $1$ en pasos incrementales de $0.1$. Los resultados se graficaron para cada grupo de autómatas celulares por su probabilidad inicial de celda viva y esas imágenes se agruparon en un `GIF` para mostrar la animación de su desarrollo. Quedaron excluidas las gráficas de probabilidad 0 y 1, puesto que muestran una gráfica en blanco y en negro, respectivamente, asímismo las gráficas en las que no quedan células vivas.

En la Figura 1 se muestran, a manera de ejemplo, las generaciones producidas para la probabilidad de $0.1$.

![Generaciones de la probabilidad de $0.1$. Una celda negra indica que está *viva* y una en blanco, lo contrario.](p1Long.png)

El resto de probabilidades corren suertes similares, en términos del cambio de sus estados, por lo que se recomienda revisar las animaciones de las generaciones si se desea una mejor ilustración de lo sucedido. Sin embargo cabe destacar el cambio en duración que las distintas probabilidades permiten a las generaciones. La Figura 2 muestra su comportamiento.

![Generaciones de sobrevivencia por probabilidad inicial de celda viva.](elapsedGenerations.png)

En ella se ve que, bajo las condiciones definidas para estos autómatas, tienen mayor índice de supervivencia aquellos que inician con probabilidades entre $0.3$ y $0.6$.

## Objetivo 2



# Apéndice

## Código del Objetivo 1

```ref
library(parallel)
suppressMessages(library("sna"))
unlink("*.png")
dimension <- 100
size <- dimension ^ 2

elapsedGenerations <- data.frame()

experiment <- function(position){
  row <- floor((position - 1) / dimension) + 1
  col <- ((position - 1) %% dimension) + 1
  neighborhood <- current[
    max(row - 1, 1) : min(row + 1, dimension),
    max(col - 1, 1) : min(col + 1, dimension)
  ]
  return(1 * ((sum(neighborhood) - current[row, col]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dimension")
for (i in seq(0, 1, 0.1)) {
  current <- matrix(runif(size), nrow = dimension, ncol = dimension)
  current <- (current < i) * 1
  generation <- 0

  while(sum(current) > 0){
    output = paste("p", i * 10, "g", sprintf("%02d", generation), ".png", sep = "")
    elapsed = paste("Porcentaje", i ,"Paso", generation)
    if(sum(current) < size){
      png(output)
      plot.sociomatrix(current, diaglab = FALSE, main = elapsed, drawlab = FALSE)
      graphics.off()
    }

    generation <- generation + 1
    initial <- sum(current)
    clusterExport(cluster, "current")
    nextMatrix <- parSapply(cluster, 1:size, experiment)
    current <- matrix(nextMatrix, nrow = dimension, ncol = dimension, byrow = TRUE)

    if(initial == sum(current)){ # evita repeticiones en grupos que intercambian sus posiciones
      break
    }
  }
  elapsedGenerations <- rbind(elapsedGenerations, c(i, generation))
}

png("elapsedGenerations.png")
plot(elapsedGenerations[,1], elapsedGenerations[,2], xlab = "Probabilidad inicial de celda viva", ylab = "Iteraciones")
axis(1, at = elapsedGenerations[,1])
graphics.off()
stopCluster(cluster)
```



[^1]: http://natureofcode.com/book/chapter-7-cellular-automata/
[^2]: http://tocs.ulb.tu-darmstadt.de/50226088.pdf
[^3]: En el caso de las células ubicadas en los extremos de la rejilla, únicamente se toman sus vecinos inmediatos, esto es $3$ vecinos para las células de las esquinas superior izquierda, etc.
[^4]: http://www.imagemagick.org/script/index.php
