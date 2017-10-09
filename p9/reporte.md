---
title: "Interacciones entre partículas"
author: "Alberto Benavides"
bibliography: bib.bib
date: "2 de octubre de 2017"
---

# Introducción

En la Práctica 9 compartida por Shaeffer [-@shaeffer] se detallan las instrucciones y códigos para simular una interacción entre partículas con carga en un espacio bidimensional. Estas partículas siguen leyes de atracción similares a las expresadas por la ley de Coulomb, moviéndose en una dirección que depende de la interacción de cargas presentes en el sistema y con una fuerza directamente proporcional a la diferencia de cargas e inversamente proporcional a la distancia euclidiana que las separa.

# Objetivos
1. Agregar a cada partícula una masa y que la masa afecte a la velocidad de movimiento. Estudiar la distribución de velocidades de las partículas y verifica gráficamente que esté presente una relación entre la velocidad y la masa de las partículas.
1. Agregar, además de la masa, un radio que esté proporcional a la masa de alguna manera claramente definida y que estos radios funcionen como los tamaños de las partículas en una visualización.
1. Que las partículas respeten su tamaño y que no se sobrepongan; intentos de sobreponerse ocupan resolverse de otra forma, por ejemplo con rebotes o vibraciones al azar.

# Simulación y resultados
Las simulaciones descritas en este trabajo se ejecutaron en una computadora portátil con sistema operativo Windows 10 Home Single Language, procesador Intel(R) Core(TM) i7-7500U CPU @ $2.70$GHz, $2904$MHz de dos núcleos principales y cuatro lógicos en el lenguaje R.

Este experimento se realizó con $n = 100$ partículas ubicadas en posiciones aleatorias que siguieron una distribución normal de cero a uno en los ejes horizontal $x$ y vertical $y$ de un plano cartesiano. A cada partícula se le asignó, también con una distribución normal aleatoria, una carga $c$ de entre -1 a 1. Adicionalmente se agregó a cada partícula una masa $m$ aleatoria en un rango de uno a cinco unidades que influyó en su desplazamiento, y una velocidad inicial de cero para sus ejes horizontal $vx$ y vertical $vy$.

```r
p <- data.frame(
  x = rnorm(n), y = rnorm(n),
  c = rnorm(n), m = runif(n, 1, 5),
  vx = 0,
  vy = 0
)
```

Una configuración inicial de lo descrito puede consultarse en la figura \ref{p9_t01} (p. \pageref{p9_t01}) generada por la función `symbols`, donde se colorearon las cargas y se asignó a cada partícula $i$ un radio $r_i = \frac{m_i}{200}$ para facilitar la visualización de estos componentes.

![Estado inicial de cien partículas coloreadas con base en su carga $c$ y con radio $r = m / 200$, donde $m$ es su masa. \label{p9_t01}](img/p9_t001.png)

Durante cada una de las cien iteraciones en que se llevó a cabo este experimento, las partículas fueron desplazadas una velocidad horizontal y vertical como consecuencia de la fuerza resultante de las cargas que las afectaban. Los cálculos de esta fuerza para cada partícula por iteración fueron hechos en paralelo mediante la función `parSapply` del paquete `parallel`. El cluster construido fue de dos núcleos, los correspondientes a los núcleos físicos del equipo. Para tomar en cuenta la masa, se dividió al desplazamiento horizontal y vertical por la masa propia de cada partícula de modo que partículas con mayor masa se movieran más lentamente en relación a partículas de menor masa. Estos cálculos se realizaron igualmente en paralelo, con el uso de la función ya citada.

```r
vx <- delta * f[c(TRUE, FALSE)] / p$m
vy <- delta * f[c(FALSE, TRUE)] / p$m
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
```

El otro momento donde se utilizó la masa de las partículas fue cuando colisionaban las cirfunferencias de dos de ellas. Este evento está definido como aquél en el que la distancia euclidiana entre dos partículas es menor que la suma de sus radios. Dado esto, una partícula $i$ es desplazada en cada uno de sus ejes en el sentido contrario de la colisión con otra partícula $j$ una distancia $d$ igual a la diferencia entre la posición de la propia partícula $p_i$ con la que colisionaba $p_j$, por un factor aleatorio $r$ en un rango entre la mitad de la masa de la partícula $j$ y su masa total, y todo esto entre la masa de $i$ de manera que se limite el movimiento de rebote de cada partícula por su propia masa, esto es $d = \frac{(p_i - p_j) \cdot r}{m_i}$. Estos cálculos fueron paralelizados con `parSapply` de la siguiente forma:

```r
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
      r <- runif(1, mj / 2, mj)
      xi <- xi + dx * r / mi
      xi <- max(min(xi, 1), 0)
      yi <- yi + dy * r / mi
      yi <- max(min(yi, 1), 0)
    }
  }
  return(c(xi, yi))
})
p[, 1:2] <- t(pxy)
```

De esta manera se lograron detectar colisiones entre partículas y sus circunferencias, denotadas por su masa, y dotar al sistema de una dinámina no determinista de repulsión frente a choques. La animación hallada en https://goo.gl/MSkSMo muestra cómo se desarrollaron estos movimientos. En ella puede observarse que las partículas más grandes se mueven más despacio y repelen con más fuerza a las partículas pequeñas, mas con el fin de evidenciar este decremento en la velocidad que sufrieron las partículas dada su masa, se calculó para cada partícula su velocidad promedio a lo largo de las cien iteraciones de la simulación, esto como resultado de la sumatoria de las velocidades, en cada iteración y para cada eje, las cuales fueron divididas entre el número de pasos para luego calcular la magnitud del vector resultante mediante teorema de Pitágoras. El cálculo de la velocidad promedio también se paralelizó con la función ya citada.

```r
for (i in 1:100) { # durante 100 iteraciones
  [...]
  p$vx <- (p$vx + abs(vx))
  p$vy <- (p$vy + abs(vy))
  [...]
}
# media de velocidad
p$vx <- p$vx / 100
p$vy <- p$vy / 100
# cálculo de la magnitud de la velocidad promedio
p$v <- parSapply(cluster, 1:n, function(i){
  sqrt(p[i,]$vx^2 + p[i,]$vy^2)
})
```

Se graficaron estos resultados, primero, en un diagrama de caja y bigotes en la figura \ref{Boxplot} (p. \pageref{Boxplot}) en donde se puede apreciar que la velocidad promedio disminuye al aumentar la masa; en segundo lugar se plasmó la velocidad promedio junto a la masa y el valor absoluto de la carga de las partículas en un diagrama ternario en el que se resalta con un gradiente de color la masa de las partículas. Este diagrama puede consultarse en la figura \ref{Velocity} (p. \pageref{Velocity}) y constatar, a manera de conclusión, que las partículas de masa mayor tienen una velocidad menor, misma que se incrementa al disminuir la masa. A esto se suma el hecho de que la velocidad de movimiento de las partículas es directamente proporcional al valor absoluto de la carga. Estos resultados corresponden con las ecuaciones utilizadas y las relaciones directa e inversamente proporcional con las que se utilizaron la carga y la masa de las partículas, respectivamente, en la simulación.

![Diagrama de caja y bigotes de las partículas agrupadas por masa por redondeo donde se muestran las medidas de posición evaluadas por la velocidad promedio. \label{Boxplot}](Boxplot.png)

![Diagrama ternario de masa, velocidad y valor absoluto de carga de las partículas, en el que se resalta la importancia de la masa respecto a la velocidad promedio de las mismas.\label{Velocity}](Velocity.png)

# Bibliografía
