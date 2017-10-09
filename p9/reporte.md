---
title: "Interacciones entre partículas"
author: "Alberto Benavides"
bibliography: bib.bib
date: "2 de octubre de 2017"
---

# Introducción

En la Práctica 9 compartida por Shaeffer [-@shaeffer] se detallan las instrucciones y códigos para simular una interacción entre partículas con carga en un espacio bidimensional. Estas partículas siguen leyes de atracción similares a las expresadas por la ley de Coulomb, moviéndose en una dirección que depende de la interacción de cargas presentes en el sistema y con una fuerza directamente proporcional a la diferencia de cargas e inversamente proporcional a la distancia euclidiana que están separadas.

# Objetivos
1. Agregar a cada partícula una masa y que la masa afecte a la velocidad de movimiento. Estudiar la distribución de velocidades de las partículas y verifica gráficamente que esté presente una relación entre la velocidad y la masa de las partículas.
1. Agregar, además de la masa, un radio que esté proporcional a la masa de alguna manera claramente definida y que estos radios funcionen como los tamaños de las partículas en una visualización.
1. Que las partículas respeten su tamaño y que no se sobrepongan; intentos de sobreponerse ocupan resolverse de otra forma, por ejemplo con rebotes o vibraciones al azar.

# Simulación y resultados
Las simulaciones descritas en este trabajo se ejecutaron en una computadora portátil con sistema operativo Windows 10 Home Single Language, procesador Intel(R) Core(TM) i7-7500U CPU @ $2.70$GHz, $2904$MHz de dos núcleos principales y cuatro lógicos.

Este experimento se realizó con cien partículas ubicadas en posiciones aleatorias que siguieron una distribución normal de cero a uno en los ejes horizontal $x$ y vertical $y$ de un plano cartesiano. A cada partícula se le asignó, también con una distribución normal aleatoria, una carga $c$ de entre -1 a 1. Adicionalmente se agregó a cada partícula una masa $m$ aleatoria en un rango de uno a cinco unidades que influyó en su desplazamiento, y una velocidad inicial de cero para sus ejes horizontal $vx$ y vertical $vy$.

```r
p <- data.frame(
  x = rnorm(n), y = rnorm(n),
  c = rnorm(n), m = runif(n, 1, 5),
  vx = 0,
  vy = 0
)
```

Una configuración inicial de lo descrito puede consultarse en la figura \ref{p9_t01} (p. \pageref{p9_t01}) donde se colorearon las cargas para facilitar la visualización de este componente y se asignó a cada partícula $n$ un radio $r_n = m_n$.

![Estado inicial de cien partículas coloreadas con base en su carga $c$ y con radio $r = m / 200$, donde $m$ es su masa. \label{p9_t01}](img/p9_t01.png)

Durante cada una de las cien iteraciones en que se llevó a cabo este experimento las partículas fueron desplazadas como consecuencia de las fuerzas resultantes de las cargas que las afectaban. Los cálculos de la fuerza resultante para cada partícula por iteración fueron hechos en paralelo mediante la función `parSapply` del paquete `parallel` para el lenguaje R. El cluster construido fue de dos núcleos, los correspondientes a los núcleos físicos del equipo. Para tomar en cuenta la masa, se dividió al desplazamiento horizontal y vertical la masa propia de cada partícula de modo que partículas con mayor masa se movieran más lentamente que partículas de menor masa. Estos cálculos se realizaron igualmente en paralelo, con el uso de función ya citada.

```r
p$x <- parSapply(cluster, 1:n, function(i){
  max(
    min(
      p[i,]$x + delta * f[c(TRUE, FALSE)][i] / p[i,]$m, 1
    ), 0
  )
})
p$y <- parSapply(cluster, 1:n, function(i){
  max(
    min(
      p[i,]$y + delta * f[c(FALSE, TRUE)][i] / p[i,]$m, 1
    ), 0
  )
})
```

El otro momento donde se utilizó la masa de las partículas fue cuando colisionaban las cirfunferencias de dos de ellas. Este evento está definido como aquél en el que la distancia euclidiana entre dos partículas es menor que la suma de sus radios. Dado esto, una partícula $i$ es desplazada en cada uno de sus ejes en el sentido contrario de la colisión con otra partícula $j$ una distancia $d$ igual a la diferencia entre la posición de la propia partícula $p_i$ con la que colisionaba $p_j$, por un factor aleatorio $r$ entre la mitad de la masa de la partícula $j$ y su masa total entre la masa de $i$, esto es $d = \frac{(p_i - p_j) \cdot r}{m_i}$. Estos cálculos fueron paralelizados con `parSapply` de la siguiente forma:

```r
p$x <- parSapply(cluster, 1:n, function(i) {
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
    }
  }
  return(max(min(xi, 1), 0))
})
```

De esta manera se logró detectar colisiones entre partículas y sus circunferencias, denotadas por su masa, y dotar al sistema de una dinámina no determinista de repulsión. La animación hallada en https://goo.gl/MSkSMo muestra cómo se desarrollaron estos movimientos. En ella puede observarse que las partículas más grandes se mueven más despacio y repelen con más fuerza a las partículas pequeñas, mas con el fin de evidenciar este decremento en la velocidad que sufrieron las partículas dada su masa, se calculó para cada partícula su rapidez promedio a lo largo de las cien iteraciones de la simulación, esto como resultado de la sumatoria de las diferencias, en cada iteración y para cada eje, de la posición final menos la inicial de cada partícula, las cuales fueron divididas entre el número de pasos para luego calcular la magnitud, no olvidar que se trata de la rapidez promedio, del vector resultante mediante teorema de Pitágoras. El cálculo de la rapidez promedio también se paralelizó con la función ya citada.

```r
for (i in 1:100) { # durante 100 iteraciones
  [...]
  # posiciones iniciales en cada paso
  tx <- p$x
  ty <- p$y
  [...]
  # sumatoria de los desplazamientos totales
  # (velocidades) antes de las colisiones
  p$vx <- (p$vx + abs(p$x - tx))
  p$vy <- (p$vy + abs(p$y - ty))
  [...]
}
# media de velocidad
p$vx <- p$vx / 100
p$vy <- p$vy / 100
# cálculo de la rapidez promedio
p$v <- parSapply(cluster, 1:n, function(i){
  sqrt(p[i,]$vx^2 + p[i,]$vy^2)
})
```

Se graficaron estos resultados, primero, en un diagrama de caja y bigotes en la figura \ref{Boxplot} (p. \pageref{Boxplot}) en donde se puede apreciar que la velocidad disminuye al aumentar la masa; en segundo lugar se plasmó la velocidad promedio junto a la masa y la carga de las partículas en un diagrama ternario en el que se resalta con un gradiente de color la masa de las partículas. Este diagrama puede consultarse en la figura \ref{Velocity} (p. \pageref{Velocity}) y constatar, a manera de conclusión, que las partículas de masa mayor tienen una rapidez menor, misma que se incrementa al disminuir la masa. A esto se suma el hecho de que la velocidad de movimiento de las partículas es directamente proporcional al valor absoluto de la carga. Estos resultados corresponden con las ecuaciones utilizadas y las relaciones directa e inversamente proporcional con las que se utilizaron la carga y la masa de las partículas, respectivamente.

![Diagrama de caja y bigotes de las partículas agrupadas por masa por redondeo donde se muestran las medidas de posición evaluadas por la velocidad promedio. \label{Boxplot}](Boxplot.png)

![Diagrama ternario de masa, velocidad y carga de las partículas, en el que se resalta la importancia de la masa respecto a la rapidez promedio de las mismas.\label{Velocity}](Velocity.png)

# Bibliografía
