---
title: "Redes neuronales"
author: "Alberto Benavides"
bibliography: bib.bib
date: "30 de octubre de 2017"
---

# Objetivos
1. Paralelizar el cálculo de la **Práctica 12: frentes de Pareto** [@schaeffer] y estudiar el efecto de esto en su tiempo de ejecución.

# Simulación y resultados
Este experimento se corrió en una computadora con sistema operativo Windows 10 Home Single Language, procesador Intel(R) Core(TM) i7-7500U CPU @ $2.70$GHz, $2904$MHz de dos núcleos principales y cuatro lógicos, en lenguaje R paralelizado mediante la función `parSapply` de la librería `parallel` con 3 núcleos. La única función paralelizada fue la correspondiente a la prueba con el perceptrón, que se corrió $1000$ veces y se muestra a continuación:

```r
neural <- function(t){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  r <- min(decimal(salida, n), k)
  return(r == correcto)
}
```

Para ambos casos, se midieron los tiempos de ejecución y los resultados de los tiempos de la función paralelizada y la secuencial reflejan un comportamiento esperado, siendo los tiempos secuenciales mayores a los del programa ejecutado en paralelo, lo cual puede consultarse en las gráficas de caja y bigotes de la figura \ref{Times} (p. \pageref{Times}).

![Comparación de tiempos de ejecución secuencial y paralela. \label{Times}](Times.png)

Al correr un test de Wilcox entre los dos conjuntos de resultados, se obtiene un valor para $p = 6.738 \times 10{8}$, por lo que se acepta el que los datos estén relacionados en los tiempos. Adicionalmente, se contaron también los porcentajes de estimaciones correctas hechas por el perceptrón en el caso secuencial y en el paralelo y, en este caso, los porcentajes secuenciales superaron a los paralelos como se observa en la figura \ref{Correct} (p. \pageref{Correct}).

![Porcentaje de aciertos del perceptrón en corrida secuencial y paralela. \label{Correct}](Correct.png)

# Referencias
