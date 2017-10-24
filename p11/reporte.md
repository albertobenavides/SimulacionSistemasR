---
title: "Frentes de Pareto"
author: "Alberto Benavides"
bibliography: bib.bib
date: "23 de octubre de 2017"
---

# Objetivos
1. Paralelizar el cálculo de la **Práctica 11: frentes de Pareto** [@schaeffer] donde convenga y graficar el porcentaje de soluciones de Pareto como función del número de funciones objetivo como diagramas de violín combinados con diagramas de caja-bigote.

2. Elegir un subconjunto del frente de Pareto de tal forma que la selección esté diversificada, es decir, no estén agrupados juntos en una sola zona de la frente las soluciones seleccionadas. Graficar los resultados de la selección, indicando con un color cuáles se incluyen en el subconjunto diverso.

# Simulación y resultados
La simulación presente se ejecutó en una computadora con sistema operativo Windows 10 Home Single Language, procesador Intel(R) Core(TM) i7-7500U CPU @ $2.70$GHz, $2904$MHz de dos núcleos principales y cuatro lógicos. El lenguaje de programación utilizado fue R y la paralelización del código se hizo utilizando las funciones `parSapply` y `parLapply` de la librería `parallel` con un cluster de dos núcleos físicos. El código paralelizado se muestra a continuación:

```r
obj <- parLapply(cluster, 1:k, function(i){
  f <- data.frame(variable=integer(), coef=integer(),
  degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
})
[...]

temp <- parSapply(cluster, 1:(n*k), function(i){
  row <- floor((i - 1) / k) + 1
  col <- ((i - 1) %% k) + 1
  return(eval(obj[[col]], sol[row,], tc))
})
val <- matrix(temp, nrow = n, ncol = k)
[...]

dominadores <- parSapply(cluster, 1:n, function(i){
  d <- logical()
  for (j in 1:n) {
    d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
  }
  cuantos <- sum(d)
  return(cuantos)
})
no.dom <- c(no.dom, dominadores == 0)
```

Las gráfica de 200 valores aleatorios para dos funciones objetivo generadas con tres variables de máximo grado cuatro y un total de cinco términos con los valores óptimos para función objetivo se graficaron a manera de ejemplo en la figura \ref{p11_mejores}.

![200 valores aleatorios para dos funciones objetivo, con valores óptimos marcados para cada una. \label{p11_mejores}](p11_mejores.png)

De estos valores, se obtuvo el frente de Pareto mediante el código de Schaeffer y de este frente se seleccionó un subconjunto de manera aleatoria, de modo que se tuviera un subconjunto del mismo que abarcara la mitad del frente.

```r
mf <- nrow(frente) / 2
mf <- sample(1:nrow(frente), mf)
```

El frente de Pareto y el subconjunto para los datos ya graficados se incluyen en la imagen \ref{p11_frente} donde los círculos de verde marcan el frente y los rojos el subconjunto.

![Frente de Pareto de en verde con un subconjunto de elegido al azar, en rojo. \label{p11_frente}](p11_frente.png)

El experimento se realizó con $k = [2; 10]$ funciones objetivo, $n = \{100, 200, 300, 400, 500\}$ valores al azar por cada $k$ función, un total de $r = [1; 20]$ repeticiones por valor por función objetivo. De estos se calculó el porcentaje de funciones que formaron el frente de pareto para cada número $k$ de funciones y los resultados se agruparon en diagramas de violines y de caja y bigotes sobrepuestos en la figura \ref{p11_violin_ggplot2}.

![Diagramas de violín y de caja y bigotes sobrepuestos para medir el porcentaje de funciones de Pareto en relación con el número de funciones objetivo. \label{p11_violin_ggplot2}](p11_violin_ggplot2.png)

Al correr un análisis de chi cuadrada entre el número $k$ y el porcentaje de soluciones de Pareto se obtuvo un $p = 2.037 \times 10^{-46}$ por lo que se acepta la hipótesis nula y se concluye que las variables son dependientes; lo mismo sucede con la relación entre $n$ y el porcentaje de soluciones de Pareto, que arroja una $p = 5.746 \times 10^{-8}$. Esto explica el comportamiento observado en la figura, donde se ve una relación creciente entre $k$ y $n$ y el porcentaje de soluciones de Pareto encontradas.

# Referencias
