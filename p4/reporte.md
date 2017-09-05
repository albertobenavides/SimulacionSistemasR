% Diagramas de Voronoi
% Alberto Benavides
% 4 de septiembre de 2017

# Hipótesis

La cristalización de materiales en diferentes formas reduce la longitud de grietas en sus estructuras.

# Distribución aleatoria de semillas

Esta simulación se corrió bajo sistema operativo Windows 10 Home Single Language con un procesador Intel(R) Core(TM) i7-7500U CPU @ 2.70GHz, 2904 MHz de dos procesadores principales y cuatro procesadores lógicos.

Inicialmente se simularon diagramas de Voronoi en matrices con dimensiones de $16 \times 16$, $32 \times 32$, $64 \times 64$ y $128 \times 128$ celdas cada uno. A cada uno de esos tamaños de matrices se le sembraron en posiciones aleatorias $\frac{1}{4}$, $\frac{1}{2}$ y $\frac{3}{4}$ de semillas en referencia a la longitud de la dimensión de cada matriz, de modo que, por ejemplo, la matriz de $16 \times 16$ se pobló con cuatro, ocho y doce semillas. Se obtuvieron un total de 12 matrices, de entre las cuales los gráficos más representativos corresponden a la matriz de $64 \times 64$ con 48 semillas mostrada en la Figura \ref{VoronoiExample} (p. \pageref{VoronoiExample}), por ser el tamaño de matriz y cantidad de semillas que generan grietas de mayor longitud. Las celdas vacías tomaron el valor de la semilla más cercana a ellas, medida esta cercanía con la fórmula de la distancia euclidiana.

![En matrices de $64 \times 64$ se muestra la distribución de 48 semillas en su posición inicial (izquierda) y su distribución siguiendo el algoritmo de los diagramas de Voronoi (derecha).  \label{VoronoiExample}](main/VoronoiExample.png)

Las grietas se obtuvieron a partir de un experimento que simulaba generar grietas desde uno de los lados de la matriz resultante tras extender las semillas por toda el área de la matriz. Estas grietas seguían preferentemente las fronteras entre áreas de semillas e iban perdiendo probabilidad de expandirse al hacerlo en el interior de una de estas regiones. Un largo significativo para las grietas en la matriz de $64 \times 64$ era de 100 (*vid infra*). En la Figura \ref{CrackExample} (p. \pageref{VoronoiExample}) se pueden ver dos ejemplos de grietas que superaron dicha longitud, de entre decenas.

![Dos ejemplos de grietas en una matriz de $64 \times 64$ y 48 semillas. \label{CrackExample}](main/CrackExample.png)

Este experimento se corrió 500 veces por cada una de las 12 matrices antes descritas. La información de las longitudes de las grietas por dimensión y cantidad de semillas se muestran en la Figura \ref{Lengths} (p. \pageref{Lengths}). Ese conjunto de diagramas de caja y bigotes evidencian que las configuraciones con más semillas por dimensión posibilitan la existencia de grietas más largas.

![Distancia recorridas por grietas en matrices de $16 \times 16$ con cuatro (1), ocho (2) y doce (3) semillas; $32 \times 32$ con $8$ (4), $16$ (5) y $24$ (6) semillas; $64 \times 64$ con $16$ (7), $32$ (8) y $48$ (9) semillas; y $128 \times 128$ con $32$ (10), $64$ (11) y $96$ (12) semillas. \label{Lengths}](main/Lengths.png)

Por otro lado, al comparar entre sí las longitudes de grietas por fracción de semillas iniciales, mostradas en la Figura \ref{LengthsSeeds} (p. \pageref{LengthsSeeds}), se ve que las medianas tienen valores muy similares, a excepción de la matriz de $32 \times 32$ con dieciséis semillas cuya mediana es ligeramente superior a sus equivalentes en fracción de semillas por dimensión.

![En tanto $d$ es dimensión, longitudes de grietas en matrices con $\frac{1}{4} · d$ semillas (superior izquierda), $\frac{1}{2} · d$ semillas (superior derecha) y $\frac{3}{4} · d$ semillas (inferior). \label{LengthsSeeds}](main/LengthsSeeds.png)

Además se graficaron los diagramas de caja y bigotes agrupados por dimensiones de las matrices en la Figura \ref{LengthsDimensions} (p. \pageref{LengthsDimensions}). Estos diagramas muestran cierta variación en las longitudes de las grietas que podría deberse al incremento en el tamaño de semillas en una misma matriz, aunque en matrices de dimensiones mayores este incremento parece paliarse.

![Distancias de las grietas en matrices de $16 \times 16$ (superior izquierda), $32 \times 32$ (superior derecha), $64 \times 64$ (inferior izquierda) y $128 \times 128$ (inferior derecha). \label{LengthsDimensions}](main/LengthsDimensions.png)

Para poder corroborar lo estipulado, se procedió a calcular la densidad de los datos, Figura \ref{Density} (p. \pageref{Density}) y se comprobó que se trata de un conjunto de datos con una distribución normal, por lo que se justifica analizarlo mediante ANOVAs.

![Densidad de las distancias de las grietas obtenidas de los doce diagramas de Voronoi resultantes de la primera experimentación descrita. \label{Density}](main/Density.png)

Los resultados de las ANOVAs analizadas entre conjuntos de semillas por dimensión arrojan todos valores menores a la significacia $\alpha = 0.05$. Se tabulan los resultados en la Tabla \ref{ANOVA} (p. \pageref{ANOVA}). Así puede comprobarse que existen diferencias estadísticas significativas entre los promedios las distancias de las grietas debidas a los cambios en la cantidad de semillas iniciales con que se puebla una matriz.

\begin{table}[!htb]
\centering
\begin{tabular}{@{}cl@{}}
Dimensión & \multicolumn{1}{c}{$p$}   \\
$16$                      & $1.811 \times 10 ^ {-5}$  \\
$32$                      & $1.322 \times 10 ^ {-12}$ \\
$64$                      & $1.628 \times 10 ^ {-11}$ \\
$128$ & $1.557 \times 10 ^ {-4}$              
\end{tabular}
\caption{Valores $p$ de ANOVAs entre semillas por dimensión} \label{ANOVA}
\end{table}

# Otras distribuciones de semillas

Una vez analizados los diagramas de Voronoi con semillas distribuidas aleatoriamente a lo largo y ancho de las matrices, se decidió realizar 4 experimentos adicionales con diagramas de Voronoi cuyas semillas estuvieran cargadas a la izquierda, en el centro, en los bordes y distribuidas como una cuadrícula. Los diagramas de esos ordenamientos se encuentran graficados en la Figura \ref{FinalDistributions} (p. \pageref{FinalDistributions}).

![Diagramas resultantes de ordenar las semillas cargadas a la izquierda (superior izquierda), al centro (superior derecha), en los bordes (inferior izquierda) y como una cuadrícula (inferior derecha). \label{FinalDistributions}](challenge1/FinalDistributions.png)

Estos experimentos se corrieron todos con combinaciones de matrices y semillas idénticas a los del experimento anterior. La comparación de los largos de sus grietas en la Figura \ref{FinalLengths} (p. \pageref{FinalLengths}) hace patente que los tamaños de grietas se mantienen similares a los expuestos en el experimento anterior entre los ordenamientos con semillas en el centro y distribuidas en red. Sin embargo, para el caso de los ordenamientos en los bordes, se incrementan las medianas de los largos. En el caso de las semillas arupadas en el lado izquierdo los largos presentan distribuciones más irregulares, lo que se explica porque presenta una configuración intermedia entre las distribuciones con semillas en el centro y las que las tienen en los bordes.

![Distancias de ordenamientos de semillas agrupadas en la izquierda (superior izquierda), en el centro (superior derecha), en los bordes (inferior izquierdo) y como cuadrícula (inferior derecho). \label{FinalLengths}](challenge1/FinalLengths.png)

# Semillas que se introducen cada generación

Con todo, los resultados anteriores no representaron una mejor implementación respecto a la hipótesis que se desea comprobar, antes bien reforzaron la idea de que mayores cantidades de semillas permiten tamaños de grietas mayores. Es por ello que se optó por realizar un último tipo de distribución de semillas. Esta vez se añadió una semilla cada generación de crecimiento hasta que se cubría toda el área con semillas y su extensión. El lector puede darse una idea de este proceso viendo la Figura \ref{Gif.png}.

![Matriz con una semilla en la generación uno (superior izquierda), ocho semillas en la octava generación (superior derecha), $16$ semillas en la generación $16$ (inferior izquierda) y $32$ semillas en la última generación (inferior derecha). \label{Gif.png}](challenge2/Gif.png)

Para este último experimento se tomó una matriz de $64 \times 64$ y se permitió generar semillas en cada generación hasta que no era posible hacerlo por carencia de células vacías donde colocar semillas. Se eligió este tamaño porque es el que mayores longitudes en la mayoría de los casos (salvo en el ordenamiento hacia la izquierda, el central y el de cuadrícula). Para esta configuración, los tamaños de grietas se vieron reducidos obteniendo una mediana tan sólo de $13$ y un tamaño máximo de grieta de $121$.

Cabe señalar que para obtener una muestra representativa de grietas en esta configuración, se tuvo que reducir el número mínimo que debía medir una grieta para ser considerado como ejemplo de $100$ a $50$, momento en que se obtenían un par de ejemplos de este tipo, mostrados en la Figura \ref{Cracks}.

![Dos ejemplos de grietas que alcanzaron un tamaño máximo en el diagrama de Voronoi al que se le agregan semillas distribuidas de manera uniforme conforme avanzan las generaciones. \label{Cracks}](challenge2/Cracks.png)

# Conclusiones

1. Agregar semillas linealmente en el paso de las generaciones disminuye los tamaños de grietas que puedan incidir en un material con esta configuración.
1. El incremento en las semillas iniciales en un diagrama de Voronoi permite que las grietas adquieran mayores longitudes en sus recorridos.
2. Ordenamientos de semillas en los bordes permiten longitudes de grietas mayores porque ofrecen mayores posibilidades para incidir en fronteras.
