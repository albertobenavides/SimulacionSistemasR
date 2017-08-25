% Teoría de colas
% Alberto Benavides
% 24 de agosto de 2017

# Hipótesis

La cantidad de núcleos usados en una simulación paralela en R y el orden con que se pasan las operaciones a dichos núcleos modifican los tiempos de ejecución de las tareas completas.

# Objetivos
1. Examinar las diferencias en los tiempos de ejecución de un mismo experimento con diferentes ordenamientos en distintos núcleos en paralelo.
2. Argumentar las posibles causas a las diferencias en los tiempos de ejecuciones y razonar cómo y por qué afecta el número de núcleos disponibles a la diferencia.
3. Aplicar pruebas estadísticas para determinar si las diferencias observadas entre los tres ordenamientos son significativas.

# Simulación y resultados
