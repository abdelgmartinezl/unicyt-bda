########################
   ### Clustering ###
########################

# Es una tarea de agrupamiento de un conjunto de objetos de tal manera que objetos del mismo
# grupo (llamado cluster) son más similares entre cada uno que los de otros grupos.

# No es un algoritmo específico, pero una tarea general que se debe resolver.

# Es un proceso iterativo del descubrimiento de conocimiento o optimización multi-objetivos
# interactiva que involucra ensayo y error.

# Algunos sinónimos son clasificación automática taxonomía numérica y análisis tipológico.

# Existe una connotación distinta entre minería de datos y machine learning. En la primera,
# los grupos resultantes son el asunto de interés. En la segunda, el poder
# discriminativo resultante es de interés.

# La diferencia entre clasificación y agrupación es que la primera la considera
# como una instancia del aprendizaje supervisado (aprende de un conjunto de
# datos de entrenamiento correctamente identificados) y la segunda como un proceso
# no supervisado (involucra agrupar datos en categorías basados en alguna medida de
# similitud o distancia).

###
# Cluster
###
# Se define como un conjunto de objetos de datos. Su noción varía dependiendo del algoritmo usado.

# Sus modelos típicos incluyen:
# - Conectividad: basado en distancia (Hierarchical)
# - Centroide: cada cluster se basea en un vector promedio individual (K-means)
# - Distribución: basado en una distribución estadística (Expectation-maximization)
# - Densidad: basado en regiones densas conectadas en el espacio de datos (DBSCAN, OPTICS)
# - Sub-espacio: modelados con miembros del cluster y atributos relevantes (Two-mode-clustering)
# - Grupos: Algunos algoritmos no proveen un modelo sino información de agrupación
# - Grafo: Subconjunto de nodos en un grafo donde cada dos nodos en el subconjunto están conectados por un eje (HCS)

# Se pueden distinguir en:
# - Hard Clustering: Cada objeto pertenece a un cluster o no
# - Soft Clustering: Cada objeto pertenece a cada cluster basado en un cierto grado (afinidad)

###
# Evaluación
###
# Se define como la validación del cluster.
# Evaluación Interna: basado en la data que fue clusterizada. Davies-Bouldin Index, Dunn Index, Silhouette Coefficient
# Evaluación Externa: basado en la data no usada para clustering. Se usan conjuntos de datos creados por expertos. Rand Measure, F-measure, Jaccard Index, Fowlkes-Mallows Index, Confusion Matrix.

###
# Aplicaciones
###
# Biología:
# - Ecología de animales y plantas
# - Análisis de secuencias (familias genéticas)
# Medicina:
# - Análisis de actividad antimicrobial
# - Análisis de imágenes médicas
# Negocios:
# - Estudios de mercado
# - Agrupación de artículos de compra
# WWW:
# - Análisis de redes sociales (comunidades)
# - Agrupamiento de resultados de búsquedas
# Computación:
# - Evolución de software
# - Segmentación de imagen
# Ciencias Sociales:
# - Análisis de crimen
# - Minería de datos educacional
# Otros:
# - Robótica de campo
# - Química matemática
# - Climatología
# - Geología del petróleo

########################################################
########################################################
########################################################
########################################################
########################################################

###########
# K-means #
###########
set.seed(8953)
iris_copy <- iris
iris_copy$Species <- NULL
(kmeans.result <- kmeans(iris_copy, 3))
# Validar resultado de cluster contra las etiquetas de clases (Species)
table(iris$Species, kmeans.result$cluster)
# La clase setosa puede separarse fácilmente de los otros clusters.
# Las clases versicolor y virginica tienen un grado pequeño de salto una con otra.
# Graficamos los resultados
plot(iris_copy[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
# Agregamos los centros de cada cluster
points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8)

#############
# K-Medoids #
#############
# Diferencia con K-Means:
# - En k-means un cluster es representado por su centro.
# - En k-medoids el objeto más cercano al centro del cluster.
# - Son más robustos por la presencia de excepciones.

# El algoritmo clásico es PAM (Partitioning Around Medoids).
# CLARA es la técnica mejorada de PAM, pero funciona con conjuntos de datos grandes.
library(fpc)
pamk.result <- pamk(iris_copy)
# Para ver el número de clusters
pamk.result$nc
# Validar el resultado del clustering contra las especies reales
# El resultado fueron 2 clusters: setosa y una mezcla de versicolor con virginica.
table(pamk.result$pamobject$clustering, iris$Species)
# Opcional: Cambia el entorno para que sean 2 graficos por pagina
layout(matrix(c(1,2), 1, 2))
# La primera grafica es un clusplot de los dos clusters y las lineas muestran la distancia enter los clusters.
# La segunda grafica muestra sus siluetas. Una silueta cerca de 1 sugiere que las observaciones están bien clusterizadas, mientras más cerca de 0 significa que la observación está entre dos clusters y un valor negativo indica un mal cluster.
plot(pamk.result$pamobject)
# Opcional: Cambia el entorno para que sea 1 grafico por pagina
layout(matrix(1))

###########################
# Hierarchical Clustering #
###########################
set.seed(2835)
# Crea una muestra de 40 registros del conjunto de datos iris, por lo que la gráfica no esté sobrepoblada
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx, ]
# Remueve etiqueta de clases
irisSample$Species <- NULL
# Aplica el algoritmo
hc <- hclust(dist(irisSample), method = "ave")
# Grafica los clusteres
plot(hc, hang = -1, labels = iris$Species[idx])
# Corta arbol en 3 clusteres
rect.hclust(hc, k = 3)
# Busca los ID de los clusters
groups <- cutree(hc, k = 3)

############
# Densidad #
############
# Agrupa objetos en un cluster si ellos están conectados a otro por area densamente populada
# El algoritmo DBSCAN, del paquete fpc, usa dos parámetros: eps (tamaño de la comunidad) y MinPts (numero minimo de puntos)
# Si el numero de puntos de la comunidad no es menor que MinPts, entonces tenemos un punto denso.
library(fpc)
# Remueve etiquetas de clases
iris_copy <- iris[-5]
ds <- dbscan(iris_copy, eps = 0.42, MinPts = 5)
# Compara el cluster con las etiquetas de clases originales
# 1 a 3 son los clusteres identificados
# 0 son los ruidos o excepciones: objetos no asignados a ningun cluster.
table(ds$cluster, iris$Species)
# Graficamos los resultados
plot(ds, iris_copy)
plot(ds, iris_copy[c(1, 4)])
plotcluster(iris_copy, ds$cluster)

# Prediccion con el Modelo de Cluster #
#######################################
# Etiqueta nueva data, basado en similitudes con los clusters
# Elige una muestra de 10 objetos de iris y agrega pequeños ruidos para hacer un nuevo conjunto de datos
# Los ruidos aleatorios son generados con una distribución uniforme.

# Creo un nuevo conjunto de datos
set.seed(435)
idx <- sample(1:nrow(iris), 10)
# Eliminamos las etiquetas de las clases
new.data <- iris[idx,-5]
# Agregamos ruido aleatorio
new.data <- new.data + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)
# Etiquetamos la nueva data
pred <- predict(ds, iris_copy, new.data)
# Verificamos las etiquetas del cluster
# 8 de los 10 objetos se asignaron a las clases correctas
table(pred, iris$Species[idx])
# Graficamos los resultados
plot(iris_copy[c(1,4)], col = 1 + ds$cluster)
points(new.data[c(1,4)], pch = "+", col = 1 + pred, cex = 3)