########################################################################
# Árboles de decisión usando CART (Classification And Regression Tree) #
########################################################################

# Instalación de librerías
install.packages("rpart")
install.packages("rpart.plot")
install.packages("C50")
install.packages("ineq")

# Inclusión de librerías
require("ineq")
require(“rpart”)
require("C50")
require(“rpart.plot”)

# Carga del conjunto de datos
data(iris)

# Visualización de datos
str(iris)
table(iris$Species)

###################################################
# Clasificación: Árboles de decisión usando RPART #
###################################################

# Preprocesamiento de datos
head(iris) # Nota: La data está ordenada
set.seed(1720) # Mezcla las filas del conjunto de datos, utilizando un valor semilla (que nos garantiza igual aleatoreidad)
num_ran <- runif(nrow(iris)) # Crea una serie de números aleatorios entre 0 y 1, con un número máximo igual al tamaño del conjunto de datos (150 filas)
iris_mod <- iris[order(num_ran),] # Nuevo conjunto de datos ya mezclado usando los valores aleatorios de una distribución normal

# Visualización de datos pre-procesados
str(iris_mod)

# Generación de árbol de decisión
arb_dec1 <- rpart(Species ~ ., data=iris_mod[1:100,], method="class") # Elegimos los 100 primeros números (training dataset) para que los 50 últimos números sean para pruebas (testing dataset)

# Visualización de resultados
arb_dec1 # Resumen de árbol de decisión
rpart.plot(arb_dec1) # Gráfico simple
rpart.plot(arb_dec1, type=3, extra=101, fallen.leaves=T) # Gráfico mejorado

# Prueba del modelo
pred1 <- predict(arb_dec1, iris_mod[101:150,], type="class") # Vector de predicciones almacenadas en pred1 de arb_dec1 usando los 50 últimos números del conjunto de datos
table(iris_mod[101:150,5], predicted=pred1) # Matriz de confusión para ver que tal está el modelo, comparando valores actuales y predicciones para medir rendimiento

##################################################
# Clasificación: Árboles de decisión usando C5.0 #
##################################################

# Nota: Se utiliza el mismo conjunto de datos. Ya la data está pre-procesada.

# Generación de árbol de decisión
arb_dec2 <- C5.0(iris_mod[1:100,-5], iris_mod[1:100,5]) # Usamos el conjunto de datos de prueba (training set)

# Visualización de resultados
arb_dec2 # Resultado en general
summary(arb_dec2) # Resultado detallado
plot(arb_dec2) # Gráfica. Nota: no es la mejor opción, pero funciona para conjuntos de datos pequeños

# Prueba del modelo
pred2 <- predict(arb_dec2, iris_mod[101:150,]) # Vector de predicciones almacenadas en pred2 de arb_dec2 usando los 50 últimos números del conjunto de datos
table(iris_mod[101:150,5], predicted=pred2) # Matriz de confusión para ver que tal está el modelo, comparando valores actuales y predicciones para medir rendimiento

#################################################
# Comparación de los Resultados de RPART y C5.0 #
#################################################
table(iris_mod[101:150,5], Rpart_predicted=pred2)
table(iris_mod[101:150,5], C50_predicted=pred1)

###########################
# Cálculo del Índice GINI #
###########################
# Índice GINI: Coeficiente para medir cualquier forma de distribución desigual
# - Es un valor entre 0 y 1, donde 0 corresponde a una perfecta igualdad y 1 a una perfecta desigualdad.
ineq(iris_mod$Species, type="Gini")

#################################
# Cálculo de la Curva de Lorenz #
#################################
# Curva de Lorenz: Representación gráfica que plasma la distribución relativa en un dominio de datos
# - Si la curva coincide con la línea de 45 grados que parte del origen, entonces hay perfecta distribución.
plot(Lc(iris_mod$Species),col="darkred",lwd=2)