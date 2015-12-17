# Instalación de librería
install.packages("RMySQL")

# Carga de librerías
library(RMySQL)
library(DBI)

# Determinación de la controladora
m <- dbDriver("MySQL");

# Establecimiento de conexión
con <- dbConnect(m,user='bda',password='bda',host='192.241.159.100',dbname='employees');

# Ejecución de consulta
res <- dbSendQuery(con, "select * from departments")

# Obtención de resultados
departments <- fetch(res, n = -1)

# Visualización de resultados
departments