# Instalación de librería
install.packages("RMySQL")

# Carga de librerías
library(RMySQL)
library(DBI)

# Determinación de la controladora
m <- dbDriver("MySQL");

# Establecimiento de conexión
con <- dbConnect(m,user='root',password='password123',host='localhost',dbname='employees');

# Ejecución de consulta
res <- dbSendQuery(con, "select * from departments")

# Obtención de resultados
departments <- fetch(res, n = -1)

# Visualización de resultados
departments
