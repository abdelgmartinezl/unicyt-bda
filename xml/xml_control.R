# Instalación de librería
install.packages("XML")

# Carga de librería
library(XML)

# Guarda la URL del archivo XML
xml.url <- "http://www.w3schools.com/xml/plant_catalog.xml"

# Obtiene el archivo XML directamente desde la web
xmlfile <- xmlTreeParse(xml.url)

# El XML está guardado como un objeto
class(xmlfile)

# Acceder al nodo superior
xmltop = xmlRoot(xmlfile)

# Ver primeros sub-nodos del XML
print(xmltop)[1:2]

# Extraer valores del XML
plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))

# Guardado en un data frame
plantcat_df <- data.frame(t(plantcat),row.names=NULL)

# Visualización de resultados
plantcat_df[1:5,1:4]
