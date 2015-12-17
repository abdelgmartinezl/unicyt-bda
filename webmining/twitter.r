##############################################
# Creación de nube de palabras desde Twitter #
##############################################

# Cargar librerías
library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)

######################
# Obtención de Datos #
######################

# Nota: Recuerde crear el acceso en https://apps.twitter.com

# Crea conexión a Twitter
setup_twitter_oauth("rbPjVvWsiiKl4KiFZg4FZ1Htm", "gEmHkQ8fMlnsSFffY3jTqbTVmbr7CaifsmU63xpSq1Eq0bBDxy", "137520613-4SKdEg2c7hz5gzi4oje5YyJvyUlM1WaIz6JQWtqn", "iGXyjCkiHFs2k10pWrxd2LDzNx9fr85Ga7nff6xJcHwu9")
# Recolecta tweets de la cuenta @unicyt
tweets = userTimeline("unicyt", 2000)
# Manda la lista de tweets a un data frame
tweets_df = twListToDF(tweets)
# Extrae el texto de los tweets
tweets_txt_raw = tweets_df$text

#####################
# Limpieza de Datos #
#####################

# Elimina retweets y citas
tweets_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_txt_raw)
# Elimina menciones a otras cuentas
tweets_txt = gsub("@\\w+", "", tweets_txt)
# Elimina símbolos de puntuación
tweets_txt = gsub("[[:punct:]]", "", tweets_txt)
# Elimina números
tweets_txt = gsub("[[:digit:]]", "", tweets_txt)
# Elimina enlaces
tweets_txt = gsub("http\\w+", "", tweets_txt)

##############################
# Pre-procesamiento de Datos #
##############################

# Construye un corpus
corpus = Corpus(VectorSource(tweets_txt))
# Convierte a minúsculas
corpus = tm_map(corpus, content_transformer(tolower))
# Remueve palabras vacías (stopwords)
corpus = tm_map(corpus, content_transformer(removeWords), c(stopwords("spanish"), "unicyt"))
# Carga archivo de palabras vacías (stopwords) personalizada y lo convierte a ASCII
pv_es <- readLines("stopwords_es.tweets_txt_raw",encoding="UTF-8") # Recuerden cambiar la ubicación del archivo de texto
pv_es = iconv(pv_es, to="ASCII//TRANSLIT")
# Remueve palabras vacías personalizada
corpus = tm_map(corpus, content_transformer(removeWords), pv_es)
# Remueve espacios en blanco
corpus = tm_map(corpus, content_transformer(stripWhitespace))
# Construcción de una matriz de términos
tdm <- TermDocumentMatrix(corpus)
# Conversión a matriz
m = as.matrix(tdm)
# Conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)
# Crea un data frame con las palabras y sus frecuencias
dm <- data.frame(word = names(wf), freq=wf)

###############################
# Visualización de Resultados #
###############################

# Grafica la nube de palabras (wordcloud)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))