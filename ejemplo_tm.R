rm(list=ls())
library(dplyr)
library(tm)#Minería de texto
library(udpipe)#Etiquetado
library(hunspell)#Ortografía
library(syuzhet)#Análisis de sentimiento en español
library(stringr)
library(pdftools)
library(ggplot2)
library(wordcloud2)
library(ggwordcloud)
library(igraph)
library(visNetwork)
#################
options(stringsAsFactors = FALSE)
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
################
load("_data/larazon.RData")
################
topico<-bd %>% filter(str_detect(tolower(titular),"economía")) 
################
bd_corpus<-VCorpus(VectorSource(topico$titular))
# Corpus en carpeta
getReaders()# ver tipos de formato
dirpdf<-"_data/pdf"
pdf_corpus <- Corpus(DirSource(
  dirpdf, pattern = ".pdf"), readerControl = list(reader = readPDF))
# Limpieza del corpus
getTransformations()
bd_corpus %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(content_transformer(str_replace),"la paz","lapaz") %>% 
  tm_map(removeWords, stopwords("es")) %>%
  tm_map(removeWords, c("ejemplo"))

##función para la limpieza
limpieza<-function(cp,extra=c(""),cambio=c("lapaz"="la paz")){
  cp %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(str_replace_all),cambio) %>% 
    tm_map(removeWords, stopwords("es")) %>%
    tm_map(removeWords, extra) %>% 
    tm_map(stripWhitespace) %>% 
    return()
}
bd_corpusd<-limpieza(bd_corpus, extra = c("hola", "dice"), cambio = c("’"="", "‘"=""))
bd_corpus[[1]]$content
bd_corpusd[[1]]$content

pdf_corpusd<-limpieza(pdf_corpus)
pdf_corpus[[1]]$content
pdf_corpusd[[1]]$content
###armar los TDM O DTM
dtm_v<-DocumentTermMatrix(bd_corpusd)
tdm_v<-TermDocumentMatrix(bd_corpusd)
dtm_pdf<-DocumentTermMatrix(pdf_corpusd)
tdm_pdf<-TermDocumentMatrix(pdf_corpusd)
###Análisis descriptivo: frecuencias
aux<-tdm_v %>% as.matrix() %>% rowSums() %>% data.frame(freq=.)
aux$words<-rownames(aux)
ggplot(aux ,aes(freq, words)) + geom_point()
wordcloud2(aux[,c(2,1)])
ggplot(aux, aes(label = words, size = freq)) +
  geom_text_wordcloud(col="darkblue") +
  scale_size_area(max_size = 15) +  # Ajusta el tamaño máximo de palabras
  theme_minimal()
###Análisis descriptivo: sentimiento
auxs<-get_nrc_sentiment(aux$words,language = "spanish")
rownames(auxs)<-aux$words
barplot(apply(auxs,2,sum),horiz =T, las=1)
auxsc<-auxs %>% mutate(words=aux$words, freq=aux$freq)
ggplot(auxsc, aes(label = words, size = freq, col=fear)) +
  geom_text_wordcloud() +
  theme_minimal()
auxsc<-auxsc %>% filter(words!="economía")
wordcloud2(auxsc %>% select(words, freq), 
           color =ifelse(auxsc$fear==1,"red","black"))
###Análisis diagnóstico: asociaciones
findAssocs(tdm_v,"ministro",0.1)
###Análisis diagnóstico: redes
mxd<-as.matrix(dtm_v)%*%t(as.matrix(dtm_v))
mxt<-as.matrix(tdm_v)%*%t(as.matrix(tdm_v))
dim(mxd)
dim(mxt)

dim(tdm_v)
tdm_reducido <- removeSparseTerms(tdm_v, sparse = 0.99)
dim(tdm_reducido)
mxt<-as.matrix(tdm_reducido)%*%t(as.matrix(tdm_reducido))
#recomendación: reducir la cantidad de términos con base a las frecuencias
g <- graph.adjacency(mxd, weighted = T, mode = 'undirected')
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
data <- toVisNetworkData(g)
nodes = data$nodes; edges = data$edges
set.seed(12345)
grupos <- cluster_label_prop(g)
nodes$value<-prop.table(nodes$degree)*1000
nodes$group<-grupos$membership
visNetwork(nodes,edges)
visNetwork(nodes,edges) %>% 
  visPhysics(enabled = FALSE)
###Análisis diagnóstico: agrupamiento
kmeans(as.matrix(dtm_v),3)
res<-kmeans(as.matrix(dtm_v),3)
data.frame(res$cluster)
