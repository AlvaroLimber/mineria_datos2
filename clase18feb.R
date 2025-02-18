rm(list=ls())
library(dplyr)
library(tm)#Minería de texto
library(udpipe)#Etiquetado
library(hunspell)#Ortografía
library(syuzhet)#Análisis de sentimiento
#################
options(stringsAsFactors = FALSE)
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")
################
load("_data/larazon.RData")
################
#Ortografía
texto<-"Esto es un tezto con algunos errores de ortografia"
list_dictionaries()
aux<-hunspell(texto,dict =  "es_ES")
hunspell_suggest(aux[[1]], dict =  "es_ES")
bd$titular
hunspell(bd$titular[1:5] , dict =  "es_ES")
clarazon <- VCorpus(VectorSource(bd$titular))
clarazon[[20]]$content




