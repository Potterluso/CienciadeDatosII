### Farid Romero Guillermo - A01748843 ###
### Modelado de Tópicos - Ciencia de Datos II ###
### Establecemos el WD ###
setwd('/Users/potterluso/Documents/CienciaDeDatos_Reto/Datos/')
### Limpiando los objetos del ambiente global ###
rm(list = ls())
### Eliminando todos los elementos gráficos ###
graphics.off()
### Declarando el uso de caracteres en Español###
Sys.setlocale("LC_ALL", 'es_ES.UTF-8')
# Install - Load tidyverse
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}   
# Instalar - Cargar tidymodels              
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}
# Instalar - Cargar udpipe              
if(require(udpipe) == FALSE){                                                
  install.packages('udpipe')                                                 
  library(udpipe)                                                            
}else{                                                                          
  library(udpipe)                                                            
}
# Instalar - Cargar tidytext              
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}
# Instalar - Cargar janitor              
if(require(janitor) == FALSE){                                                
  install.packages('janitor')                                                 
  library(janitor)                                                            
}else{                                                                          
  library(janitor)                                                            
}
# Install - stm
if(require(stm) == FALSE){                                                
  install.packages('stm')                                                 
  library(stm)                                                            
}else{                                                                          
  library(stm)                                                            
} 
# Install - geometry
if(require(geometry) == FALSE){                                                
  install.packages('geometry')                                                 
  library(geometry)                                                            
}else{                                                                          
  library(geometry)                                                            
}
# Install - Rtsne
if(require(Rtsne) == FALSE){                                                
  install.packages('Rtsne')                                                 
  library(Rtsne)                                                            
}else{                                                                          
  library(Rtsne)                                                            
}
# Install - rsvd
if(require(rsvd) == FALSE){                                                
  install.packages('rsvd')                                                 
  library(rsvd)                                                            
}else{                                                                          
  library(rsvd)                                                            
}
### Leemos el primer debate###
primerDebate=read.delim("/Users/potterluso/Documents/CienciaDeDatos_Reto/Datos/primer_debate_edomex.txt")

### Convertimos el primer debate a un Tibble y extraemos los interlocutores para quedarnos sólo
### con las candidatas Delfina Gómez y Alejandra del Moral ###
primerDebateCandidatas=as.tibble(primerDebate)%>%
  rename(Intervenciones=Primer.Debate) %>% 
  mutate(
    # Extraer los interlocutores
    interlocutores = str_extract(Intervenciones, '^[:lower:]{2,} ?[[:lower:]{2,} ?]+'),
    # Remover los interlocutores del texto
    Intervenciones = str_remove(Intervenciones, '^[:lower:]{2,} ?[[:lower:]{2,} ?]+: ?'),
    NoIntervencion=(c(1:43))/2,TipoIntervencion=case_when(NoIntervencion>17~'Declaración Final',
                                                       NoIntervencion<3~'Declaración Inicial',
                                                       TRUE~'Respuesta a Pregunta')) %>% 
  # Rellenar NAs
  fill(interlocutores) %>% 
  ### Seleccionamos sólo las intervenciones de Alejandra del Moral ###
  subset(interlocutores!='ana paula ordorica')

### Leemos el segundo debate###
segundoDebate=read.delim("/Users/potterluso/Documents/CienciaDeDatos_Reto/Datos/segundo_debate_edomex.txt")

### Convertimos el segundo debate a un Tibble y extraemos los interlocutores para quedarnos sólo
### con las candidatas Delfina Gómez y Alejandra del Moral###
segundoDebateCandidatas=as.tibble(segundoDebate)%>%
  rename(Intervenciones=Segundo.Debate) %>% 
  mutate(
    # Extraer los interlocutores
    interlocutores = str_extract(Intervenciones, '^[:lower:]{2,} ?[[:lower:]{2,} ?]+'),
    # Remover los interlocutores del texto
    Intervenciones = str_remove(Intervenciones, '^[:lower:]{2,} ?[[:lower:]{2,} ?]+: ?'),
    NoIntervencion=(c(1:41))/2,TipoIntervencion=case_when(NoIntervencion>18~'Declaración Final',
                                                       NoIntervencion<3~'Declaración Inicial',
                                                       TRUE~'Respuesta a Pregunta')
  ) %>% 
  # Rellenar NAs
  fill(interlocutores) %>% 
  ### Seleccionamos sólo las intervenciones de Alejandra del Moral ###
  subset(interlocutores!='gina arely valencia alcántara')

### Unimos ambos documentos de las intervenciones ###
intervenciones=full_join(segundoDebateCandidatas,primerDebateCandidatas)

### Descargamos el modelo de Udpipe en español ###
udmodel = udpipe_download_model(language = "spanish")
### Cargamos el modelo de Udpipe en español ###
udmodel = udpipe_load_model(file = udmodel$file_model)

### Realizamos una lematización sobre las intervenciones de ambas candidatas ###
lemmasCandidatas= udpipe(
  x = intervenciones$Intervenciones, 
  object = 'spanish',
  parallel.cores = 4
) %>% 
  ## Filtrar elementos relevantes ###
  filter(
    !upos %in% c('PUNCT','SCONJ','CCONJ','SYM','NUM','INTJ','AUX'),
    !is.na(upos),
    !lemma %in% c(stopwords::stopwords('es',source = 'stopwords-iso'), 
                  'mil', 'miles', 'meses', 'año' ,'años', 'uno', 'dos', 'tres', 
                  'cuatro', 'cinco', 'nueve', 'seis', 'siete', 'ocho', 
                  'nueve', 'diez', 'millón','millones', 'billón','billones',
                  'peso', 'pesos','unido', 'mes','construcción', 'paraíso',
                  'muchas','gracias','noches','buenas',
                  'bien','miren','pues','comentar','pregunta','gina','Ana','Paula',
                  'gracia','noche','hablar','cosa','pon','minuto','importantísimo',
                  'cuestión','alcanzamo','boscoso','junio','issemín','felipe',
                  'san','sur','alguien','querer','creer'),
    nchar(lemma) > 2,
    !str_detect(lemma,'[:digit:]')
  ) %>% 
  ### Pasar a minúsculas ###
  mutate(lemma = tolower(lemma)) %>% 
  ### Concatenar textos ###
  with_groups(
    .groups = doc_id,
    reframe,
    text = str_c(lemma, collapse = ' ')
  ) 

### Iniciamos con el Modelado de Tópicos ###
### Preprocesamos los datos de ambas candidatas###
procesamientoCandidatas= textProcessor(
  ### Identificamos los documentos ###
  documents = pull(lemmasCandidatas, text), 
  ### Agregamos metadatos ###
  metadata = intervenciones, 
  ### Definimos el lenguaje en español ###
  language = "es", 
  ### Definimos la radicalización ###
  stem = FALSE,
  ### Removemos signos de puntuación ###
  removepunctuation = FALSE,
  ### Usamos minúsculas ###
  lowercase = TRUE,
  ### Removemos palabras de paro ###
  removestopwords = TRUE,
  ### Removemos los números ###
  removenumbers = TRUE,
  ### Longitud de palabras ###
  wordLengths = c(3, Inf),
  ### Agregamos palabras de paro personalizadas ###
  customstopwords = c('méxico')
)

### Preparamos los documentos de las intervenciones de las candidatas ###
outCandidatas= prepDocuments(
  ### Extraer documentos ###
  documents = pluck(procesamientoCandidatas, 'documents'), 
  ### Extraer vocabulario ###
  vocab = pluck(procesamientoCandidatas, 'vocab'), 
  ### Metadata ###
  meta = pluck(procesamientoCandidatas, 'meta'),
  # Fijar umbral inferior de aparición en documentos
  lower.thresh = nrow(intervenciones) * 0.05, 
  # Fijar umbral superior de aparición en documentos
  upper.thresh = nrow(intervenciones) * 0.95
)

### Realizamos el modelo estructural ###
stm_Candidatas = stm(
  # Documentos
  documents = pluck(outCandidatas, 'documents'), 
  # Vocabulario
  vocab = pluck(outCandidatas, 'vocab'), 
  # Metadatos
  data = pluck(outCandidatas, 'meta'),
  # Prevalencia
  prevalence = ~ Intervenciones + interlocutores,
  # Contenido
  content = ~ Intervenciones,
  # Algorimto 
  K = 20, 
  max.em.its = 10000,
  init.type = "Spectral", 
  seed = 123
)

### Mostramos las etiquetas del modelo stm ###
labelTopics(stm_Candidatas)

### Mostramos un resumen gráfico del modelo ###
plot(stm_Candidatas, 
     type = "summary", 
     # Tamaño de letra
     text.cex = 1, 
     # Título
     main = "Modelo Estructural de Tópicos: Candidatas EdoMex - 2023", 
     # Nombre del eje
     xlab = "Preponderancia promedio"
)

### Graficamos los principales términos utilizados en los tópicos más importantes ###
plot(stm_Candidatas, type = "labels", main = "Términos más relevantes en el Debate Electoral del Estado de México - 2023",
     topics = c(7,4))

### Solicitamos la correlación entre los tópicos del debate ###
mod.out.corr = topicCorr(model = stm_Candidatas)
### Graficamos esta correlación ###
plot(mod.out.corr)

### Estimamos efectos estructurales ###
efectoTipoIntervencion=estimateEffect(
  # Ecuación estrucutrar
  formula = 1:20  ~ TipoIntervencion, 
  # Modelo de tópicos
  stmobj = stm_Candidatas,
  # Metadatos
  meta = pluck(outCandidatas, 'meta'), 
  # No queremos contemplar incertidumbre
  uncertainty = "None"
)

### Gráfico de efectos estructurales ###
plot(efectoTipoIntervencion, 
     # Covariable a considerar
     covariate = "TipoIntervencion", 
     # Tópicos
     topics = 7
) 

### Realizamos un resumen del modelo con el efecto del Tipo de Intervención en el dicurso 
### de las candidatas en el debate ###
summary(efectoTipoIntervencion)
### Mostramos el resumen de los tópicos 7 y 4, los más importantes establecidos 
### previamente ###
summary(efectoTipoIntervencion, topics = c(7,4))

