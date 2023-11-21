### Farid Romero Guillermo - A01748843 ###
### Construyendo un modelo de clasificación - Ciencia de Datos II ###
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
# Install - glmnet
if(require(glmnet) == FALSE){                                                
  install.packages('glmnet')                                                 
  library(glmnet)                                                            
}else{                                                                          
  library(glmnet)                                                            
}
# Install - doSNOW
if(require(doSNOW) == FALSE){                                                
  install.packages('doSNOW')                                                 
  library(doSNOW)                                                            
}else{                                                                          
  library(doSNOW)                                                            
}
# Install - broom
if(require(broom) == FALSE){                                                
  install.packages('broom')                                                 
  library(broom)                                                            
}else{                                                                          
  library(broom)                                                            
}
# Install - forcats
if(require(forcats) == FALSE){                                                
  install.packages('forcats')                                                 
  library(forcats)                                                            
}else{                                                                          
  library(forcats)                                                            
}
# Install - yardstick
if(require(yardstick) == FALSE){                                                
  install.packages('yardstick')                                                 
  library(yardstick)                                                            
}else{                                                                          
  library(yardstick)                                                            
}
# Instalar - cargar textrecipes
if(require(textrecipes) == FALSE){                                                
  install.packages('textrecipes')                                                 
  library(textrecipes)                                                            
}else{                                                                          
  library(textrecipes)                                                            
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
    Intervenciones = str_remove(Intervenciones, '^[:lower:]{2,} ?[[:lower:]{2,} ?]+: ?')
    ) %>% 
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
    Intervenciones = str_remove(Intervenciones, '^[:lower:]{2,} ?[[:lower:]{2,} ?]+: ?')
  ) %>% 
  # Rellenar NAs
  fill(interlocutores) %>% 
  ### Seleccionamos sólo las intervenciones de Alejandra del Moral ###
  subset(interlocutores!='gina arely valencia alcántara')

### Unimos ambos documentos de las intervenciones ###
intervenciones=full_join(segundoDebateCandidatas,primerDebateCandidatas) %>% 
  mutate(NoIntervencion=c(1:39)) 

### Creamos un set de entrenamiento ###
intervenciones_split = initial_split(intervenciones, strata = interlocutores)

### Creamos un conjunto de prueba y de entrenamiento ###
trainIntervenciones = training(intervenciones_split)
testIntervenciones = testing(intervenciones_split)

### Crear una receta (usando el texto para predecir) ###
intervenciones_receta = recipe(interlocutores ~ Intervenciones, data = trainIntervenciones) %>% 
  step_tokenize(Intervenciones) %>% 
  step_stopwords(Intervenciones, language = 'es', keep = FALSE) %>% 
  step_tokenfilter(Intervenciones, max_tokens = 2000) %>%
  step_tfidf(Intervenciones)

### Validación cruzada ###
set.seed(234)
folds <- vfold_cv(trainIntervenciones, v = 5)

### Regresión de lasso ###
logit_lasso = logistic_reg() %>% 
  # Define el motor
  set_engine('glmnet') %>% 
  # Define el modo
  set_mode('classification') %>% 
  # Define los hyperparametros
  set_args(penalty = tune(), mixture = 1)

### Creamos un flujo de trabajo ###
modelo_lasso = workflow() %>%
  # Añade una receta
  add_recipe(intervenciones_receta) %>% 
  # Agrega un modelo
  add_model(logit_lasso)

### Construye una malla para optimizar ###
lasso_grid = grid_regular(penalty(), levels = 50)

### Agrega elementos de control ###
ctrl = control_grid(save_pred = FALSE, verbose = TRUE)

### Ajustamos el modelo de lasso ###
ajuste_lasso = modelo_lasso %>% 
  tune_grid(
    # Usa las muestras de validación cruzada
    resamples = folds,
    # Recorre la maya de hyperparametros
    grid = lasso_grid,
    # Agrega los elementos de control
    control = ctrl,
    # Define las métricas
    metrics = metric_set(accuracy, sens, spec, roc_auc, kap))

### Imprimimos las métricas del modelo lasso ###       
metricas_lasso = ajuste_lasso %>% 
  collect_metrics() 
print(metricas_lasso)

### Grafica las métricas ###
metricas_lasso %>%
  # Agrega el lienzo con las estéticas que se heredan
  ggplot(aes(penalty, mean, color = .metric)) +
  # Agrega una línea
  geom_line(size = 1) +
  # Separa por métrica
  facet_wrap(~.metric, scales = "free", ncol = 1) +
  # Agrega títulos
  ggtitle('lasso Regressión Tunning',
          subtitle = 'Metric specification by penalty level') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title.y = element_blank(),
    # Posición de la leyenda
    legend.position = 'none',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )

# Revisamos el mejor modelo según cada métrica
ajuste_lasso %>% select_best('kap')
ajuste_lasso %>% select_best('accuracy')
ajuste_lasso %>% select_best('sens')
ajuste_lasso %>% select_best('spec')
ajuste_lasso %>% select_best('roc_auc')

# Mejor modelo segun la exactitud  
modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(ajuste_lasso %>% 
                      select_best('roc_auc')) %>% 
  # Realiza el último ajuste
  last_fit(intervenciones_split) %>%  
  # Extrae las predicciones
  collect_predictions() %>% 
  # Contruye la matriz de confusión
  conf_mat(truth = interlocutores, estimate = .pred_class)

# Mejor modelo lasso
mejor_lasso = ajuste_lasso %>% 
  select_best('roc_auc')

# Selecciona el mejor modelo
ajuste_lasso_final =  modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(mejor_lasso) 

# Revisa cómo se comporta el conjunto de entrenamiento
ajuste_lasso_final %>% 
  # Ajusta sobre el conjunto
  fit(trainIntervenciones) %>% 
  # Extrae el ajuste
  extract_fit_parsnip()  %>% 
  # Revisa la importancia de variables
  vip::vi() %>%
  mutate(
    Importance = abs(Importance),
    # Ordena por importancia
    Variable = fct_reorder(Variable, Importance),
    # Completa las etiquetas
    Sign = ifelse(Sign == 'POS', 'Positiva','Negativa')
  ) %>%
  top_n(Importance, n = 35) %>% 
  # Grafica de importancia
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  # Agrega columnas
  geom_col() +
  # Modifica el eje x
  scale_x_continuous(
    # Ajusta los márgenes
    expand = c(0, 0)
  ) +
  # Modifica las etiquetas
  labs(x = 'Importancia') +
  # Agrega títulos
  ggtitle('Importancia de variables en el modelo LASSO',
          subtitle = paste0('Valores para lambda = ',mejor_lasso$penalty)) +
  # Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title.y = element_blank(),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )


# Selecciona el mejor modelo
modelo_final =  modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(mejor_lasso) %>% 
  # Realiza el último ajuste
  last_fit(intervenciones_split)

# Matriz de confusión lasso 
matriz_lasso= modelo_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = interlocutores, estimate = .pred_class) 

matriz_lasso %>% 
  autoplot(type='heatmap')

# Métricas lasso
metricas_lasso = modelo_final %>% 
  collect_metrics()

print(metricas_lasso)      

# Predicciones ------------------------------------------------------------
texto = intervenciones_receta %>% 
  prep() %>% 
  bake(
    new_data = tibble(
      Intervenciones = 'violencia de género')
  )

modelo_final %>% 
  extract_fit_parsnip() %>% 
  predict(texto, type="prob" )



