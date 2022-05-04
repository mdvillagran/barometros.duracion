
library(haven)
library(dplyr)
library(lubridate)

setwd("C:/Users/villagran/Desktop/datavoz/Barometros/Informe pre test/los lagos")

duracion<-read_spss("duracionloslagos.sav")

# selección de variables de tiempo

duracion1<-duracion %>% dplyr:: select(ends_with("_D"))

duracion1<-duracion1[,880:1063]

duracion1$total<-rowSums(duracion1, na.rm = T)

duracion1$id<-(row.names(duracion1))

id<-duracion1[,c("total", "id")]


# Ordenamiento en cuanto a variables sociodemográficas


duracion$id<-row.names(duracion)

duracion<-merge(duracion, id, all.x = T)

ranking<-duracion[,c("total", "id", "S1_sexo", "S4_nivel_estudio", "S2_edad")]


ranking$minutos.totales<-ranking$total%/%60

ranking<-ranking %>%
  mutate(Horas = hour(seconds_to_period(total)),
         Minutos = minute(seconds_to_period(total))) 






# modulos

modulo1<-duracion1[,1:7]

modulo2<-duracion1[,8:20]

modulo3<-duracion1[,21:46]

modulo4<-duracion1[,47:64]

modulo5<-duracion1[,65:78]

modulo6<-duracion1[,79:81]

modulo7<-duracion1[,82:102]

modulo8<-duracion1[,103:126]

modulo9<-duracion1[,127:135]

modulo9<-duracion1[,136:143]

modulo10<-duracion1[,144:146]

modulo11r<-duracion1[,147:157]

modulo12<-duracion1[,158:183]

modulos<-list("modulo1"= modulo1, "modulo2" = modulo2, "modulo3" = modulo3, "modulo4" = modulo4,
              "modulo5" = modulo5, "modulo6" = modulo6, "modulo7" = modulo7, "modulo8" = modulo8,
              "modulo9" = modulo9, "modulo10" = modulo10, "modulo11r" = modulo11r, "modulo12" = modulo12)


##############################################################################
#################################  modulo 1  ##################################
##############################################################################

modulo1$total<-rowSums(modulo1, na.rm = T)

modulo1$total.minutos<-modulo1$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo1[,1:7], na.rm = T) # en segundos

#-----------

duracion <- function(segundos){
objeto1 = segundos%/%60
objeto2 =  (floor(segundos-(objeto1*60)))/100
objeto1+objeto2
}

#----------
# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.49


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo1[,1:7]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
  }

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)
  
# total 1.39

# gráfico

grafo1<-data.frame(modulo.q)
nombres<-names(modulo1)[1:7]
grafo1$nombres<-nombres



###############################################################################
#################################  modulo 2  ##################################
###############################################################################

modulo2$total<-rowSums(modulo2, na.rm = T)

modulo2$total.minutos<-modulo2$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo2[,1:13], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.36


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo2[,1:13]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 3.06

# gráfico

grafo2<-data.frame(modulo.q)
nombres<-names(modulo1)[1:13]
grafo2$nombres<-nombres



###############################################################################
#################################  modulo 3  ##################################
###############################################################################


modulo3$total<-rowSums(modulo3, na.rm = T)

modulo3$total.minutos<-modulo3$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo3[,1:26], na.rm = T) # en segundos

# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.53


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo3[,1:26]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 3.03

# gráfico

grafo3<-data.frame(modulo.q)
nombres<-names(modulo1)[1:26]
grafo3$nombres<-nombres




###############################################################################
#################################  modulo 4  ##################################
###############################################################################


modulo4$total<-rowSums(modulo1, na.rm = T)

modulo4$total.minutos<-modulo4$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo4[,1:18], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 2.09


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo4[,1:18]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 2.2

# gráfico

grafo4<-data.frame(modulo.q)
nombres<-names(modulo4)[1:18]
grafo4$nombres<-nombres




###############################################################################
#################################  modulo 5  ##################################
###############################################################################



modulo5$total<-rowSums(modulo5, na.rm = T)

modulo5$total.minutos<-modulo5$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo5[,1:14], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.52


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo5[,1:14]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 1.52

# gráfico

grafo5<-data.frame(modulo.q)
nombres<-names(modulo5)[1:14]
grafo5$nombres<-nombres



###############################################################################
#################################  modulo 6  ##################################
###############################################################################



modulo6$total<-rowSums(modulo6, na.rm = T)

modulo6$total.minutos<-modulo6$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo6[,1:3], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.32


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo6[,1:3]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 0.39

# gráfico

grafo6<-data.frame(modulo.q)
nombres<-names(modulo6)[1:3]
grafo6$nombres<-nombres



###############################################################################
#################################  modulo 7  ##################################
###############################################################################


modulo7$total<-rowSums(modulo7, na.rm = T)

modulo7$total.minutos<-modulo7$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo7[,1:21], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.54


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo7[,1:21]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 4.34

# gráfico

grafo7<-data.frame(modulo.q)
nombres<-names(modulo7)[1:21]
grafo7$nombres<-nombres


###############################################################################
#################################  modulo 8  ##################################
###############################################################################



modulo8$total<-rowSums(modulo1, na.rm = T)

modulo8$total.minutos<-modulo8$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo8[,1:24], na.rm = T) # en segundos

# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 4.19


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo8[,1:24]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 4.06

# gráfico

grafo8<-data.frame(modulo.q)
nombres<-names(modulo8)[1:24]
grafo8$nombres<-nombres



###############################################################################
#################################  modulo 9  ##################################
###############################################################################


modulo9$total<-rowSums(modulo1, na.rm = T)

modulo9$total.minutos<-modulo9$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo9[,1:8], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.23


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo9[,1:8]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 1.42

# gráfico

grafo9<-data.frame(modulo.q)
nombres<-names(modulo9)[1:8]
grafo9$nombres<-nombres




###############################################################################
#################################  modulo 10R  #################################
###############################################################################


modulo10$total<-rowSums(modulo10, na.rm = T)

modulo10$total.minutos<-modulo10$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo10[,1:3], na.rm = T) # en segundos



# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.26


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo10[,1:3]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 0.4

# gráfico

grafo10<-data.frame(modulo.q)
nombres<-names(modulo10)[1:3]
grafo10$nombres<-nombres





###############################################################################
#################################  modulo 11  ##################################
###############################################################################


modulo11r$total<-rowSums(modulo11r, na.rm = T)

modulo11r$total.minutos<-modulo11r$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo11r[,1:11], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.37


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo11r[,1:11]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 3.47

# gráfico

grafo11r<-data.frame(modulo.q)
nombres<-names(modulo11r)[1:11]
grafo11r$nombres<-nombres



###############################################################################
#################################  modulo 12  #################################
###############################################################################

modulo12$total<-rowSums(modulo12, na.rm = T)

modulo12$total.minutos<-modulo12$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo12[,1:26], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 14.02


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo12[,1:26]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 4.56

# gráfico

grafo11r<-data.frame(modulo.q)
nombres<-names(modulo11r)[1:11]
grafo11r$nombres<-nombres














# automatizar a través de un loop duraciones totales por individuo

for (i in 1){
   resultado<-unname(rowSums(modulos[[i]], na.rm = T))
  # print(resultado)
   print(length(names(modulos[[i]])))
   modulos[[i]][, length(names(modulos[[i]])) + 1]<-resultado
   colnames(modulos[[i]])[length(names(modulos[[i]]))+1] <- paste0("new")
   
  # print(contador)
  # i[ , contador + 1] <- 1

  }







data <- data.frame(x1 = 1:5,                      # Create example data
                   x2 = 6:10,
                   x3 = 11:15)



for(i in 1:3) {                                   # Head of for-loop
  new <- rep(i, nrow(data))                       # Create new column
  data[ , ncol(data) + 1] <- new                  # Append new column
  colnames(data)[ncol(data)] <- paste0("new", i)  # Rename column name
}