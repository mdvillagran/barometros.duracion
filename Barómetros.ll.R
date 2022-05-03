
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

modulos<-list(modulo1,modulo2, modulo3, modulo4,
              modulo5, modulo6, modulo7, modulo8,
              modulo9,modulo10,modulo11r,modulo12)

for (i in modulos){
  resultado<-(colMeans(i, na.rm = T))/60
  print(sum(resultado, na.rm = T))
}

for (i in modulos){
  resultado<-(colMeans(i, na.rm = T))/60
  print(sum(resultado, na.rm = T))
}

