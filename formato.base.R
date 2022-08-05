
library(haven)
library(dplyr)
library(lubridate)


rm(list = ls())
setwd("C:/Users/villagran/Desktop/datavoz/Barometros/Informe pre test/metropolitana")

duracion<-read_spss("Barometro_2022-abr-22_2022_26_07_10_02.sav")

duracion1<-subset(duracion, region==13)

duracion1<- duracion1 %>% filter(Status %in% c("Approved", "Requires Approval"))

# duracion1 <- duracion1[!is.na(duracion1$S1_sexo),]

# selección de variables de tiempo

duracion2<-duracion1 %>% dplyr:: select(ends_with("_D"))


# Eliminamos preguntas regionales de otras regiones y de incidencias

duracion2<-duracion2 %>% dplyr::select(-(I_1_tipo_vivienda_D:I_6_I_3_I_3_acepta_1_D))

variables.regionales<-c("RM_1_1_D","RM_1_2_D","RM_1_3_D","RM_1_4_D","RM_1_5_D",
                        "RM_1_6_D","RM_1_7_D","RM_1_8_D","RM_2_D","RM_3_D",
                        "BIOBIO_1_1_D","BIOBIO_1_2_D","BIOBIO_1_3_D","RM_4_1_D",
                        "RM_4_2_D","RM_4_3_D","RM_4_4_D","LL1_D","LL2_D","LL3_D",
                        "LL4_D","OH1_1_D","OH1_2_D","OH1_3_D","OH2_D","OH3_D",
                        "OH4_D","OH5_D","NU1_D","NU2_D","NU3_D","NU4_D","NU5_D",
                        "LR1_D","LR2_D","LR3_D","LR4_D","LR5_1_D" ,"LR5_2_D" ,"LR5_3_D" ,
                        "BB1_1_D","BB1_2_D","BB1_3_D","BB1_4_D","BB1_5_D","BB1_6_D",
                        "BB1_7_D","BB2_1_D","BB2_2_D","BB2_3_D","BB2_4_D","BB2_5_D",
                        "BB3_1_D","BB3_2_D","BB3_3_D","BB3_4_D" ,"BB3_5_D","BB3_6_D",
                        "BB3_7_D","BB3_8_D","BB3_9_D","BB3_10_D","BB3_11_D","BB3_12_D",
                        "BB3_13_D" ,"BB3_14_D" ,
                        "BIOBIO_2_2_D" ,"BIOBIO_2_3_D","BIOBIO_3_1_D","BIOBIO_3_2_D","BIOBIO_3_3_D",
                        "BIOBIO_3_4_D", "BIOBIO_3_5_D",
                        "BIOBIO_1_1_D","BIOBIO_1_2_D","BIOBIO_1_3_D")


variable.regional.de.interes<-c("BIOBIO_2_2_D" ,"BIOBIO_2_3_D","BIOBIO_3_1_D","BIOBIO_3_2_D","BIOBIO_3_3_D",
                                "BIOBIO_3_4_D", "BIOBIO_3_5_D")


(varriables.a.borrar<-setdiff(variables.regionales,variable.regional.de.interes))



duracion2 <- duracion2[ , !(names(duracion2) %in% varriables.a.borrar)]


# check

names(duracion2)

# identificar comienzo de las preguntas de la encuesta

variables<-names(duracion2)

grep("p1_a_D", variables)

# Extraemos las duraciones de interés

duracion2<-duracion2[,8:ncol(duracion2)]

duracion2$total<-rowSums(duracion2, na.rm = T)

duracion2$id<-(row.names(duracion2))

id<-duracion2[,c("total", "id")]


# Ordenamiento en cuanto a variables sociodemográficas


duracion1$id<-row.names(duracion1)

duracion1<-merge(duracion1, id, all.x = T)

ranking<-duracion1[,c("total", "id", "S1_sexo", "S4_nivel_estudio", "S2_edad")]


ranking$minutos.totales<-ranking$total%/%60

ranking<-ranking %>%
  mutate(Horas = hour(seconds_to_period(total)),
         Minutos = minute(seconds_to_period(total))) 


duraciones.may<-subset(ranking, minutos.totales>25)

duraciones.men<-subset(ranking, minutos.totales<25)





# modulos

# p1_a_D - p4_D
modulo1<-duracion2[,1:7]

# p5_1_D - p7_D
modulo2<-duracion2[,8:20]

# p8_D - p11_12_D
modulo3<-duracion2[,21:46]

# p12_1_D - p14_1_D
modulo4<-duracion2[,47:63]

# p16_1_D - p17_7_D
modulo5<-duracion2[,72:87]
modulo5<-modulo5 %>% dplyr::select(-(RM_2_D:RM_3_D))

# P18_a_D - p18_c_D
modulo6<-duracion2[,88:90]

# p19_a_D - p23_12_D
modulo7<-duracion2[,91:111]


# p26_1_D - p31_D
modulo8<-duracion2[,112:133]

# p32_D - p33_c_D
modulo9<-duracion2[,134:137]

# p35_D - p37_D
modulo10<-duracion2[,138:140]

# p39_D - p41_D
modulo11<-duracion2[,141:143]

# modulo12regional

modulo12r<-duracion2 %>% dplyr:: select(starts_with("RM_"))


# S1_sexo_D
modulo13<-duracion2[,149:175]




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

# total 0.3


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

# total 1.27

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

# total 1.39


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

# total 3.01

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

# total 1.31


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

# total 2.26

# gráfico

grafo3<-data.frame(modulo.q)
nombres<-names(modulo1)[1:26]
grafo3$nombres<-nombres




###############################################################################
#################################  modulo 4  ##################################
###############################################################################


modulo4$total<-rowSums(modulo4, na.rm = T)

modulo4$total.minutos<-modulo4$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo4[,1:17], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total .50


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

# total 2

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

# total .52


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

# total 1.35

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

# total 0.18


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

# total 0.36

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

# total 1.36


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

# total 3.25

# gráfico

grafo7<-data.frame(modulo.q)
nombres<-names(modulo7)[1:21]
grafo7$nombres<-nombres


###############################################################################
#################################  modulo 8  ##################################
###############################################################################



modulo8$total<-rowSums(modulo8, na.rm = T)

modulo8$total.minutos<-modulo8$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo8[,1:22], na.rm = T) # en segundos

# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.29


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

# total 3.13

# gráfico

grafo8<-data.frame(modulo.q)
nombres<-names(modulo8)[1:24]
grafo8$nombres<-nombres



###############################################################################
#################################  modulo 9  ##################################
###############################################################################


modulo9$total<-rowSums(modulo9, na.rm = T)

modulo9$total.minutos<-modulo9$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo9[,1:4], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.53


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo9[,1:9]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 1

# gráfico

grafo9<-data.frame(modulo.q)
nombres<-names(modulo9)[1:9]
grafo9$nombres<-nombres




###############################################################################
#################################  modulo 10  #################################
###############################################################################


modulo10$total<-rowSums(modulo10, na.rm = T)

modulo10$total.minutos<-modulo10$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo10[,1:3], na.rm = T) # en segundos



# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total .13


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo10[,1:8]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 1.11

# gráfico

grafo10<-data.frame(modulo.q)
nombres<-names(modulo10)[1:8]
grafo10$nombres<-nombres





###############################################################################
#################################  modulo 11  ##################################
###############################################################################


modulo11$total<-rowSums(modulo11, na.rm = T)

modulo11$total.minutos<-modulo11$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo11[,1:3], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.12

# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo11[,1:3]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 0.27

# gráfico

grafo11<-data.frame(modulo.q)
nombres<-names(modulo11)[1:3]
grafo11$nombres<-nombres



###############################################################################
#################################  modulo 12r  #################################
###############################################################################



modulo12r$total<-rowSums(modulo12r, na.rm = T)

modulo12r$total.minutos<-modulo12r$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo12r[,1:14], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.05


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo12r[,1:11]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 1.19

# gráfico

grafo12r<-data.frame(modulo.q)
nombres<-names(modulo12r)[1:11]
grafo12r$nombres<-nombres

###############################################################################
#################################  modulo 13  #################################
###############################################################################

modulo13$total<-rowSums(modulo13, na.rm = T)

modulo13$total.minutos<-modulo13$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo13[,1:27], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 17.47


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo13[,1:27]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duración de modulo 1 de acuerdo al 3cuartil de duración de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 16.32

# duración media de pregunta S_A_D 352 segundos; 5,52 minutos 

# gráfico

grafo12<-data.frame(modulo.q)
nombres<-names(modulo11r)[1:27]
grafo12$nombres<-nombres













# automatizar a través de un loop duraciones totales por individuo (no terminado)

for (i in 1){
  resultado<-unname(rowSums(modulos[[i]], na.rm = T))
  # print(resultado)
  print(length(names(modulos[[i]])))
  modulos[[i]][, length(names(modulos[[i]])) + 1]<-resultado
  colnames(modulos[[i]])[length(names(modulos[[i]]))+1] <- paste0("new")
  
  # print(contador)
  # i[ , contador + 1] <- 1
  
}



ejemplo <- list(a=c(0,0,1), b=c(0,0,2))

sapply(ejemplo, sum)


