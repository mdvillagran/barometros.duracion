
library(haven)
library(dplyr)
library(lubridate)

setwd("C:/Users/villagran/Desktop/datavoz/Barometros/Informe pre test/nhuble.losrios")

duracion<-read_spss("Barometro_2022-abr-22_2022_16_05_09_37_R16.sav")

# selección de variables de tiempo

# nhuble<-duracion %>% filter(region==16)

duracion1<-duracion %>% dplyr:: select(ends_with("_D"))

# identificar comienzo de las preguntas de la encuesta

variables<-names(duracion1)

grep("p1_a_D", variables)

# Extraemos las duraciones de interés

duracion1<-duracion1[,892:1074]

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



# FIJAMOS EL LÍMITE EN 4, CON EL OBJETIVO DE DEJAR LOS 10 CASOS DE MAYOR DURACIÓN
duraciones.may<-subset(ranking, minutos.totales>42)

duraciones.men<-subset(ranking, minutos.totales<26)




# modulos

# p1_a_D - p4_D
modulo1<-duracion1[,1:7]
modulo1$id<-nhuble$Srvyr

# p5_1_D - p7_D
modulo2<-duracion1[,8:20]
modulo2$id<-nhuble$Srvyr

# p8_D - p11_12_D
modulo3<-duracion1[,21:46]
modulo3$id<-nhuble$Srvyr

# p12_1_D - p14_1_D
modulo4<-duracion1[,47:63]
modulo4$id<-nhuble$Srvyr

# p16_1_D - p17_7_D
modulo5<-duracion1[,64:77]
modulo5$id<-nhuble$Srvyr

# P18_a_D - p18_c_D
modulo6<-duracion1[,78:80]
modulo6$id<-nhuble$Srvyr

# p19_a_D - p23_12_D
modulo7<-duracion1[,81:90]
modulo7$id<-nhuble$Srvyr

# p26_1_D - p31_D
modulo8<-duracion1[,102:123]
modulo8$id<-nhuble$Srvyr

# p32_D - p33_c_D
modulo9<-duracion1[,124:127]
modulo9$id<-nhuble$Srvyr

# p35_D - p37_D
modulo10<-duracion1[,128:130]
modulo10$id<-nhuble$Srvyr

# p39_D - p41_D
modulo11<-duracion1[,131:133]
modulo11$id<-nhuble$Srvyr

# NU1_D - NU5_D
modulo12r<-duracion1[,145:149]
modulo12r$id<-nhuble$Srvyr

# S1_sexo_D
modulo13<-duracion1[,157:185]
modulo13$id<-nhuble$Srvyr


modulos<-list("modulo1"= modulo1, "modulo2" = modulo2, "modulo3" = modulo3, "modulo4" = modulo4,
              "modulo5" = modulo5, "modulo6" = modulo6, "modulo7" = modulo7, "modulo8" = modulo8,
              "modulo9" = modulo9, "modulo10" = modulo10, "modulo11" = modulo11, "modulo12r" = modulo12r,
              "modulo13" = modulo13)


##############################################################################
#################################  modulo 1  ##################################
##############################################################################

modulo1$total<-rowSums(modulo1[,1:7], na.rm = T)

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

# total 4.35


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

modulo2$total<-rowSums(modulo2[,8:13], na.rm = T)

modulo2$total.minutos<-modulo2$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo2[,8:13], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.5


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


modulo3$total<-rowSums(modulo3[,1:26], na.rm = T)

modulo3$total.minutos<-modulo3$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo3[,1:26], na.rm = T) # en segundos

# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 2.33


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


modulo4$total<-rowSums(modulo4[,1:17], na.rm = T)

modulo4$total.minutos<-modulo4$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo4[,1:17], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.53


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo4[,1:17]){
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



modulo5$total<-rowSums(modulo5[,1:14], na.rm = T)

modulo5$total.minutos<-modulo5$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo5[,1:14], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.27


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



modulo6$total<-rowSums(modulo6[,1:3], na.rm = T)

modulo6$total.minutos<-modulo6$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo6[,1:3], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.26


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


modulo7$total<-rowSums(modulo7[,1:10], na.rm = T)

modulo7$total.minutos<-modulo7$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo7[,1:10], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 4.28


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo7[,1:10]){
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



modulo8$total<-rowSums(modulo8[,1:22], na.rm = T)

modulo8$total.minutos<-modulo8$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo8[,1:22], na.rm = T) # en segundos

# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.01


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo8[,1:22]){
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


modulo9$total<-rowSums(modulo9[,1:4], na.rm = T)

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

for (i in modulo9[,1:4]){
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


modulo10$total<-rowSums(modulo10[,1:3], na.rm = T)

modulo10$total.minutos<-modulo10$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo10[,1:3], na.rm = T) # en segundos



# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.22


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

# total 1.11

# gráfico

grafo10<-data.frame(modulo.q)
nombres<-names(modulo10)[1:8]
grafo10$nombres<-nombres





###############################################################################
#################################  modulo 11  ##################################
###############################################################################


modulo11$total<-rowSums(modulo11[,1:3], na.rm = T)

modulo11$total.minutos<-modulo11$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo11[,1:3], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.41


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

modulo12r$total<-rowSums(modulo12r[,1:5], na.rm = T)

modulo12r$total.minutos<-modulo12r$total%/%60

# media de duración del modulo

media.modulo<-colMeans(modulo12r[,1:5], na.rm = T) # en segundos


# media de duración del módulo transformación a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.05


# Media de cuartiles (se tomó el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo12r[,1:5]){
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

modulo13$total<-rowSums(modulo13[,1:27], na.rm = T)

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




# Duraciones totales por entrevista

totales<-data.frame(nhuble$Srvyr)

totales$m1.s <-modulo1$total
totales$m2.s <-modulo2$total
totales$m3.s <-modulo3$total
totales$m4.s <-modulo4$total
totales$m5.s <-modulo5$total
totales$m6.s <-modulo6$total
totales$m7.s <-modulo7$total
totales$m8.s <-modulo8$total
totales$m9.s <-modulo9$total
totales$m10.s <-modulo10$total
totales$m11.s <-modulo11$total
totales$m12.s <-modulo12r$total
totales$m13.s <-modulo13$total


totales$suma.t<-rowSums(totales[2:14])
totales$t.min<-duracion(totales$suma.t)

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





