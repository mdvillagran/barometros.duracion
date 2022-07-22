
library(haven)
library(dplyr)
library(lubridate)
library(labelled)

setwd("C:/Users/villagran/Desktop/datavoz/Barometros/Informe pre test/total y biobio")


duracion<-read_spss("Barometro_duracion_1606.sav")

# selecci?n de variables de tiempo

duracion<-subset(duracion, region==8)

duracion1<-duracion %>% dplyr:: select(ends_with("_D"))

# identificar comienzo de las preguntas de la encuesta

variables<-names(duracion1)

grep("p1_a_D", variables)

# Extraemos las duraciones de inter?s

duracion1<-duracion1[,918:1127]

duracion1$total<-rowSums(duracion1, na.rm = T)

duracion1$id<-(row.names(duracion1))

id<-duracion1[,c("total", "id")]


# Ordenamiento en cuanto a variables sociodemogr?ficas


duracion$id<-row.names(duracion)

duracion<-merge(duracion, id, all.x = T)

ranking<-duracion[,c("total", "id", "S1_sexo", "S4_nivel_estudio", "S2_edad")]


ranking$minutos.totales<-ranking$total%/%60

ranking<-ranking %>%
  mutate(Horas = hour(seconds_to_period(total)),
         Minutos = minute(seconds_to_period(total))) 


# FIJAMOS EL LÍMITE EN 4, CON EL OBJETIVO DE DEJAR LOS 10 CASOS DE MAYOR DURACI?N
duraciones.may<-subset(ranking, minutos.totales>30)

duraciones.men<-subset(ranking, minutos.totales<31)

registro<-tibble(nombre=look_for(duracion1)[2],etiqueta=look_for(duracion1)[3])

# modulos

# p1_a_D - p4_D
modulo1<-duracion1[,1:7]

# p5_1_D - p7_D
modulo2<-duracion1[,8:20]

# p8_D - p11_12_D
modulo3<-duracion1[,21:46]

# p12_1_D - p14_1_D
modulo4<-duracion1[,47:63]

# p16_1_D - p17_7_D
modulo5<-duracion1[,64:77]

# P18_a_D - p18_c_D
modulo6<-duracion1[,78:80]

# p19_a_D - p23_12_D
modulo7<-duracion1[,81:101]


# p26_1_D - p31_D
modulo8<-duracion1[,103:123]

# p32_D - p33_c_D
modulo9<-duracion1[,124:127]

# p35_D - p37_D
modulo10<-duracion1[,128:130]

# p39_D - p41_D
modulo11<-duracion1[,131:133]

# BB1_1_D -  BB3_14_D
modulo12r<-duracion1[,157:182]

# S1_sexo_D
modulo13<-duracion1[,183:210]


modulos<-list("modulo1"= modulo1, "modulo2" = modulo2, "modulo3" = modulo3, "modulo4" = modulo4,
              "modulo5" = modulo5, "modulo6" = modulo6, "modulo7" = modulo7, "modulo8" = modulo8,
              "modulo9" = modulo9, "modulo10" = modulo10, "modulo11" = modulo11, "modulo12r" = modulo12r,
              "modulo13" = modulo13)


##############################################################################
#################################  modulo 1  ##################################
##############################################################################

modulo1$total<-rowSums(modulo1, na.rm = T)

modulo1$total.minutos<-modulo1$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo1[,1:7], na.rm = T) # en segundos

#-----------

duracion <- function(segundos){
objeto1 = segundos%/%60
objeto2 =  (floor(segundos-(objeto1*60)))/100
objeto1+objeto2
}


#----------
# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.41


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo1[,1:7]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
  }

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)
  
# total 1.58

# gr?fico

grafo1<-data.frame(modulo.q)
nombres<-names(modulo1)[1:7]
grafo1$nombres<-nombres



###############################################################################
#################################  modulo 2  ##################################
###############################################################################

modulo2$total<-rowSums(modulo2, na.rm = T)

modulo2$total.minutos<-modulo2$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo2[,1:13], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 2.32


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo2[,1:13]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 3.1

# gr?fico

grafo2<-data.frame(modulo.q)
nombres<-names(modulo1)[1:13]
grafo2$nombres<-nombres



###############################################################################
#################################  modulo 3  ##################################
###############################################################################


modulo3$total<-rowSums(modulo3, na.rm = T)

modulo3$total.minutos<-modulo3$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo3[,1:26], na.rm = T) # en segundos

# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.34


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo3[,1:26]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 3.48

# gr?fico

grafo3<-data.frame(modulo.q)
nombres<-names(modulo1)[1:26]
grafo3$nombres<-nombres




###############################################################################
#################################  modulo 4  ##################################
###############################################################################


modulo4$total<-rowSums(modulo4, na.rm = T)

modulo4$total.minutos<-modulo4$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo4[,1:17], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 2.12


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo4[,1:17]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 2.24

# gr?fico

grafo4<-data.frame(modulo.q)
nombres<-names(modulo4)[1:17]
grafo4$nombres<-nombres




###############################################################################
#################################  modulo 5  ##################################
###############################################################################



modulo5$total<-rowSums(modulo5, na.rm = T)

modulo5$total.minutos<-modulo5$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo5[,1:14], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 2.06


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo5[,1:14]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 2.2

# gr?fico

grafo5<-data.frame(modulo.q)
nombres<-names(modulo5)[1:14]
grafo5$nombres<-nombres



###############################################################################
#################################  modulo 6  ##################################
###############################################################################



modulo6$total<-rowSums(modulo6, na.rm = T)

modulo6$total.minutos<-modulo6$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo6[,1:3], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.45


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo6[,1:3]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 0.55

# gr?fico

grafo6<-data.frame(modulo.q)
nombres<-names(modulo6)[1:3]
grafo6$nombres<-nombres



###############################################################################
#################################  modulo 7  ##################################
###############################################################################


modulo7$total<-rowSums(modulo7, na.rm = T)

modulo7$total.minutos<-modulo7$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo7[,1:21], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 5.24


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo7[,1:21]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 6.08

# gr?fico

grafo7<-data.frame(modulo.q)
nombres<-names(modulo7)[1:21]
grafo7$nombres<-nombres


###############################################################################
#################################  modulo 8  ##################################
###############################################################################



modulo8$total<-rowSums(modulo8, na.rm = T)

modulo8$total.minutos<-modulo8$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo8[,1:21], na.rm = T) # en segundos

# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.47


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo8[,1:21]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 4.15

# gr?fico

grafo8<-data.frame(modulo.q)
nombres<-names(modulo8)[1:21]
grafo8$nombres<-nombres



###############################################################################
#################################  modulo 9  ##################################
###############################################################################


modulo9$total<-rowSums(modulo9, na.rm = T)

modulo9$total.minutos<-modulo9$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo9[,1:4], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 1.05


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo9[,1:4]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 1.17

# gr?fico

grafo9<-data.frame(modulo.q)
nombres<-names(modulo9)[1:4]
grafo9$nombres<-nombres




###############################################################################
#################################  modulo 10  #################################
###############################################################################


modulo10$total<-rowSums(modulo10, na.rm = T)

modulo10$total.minutos<-modulo10$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo10[,1:3], na.rm = T) # en segundos



# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.47


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo10[,1:3]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 0.53

# gr?fico

grafo10<-data.frame(modulo.q)
nombres<-names(modulo10)[1:3]
grafo10$nombres<-nombres





###############################################################################
#################################  modulo 11  ##################################
###############################################################################


modulo11$total<-rowSums(modulo11, na.rm = T)

modulo11$total.minutos<-modulo11$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo11[,1:3], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 0.51


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo11[,1:3]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 0.53

# gr?fico

grafo11<-data.frame(modulo.q)
nombres<-names(modulo11)[1:3]
grafo11$nombres<-nombres



###############################################################################
#################################  modulo 12r  #################################
###############################################################################

modulo12r$total<-rowSums(modulo12r, na.rm = T)

modulo12r$total.minutos<-modulo12r$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo12r[,1:26], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 3.09


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo12r[,1:26]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 3.35

# gr?fico

grafo12r<-data.frame(modulo.q)
nombres<-names(modulo12r)[1:26]
grafo12r$nombres<-nombres

###############################################################################
#################################  modulo 13  #################################
###############################################################################

modulo13$total<-rowSums(modulo13, na.rm = T)

modulo13$total.minutos<-modulo13$total%/%60

# media de duraci?n del modulo

media.modulo<-colMeans(modulo13[,1:28], na.rm = T) # en segundos


# media de duraci?n del m?dulo transformaci?n a minutos y segundos

total<-sum(media.modulo, na.rm = T)
duracion(total)

# total 4.04


# Media de cuartiles (se tom? el tercer cuartil como referencia)

modulo.q<-c()

# filas con preguntas

for (i in modulo13[,1:28]){
  objeto<-unname(i)
  registro<-quantile(objeto, prob =(0.75),na.rm=T)
  #print(registro)
  modulo.q<-append(modulo.q,registro)
}

# Duraci?n de modulo 1 de acuerdo al 3cuartil de duraci?n de las preguntas

total<-sum(modulo.q, na.rm = T)
duracion(total)

# total 4.27

# duraci?n media de pregunta S_A_D 352 segundos; 5,52 minutos 

# gr?fico

grafo12<-data.frame(modulo.q)
nombres<-names(modulo11r)[1:28]
grafo12$nombres<-nombres













# automatizar a trav?s de un loop duraciones totales por individuo (no terminado)

for (i in 1){
   resultado<-unname(rowSums(modulos[[i]], na.rm = T))
  # print(resultado)
   print(length(names(modulos[[i]])))
   modulos[[i]][, length(names(modulos[[i]])) + 1]<-resultado
   colnames(modulos[[i]])[length(names(modulos[[i]]))+1] <- paste0("new")
   
  # print(contador)
  # i[ , contador + 1] <- 1

  }





