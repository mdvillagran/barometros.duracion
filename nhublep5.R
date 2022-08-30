
library(haven)
library(dplyr)
library(plyr)
library(openxlsx)
library(stringr)
library(stringi)


rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/Barometros/informes de avance/informes.cierre")



baro1<-read_sav("Barometro_2022-abr-22_2022_22_08_10_28.sav")


# Filtrado por región

baro<-subset(baro1, region==16)

baro<- baro %>% filter(Status %in% c("Approved", "Requires Approval"))

baro <- baro[!is.na(baro$S1_sexo),]


################################################################################
              ################ Extraer preguntas p5  ############
################################################################################

p5<-baro %>% select(SbjNum,comuna,starts_with("p5_"))

################################################################################
              ############## Normalizar respuestas p5  ############
################################################################################


for (i in 2:ncol(p5)){
  p5[[i]]<-tolower(stri_trans_general(p5[[i]],"Latin-ASCII"))
}


############################## RECODIFICACIÓN ##################################

# Gobernador

p5$p5_1rec<-ifelse((grepl('crisosromo|crisost|crisostom|crisostomo|
                          |oscar|osacr|oscarcrisostomo|oscra',p5$p5_1)),
                   "conoce","no conoce")

table(p5$p5_1,p5$p5_1rec)


# Constituyente

p5$p5_2rec<-ifelse((grepl('letelier|arrau| cesar|martin|arbie',
                          p5$p5_2)),
                   "conoce","no conoce")

table(p5$p5_2,p5$p5_2rec)


# Delegado presidencial

p5$p5_3rec<-ifelse((grepl('alarcom|claudio|ferrada',
                          p5$p5_3)),
                   "conoce","no conoce")

table(p5$p5_3,p5$p5_3rec)


# Diputado

p5$p5_5rec<-ifelse((grepl('camano|martines|bravo|martinez|frank|echawerbaum|
                          |felipe|sahuerbaum|sawerdaw|sahuerborn|sawurvan',
                          p5$p5_5)),
                   "conoce","no conoce")

table(p5$p5_5,p5$p5_5rec)


# Senador

p5$p5_6rec<-ifelse((grepl('gustavo|carvajal|loreto|sanhueza',
                          p5$p5_6)),
                   "conoce","no conoce")

table(p5$p5_6,p5$p5_6rec)


# Consejero regional

p5$p5_7rec<-ifelse((grepl('arnoldo|avila|cristian |redlich|jarpa|sepulveda|
                          |polanco|aravena|lorenavera|pozo',
                          p5$p5_7)),
                   "conoce","no conoce")

table(p5$p5_7,p5$p5_7rec)

################################################################################
                 ################ alcalde y consejales ############
################################################################################


# Alcalde

p5$comuna.alcalde<-paste0(p5$comuna,".",p5$p5_4)

table(p5$comuna.alcalde)



p5$p5_4rec<-ifelse((grepl('bulnes.guillermo|bulnes.yeber|viejo.jorge|viejo.poso|
                          |chillan.benavente|chillan.camili|chillan.camilo|
                          |chillan.don|coelemu.alejandro|coihueco.carlos|coihueco.chandia|
                          |carmen.don|carmen.jose|carmen.san|ninhue.luis|niquen.manuel|
                          |pemuco.jhonson|portezuelo.rene|quillon.miguel|quillon.pena|
                          |quirihue.richard|ranquil.nicolas|carlos.gasto|carlos.suazo|
                          |carlos.william|fabian.almuna|ignacio.cesar|nicolas.hugo|
                          |nicolas.victor|treguaco.raul|yungay.rafael', p5$comuna.alcalde)),
                   "conoce","no conoce")

table(p5$comuna.alcalde,p5$p5_4rec)


# Concejal

p5$comuna.consejal<-paste0(p5$comuna,".",p5$p5_8)

table(p5$comuna.consejal)



p5$p5_8rec<-ifelse((grepl('bulnes.golzalo|bulnes.nelson|bulnes.troncoso|viejo.evelin|
                          |chillan.careaga|chillan.cariaga|chillan.gustavi|chillan.josepe|
                          |chillan.patricio|chillan.ricardo|chillan.wini|coelemu.carmen|
                          |coelemu.natalia|coihueco.carlos|coihueco.erika|coihueco.luis|
                          |coihueco.wilson|carmen.blanca|carmen.hugo|ninhue.carlita|
                          |portezuelo.alan|portezuelo.ibanez|portezuelo.juan|portezuelo.modesto|
                          |quillon.felipe|carlos.loerna|carlos.lorena|carlos.pedro|
                          |carlos.rafael|carlos.silva|nicolas.antonio|nicolas.julio|
                          |nicolas.palavecinos|treguaco.pedro|yungay.patricia|yungay.rojas|
                          |yungay.vertita', p5$comuna.consejal)),
                   "conoce","no conoce")

table(p5$comuna.consejal,p5$p5_8rec)

##############################      PEGADO        ##############################

# p5recod<-p5 %>% select(SbjNum,ends_with("rec"))
# 
# baro1<-merge(baro1, p5recod, all.x = T)

p5$comuna.alcalde<-NULL
p5$comuna.consejal<-NULL

########

write_sav(p5, "nhuble.p5.sav")
