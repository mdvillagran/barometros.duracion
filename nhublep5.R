
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



p5$p5_4rec<-ifelse((grepl('chepica.fabian|chepica.soto|chimbarongo.contreras|
|chimbarongo.marcos|codegua.alexis|codegua.flores|coinco.juan|graneros.claudio|
|graneros.segovia|las cabras.juan|las cabras.pablo|las cabras.paulo|litueche.acuna|
|litueche.rene|litueche.tito|machali.abud|mostazal.escarate|mostazal.garate|mostazal.santiago|
|olivar.estrella|olivar.maria|palmilla.gloria|paredones.antonii|paredones.antonio|
|peralillo.caludio|peralillo.claudio|pichidegua.marcos|pichilemu.cristian|placilla.tulio|
|pumanque.gonzalo|quinta de tilcoco.sebastian|rancagua.carlos|rancagua.gidoy|rancagua.godoy|
|rancagua.gogoy|rancagua.jpablo|rancagua.jua|rancagua.juan|rancagua.ramon|rengo.carlos|rengo.soto|
|requinoa.waldo|san fernando.pablo|san fernando.silva|san vicente.gonzalez|san vicente.jaime|
|san vicente.jaimr|san vicente.jsimr|santa cruz.arevalo|santa cruz.gustavo|santa cruz.william|
|santa cruz.williams', p5$comuna.alcalde)),
                   "conoce","no conoce")

table(p5$comuna.alcalde,p5$p5_4rec)


# Consejal

p5$comuna.consejal<-paste0(p5$comuna,".",p5$p5_8)

table(p5$comuna.consejal)



p5$p5_8rec<-ifelse((grepl('chepica.jaime|chimbarongo.jorge|chimbarongo.marisol|
                          |chimbarongo.serio|chimbarongo.sergio|graneros.angelica|
                          |graneros.bastian|graneros.claudina|graneros.jorge|
                          |las cabras.fernando|las cabras.matias|litueche.claudia|
                          |mostazal.jose|mostazal.sergio|olivar.eduaedo|olivar.eduardo|
                          olivar.sergio|paredones.jose|paredones.nqldo|paredones.sami|
                          |paredones.sotero|peralillo.don|peralillo.fabian|peralillo.jose|
                          |peralillo.luis|peralillo.rivera|peralillo.soledad|pichidegua.mariegane|
                          |pichidegua.merillan|pichidegua.miguel|placilla.daniela|placilla.marcelo|
                          |placilla.santa|pumanque.emilio|pumanque.jaime|pumanque.sergio|
                          |quinta de tilcoco.ricardo|quinta de tilcoco.roberto|rancagua.hugo|
                          |rancagua.maricarmen|rancagua.patricio enrique|rengo.carlos|rengo.nicolas|
                          |san fernando.carlos|san fernando.miguel|fernando.robert|vicente.daniel|
                          |vicente.pavez|vicente.pilar|vicente.rojas|vicente.sebastian|vicente.victor|
                          |cruz.luis|cruz.moreno|cruz.pina|cruz.roxana', p5$comuna.consejal)),
                   "conoce","no conoce")

table(p5$comuna.consejal,p5$p5_8rec)

##############################      PEGADO        ##############################

p5recod<-p5 %>% select(SbjNum,ends_with("rec"))

baro1<-merge(baro1, p5recod, all.x = T)

########

write_sav(baro1, "ohiggins3.sav")
