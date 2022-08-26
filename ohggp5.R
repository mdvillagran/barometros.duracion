
library(haven)
library(dplyr)
library(plyr)
library(openxlsx)
library(stringr)
library(stringi)


rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/Barometros/informes de avance/informes.cierre")



baro1<-read_sav("ohiggins2.sav")


# Filtrado por región

baro<-subset(baro1, region==6)

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


p5$p5_1rec<-ifelse((grepl('pablo silva|silva|amaya|macaya|silva amaya',p5$p5_1)),
                   "conoce","no conoce")

table(p5$p5_1,p5$p5_1rec)

p5$p5_2rec<-ifelse((grepl('loreto vallejos|alvin saldana|saldana|carol bown|bown|
                          |matias orellana|orellana|damaris abarca|abarca|ricardo neumann|neuman|
                         |nicolas nunez|nunez ganga|nunez|gangas|
                          |gloria alvarado|alvarado|
                          |adriana cancino|cancino',
                          p5$p5_2)),
                   "conoce","no conoce")

table(p5$p5_2,p5$p5_2rec)


p5$p5_3rec<-ifelse((grepl('fabio|flavio',
                          p5$p5_3)),
                   "conoce","no conoce")

table(p5$p5_3,p5$p5_3rec)



p5$p5_5rec<-ifelse((grepl('alejandra|sepulveda|castro|macaya|macaya|
                          |shalper|scalper|scharper|sharper|raul|soto|udi',
                          p5$p5_5)),
                   "conoce","no conoce")

table(p5$p5_5,p5$p5_5rec)

p5$p5_6rec<-ifelse((grepl('letelier|alejandro garcia huidobro|garcia huidobro|huidobro',
                          p5$p5_6)),
                   "conoce","no conoce")

table(p5$p5_6,p5$p5_6rec)

p5$p5_7rec<-ifelse((grepl('mujer|contreras|marchant|toro|gerardo|gonzalez|
                          |jorquera|ethit|llamil|silva|pablo diaz|paula munoz|
                          |rubia|monroy|yamil',
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
