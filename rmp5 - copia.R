
library(haven)
library(dplyr)
library(plyr)
library(openxlsx)
library(stringr)
library(stringi)


rm(list = ls())
setwd("C:/Users/Villagran/Desktop/datavoz/Barometros/informes de avance/informes.cierre")



baro1<-read_sav("bbdd.5.9.sav")


# Filtrado por región

baro<-subset(baro1, region==13)

baro<- baro %>% filter(Status %in% c("Approved", "Requires Approval"))

baro <- baro[!is.na(baro$S1_sexo),]

baro<-baro %>% filter(!ump %in% 999) %>% filter(!SbjNum %in% 171058653)

table(baro$comuna)


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

p5$p5_1rec<-ifelse((grepl('claudio|oorrego|oreego|oregano|urreg|urrego|
                          |orego|orreg|orrego|orregol|orrrgo',p5$p5_1)),
                   "conoce","no conoce")

table(p5$p5_1,p5$p5_1rec)


# Constituyente

p5$p5_2rec<-ifelse((grepl('achurra|astingo|atria|baradi|baradit|barbara|barrasa,|fonteine|
                          |hube|monckeberg|cubillos|daniel|estingo|atrias|manuchapinto|
                          |larrain|achurra|la masa|picachu|malucha|barraza|marino|
                          |marinovic|marinovich|paradidt|stin|stinger|stingo|tingo|
                          |valentina',
                          p5$p5_2)),
                   "conoce","no conoce")

table(p5$p5_2,p5$p5_2rec)


# Delegado presidencial

p5$p5_3rec<-ifelse((grepl('constanza|martinez',
                          p5$p5_3)),
                   "conoce","no conoce")

table(p5$p5_3,p5$p5_3rec)


# Diputado

p5$p5_5rec<-ifelse((grepl('heggs|hertz|carol|real|labbe|melo|la carrera|
                          |cordero|carreea|hitch|jiles|kaiser|cariola|leiva|
                          |leonardo|santibanez|marisela|moreira|giles|pamela.jiles|
                          |placencia|leiva|undurrega|undurraga|winter',
                          p5$p5_5)),
                   "conoce","no conoce")

table(p5$p5_5,p5$p5_5rec)


# Senador

p5$p5_6rec<-ifelse((grepl('campillai|pascual|coke|fabiorla|ossandon|osandon|
                          |ossandom|ossandon|pascual|rojo',
                          p5$p5_6)),
                   "conoce","no conoce")

table(p5$p5_6,p5$p5_6rec)


# Consejero regional

p5$p5_7rec<-ifelse((grepl('celin|pardo',
                          p5$p5_7)),
                   "conoce","no conoce")

table(p5$p5_7,p5$p5_7rec)

################################################################################
                 ################ alcalde y consejales ############
################################################################################


# Alcalde

p5$comuna.alcalde<-paste0(p5$comuna,".",p5$p5_4)

table(p5$comuna.alcalde)



p5$p5_4rec<-ifelse((grepl('buin.jose|buin.miguel|navia.mauro|navia.tamaro|navia.tamayo|
                          |valensuela|colina.isabel|colina.isabela|colina.isabella|
                          |conchali.de|conchali.rene|bosque.manuel|bosque.zuniga|central.felipe|
                          |independencia.duran|independencia.gonzalo|cisterna.joel|florida.carter|
                          |florida.rodolfo|reina.antonio|reina.jose|condes.daniela|
                          |condes.mujer penaloza|condes.penaloza|condes.penalozs|
                          |barnechea.cristobal|barnechea.lira|espejo.javiera|prado.maximiliano|
                          |macul.montoya|maipu.bodanocic|maipu.bodanovic|maipu.bodanovich|maipu.tomas|
                          |maipu.vodanivic|maipu.vodanocic|maipu.vodanovich|melipilla.lorena|
                          |melipilla.olavarria|nunoa.emilia|nunoa.rios|paine.contreras|paine.rodrigo|
                          |cerda.luis|penaflor.nibaldo|penaflor.nivaldo|penalolen.carolina|
                          |penalolen.leitao|providencia.evelin|providencia.matei|providencia.matey|
                          |pudahuel.italo|alto.german|alto.godina|alto.jaime|quilicura.paulina|normal.karina|
                          |recoleta.daniel|recoleta.haduel|recoleta.jadiel|recoleta.jadue|recoleta.jaduel
                          |renca.claudio|bernardo.christopher|bernardo.cristofer|bernardo.white|
                          |joaquin.labra|joaquin.labrq|miguel.erica|ramon.gustavovtoro|ramon.toro|
                          |santiago.asler|santiago.hassler|santiago.irani|santiago.irasi|santiago.irasy|
                          |santiago.iriashy|santiago.iriasy|santiago.isiarhy|santiago.isibet|santiago.irassi|
                          |talagante.carlos|vitacura.merino|vitacura.merida',
                          p5$comuna.alcalde)),
                   "conoce","no conoce")

table(p5$comuna.alcalde,p5$p5_4rec)


# Concejal

p5$comuna.consejal<-paste0(p5$comuna,".",p5$p5_8)

table(p5$comuna.consejal)



p5$p5_8rec<-ifelse((grepl('buin.marcelo|navia.yudith|bosque.claudin|
                          |florida.seves|macul.margarita|penaflor.hernan|
                          |joaquin.lulo|joaquin.ortis',
                          p5$comuna.consejal)),
                   "conoce","no conoce")

table(p5$comuna.consejal,p5$p5_8rec)

##############################      PEGADO        ##############################

p5recod<-p5 %>% select(SbjNum,ends_with("rec"))

baro<-merge(baro, p5recod, all.x = T)
# 
# p5$comuna.alcalde<-NULL
# p5$comuna.consejal<-NULL

########

write_sav(baro, "RMETROPOLITANA.p5.sav")
