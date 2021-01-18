rm(list=ls())

library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(stringr)
library(RColorBrewer)
setwd("~/Documents/OnCampusJob/analisis-decesos-covid-19")

#Color palette  config
getPalette = colorRampPalette(brewer.pal(9, "Blues"))

dataDef <- read.csv("csv/registro_decesos.csv")
dataDef19 <- read.csv("csv/Defunciones_2019.CSV")

#decesos totales por estado
defTotEst <- data.frame(table(dataDef$ENTIDAD_REG))
colnames(defTotEst) <- c("Estado", "Decesos")

#decesos totales por estado en 2019
defTotEst19 <- data.frame(table(dataDef19$ent_regis))
colnames(defTotEst19) <- c("Estado", "Decesos")

#decesos totales por semana
dataDef$SEMANA_NUM <- 1+ as.numeric(as.Date(dataDef$FECHA_REGISTRO) - as.Date("2020-01-01")) %/% 7
defTotSem  <- data.frame(table(1+ as.numeric(as.Date(dataDef$FECHA_REGISTRO) - as.Date("2020-01-01")) %/% 7))

#decesos totales por mes
dataDef$MES_NUM <- as.numeric(substr(dataDef$FECHA_REGISTRO,6,7))
defTotMes <- data.frame(table(dataDef$MES_NUM))


#Create column Fecha Registro
dataDef19$FECHA_REGISTRO <- paste(dataDef19$anio_regis, ifelse(dataDef19$mes_regis < 10, paste("0", dataDef19$mes_regis, sep=""), dataDef19$mes_regis), sep = "-")
for(i in 1:length(dataDef19$anio_regis)){
  l <- i
  while(dataDef19$dia_regis[l] == 99){
    l <- l - 1
    print("subtracting..")
    print(l)
  }
  dataDef19$FECHA_REGISTRO[i] <- paste(dataDef19$FECHA_REGISTRO[i], ifelse(dataDef19$dia_regis[l] < 10, paste("0", dataDef19$dia_regis[l], sep=""), dataDef19$dia_regis[l]), sep="-")
}

#Create column semana num
dataDef19$SEMANA_NUM <- 1+ as.numeric(as.Date(dataDef19$FECHA_REGISTRO) - as.Date("2019-01-01")) %/% 7

#Create column mes num
dataDef19$MES_NUM <- as.numeric(substr(dataDef19$FECHA_REGISTRO,6,7))



#aguascalientes
AS <- data.frame(dataDef[dataDef$ENTIDAD_REG == 1,])
AS$SEMANA_NUM <- factor(AS$SEMANA_NUM)
AS <- data.frame(AS[order(AS$SEMANA_NUM),])

aguascalientes <- data.frame(table(AS$SEMANA_NUM))
colnames(aguascalientes) <- c("Semana", "Decesos")
aguascalientes <- aguascalientes[as.numeric(aguascalientes$Semana) < 48,]

AS19 <- data.frame(dataDef19[dataDef19$ent_regis == 1,])
AS19$SEMANA_NUM <- factor(AS19$SEMANA_NUM)
AS19 <- data.frame(AS[order(AS19$SEMANA_NUM),])
aguascalientes19 <- data.frame(table(AS19$SEMANA_NUM))
colnames(aguascalientes19) <- c("Semana", "Decesos")
aguascalientes19 <- aguascalientes19[as.numeric(aguascalientes19$Semana) < 48,]


#Nuevo León - 19 - semana
NL <- data.frame(dataDef[dataDef$ENTIDAD_REG == 19,])
NL$SEMANA_NUM <- factor(NL$SEMANA_NUM)
NL <- data.frame(NL[order(NL$SEMANA_NUM),])

nuevoLeon <- data.frame(table(NL$SEMANA_NUM))
colnames(nuevoLeon) <- c("Semana", "Decesos")
nuevoLeon <- nuevoLeon[as.numeric(nuevoLeon$Semana) < 48,]

NL19 <- data.frame(dataDef19[dataDef19$ent_regis == 19,])
NL19$SEMANA_NUM <- factor(NL19$SEMANA_NUM)
NL19 <- data.frame(AS[order(NL19$SEMANA_NUM),])
nuevoLeon19 <- data.frame(table(NL19$SEMANA_NUM))
colnames(nuevoLeon19) <- c("Semana", "Decesos")
nuevoLeon19 <- nuevoLeon19[as.numeric(nuevoLeon19$Semana) < 48,]

porExNL <- data.frame(round((as.numeric(nuevoLeon$Decesos)/as.numeric(nuevoLeon19$Decesos)*100-100),1))
colnames(porExNL) <- c("Porcentage")

nuevoLeon$Semana <- factor(nuevoLeon$Semana, levels = rev(nuevoLeon$Semana))

ggplot(nuevoLeon, aes(fill=Semana, y=as.numeric(Decesos), x=Semana)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(nuevoLeon$Semana))) +
  geom_text(aes(label = paste(porExNL$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Número de semana") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por semana en Nuevo León") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#Nuevo León - 19 - mes
MesNL <- data.frame(dataDef[dataDef$ENTIDAD_REG == 19,])
MesNL$MES_NUM <- factor(MesNL$MES_NUM)
MesNL <- data.frame(MesNL[order(MesNL$MES_NUM),])

mesNuevoLeon <- data.frame(table(MesNL$MES_NUM))
colnames(mesNuevoLeon) <- c("Mes", "Decesos")
mesNuevoLeon <- mesNuevoLeon[as.numeric(mesNuevoLeon$Mes) < 12,]

MesNL19 <- data.frame(dataDef19[dataDef19$ent_regis == 19,])
MesNL19$MES_NUM <- factor(MesNL19$MES_NUM)
MesNL19 <- data.frame(MesNL19[order(MesNL19$MES_NUM),])
mesNuevoLeon19 <- data.frame(table(MesNL19$MES_NUM))
colnames(mesNuevoLeon19) <- c("Mes", "Decesos")
mesNuevoLeon19 <- mesNuevoLeon19[as.numeric(mesNuevoLeon19$Mes) < 12,]

mesPorExNL <- data.frame(round((as.numeric(mesNuevoLeon$Decesos)/as.numeric(mesNuevoLeon19$Decesos)*100-100),1))
colnames(mesPorExNL) <- c("Porcentage")

mesNuevoLeon$Mes <- factor(mesNuevoLeon$Mes, levels = rev(mesNuevoLeon$Mes))

ggplot(mesNuevoLeon, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(mesNuevoLeon$Mes))) +
  geom_text(aes(label = paste(mesPorExNL$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Número de mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Nuevo León") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")









