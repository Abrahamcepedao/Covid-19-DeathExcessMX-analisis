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

#Decesos data
dataDef <- read.csv("csv/registro_decesos.csv")
dataDef19 <- read.csv("csv/Defunciones_2019.CSV")

#decesos totales por estado
defTotEst <- data.frame(table(dataDef$ENTIDAD_REG))
colnames(defTotEst) <- c("Estado", "Decesos")

#decesos totales por estado en 2019
defTotEst19 <- data.frame(table(dataDef19$ent_regis))
colnames(defTotEst19) <- c("Estado", "Decesos")

#decesos totales por semana
dataDef$SEMANA_NUM <- 1+ as.numeric(as.Date(dataDef$FECHA_DEFUNCION) - as.Date("2020-01-01")) %/% 7
defTotSem  <- data.frame(table(1+ as.numeric(as.Date(dataDef$FECHA_DEFUNCION) - as.Date("2020-01-01")) %/% 7))

#decesos totales por mes
dataDef$MES_NUM <- as.numeric(substr(dataDef$FECHA_DEFUNCION,6,7))
defTotMes <- data.frame(table(dataDef$MES_NUM))


#Create column Fecha Registro
dataDef19$FECHA_DEFUNCION <- paste(dataDef19$anio_ocur, ifelse(dataDef19$mes_ocurr < 10, paste("0", dataDef19$mes_ocurr, sep=""), dataDef19$mes_ocurr), ifelse(dataDef19$dia_ocurr == 99, "10", ifelse(dataDef19$dia_ocurr < 10, paste("0", dataDef19$dia_ocurr, sep=""), dataDef19$dia_ocurr)), sep = "-")
#Create column semana num
#dataDef19$SEMANA_NUM <- 1+ as.numeric(as.Date(dataDef19$FECHA_REGISTRO) - as.Date("2019-01-01")) %/% 7

#Create column mes num
dataDef19$MES_NUM <- as.numeric(substr(dataDef19$FECHA_DEFUNCION,6,7))
                                
#Estados Data
estados <- as.data.frame(read_xlsx("csv/Catalogos_Exceso_Mortalidad_2020.xlsx", sheet = "Estados"))

                                                                
#Meses Data
mesesData <- data.frame(
  Meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre")
)
colnames(mesesData) <- c("Meses")
meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre")


#<----------Aguascalientes - 1 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 1,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 1,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Aguascalientes") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Baja California - 2 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 2,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 2,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Baja California") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Baja California Sur - 3 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 3,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 3,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Baja California Sur") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Campeche - 4 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 4,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 4,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Campeche") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Coahuila - 5 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 5,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 5,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Coahuila") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Colima - 6 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 6,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 6,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Colima") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Chiapas - 7 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 7,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 7,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Chiapas") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Chihuahua - 8 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 8,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 8,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Chihuahua") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------CDMX - 9 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 9,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 9,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en CDMX") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Durango - 10 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 10,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 10,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Durango") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Guanajuato - 11 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 11,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 11,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Guanajuato") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Guerrero - 12 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 12,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 12,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Guerrero") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Hidalgo - 19 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 13,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 13,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Hidalgo") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Jalisco - 14 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 14,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 14,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Jalisco") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------EDOMEX - 15 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 15,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 15,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en EDOMEX") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Michoacán - 16 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 16,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 16,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Michoacán") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Morelos - 17 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 17,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 17,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Morelos") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Nayarit - 18 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 18,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 18,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Nayarit") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Nuevo León - 19 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 19,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 19,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Nuevo León") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Oaxaca - 20 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 20,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 20,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Oaxaca") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Puebla - 21 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 21,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 21,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Puebla") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Querétaro - 22 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 22,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 22,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Querétaro") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Quintana Roo - 23 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 23,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 23,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Quintana Roo") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------San Luis Potosi - 24 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 24,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 24,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en San Luis Potosí") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Sinaloa - 19 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 25,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 25,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Sinaloa") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Sonora - 26 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 26,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 26,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Sonora") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Tabasco - 19 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 27,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 27,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Tabasco") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Tamaulipas - 28 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 28,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 28,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Tamaulipas") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Tlaxcala - 29 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 29,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 29,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Tlaxcala") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Veracruz - 30 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 30,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 30,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Veracruz") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Yucatan - 31 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 31,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 31,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Yucatán") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

#<----------Zacatecas - 32 - mes----------->
EST <- data.frame(dataDef[dataDef$ENTIDAD_REG == 32,])
EST$MES_NUM <- factor(EST$MES_NUM)
EST <- data.frame(EST[order(EST$MES_NUM),])
estado <- data.frame(table(EST$MES_NUM))
colnames(estado) <- c("Mes", "Decesos")
estado <- estado[as.numeric(estado$Mes) < 12,]
estado$Mes <- mesesData$Meses
estado$Mes <- factor(estado$Mes, levels = meses)
estado <-  data.frame(estado[order(estado$Mes),])
estado$Mes <- factor(estado$Mes, levels = rev(estado$Mes))

#get 2019 data
EST19 <- data.frame(dataDef19[dataDef19$ent_regis == 32,])
EST19 <- EST19[EST19$anio_ocur == 2019,]
EST19$MES_NUM <- factor(EST19$MES_NUM)
EST19 <- data.frame(EST19[order(EST19$MES_NUM),])
estado19 <- data.frame(table(EST19$MES_NUM))
colnames(estado19) <- c("Mes", "Decesos")
estado19 <- estado19[as.numeric(estado19$Mes) < 12,]

#get percentages
mesPorExEst <- data.frame(round((as.numeric(estado$Decesos)/as.numeric(estado19$Decesos)*100-100),1))
colnames(mesPorExEst) <- c("Porcentage")

#plot
ggplot(estado, aes(fill=Mes, y=as.numeric(Decesos), x=Mes)) + 
  geom_bar(stat="identity", width=0.7, color="white", fill = getPalette(length(estado$Mes))) +
  geom_text(aes(label = paste(mesPorExEst$Porcentage, "%", sep="")), hjust = -0.1, size=4) +
  xlab("Mes") + ylab("Excedente de decesos") + coord_flip() +
  ggtitle("Excedente de decesos por mes en Zacatecas") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none")

