## Mini taller de manipulación y análisis de datos
## José A. Lastra
## 08.08.2020
#########################################################
## configurar el directorio de tabajo
setwd("/cloud/project")
#########################################################
## librerías
library(tidyverse)
library(readxl)
library(TTR)
library(tidyquant)
library(scales)
library(npphen)
#########################################################
##Inicio de análisis##
CTD <- read_excel("Araucania2018_CTD_C1.xls")

## Seleccionando datos
##Columnas
## Forma 1
CTD.sub <- as.data.frame(CTD[,c(6:9)])
head(CTD.sub)

##Forma 2
columnas <- colnames(CTD)[6:9]
CTD.sub <- as.data.frame(CTD[,columnas])
head(CTD.sub)

##Forma 3
CTD.sub <- CTD %>% dplyr::select(`Temperatura (°C)`, `Sigma T (Kg/m3)`, `Salinidad (UPS)`,`Oxigeno (ml/L)`)
head(CTD.sub)

## Filas
##seleccionando 50 registros
CTD_50 <- CTD[1:50,]

##seleccionando registros según condiciones
##seleccionando Nehuentué 2 millas

CTD_nehuentue2 <- filter(CTD, Estaciones == 'Nehuentué2')

##este código es lo mismo, pero usa tidyverse
CTD_nehuentue2 <- CTD %>% filter(Estaciones=='Nehuentué2')

## si quiero más de una estación
##seleccionando registros 
estaciones <- unique(CTD$Estaciones)[1:3]

#seleccionando estaciones
CTD_nehuentue <- filter(CTD, Estaciones %in% estaciones)

#esto es lo mismo
CTD_nehuentue <- CTD %>% filter(Estaciones %in% estaciones) 

##Jugando con el laboratorio 2
## Correlación
#leyendo los datos
cimar<-read_xls('cimarfiordos20.xls')

## seleccionando estación de interés
estacion <- cimar %>% filter(ESTACIÓN == 2) %>% as.data.frame()
summary(estacion)
head(estacion)

##
#grafiquemos la información
plot(x = estacion$`TEMPERATURA (°C)`,y = estacion$`PROFUNDIDAD (m)`*-1, type='l',
     xlab='Temp (°C)', ylab='Depth (m)')

#columnas de interés
estacion.cor <- estacion[,c(9:11)] #creando matriz de columnas de interés para correlación

#matriz numérica
est.num <- cor(estacion.cor)

#calculando significancia
p <- cor.mtest (est.num, conf.level = .95)

#graficando correlaciones significativas
corrplot(est.num, method ="number", type="upper",sig.level = 0.05,
         p.mat =p$p,insig = "blank")#correlation matrix

#cambiando estación
#estación interior
estacion12 <- cimar %>% filter(ESTACIÓN == 12) %>% as.data.frame()

#seleccionando columna de esterés
estacion.cor<-estacion12[,c(9:11)]
#matriz numérica
est.num <- cor(estacion.cor)
#calculando significancia
p <- cor.mtest (est.num, conf.level = .95)
#graficando correlaciones significativas
corrplot(est.num,method ="number", type="upper",sig.level = 0.05,
         p.mat =p$p,insig = "blank")#correlation matrix

###########################################################################
## Series de tiempo con R
ts.data<-read.csv('Iquique_TS.csv')

#graficar nuestra serie de tiempo
ggplot(data = ts.data, aes(x = as.Date(fechas), y= sst)) + geom_line(lwd=0.5) +  
  xlab('') + ylab('SST (°C)') + theme_bw()

### npphen ###
#Data prep
fechas <- as.Date(ts.data$fechas, format("%Y-%m-%d"))
ts.data$fechas <- fechas

#Fenología
PhenKplot(x = ts.data$sst, dates = ts.data$fechas,h = 2,
          nGS = 365, rge = c(12.5,28),xlab = '',ylab = 'SST (°C)')

## Descomponen la serie
#creando objeto time-series
ts.iquique <- ts(ts.data[,3],start = c(1981,9),frequency = 365)

#descomponiendo y graficando
plot(decompose(ts.iquique))

ts.iquique %>% decompose() %>% plot()

## ir un poco más allá

#objeto con descomposición
ts.decompose <- ts.iquique %>% decompose()

#creando minitablas para graficar
serie <- data.frame(sst=ts.decompose$x, categoria='Serie original', fechas=fechas)

tendencia <- data.frame(sst=ts.decompose$trend, categoria='tendencia', fechas=fechas)

#peguemos los datos
serie.ts <- list(serie, tendencia) %>% reduce(bind_rows)

#graficando serie y tendencia
ggplot(data = serie.ts,aes(x=fechas, y= sst, color=categoria)) + 
  geom_line() + xlab('') + ylab('SST (°C)') + 
  scale_x_date(labels=date_format("%Y"), breaks=date_breaks(width="2 year"), 
               limits=c(as.Date('1981-8-1'), as.Date('2019-4-30'))) + theme_bw()

