#LABORATORIO 4
#Cambiar lugar para trabajar
setwd("~/trading/Myst_Lab1_ElimonOchoa")
# Remover todos los objetos del "Environment"
rm(list = ls())
mdir <- getwd()
### Cargas librerias a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teoría Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 
suppressMessages(library(readxl))  #leer documentos de excel
#Paqueteria
# -- Establecer el sistema de medicion de la computadora
Sys.setlocale(category = "LC_ALL", locale = "")

# -- Huso horario
Sys.setenv(tz="America/Mexico_City", TZ="America/Mexico_City")
options(tz="America/Mexico_City", TZ="America/Mexico_City")

# -- Cargar y/o instalar en automatico paquetes a utilizar -- #

pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","quantmod",
         "reshape2","RCurl", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Cargar archivos desde GitHub, paqueteria de FRANCISCOME -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- Parametros para usar API-OANDA

# Tipo de cuenta practice/live, cuenta prueba
OA_At <- "practice"
# ID de cuenta
OA_Ai <- 1742531
# Token para llamadas a API
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
# Hora a la que se considera "Fin del dia"
OA_Da <- 18
# Huso horario ciudad de mexico
OA_Ta <- "America/Mexico_City"
# Instrumento
OA_In <- "USD_MXN"
# Granularidad o periodicidad de los precios H4 = Cada 4 horas
# S5, S10, S30, M1, M5, M15, M30, H1, H4, H8, D, m
OA_Pr <- "M1"#vamos a jalar datos cada minuto
# Convertimos los precios a PIPS
MultPip_MT1 <- 10000

# Esta se obtiene de cada observacion en el archivo xls que descargas. Este fue
# un ejemplo de una fecha que podrias leer de tal archivo una vez que lo descargas


###-----cargar Datos de fxstreet--------------###
#aqui queremos analizar como se ve afectado el tipo de cambio con los reportes de confianza de consumo
fulldata=read.csv('eventdatesUSMX.csv')
# Filter data
indicators=array(unique(fulldata$Name))
filter=fulldata[fulldata$Name==indicators[4],1:7]


# Clasification
a=filter$Actual>=filter$Consensus & filter$Consensus>=filter$Previous
b=filter$Actual>=filter$Consensus & filter$Consensus<filter$Previous
c=filter$Actual<filter$Consensus & filter$Consensus>=filter$Previous
d=filter$Actual<=filter$Consensus & filter$Consensus<filter$Previous

t=data.frame("Name"=filter$Name,"Class"=0)
t$Class[a]=1
t$Class[b]=2
t$Class[c]=3
t$Class[d]=4

data=data.frame(append(filter,t))



window_prices <- list()
metrics=data.frame("Name"=filter$DateTime,"Metric 1"=0,"Metric 2"=0,"Metric 3"=0)

for(i in 1:length(data$DateTime)){
  
  
  Fecha_Ejemplo <- data$DateTime[[i]]
  
  # Opcion 1 para convertir a "YYYY-MM-DD"
  #F1 <- as.Date(strptime(x = Fecha_Ejemplo, format = "%m/%d/%Y %H:%M:%S",tz = "GMT"))
  
  # Opcion 2 para convertir a "YYYY-MM-DD"
  F2 <- as.Date(substr(Fecha_Ejemplo,1 ,10),format = "%m/%d/%Y")
  #F2 <- as.Date(substr(Fecha_Ejemplo,1 ,10),format = "%d/%m/%Y")
  
  # Ejemplo Si el comunicado fue en domingo, que pida precios desde viernes a domingo
  #F3 <- as.Date("2018-09-30")
  
  # Condicion por si acaso toca en domingo el comunicado, no se podria restar un dia y 
  # que terminara siendo sabado, porque ese dia no hay precios.
  if(wday(F2) != 6){
    
    Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                               DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                               Instrument = OA_In, 
                               Start = F2, End = F2+2, Count = NULL)
  } else {
    Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                               DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                               Instrument = OA_In, 
                               Start = F2, End = F2+3, Count = NULL)
  }
  
  
  #POSIXct  Para convertir las fechas del xlsx con historico de indicadores al mismo formato
  # de fecha que entrega Oanda los precios
  fecha <- as.character(as.POSIXct(Fecha_Ejemplo, format = "%m/%d/%Y %H:%M"))
  
  # Para encontrar el registro donde coincide la fecha y hora del comunicado de 
  # indicador y los historicos de precios descargados
  ind <- which(Precios_Oanda$TimeStamp == fecha)
  
  # Para visualizar un data frame con los 31 precios deseados. 15 antes del comunicad, 1 
  # durante el comunicado y 15 después del comunicado
  df <- Precios_Oanda[(ind-15):(ind+15),]
  window_prices[[i]]<-df
  metrics$Metric.1[[i]]<-sd(diff(log(window_prices[[i]]$Close))*MultPip_MT1)
  metrics$Metric.2[[i]]<-window_prices[[i]]$Close[[15]]-window_prices[[i]]$Close[[31]]
  metrics$Metric.3[[i]]<-(min(window_prices[[i]]$Low)-max(window_prices[[i]]$High))*MultPip_MT1
}

data=data.frame(append(data,metrics))

#  Se presentan los resultados en un data frame para que se vean las variables
# la metrica 2 no tuvo exito, no se encontraron valores,la columna de los resultP
results=data.frame("case"=sort(unique(data$Class)),"resultP"=0,"resultN"=0)
results$resultP[[4]]<-count(data$Class==4&data$Metric.2>0)[[2]][2]/count(data$Class==4)[[2]][2]
results$resultP[[3]]<-count(data$Class==3&data$Metric.2>0)[[2]][2]/count(data$Class==3)[[2]][2]
results$resultP[[2]]<-count(data$Class==2&data$Metric.2>0)[[2]][2]/count(data$Class==2)[[2]][2]
results$resultP[[1]]<-count(data$Class==1&data$Metric.2>0)[[2]][2]/count(data$Class==1)[[2]][2]
results$resultN= 1-results$resultP
