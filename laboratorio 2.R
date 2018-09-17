#LABORATORIO 2

# Remover todos los objetos del "Environment"
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
library(Quandl) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teor?a Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio

suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
#suppressMessages(library(xlsx))
suppressMessages(library(openxlsx))

options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("dN9QssXxzTxndaqKUQ_i")


# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)}


#Tickers de acciones en ETF-IAK
#IAK <- read_excel("trading/Myst_Lab1_ElimonOchoa/IAK.xlsx")
# tk<-as.data.frame(read.xlsx(file="IAK.xlsx", sheetName="Holdings",
#                             colIndex=1,sartRow=10,endRow=73,
#                             header=FALSE))
#IAK<- read.xlsx("IAK.xlsx", sheet = 1)
Datos_ETF<- read.xlsx("IAK.xlsx" , sheet=1)
tk<- as.character(na.omit(Datos_ETF[which(Datos_ETF[,1]== "Ticker")+1:length(Datos_ETF[,1]),1]))
cs<- c("date", "adj_close")


# Fecha inicial y fecha final
fs <- c("2015-08-01", "2017-08-01")

# Descargar Precios
Datos <- list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

longitudes<-c()

for (i in 1: length(Datos)){
  longitudes[[i]] <- length(Datos[[i]]$date)
}

maximo <- max(longitudes)
completos <- which(longitudes== maximo)
DatosN <- Datos[completos]

#vector para almacenar columnas
columnas<- c()
nuevos <- c()

#funcion para repetir una funcion por cada columna del data.frame
Precios<- do.call(cbind, DatosN)

#crear vector con nombres de columnas de interes 
for (i in 1:length(tk)){
  nuevos[i]<- paste(tk[i], ".adj_close", sep="")
}

#extraer 1 renglon para obtener para obtener los nombres de las columnas
nombres <- colnames(Precios[1, (names(Precios) %in% nuevos)])

# Elejir una columna date y las demas columnas de rendimientos
Precios <- Precios[,(names(Precios) %in% nuevos)]
row.names(Precios) <- DatosN[[1]]$date

# Reasignar nombres al data.frame
tk_completos <- as.character(tk[completos])
colnames(Precios) <- tk_completos 


Historico <- data.frame("Date"= row.names(Precios),
                        "Precio"= Precios[,1],
                        "R_Precio" = 0,
                        "R_Activo" = 0,
                        "R_Cuenta" = 0,
                        "Capital" = 0, "Balance" = 0, "Titulos" = 0,
                        "Titulos_a" = 0,
                        "Operacion" = NA, "Comisiones"= 0, "Mensaje" = NA)

#Date       : Fecha (proviene desde los precios que bajaron)
#Precio     : Precio individual del activo
#R_Precio   : Rendimiento diario del precio (dia a dia)
#R_Activo   : Rendimiento acumulado del precio (Cada dia respecto al precio inicial)
#Capital    : El dinero no invertido (equivalente a efectivo)
#Balance    : El valor del portafolio (precio diario X titulos)
#R_Cuenta   : Balance + Capital (cada dia respecto al capital inicial)
#Titulos    : Acciones que se tienen
#Titulos_a  : Titulos acumulados
#Operacion  : Indicativo de compra (1), mantener (0), venta (-1)
#Comisiones : 0.0025 o 0.25% por el valor de la transaccion
#Mensaje    : Un texto que indique alguna decision o indicativo de que ocurrio algo

Regla0_R <- -0.03  # Considerar una oportunidad de compra en un rendimiento de -3% o menor
Regla1_I <- 0.20   # % de capital para comprar titulos para posicion inicial
Regla2_P <- 0.25   # Se utiliza el P% del L capital restante en cada compra
Regla3_W <- tk_completos # Se realiza la misma estrategia para todos los activos en el portafolio
Regla4_C <- 0.0025 # Comisiones pagadas por compra
Regla5_K <- 100000 # Capital inicial
