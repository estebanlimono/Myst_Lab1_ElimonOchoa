###Cargas librerias a utilizar
#install.packages('knitr', dependencies = TRUE)
suppressMessages(library(plotly))#graficas iterativas
suppressMessages(library(Quandl))#Descargar Precios
suppressMessages(library(PortafolioAnalytics))#Teoria Moderna de portafolios
suppressMessages(library(ROI))#Optimizacion de portafolio
suppressMessages(library(kintr))#opciones de documentacion + codigo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(kintr.table.format="html")

#Cargar el token QUANDL
Quandl.api_key("XrMP9jQysz1Z_6DdWgfi")

#Funcion para bajar precios
Bajar_Precos <-function(Columns,Tickers,Fecha_In,Fecha_Fn) {
  #Funcion para descargar N cantidad de activos desde QUANDL
  #--Dependencias:QUANDL
  #--Columns:columnas a incluir:c"date","adj_close")
  #--Tickers:subyacentes a revisar: c("TSLA","BBY","HD")
  Datos<-Quandl.datatable("WIKI/PRICES",qopts.columns=Columns,ticker=Tickers,
                          date.gte=Fecha_In,date.lte=Fecha_Fn)
  return(Datos)
}

#Tickers de acciones y datos a solicitar a QANDL
tk<-c("TSLA","BBY","HD")
CS<-c("date","adj_close")

#Fechas inicial y final
fs<- c("2016-08-01","2018-08-01")

#capital Inicial a considerar
#Capital Inicial<- 100000
#Comision
#Descargar Precios y Calcular rendimientos
Datos<-list()

for(i in 1:length(tk))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk
#los resultados se ven como xts
#datos[3 es donde se ponen la cantidad de columnas]
