###Cargas librerias a utilizar
suppressMessages(library(plotly))#graficas iterativas
suppressMessages(library(Quandl))#Descargar Precios
suppressMessages(library(PortafolioAnalytics))#Teoria Moderna de portafolios
suppressMessages(library(ROI))#Optimizacion de portafolio
suppressMessages(library(kintr))#opciones de documentacion + codigo
options(kintr.table.format="html")

#Cargar el token QUANDL
Quandl.api_key("sBvtp8Rxv1nzggxAnLbo")

#Funcion para bajar precios
Bajar_Precos <-function(Columns,Tickers,Fecha_In,Fecha_Fn) {
  #Funcion para descargar N cantidad de activos desde QUANDL
  #--Dependencias:QUANDL
  #--Columns:columnas a incluir:c"date","adj_close")
  #--Tickers:subyacentes a revisar: c("TSLA","BBY","HD")
  Datos<-Quandl.datatable(code="WIKI/PRICES",qopts.columns=Columns,ticker=Tickers,
                          date.gte=Fecha_In,date.lte=Fecha_Fn)
  return(Datos)
}

#Tickers de acciones y datos a solicitar a QANDL
tk<-c("TSLA","BBY","HD")
CS<-c("date","adj_close")

