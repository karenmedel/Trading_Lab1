### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teor?a Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio

suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("Us7sh5qQJQtwrPCA3rc1")

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Funcion para descargar N cantidad de activos desde QUANDL
  # -- Dependencias: QUANDL
  # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
  # -- Tickers : Tickers o claves de pizarra de los activos : character : "TSLA"
  # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
  # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}

funcion_pesos<-function() {
  data_iak<- read.csv("ITA_holdings-3.csv")
  primero<-which(x = data_iak[,1]=="Ticker")
  ultimo<-length(data_iak[,1])
  tk <- data_iak[(primero+1):ultimo,1]
  
  pesos <-data_iak[(primero+1):ultimo,4]
  
  historico_iak(i)<-('nombre') <-list("tickers"=tk,"pesos"=pesos)
  
  return()
}
archivos <- c()
pesos <- list()
# Tickers de accciones y datos a solicitar a QUANDL

cs <- c("date", "adj_close")
# Fecha inicial y fecha final
fs <- c("2017-01-31", "2018-01-01")

for (i in 1:10){
 archivos[i] <- paste("iShares-US-Insurance-ETF_fund_",i,".csv",sep="")
}



# Descargar Precios y Calcular rendimientos
Datos <- list()


for(i in 1:length(50)) #va tk en lugar de 50, pero no lee tk
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tk

for(i in 1:length(tk))
  Datos[[i]]$adj_close_r <- c(0, diff(log(Datos[[i]]$adj_close)))

Rends <- xts(x = cbind(Datos[[1]]$adj_close_r, Datos[[2]]$adj_close_r, Datos[[3]]$adj_close_r),
             order.by = Datos[[1]]$date)[-1]
names(Rends) <- tk

Port1 <- portfolio.spec(assets=tk)
Port1 <- add.constraint(portfolio=Port1,
                        type="full_investment")