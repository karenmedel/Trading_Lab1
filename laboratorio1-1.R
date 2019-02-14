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
########################################### Graficas


Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                            name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                            text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                          '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
  layout(title = "Portafolios (Markowitz)",
         xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                      showgrid = F),
         yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
         legend = list(orientation = 'h', y = -0.25))
Plot_portafolios

Port_1 <- df_Portafolios[which.max(df_Portafolios$Rend),]

# Portafolio con m?nima varianza
Port_2 <- df_Portafolios[which.min(df_Portafolios$Var),]

# Tasa libre de riesgo
rf <- 0.0025          
# Rendimiento de portafolio
rp <- df_Portafolios$Rend
# Varianza de portafolio
sp <- df_Portafolios$Var
# Indice de sharpe
sharpe <- (rp-rf)/sp

# Portafolio con m?ximo Sharpe ratio 
Port_3 <- df_Portafolios[which.max(sharpe),]

Ports <- cbind(rbind(Port_1, Port_2, Port_3),
               "Portafolio" = c("M?ximo Rendimiento","M?nima Varianza","M?ximo Sharpe Ratio"))

Plot_portafolios <- plot_ly(x=df_Portafolios$Var, y=df_Portafolios$Rend, type='scatter', mode='markers',
                            name = "Portafolios", marker = list(color="grey", size=7), hoverinfo='text', 
                            text = ~paste('Rendimiento: ', paste(df_Portafolios$Rend*100, "%") ,
                                          '<br> Riesgo: ', paste(df_Portafolios$Var*100, "%") )) %>% 
  layout(title = "Portafolios (Markowitz)",
         xaxis = list(title = "Riesgo (Desviaci?n Est?ndar Anualizada)",
                      showgrid = F),
         yaxis = list(title = "Valor Esperado (Rendimiento Anualizado)"),
         legend = list(orientation = 'h', y = -0.25)) %>%
  add_trace(x = ~Ports$Var[1], y = ~Ports$Rend[1], name = Ports$Portafolio[1],
            mode = 'marker', marker = list(color="red", size=10)) %>%
  add_trace(x = ~Ports$Var[2], y = ~Ports$Rend[2], name = Ports$Portafolio[2],
            mode = 'marker', marker = list(color="blue", size=10)) %>%
  add_trace(x = ~Ports$Var[3], y = ~Ports$Rend[3], name = Ports$Portafolio[3],
            mode = 'marker', marker = list(color="orange", size=10))
Plot_portafolios

# Pesos y titulos iniciales, de todos los activos, para los 3 portafolios
Pesos_Titulos <- Ports[,-c(1,2,3)]

# Encontrar las columnas cuyo nombre contenga "Titulos_ini", con esas encontraremos m?s f?cil los t?tulos
# por portafolio por activo
Ind <- grep(pattern = "Titulos_ini",x = colnames(Pesos_Titulos))
Historicos_Ports <- data.frame("Date" = Datos[[1]]$date)

# Crear data frame que contendr? los datos finales de cada estrategia
for(i in 1:length(Ports[,1])) {
  Historicos_Ports[[paste("Portafolio_",i,sep="")]] <- 
    (Datos[[1]]$adj_close*Pesos_Titulos[i,Ind[1]]  + 
       Datos[[2]]$adj_close*Pesos_Titulos[i,Ind[2]] +
       Datos[[3]]$adj_close*Pesos_Titulos[i,Ind[3]])
}


plot_ly(Historicos_Ports) %>%
  add_trace(x = ~Date, y = ~round(Portafolio_1,2), type = 'scatter', mode = 'lines', name = 'M?ximo Rendimiento',
            line = list(color = 'red'), hoverinfo = "text", text = ~paste('Port_1',round(Portafolio_1,2))) %>%
  add_trace(x = ~Date, y = ~round(Portafolio_2,2), type = 'scatter', mode = 'lines', name = 'M?nima Varianza',
            line = list(color = 'blue'), hoverinfo = "text", text = ~paste('Port_2',round(Portafolio_2,2)))  %>%
  add_trace(x = ~Date, y = ~round(Portafolio_3,2), type = 'scatter', mode = 'lines', name = 'M?ximo Sharpe Ratio',
            line = list(color = 'orange'), hoverinfo = "text", text = ~paste('Port_3',round(Portafolio_3,2)))%>% 
  layout(title = "3 Portafolios distintos objetivos",
         xaxis = list(title = "Fechas", showgrid = T),
         yaxis = list(title = "Balance"), 
         legend = list(orientation = 'h', y = -0.25, x = 0.5))

#Los 3 activos utilizados para este portafolio son Bank of America, Pfizer y Microsoft; activos de distintos sectores. 
#Se tomaron los precios del 1/08/15 al 1/08/17 con un capital inicial de $10,000.
#El tamaño de la simulación fue de 5000 de los cuales se obtuvieron 430 posibles portafolios con:
#rendimiento, varianza, pesos y numero de titulos de cada accion.
#Se eligieron los 3 mejores portafolios en base a max. rendimiento, min. varianza y max. Sharpe
#Maximo rendimiento: Pesos: BAC:32.6%, PFE:1.4%, MSFT:66% 
#Minima varianza: Pesos: BAC:5.4%, PFE:60.8%, MSFT:33.8% 
#Maximo Sharpe: Pesos: BAC:24.6%, PFE:5.4%, MSFT:70% 
#Dependiendo del objetivo que se busca en relación rendimiento-riesgo los pesos de los activos en cada portafolio son diferentes.
#En 2 de los 3 escenarios PFE tiene muy poco % por lo que puede ser recomendable probar con otro activo.
#La decision de cual portafolio utilizar depende del perfil del inversionista y lo que este buscando.
