#Laboratorio 1

# Remover todos los objetos del "Environment"
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teor?a Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio
suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
suppressMessages(library(g.data))# Para leer archivos
options(knitr.table.format = "html") 

#Importacion de datos para un archivo csv primer mes
datos_iak1 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-1.csv",header= TRUE, sep=",")
primero <- which(x = datos_iak1[,1] == "Ticker")
ultimo <- length(datos_iak1[,1])

tk1 = datos_iak1[(primero+1):ultimo,1]
pesos = datos_iak1[(primero+1):ultimo,4]
pesos1 = as.numeric(pesos)
fecha1 = datos_iak1[2,2]
precios1 = datos_iak1[(primero+1):ultimo,5]
precios1 = as.numeric(precios1)
historico_iak1 = list("tickers" = tk1, "pesos" = pesos1, "precios"=precios1)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport1 <- (pesos1*precios1)

#Importacion de datos para un archivo csv segundo
datos_iak2 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-2.csv",header= TRUE, sep=",")
segundo <- which(x = datos_iak2[,1] == "Ticker")
ultimo2 <- length(datos_iak2[,1])

tk2 <- datos_iak2[(segundo+1):ultimo2,1]
peso2 <- datos_iak2[(segundo+1):ultimo2,4]
pesos2 <- as.numeric(peso2)
fecha2 <- datos_iak2[2,2]
precios2 <- datos_iak2[(segundo+1):ultimo2,5]
precios2 <- as.numeric(precios2)
historico_iak2 <- list("tickers" = tk2, "pesos" = pesos2, "precios"=precios2)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport2 <- (pesos2*precios2)

#Importacion de datos para un archivo csv tercero
datos_iak3 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-3.csv",header= TRUE, sep=",")
tercero <- which(x = datos_iak3[,1] == "Ticker")
ultimo3 <- length(datos_iak3[,1])

tk3 <- datos_iak3[(tercero+1):ultimo3,1]
peso3 <- datos_iak3[(tercero+1):ultimo3,4]
pesos3 <- as.numeric(peso3)
fecha3 <- datos_iak3[2,2]
precios3 <- datos_iak3[(primero+1):ultimo,5]
precios3 <- as.numeric(precios3)
historico_iak3 <- list("tickers" = tk3, "pesos" = pesos3, "precios"=precios3)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport3 <- (pesos3*precios3)

#Importacion de datos para un archivo csv cuarto
datos_iak4 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-4.csv",header= TRUE, sep=",")
cuarto <- which(x = datos_iak4[,1] == "Ticker")
ultimo4 <- length(datos_iak4[,1])

tk4 <- datos_iak4[(cuarto+1):ultimo4,1]
peso4 <- datos_iak4[(cuarto+1):ultimo4,4]
pesos4 <- as.numeric(peso4)
fecha4 <- datos_iak4[2,2]
precios4 <- datos_iak4[(cuarto+1):ultimo4,5]
precios4 <- as.numeric(precios4)
historico_iak4 <- list("tickers" = tk4, "pesos" = pesos4, "precios"=precios4)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport4 <- (pesos4*precios4)


#Importacion de datos para un archivo csv quinto
datos_iak5 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-5.csv",header= TRUE, sep=",")
quinto <- which(x = datos_iak5[,1] == "Ticker")
ultimo5 <- length(datos_iak5[,1])

tk5 <- datos_iak5[(quinto+1):ultimo5,1]
peso5 <- datos_iak5[(quinto+1):ultimo5,4]
pesos5 <- as.numeric(peso5)
fecha5 <- datos_iak5[2,2]
precios5 <- datos_iak5[(quinto+1):ultimo,5]
precios5 <- as.numeric(precios5)
historico_iak5 <- list("tickers" = tk5, "pesos" = pesos5, "precios"=precios5)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport5 <- (pesos5*precios5)


#Importacion de datos para un archivo csv sexto
datos_iak6 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-6.csv",header= TRUE, sep=",")
sexto <- which(x = datos_iak6[,1] == "Ticker")
ultimo6 <- length(datos_iak6[,1])

tk6 <- datos_iak6[(sexto+1):ultimo6,1]
peso6 <- datos_iak[(sexto+1):ultimo6,4]
pesos6 <- as.numeric(pesos6)
fecha6 <- datos_iak6[2,2]
precios6 <- datos_iak6[(sexto+1):ultimo6,5]
precios6 <- as.numeric(precios6)
historico_iak6 <- list("tickers" = tk6, "pesos" = pesos6, "precios"=precios6)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport6 <- (pesos6*precios6)

#Importacion de datos para un archivo csv séptimo
datos_iak7 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-7.csv",header= TRUE, sep=",")
septimo <- which(x = datos_iak7[,1] == "Ticker")
ultimo7 <- length(datos_iak7[,1])

tk7 <- datos_iak7[(septimo+1):ultimo7,1]
peso7 <- datos_iak7[(septimo+1):ultimo7,4]
pesos7 <- as.numeric(peso7)
fecha <- datos_iak[2,2]
precios7 <- datos_iak[(septimo+1):ultimo7,5]
precios7 <- as.numeric(precios7)
historico_iak7 <- list("tickers" = tk7, "pesos" = pesos7, "precios"=precios7)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport7 <- (pesos7*precios7)


#Importacion de datos para un archivo csv octavo
datos_iak8 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-8.csv",header= TRUE, sep=",")
octavo <- which(x = datos_iak8[,1] == "Ticker")
ultimo8 <- length(datos_iak8[,1])

tk8 <- datos_iak8[(octavo+1):ultimo8,1]
peso8 <- datos_iak8[(octavo+1):ultimo8,4]
pesos8 <- as.numeric(peso8)
fecha8 <- datos_iak8[2,2]
precios8 <- datos_iak8[(octavo+1):ultimo,5]
precios8 <- as.numeric(precios8)
historico_iak8 <- list("tickers" = tk8, "pesos" = pesos8, "precios"=precios8)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport8 <- (pesos8*precios8)


#Importacion de datos para un archivo csv noveno
datos_iak9 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-9.csv",header= TRUE, sep=",")
noveno <- which(x = datos_iak9[,1] == "Ticker")
ultimo9 <- length(datos_iak9[,1])

tk9 <- datos_iak9[(noveno+1):ultimo9,1]
peso9 <- datos_iak9[(noveno+1):ultimo9,4]
pesos9<- as.numeric(peso9)
fecha9 <- datos_iak9[2,2]
precios9 <- datos_iak[(noveno+1):ultimo9,5]
precios9 <- as.numeric(precios9)
historico_iak9 <- list("tickers" = tk9, "pesos" = pesos9, "precios"=precios9)


##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport9 <- (pesos9*precios9)

#Importacion de datos para un archivo csv décimo
datos_iak10 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-10.csv",header= TRUE, sep=",")
decimo <- which(x = datos_iak10[,1] == "Ticker")
ultimo10 <- length(datos_iak10[,1])

tk10 <- datos_iak10[(decimo+1):ultimo,1]
peso10 <- datos_iak[(decimo+1):ultimo,4]
pesos10 <- as.numeric(peso10)
fecha10 <- datos_iak10[2,2]
precios10 <- datos_iak10[(decimo+1):ultimo,5]
precios10 <- as.numeric(precios10)
historico_iak10 <- list("tickers" = tk10, "pesos" = pesos10, "precios"=precios10)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport10 <- (pesos10*precios10)

#Importacion de datos para un archivo csv onceavo
datos_iak11 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-11.csv",header= TRUE, sep=",")
onceavo <- which(x = datos_iak11[,1] == "Ticker")
ultimo11 <- length(datos_iak11[,1])

tk11 <- datos_iak11[(onceavo+1):ultimo11,1]
peso11 <- datos_iak11[(onceavo+1):ultimo11,4]
pesos11 <- as.numeric(peso11)
fecha11 <- datos_iak[2,2]
precios11 <- datos_iak11[(onceavo+1):ultimo11,5]
precios11 <- as.numeric(precios11)
historico_iak11 <- list("tickers" = tk11, "pesos" = pesos11, "precios"=precios11)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport11 <- (pesos11*precios11)

#Importacion de datos para un archivo csv doceavo
datos_iak12 = read.csv("/Users/Karen/Desktop/Trading/ITA_holdings-12.csv",header= TRUE, sep=",")
doceavo <- which(x = datos_iak12[,1] == "Ticker")
ultimo12 <- length(datos_iak12[,1])

tk12 <- datos_iak12[(doceavo+1):ultimo12,1]
pesos12 <- datos_iak12[(doceavo+1):ultimo12,4]
pesos12 <- as.numeric(pesos12)
fecha12 <- datos_iak12[2,2]
precios12 <- datos_iak12[(doceavo+1):ultimo12,5]
precios12 <- as.numeric(precios12)
historico_iak12 <- list("tickers" = tk12, "pesos" = pesos12, "precios"=precios12)

##calcular rendimientos mensual de cada accion que conforma el ETF precio*peso##

rendport12 <- (pesos12*precios12)


# Cargar el token de QUANDL
Quandl.api_key("Us7sh5qQJQtwrPCA3rc1")

# Funcion para descagar precios

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

for (i in 1:12){
  archivos[i] <- paste("ITA_holdings-",i,".csv",sep="")
}



# Descargar Precios y Calcular rendimientos
Datos <- list()


for(i in 1:length(50)) #va tk en lugar de 50, pero no lee tk
  Datos[[i]] <- Bajar_Precios(Columns = cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

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

##############DataFrame resultados############
# Cargar el token de QUANDL
Quandl.api_key("Us7sh5qQJQtwrPCA3rc1")

# Funcion para descagar precios

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

for (i in 1:12){
  archivos[i] <- paste("ITA_holdings-",i,".csv",sep="")
}



# Descargar Precios y Calcular rendimientos
Datos <- list()


for(i in 1:length(50)) #va tk en lugar de 50, pero no lee tk
  Datos[[i]] <- Bajar_Precios(Columns = cs, Ticker=tk[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

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
dataframe_final <- data.frame(matrix(ncol=5, nrow=12, data=0))
#################### DATAFRAME
colnames(dataframe_final) <- c("Mes", "Rends", "Riesgo", "Sharpe", "Treynor")

for(i in 1:12) {
  dataframe_final$Series[i] <- paste("Serie_", i, sep="")
}


aqui <- list()

for(i in 1:12){
  aqui[[i]] <- funcion_pesos(i)
}

general<- read_excel("Trading/lab1/general1.xlsx")
rendimientos <- general[1:12,30]

{
  dataframe_final$Rends <- rendimientos
}

desvest <- general[1:12,31]

{
  dataframe_final$Riesgo <- desvest
}