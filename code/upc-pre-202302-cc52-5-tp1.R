## 0.BORRA TODAS LAS VARIABLES DE MEMORIA

rm(list=ls(all=TRUE)) 

# Limpia consola
cat("\014")

## I.CARGA DE DATOS
setwd("C:/Users/Anthony/Downloads/R")
hotel_bookings<-read.csv('hotel_bookings.csv', header=TRUE,stringsAsFactors = FALSE, sep=',',dec='.')

## I.Inspeccionar datos

# Observamos datos cargados
View(hotel_bookings)

# Nombres de las variables o columnas
names(hotel_bookings)

# Filas Iniciales
head(hotel_bookings,10)

# Filas Finales
tail(hotel_bookings,10)

hotel_bookings_final<-hotel_bookings

# Convertimos a factores las variables correspondientes 
hotel_bookings$is_canceled<-as.numeric(hotel_bookings$is_canceled)
hotel_bookings$is_canceled  <- as.factor(hotel_bookings$is_canceled)
hotel_bookings$arrival_date_year  <- as.factor(hotel_bookings$arrival_date_year)
hotel_bookings$arrival_date_month  <- as.factor(hotel_bookings$arrival_date_month)
hotel_bookings$meal  <- as.factor(hotel_bookings$meal)
hotel_bookings$country  <- as.factor(hotel_bookings$country)
hotel_bookings$market_segment  <- as.factor(hotel_bookings$market_segment)
hotel_bookings$distribution_channel  <- as.factor(hotel_bookings$distribution_channel)
hotel_bookings$reserved_room_type  <- as.factor(hotel_bookings$reserved_room_type)
hotel_bookings$assigned_room_type  <- as.factor(hotel_bookings$assigned_room_type)
hotel_bookings$deposit_type  <- as.factor(hotel_bookings$deposit_type)
hotel_bookings$customer_type  <- as.factor(hotel_bookings$customer_type)
hotel_bookings$reservation_status  <- as.factor(hotel_bookings$reservation_status)
hotel_bookings$agent  <- as.factor(hotel_bookings$agent)
hotel_bookings$is_repeated_guest  <- as.factor(hotel_bookings$is_repeated_guest)

# Estructura del conjunto de datos
str(hotel_bookings)

# Resumen del conjunto de datos
summary(hotel_bookings)
#Por país
summary(hotel_bookings$country)
#Por cliente habitual
summary(hotel_bookings$is_repeated_guest)
#Por estado de reserva
summary(hotel_bookings$reservation_status)

## III. PRE-PROCESAR DATOS
View(hotel_bookings_final)
summary(hotel_bookings_final)

## convertir los NULLS en NA
nullos<-which(hotel_bookings_final=="NULL",arr.ind=TRUE)
hotel_bookings_final[nullos]<-NA
## convertir los UNDEFINED en NA
indef<-which(hotel_bookings_final=="Undefined",arr.ind=TRUE)
hotel_bookings_final[indef]<-NA

## Funcion para hallar la cantidad de datos faltantes en cada variable o columna

nroNA <- function(x){
  sum = 0
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
nroNA(hotel_bookings_final)
#children, meal,country,market_segment , distribution_channel,agent y company presentan datos faltantes. 

##hallo la moda

Mode <- function(x) {
  uniq_x <- unique(x)
  freq_x <- tabulate(match(x, uniq_x))
  return(uniq_x[which.max(freq_x)])
}


#Comenzamos a reemplazar los datos faltantes
#children
#Presenta solo 4 valores NA de 119390 por lo que podemos eliminarlos.
hotel_bookings_final<-hotel_bookings_final[!is.na(hotel_bookings_final$children),]
#Al eliminar estas dos filas tambien eliminamos aquellas cuales su market_segment era NA.

#meal
#Remplazamos los valores vacios por la moda
moda<-Mode(hotel_bookings_final$meal)
hotel_bookings_final$meal[is.na(hotel_bookings_final$meal)]<-moda

#country
#Remplazamos los valores vacios por la moda
moda<-Mode(hotel_bookings_final$country)
hotel_bookings_final$country[is.na(hotel_bookings_final$country)]<-moda

#distribution_channel
#Eliminamos la fila ya que es 1 de 119390
hotel_bookings_final<-hotel_bookings_final[!is.na(hotel_bookings_final$distribution_channel),]

#agent
#podriamos reemplazar los valores vacios por la moda pero esta variable no afecta en la estadística 
#por esto lo eliminaremos
hotel_bookings_final$agent <- NULL
#company
#El 94.30% presentan valores NA por lo que lo mejor sera eliminar esta variable ya que, además,
#no presenta ningún dato estadistico más
hotel_bookings_final$company <- NULL

#Volvemos a ver si hay valores NA:
nroNA(hotel_bookings_final)

#OUTLIERS
summary(hotel_bookings_final[,1])
str(hotel_bookings_final[,3])
#funcion para determinar la cantidad de valores atipicos de una variable
is_outlier <- function(vector) {
  q1 <- quantile(vector, 0.25)
  q3 <- quantile(vector, 0.75)
  iqr <- q3 - q1
  limite_superior <- q3 + 1.5 * iqr
  limite_inferior <- q1 - 1.5 * iqr
  valores_atipicos <- vector > limite_superior | vector < limite_inferior
  return(valores_atipicos)
}
#Funcion que revisa cuantos valores atipicos tiene cada variable.
for(i in 1:ncol(hotel_bookings_final)){
  if(is.numeric(hotel_bookings_final[,i])){
    sum(is_outlier(hotel_bookings_final[,i]))
    if (sum(is_outlier(hotel_bookings_final[,i]))){cat("En la columna",colnames(hotel_bookings_final[i]),"total de valores atipicos:",sum(is_outlier(hotel_bookings_final[,i])),"\n")
  }else(cat("no tiene\n")) }else(cat("no tiene\n"))}


#Reemplazamos lo valores atipicos de cada variable
#FUNCION PARA REEMPLAZAR VALORES ATIPICOS POR LA MODA:
convert <- function(x){
  b<-boxplot.stats(x)
  while(length(b$out)>0){
    b<-boxplot.stats(x)
    for(i in 1:nrow(hotel_bookings_final)){
      if(x[i] %in% b$out){
        x[i]<-b$stats[3]
      } }
  }
  return(x)
}
#FUNCION PARA REEMPLAZAR VALORES ATIPICOS POR EL LIMITE SUPERIOR:
convertmax <- function(x){
  b<-boxplot.stats(x)
  while(length(b$out)>0){
    b<-boxplot.stats(x)
    for(i in 1:nrow(hotel_bookings_final)){
      if(x[i] %in% b$out){
        x[i]<-b$stats[5]
      } }
  }
  return(x)
}

#LEAD_TIME
#en este caso convertimos los valores atipicos a valores del limite superior
#ANTES DEL REEMPLAZO:
boxplot.stats(hotel_bookings_final$lead_time)
boxplot(hotel_bookings_final$lead_time, horizontal = TRUE)
sum(is_outlier(hotel_bookings_final$lead_time))
#DESPUES DEL REEMPLAZO:
hotel_bookings_final[,3]<-convertmax(hotel_bookings_final[,3])
boxplot.stats(hotel_bookings_final$lead_time)
boxplot(hotel_bookings_final$lead_time, horizontal = TRUE)
sum(is_outlier(hotel_bookings_final$lead_time))

#adr
#Reemplazamos los valores por la moda
#ANTES DEL REEMPLAZO:
boxplot.stats(hotel_bookings_final$adr)
boxplot(hotel_bookings_final$adr, horizontal = TRUE)
sum(is_outlier(hotel_bookings_final$adr))
#DESPUES DEL REEMPLAZO:
hotel_bookings_final$adr<-convert(hotel_bookings_final$adr)
boxplot.stats(hotel_bookings_final$adr)
boxplot(hotel_bookings_final$adr, horizontal = TRUE)
sum(is_outlier(hotel_bookings_final$adr))

#Las demás variables no vemos la necesidad de modificar sus valores atípicos puesto que estos
#nos ayudarán en ciertas situaciones estadísticas

#¿Cuantas reservas se realizan por tipo de hotel?
hotel_bookings_final$hotel<-as.factor(hotel_bookings_final$hotel)
resumen_hotel<-summary(hotel_bookings_final$hotel)
barplot(resumen_hotel, col=c("green","yellow"), legend = c("City Hotel", "Resort Hotel"), 
        main = "reservas por tipo de hotel", names= c("City Hotel", "Resort Hotel") )
summary(hotel_bookings_final$hotel)

#Esta aumentando la demanda con el tiempo?
hotel_bookings_final$arrival_date_year<-as.factor(hotel_bookings_final$arrival_date_year)
resumen_anios<-summary(hotel_bookings_final$arrival_date_year)
barplot(resumen_anios, col=c("green","yellow","red"), legend = c("2015", "2016","2017"), 
        main = "trayectoria de la demanda con el tiempo", names= c("2015", "2016","2017") )
resumen_anios
#¿Cuándo se producen las temporadas de reservas: alta, media y baja?
orden_meses <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
hotel_bookings_final$arrival_date_month <- factor(hotel_bookings_final$arrival_date_month, levels = orden_meses)
resumen_mes<-summary(hotel_bookings_final$arrival_date_month)
barplot(resumen_mes)
summary(hotel_bookings_final$arrival_date_month)
#¿Cuándo es menor la demanda de reservas?
orden_meses <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
hotel_bookings_final$arrival_date_month <- factor(hotel_bookings_final$arrival_date_month, levels = orden_meses)
resumen_mes<-summary(hotel_bookings_final$arrival_date_month)
barplot(resumen_mes)
summary(hotel_bookings_final$arrival_date_month)
#¿Cuántas reservas incluyen niños y/o bebes?
# Calcular la frecuencia de reservas con niños y/o bebés
reservas_con_ninos <- table(hotel_bookings_final$babies > 0 | hotel_bookings_final$children > 0)
names(reservas_con_ninos)<-c("Sin Niños/Bebés", "Con Niños/Bebés")
# Crear el gráfico de barras
barplot(reservas_con_ninos, 
        names.arg = c("Sin Niños/Bebés", "Con Niños/Bebés"),
        main = "Reservas que incluyen Niños y/o Bebés",
        xlab = "Estado de la Reserva",
        ylab = "Cantidad de Reservas",
        ylim = c(0, max(reservas_con_ninos) * 1.1))  # Establecer un rango de ejes y

text(x = 1:2, y = reservas_con_ninos + 50, labels = reservas_con_ninos, pos = 3)
names(reservas_con_ninos)<-c("Sin Niños/Bebés", "Con Niños/Bebés")
reservas_con_ninos

#¿Es importante contar con espacios de estacionamiento?
options(scipen = 999)
hotel_bookings_final$required_car_parking_spaces<-as.factor(hotel_bookings_final$required_car_parking_spaces)
resumen_estac<-summary(hotel_bookings_final$required_car_parking_spaces)
barplot(resumen_estac, col=c("green","yellow","orange","brown","red"), legend = c("0", "1","2","3","8"), 
        main = "trayectoria de la demanda con el tiempo", names= c("0", "1","2","3","8"))
resumen_estac
#¿En qué meses del año se producen más cancelaciones de reservas?
# Calcular la cantidad de cancelaciones por mes
cancelaciones_por_mes <- aggregate(is_canceled ~ arrival_date_month, data = hotel_bookings_final, FUN = sum)

# Ordenar los meses cronológicamente
meses_ordenados <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
cancelaciones_por_mes$arrival_date_month <- factor(cancelaciones_por_mes$arrival_date_month, levels = meses_ordenados)

# Crear el gráfico de barras
barplot(cancelaciones_por_mes$is_canceled, 
        names.arg = cancelaciones_por_mes$arrival_date_month,
        col = "red",
        main = "Cancelaciones de Reservas por Mes",
        xlab = "Mes",
        ylab = "Cantidad de Cancelaciones")
cancelaciones_por_mes


#install and load writexl package
install.packages('writexl')
library(writexl)
write_xlsx(hotel_bookings_final, "C:/Users/Anthony/Downloads/R/TP.xlsx")
write.csv(hotel_bookings_final, "C:/Users/Anthony/Downloads/R/TP1.csv", row.names=TRUE)
