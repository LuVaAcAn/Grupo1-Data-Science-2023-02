setwd('C:/Users/ANDREA/Desktop/archivos/andrea/tp')
data<-read.csv('hotel_bookings.csv',header=TRUE,stringsAsFactors = FALSE)
titulos<-names(data[1,])
data
str(data)
str(titulos)
summary(data)
bookings <-data
valor_na <- function(x){
  sum = for(i in 1:ncol(x)){cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")}}
valor_na(bookings)

#guardamos los indices de filas con NA en children
filas_con_valores_NA <- which(is.na(bookings$children))
filas_con_valores_NA
bookings[40601,]

#cambio de los valores de las filas con valores NA en la columna children por 0(no hay niños en reserva) 
bookings$children[filas_con_valores_NA] <- 0
bookings[40601,]

#Obtenemos todos los hoteles del dataset y visualizamos su tipo de dato
hoteles <- (bookings[ , 1])
class(hoteles)

#Eliminamos la palabra "Hotel" de cada dato
hoteles <- gsub(" Hotel", "", hoteles)

#Eliminamos espacios en blanco innnecesarioss
hoteles <- gsub(" ", "", hoteles)

#Tomamos solo los valores unicos
hoteles <- unique(hoteles)

#Imprimimos los tipos de hoteles existentes
hoteles

install.packages("ggplot2")
library(ggplot2)

install.packages("crayon")
#clientes vs lead_time
ggplot(bookings, aes(x=lead_time, y= adr))+geom_point(size=0.2, color="red")+scale_y_log10()+
  labs(title="Clientes vs lead_time", x ="lead_time", y="Clientes", fill="Cantidad")+
  theme(plot.title=element_text(size=20,face="bold"))

reservas_procedidas<-bookings[bookings$is_canceled == 0, ]
#Ahora hacemos nuestra gráfica de hoteles para visualizar la cantidad de reservas para cada uno
ggplot(reservas_procedidas, aes(x = hotel, fill = factor(after_stat(count)))) +
  geom_bar() +  # grafico de barras
  scale_fill_manual(values = c("#F89880", "#FFC0CB")) +
  labs(title = "Distribucion de hoteles segun tipo", x = "Hoteles", y = "Cantidad", fill = "Casos") +
  theme(plot.title = element_text(face = "bold"))

#Cantidad de clientes que cancelaron reservas 
ggplot(bookings, aes(x = is_canceled, fill = factor(after_stat(count)) )) +
  geom_bar() +
  labs(title = "Distribucion de clientes segun estado de reserva", x = "Reservas", 
       y = "Cantidad",fill="Cantidad") +
  theme(plot.title = element_text(face = "bold"))+
  scale_x_continuous(breaks = c(0,1))

#Debido a que la fecha de estado de reserva esta en forma char lo pasamos a ser fecha
reservas_procedidas$reservation_status_date <- as.Date(reservas_procedidas$reservation_status_date)

#Extraemos el año de 'reservation_status_date'
reservas_procedidas$reservation_status_year <- as.integer(format(reservas_procedidas$reservation_status_date, "%Y"))

#Realiza el conteo por año
counts1 = table(reservas_procedidas$hotel, reservas_procedidas$reservation_status_year)
counts1 
#Grafica de la demanda de hoteles por año(a traves del tiempo)
barplot(counts1, col=c("#F89880","#FFC0CB"), legend = c("City Hotel","Resort Hotel"), 
        main = "Demanda de hoteles en el tiempo", names= c("2015", "2016", "2017"))

#realizamos la tabla de datos de total de reservas por año
total_reservas_tiempo <- table(reservas_procedidas$reservation_status_year)
total_reservas_tiempo 

#Grafica de la demanda de hoteles por año(a traves del tiempo)
barplot(total_reservas_tiempo, col=c("#F89880"), 
        main = "Demanda de hoteles en el tiempo", names= c("2015", "2016", "2017"))



##reservas a lo largo del tiempo(mes)
reservas_procedidas$reservation_status_month <- as.integer(format(reservas_procedidas$reservation_status_date, "%m"))
total_reservas_tiempo <- table(reservas_procedidas$reservation_status_month)
total_reservas_tiempo
total_reservas_tiempo <- data.frame(
  mes = as.numeric(names(total_reservas_tiempo)),
  reservas = as.numeric(total_reservas_tiempo)
)
ggplot(total_reservas_tiempo, aes(x = mes, y = reservas)) +
  geom_line() +
  labs(title = "Reservas a lo largo del Tiempo(mes)", x = "Mes", y = "Reservas") +
  theme(plot.title = element_text(face = "bold"))+
  scale_x_continuous(breaks = total_reservas_tiempo$mes)


#reservas a lo largo del tiempo por temporada(baja,media,alta)

#temp <-  c(-Inf, 5000, 7000,Inf)
#total_reservas_tiempo$Temporada <-cut(total_reservas_tiempo$reservas,breaks=temp,labels = nom.temporada )

nom.temporada <- c('Baja', 'Media','Alta')
total_reservas_tiempo$Temporada <-cut(total_reservas_tiempo$reservas,breaks=3,labels = nom.temporada )
ggplot(total_reservas_tiempo, aes(x = Temporada, fill = factor(after_stat(count)))) +
  geom_bar() +  # grafico de barras
  labs(title = "Cantidad de meses ", x = "Temporada", y = "Cantidad", fill= "Meses") +
  theme(plot.title = element_text(face = "bold"))


#espacios de estacionamiento
ggplot(reservas_procedidas, aes(x =required_car_parking_spaces, fill=factor(after_stat(count)))) +
  geom_histogram()  +
  labs(title = "Espacios de estacionamiento segun reserva", x = "Estacionamientos necesitados", y = "Reservas", fill="Cantidad")

#meses con mas cancelaciones
reservas_canceladas<-bookings[bookings$is_canceled == 1, ]
reservas_canceladas$reservation_status_date <- as.Date(reservas_canceladas$reservation_status_date)
reservas_canceladas$reservation_status_month <- as.integer(format(reservas_canceladas$reservation_status_date, "%m"))

total_cancelado <-table(reservas_canceladas$reservation_status_month)
total_cancelado
total_cancelado <- data.frame(
  mes = as.numeric(names(total_cancelado)),
  canceladas = as.numeric(total_cancelado)
)

ggplot(total_cancelado, aes(x = mes, y = canceladas)) +
  geom_line() +
  labs(title = "Reservas canceladas a lo largo del Tiempo(mes)", x = "Mes", y = "Reservas Canceladas") +
  theme(plot.title = element_text(face = "bold"))+
  scale_x_continuous(breaks = total_cancelado$mes)



install.packages("ggdist")
library(ggplot2)
library(dplyr)
# Calcular el número de reservas que incluyen niños y/o bebés
reservas_procedidas$reserva_con_ninos_o_bebes <- ifelse(reservas_procedidas$children > 0 | reservas_procedidas$babies > 0, 
                                            "Con Niños/Bebés", "Sin Niños/Bebés")

# Crear un gráfico de barras con etiquetas de frecuencia
ggplot(reservas_procedidas, aes(x = reserva_con_ninos_o_bebes, fill = reserva_con_ninos_o_bebes)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3, 
            position = position_stack()) +
  labs(x = "Reservas", y = "Cantidad") +
  ggtitle("Reservas que Incluyen Niños y/o Bebés") +
  theme_minimal() +
  scale_fill_manual(values = c("Con Niños/Bebés" = "red", "Sin Niños/Bebés" = "blue"))

#segmento mercado vs tarifa diaria
ggplot(reservas_procedidas, aes(x = market_segment, y = adr)) +
  geom_boxplot() +
  labs(x = "Segmento de Mercado", y = "ADR (Tarifa Diaria Promedio)") +
  ggtitle("Box Plot de Market Segment vs ADR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
summary(bookings)
