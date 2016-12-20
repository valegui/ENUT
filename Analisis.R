#install.packages("data.table")
library(data.table)
#install.packages("dplyr")
library(dplyr)
#install.packages("matrixStats")
library(matrixStats)
#install.packages("plyr")
library(plyr)
#install.packages("ggplot2")
library(ggplot2)


##Adecuar datos

#Trabajar con tiempos totales En ocupación y traslados asociados en el dia tipo
enut[,"tot_dt"]<-as.numeric(as.character(enut[,"tot_dt"]))
#Todos los datos NA se pasan a 0
enut[,"tot_dt"][is.na(enut[,"tot_dt"])] <- 0

#Trabajar con tiempos total de Trabajo domestico no remuneradio para propio hogar dia tipo
enut[,"tdnr_dt"]<-as.numeric(as.character(enut[,"tdnr_dt"]))
#Todos los datos NA se pasan a 0
enut[,"tdnr_dt"][is.na(enut[,"tdnr_dt"])] <- 0

#Trabajar con tiempos de cuidados a integrantes que requieren cuidados permanentes dia tipo
enut[,"tc_dt"]<-as.numeric(as.character(enut[,"tc_dt"]))
#Todos los datos NA se pasan a 0
enut[,"tc_dt"][is.na(enut[,"tc_dt"])] <- 0

#El factor a numerico
enut[,"wgt2"]<-as.numeric(as.character(enut[,"wgt2"]))
#Todos los datos NA se pasan a 0
enut[,"wgt2"][is.na(enut[,"wgt2"])] <- 0

#Se crea una tabla auxiliar con la variable de interés
enut_auxiliar <- enut[,c("wgt2", "c13_1_1","region", "tot_dt", "tdnr_dt", "tc_dt","cise_5","cae")]
#Se eliminan los datos que no tienen factor de expansion de personas
#Nueva tabla con variables de interes
enut_var_int <- enut_auxiliar[enut_auxiliar[,"wgt2"]!=0,]


## Trabajo de analisis de datos

#Promedio de tiempo trabajado por actividad, sexo y region que habita
meantime_domestico_sexo_y_region<- ddply(enut_var_int, .(c13_1_1,region), summarise, FUN = matrixStats::weightedMean(tdnr_dt, w = wgt2, na.rm = TRUE))
meantime_cuidados_sexo_y_region<- ddply(enut_var_int, .(c13_1_1,region), summarise, FUN = matrixStats::weightedMean(tc_dt, w = wgt2, na.rm = TRUE))
meantime_totaltransp_sexo_y_region<- ddply(enut_var_int, .(c13_1_1,region), summarise, FUN = matrixStats::weightedMean(tot_dt, w = wgt2, na.rm = TRUE))
#Cambiar nombre a las columnas
setnames(meantime_domestico_sexo_y_region, colnames(meantime_domestico_sexo_y_region), c("Sexo", "Region", "Tiempo"))
setnames(meantime_cuidados_sexo_y_region, colnames(meantime_cuidados_sexo_y_region), c("Sexo", "Region", "Tiempo"))
setnames(meantime_totaltransp_sexo_y_region, colnames(meantime_totaltransp_sexo_y_region), c("Sexo", "Region", "Tiempo"))

#Promedio por sexo y actividad
meantime_domestico_sexo <- ddply(enut_var_int, .(c13_1_1), summarise, FUN = matrixStats::weightedMean(tdnr_dt, w = wgt2, na.rm = TRUE))
meantime_cuidados_sexo <-ddply(enut_var_int, .(c13_1_1), summarise, FUN = matrixStats::weightedMean(tc_dt, w = wgt2, na.rm = TRUE))
meantime_totaltransp_sexo <- ddply(enut_var_int, .(c13_1_1), summarise, FUN = matrixStats::weightedMean(tot_dt, w = wgt2, na.rm = TRUE))
#Cambiar nombre a las columnas
setnames(meantime_domestico_sexo, colnames(meantime_domestico_sexo), c("Sexo","Tiempo"))
setnames(meantime_cuidados_sexo, colnames(meantime_cuidados_sexo), c("Sexo","Tiempo"))
setnames(meantime_totaltransp_sexo, colnames(meantime_totaltransp_sexo), c("Sexo","Tiempo"))
#Se agrega columna que diferencia el tipo de trabajo
meantime_domestico_sexo$Tipo<-"Trabajo Domestico"
meantime_cuidados_sexo$Tipo<-"Cuidados a personas"
meantime_totaltransp_sexo$Tipo<-"Ocupacion"
#Tablas que unen las filas de las anteriores
meantime_todos_sexo<-rbind(meantime_domestico_sexo,meantime_cuidados_sexo,meantime_totaltransp_sexo)



##Graficos
#Paleta de graficos en http://www.color-hex.com/color-palette/27927

#Grafico de promedios domesticos, cuidados y de ocupacion respectivamente
#Se redondea los digitos a dos decimales
bd1 <- ggplot(data=meantime_domestico_sexo, aes(x=Sexo, y=Tiempo)) +
  geom_bar(stat="identity", fill="#cad19b") + ggtitle("Horas promedio diarias ocupadas en trabajo doméstico por sexo") + 
  geom_text(data=meantime_domestico_sexo, aes(x=Sexo, y=Tiempo, label=sprintf("%0.2f", round(Tiempo, digits = 2))), size=4) + labs(y="Tiempo [Horas]")
bd1

bc1 <- ggplot(data=meantime_cuidados_sexo, aes(x=Sexo, y=Tiempo)) +
  geom_bar(stat="identity", fill="#cad19b") + ggtitle("Horas promedio diarias ocupadas en cuidados a otras personas por sexo") +
  geom_text(data=meantime_cuidados_sexo, aes(x=Sexo, y=Tiempo, label=sprintf("%0.2f", round(Tiempo, digits = 2))), size=4) + labs(y="Tiempo [Horas]")
bc1

bo1 <- ggplot(data=meantime_totaltransp_sexo, aes(x=Sexo, y=Tiempo)) +
  geom_bar(stat="identity", fill="#cad19b") + ggtitle("Horas promedio diarias utilizadas en la ocupación y en traslado a esta, por sexo") +
  geom_text(data=meantime_totaltransp_sexo, aes(x=Sexo, y=Tiempo, label=sprintf("%0.2f", round(Tiempo, digits = 2))), size=4) + labs(y="Tiempo [Horas]")
bo1


#Gráfico stacked de promedios
#arreglo de posicion de etiquetas
meantime_todos_sexo$pos<-c(0.8, 1.8, 5.3, 6.0, 3.0, 4.3)

stk <- ggplot() + geom_bar(aes(y=Tiempo, x= Sexo, fill= Tipo), data=meantime_todos_sexo, stat="identity") + ggtitle("Uso de tiempo promedio diario por sexo en cuidados de personas, en la ocupación y trabajo doméstico") +
  scale_fill_manual(values=c("#f5c15c","#7f8f2c", "#cad19b"))
stk <- stk + geom_text(data=meantime_todos_sexo, aes(x=Sexo, y= pos, label= sprintf("%0.2f", round(Tiempo, digits = 2))), size=4) + labs(y="Tiempo [Horas]")
stk


##Situacion de empleo

#Nueva data para ver la clasificacion de situacion de empleo
#Solo se consideran aquellas personas que clasifican como ocupados y por lo tanto tienen CISE
enut_so <- enut_var_int[enut_var_int[,"cae"]== "Ocupada(o)",]
#Se conservan solo las variables de interes: factor de expansion, sexo y CISE
enut_so<- enut_so[,c("wgt2","c13_1_1","cise_5")]
#Se agrupan segun sexo y CISE
enut_so<-as.data.frame(enut_so %>% group_by(c13_1_1, cise_5) %>% 
                         summarise_each(funs(sum)))
#Se elimina aquella fila que solo considera a una persona
enut_so <- enut_so[enut_so[,"cise_5"]!= "Ocupada(o) sin clasificaciÃ³n",]
#Cambiar los nombres de las columnas
setnames(enut_so, colnames(enut_so), c("Sexo","CISE Abreviada","wgt2"))


## Trabajo en Json para D3Plus

#install.packages("jsonlite")
library(jsonlite)
#crear json y guardar archivo
json_CISE <- toJSON(enut_so, pretty=TRUE)
write(json_CISE, file="CISE2")
#crear json y guardar archivo
json_TPSR_Domestico <-toJSON(meantime_domestico_sexo_y_region, pretty=TRUE)
write(json_TPSR_Domestico, file="TPSR-Domestico")
#crear json y guardar archivo
json_TPSR_Cuidados<-toJSON(meantime_cuidados_sexo_y_region, pretty=TRUE)
write(json_TPSR_Cuidados, file="TPSR-Cuidados")
#crear json y guardar archivo
json_TPSR_Ocupacion<-toJSON(meantime_totaltransp_sexo_y_region, pretty=TRUE)
write(json_TPSR_Ocupacion, file="TPSR-Ocupacion")
