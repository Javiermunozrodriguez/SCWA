# En este capitulo del trabajo , se desarrolla la programación necesaria para la visualización de los datos

library(readr)

#por favor, el siguinete dataset, hay que cargarlso desde al carpeta que esté alojado en su servidor

consumption_prediction_2016 <- read_csv("C:/Users/jmuro/Desktop/tfm/step_04_forecastmodels/consumption_prediction_2016.csv")

View(consumption_prediction_2016)

#cargamos las librerias que necesitamos apara comenzar a plantear un modelo

if(!require("plyr")){
  install.packages("plyr")
  library("plyr")
}
install.packages("caTools")
if(!require("caTools")){
  install.packages("caTools")
  library("caTools")
}

if(!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")
}

library(ggplot2)

library(tidyverse)

# primero vasmo a enriquecer la data set de resultados con las coordenadas de algunos puntos de distrifución de funetes de agua, 
# escalando y distribuyendo en torno a un criterio de asignación según el distrito.

Real_consumption_data_madrid_2016 <- read_excel("C:/Users/jmuro/Desktop/tfm/step_05_front end/Real_consumption_data_madrid_2016.xls")

View(Real_consumption_data_madrid_2016)

# limpiamos un poco los Datos para tener un perpectiva de lo que se intenta implementar

Real_consumption_data_madrid_2016 <- Real_consumption_data_madrid_2016[,-c(1)] # quitamos al primera comulna por defecto de carga

Real_consumption_data_madrid_2016 <- Real_consumption_data_madrid_2016[-c(1,2,3,4,5,6,29,30),] # las filas que no aportan inforamción


# Quitamos las filas de varibles colineales, litros por habitante e indices que no aportan información

Real_consumption_data_madrid_2016 <- Real_consumption_data_madrid_2016[,-c(4,5)] 

colnames(Real_consumption_data_madrid_2016)<-c("Distrito", " Consumo [m3]", "Poblacion", "Poblacion_relativa_%", "Consumo_relativo_%")

# vamos a cambiar al clase para poder opeararla

Real_consumption_data_madrid_2016[, c(2:5)] <- sapply(Real_consumption_data_madrid_2016[, c(2:5)],as.character)

Real_consumption_data_madrid_2016[, c(2:5)] <- sapply(Real_consumption_data_madrid_2016[, c(2:5)],as.numeric)


Real_consumption_data_madrid_2016[, c(1)] <- sapply(Real_consumption_data_madrid_2016[, c(1)],as.character)


# Vemos que porcentajes tenemos por distrito

Real_consumption_data_madrid_2016$`Consumo_relativo_%` <- Real_consumption_data_madrid_2016$`Consumo_relativo_%`*100

Real_consumption_data_madrid_2016$`Población_relativa_%` <- Real_consumption_data_madrid_2016$`Población_relativa_%`*100

# tambien eliminamos la fila de consumos sin registro de consumidores, porque, no vamos a poder absorber este patrón en el encaje
# con las predicciones

Real_consumption_data_madrid_2016 <- Real_consumption_data_madrid_2016[-c(22),]

# vemaso la coherencia delas columnas de pesaso relativos

sum(Real_consumption_data_madrid_2016$`Población_relativa_%`)

sum(Real_consumption_data_madrid_2016$`Consumo_relativo_%`)

write.csv(Real_consumption_data_madrid_2016,file="Real_consumption_data_madrid_2016.csv")

# hay un pequeño sesgo en los consumos por quitarnos al parte de consumos sin registrar, pero avancemos con lso que si sabemos 

# de al fotografía del 2016


# la idea siguiente en crear un dataset que recoja los pesos de las poblacione por distritos y un regsitro aleatorio de direccione UTM
# incluidas en las 178598 observaciones potenciales que podemos asiganar

# en criterio de asignación v a aser por  peso relativo de los consumos, que es un huella más cnsistente con la filosofia de construcción de la
# madtriz de predicciones 

ggplot(Real_consumption_data_madrid_2016, aes(x = `Consumo_relativo_%`, y = `Población_relativa_%`)) + geom_point() +geom_smooth(model="lm")

# se ve una tendencia clara entre el consumos relativos 

ggplot(Real_consumption_data_madrid_2016, aes(x = Distrito, y = ` Consumo [m3]`))+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(Real_consumption_data_madrid_2016, aes(x = Distrito, y = Población))+geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))



colnames(consumption_prediction_2016)<-c("Tipo_consumidor", "2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","2016/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

write.csv(consumption_prediction_2016,file="consumption_prediction_2016.csv")


sum(consumption_prediction_2016[c(1:2),c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","2016/07", "2016/08","2016/09","2016/10","2016/11","2016/12")])




head(consumption_prediction_2016)

# añadimos una columna de distrito para asignarle un valor entre


Distrito <- consumption_prediction_2016$Tipo_consumidor


class(consumption_prediction_2016$Distrito [2])


consumption_prediction_2016[, c(1)] <- sapply(consumption_prediction_2016[, c(1)],as.character)


consumption_prediction_2016$Distrito[1:7113]= "Centro"

consumption_prediction_2016 <- cbind(Distrito,consumption_prediction_2016)

--------------------------------
# esta parte del código se puede mejorar, haciendo más elegante el loop que recorrael datafraem y asignar el distrito. trabajjaremso en en una segunda fase
# en este punto

total=0

for (i in 1:178598) {
  
  total<-total+sum(consumption_prediction_2016[i,c(3:14)])
    
    if (total == 7296331){
      
      consumption_prediction_2016$Distrito[i] == "XXX"
      
    }

}

library(dplyr)

Total_2016<-colSums (select (consumption_prediction_2016, contains ("2016")))

sum(Total_2016)

-----------------------------------------------

  
#Asignación de distritos a los datos


#vemos cuales son los consumos aplicados al matriz de prediccion consumption_prediction_2016 por cada distrito, empezamos por el centro


centro_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[1]/100)

sum(consumption_prediction_2016[c(1:6612),c(3:14)])

consumption_prediction_2016$Distrito[1:6612]= "Centro"

centro_pred_consumption

# Arganzuela

arganzuela_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[2]/100)

sum(consumption_prediction_2016[c(6613:8169),c(3:14)])

consumption_prediction_2016$Distrito[6613:8169]= "Arganzuela"


#Retiro

retiro_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[3]/100)

sum(consumption_prediction_2016[c(8170:8966),c(3:14)])

consumption_prediction_2016$Distrito[8170:8966]= "Retiro"

#Salamanca

salamanca_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[4]/100)

sum(consumption_prediction_2016[c(8967:10630),c(3:14)])

consumption_prediction_2016$Distrito[8967:10630]= "Salamanca"

#Chamartin

chamartin_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[5]/100)

sum(consumption_prediction_2016[c(10631:14156),c(3:14)])

consumption_prediction_2016$Distrito[10631:14156]= "Chamartin"

#Tetuan

tetuan_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[6]/100)

sum(consumption_prediction_2016[c(14157:30969),c(3:14)])

consumption_prediction_2016$Distrito[14157:30969]= "Tetuan"

#Chamberi

chamberi_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[7]/100)

sum(consumption_prediction_2016[c(30970:59301),c(3:14)])

consumption_prediction_2016$Distrito[30970:59301]= "Chamberi"

#Fuencarral

fuencarral_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[8]/100)

sum(consumption_prediction_2016[c(59302:77081),c(3:14)])

consumption_prediction_2016$Distrito[59302:77081]= "Fuencarral"

#Moncloa

moncloa_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[9]/100)

sum(consumption_prediction_2016[c(77082:79273),c(3:14)])

consumption_prediction_2016$Distrito[77082:79273]= "Moncloa"


#Latina

latina_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[10]/100)

sum(consumption_prediction_2016[c(79274:81492),c(3:14)])

consumption_prediction_2016$Distrito[79274:81492]= "Latina"

#Carabanchel

carabanchel_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[11]/100)

sum(consumption_prediction_2016[c(81492:83947),c(3:14)])

consumption_prediction_2016$Distrito[81493:83947]= "Carabanchel"


#Usera

usera_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[12]/100)

sum(consumption_prediction_2016[c(83948:86521),c(3:14)])

consumption_prediction_2016$Distrito[83948:86521]= "Usera"

# Puentevallecas

puentevallecas_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[13]/100)

sum(consumption_prediction_2016[c(86522:94421),c(3:14)])

consumption_prediction_2016$Distrito[86522:94421]= "Puente_Vallecas"

# Moratalaz

moratalaz_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[14]/100)

sum(consumption_prediction_2016[c(94422:95571),c(3:14)])

consumption_prediction_2016$Distrito[94422:95571]= "Moratalaz"


# Ciudad Lineal

ciudadlineal_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[15]/100)

sum(consumption_prediction_2016[c(95572:113461),c(3:14)])

consumption_prediction_2016$Distrito[95572:113461]= "Ciudad_Lineal"


# Hortaleza

hortaleza_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[16]/100)

sum(consumption_prediction_2016[c(113462:139587),c(3:14)])

consumption_prediction_2016$Distrito[113462:139587]= "Hortaleza"


# Villaverde

villaverde_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[17]/100)

sum(consumption_prediction_2016[c(139588:161755),c(3:14)])

consumption_prediction_2016$Distrito[139588:161755]= "Villaverde"


# Vallecas

vallecas_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[18]/100)

sum(consumption_prediction_2016[c(161756:168490),c(3:14)])

consumption_prediction_2016$Distrito[161756:168490]= "Vallecas"


# Vicalvaro

vicalvaro_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[19]/100)

sum(consumption_prediction_2016[c(168491:169489),c(3:14)])

consumption_prediction_2016$Distrito[168491:169489]= "Vicalvaro"

# san blas

sanblas_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[20]/100)

sum(consumption_prediction_2016[c(169490:172190),c(3:14)])

consumption_prediction_2016$Distrito[169490:172190]= "San_Blas"

# Barajas

barajas_pred_consumption= sum(Total_2016,1:12)*(Real_consumption_data_madrid_2016$`Consumo_relativo_%`[21]/100)

sum(consumption_prediction_2016[c(172190:178598),c(3:14)])

consumption_prediction_2016$Distrito[172190:178598]= "barajas"

#ponemos en minusculas la columna de consumidores

consumption_prediction_2016$Tipo_consumidor<- tolower(consumption_prediction_2016$Tipo_consumidor)

write.csv(consumption_prediction_2016,file="consumption_prediction_2016_final.csv")

# vamso a enriquecer la tabla cons las coordenadas desde lso archivos de catastro de madrid

"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

require(XML)

install.packages("XML")
library("XML")


centro_coor <- "C:/Users/jmuro/Desktop/tfm/step_05_front end/cartografia Madrid/ConsultaMasiva_centro.xml"

doc_coor_centro <- xmlTreeParse(centro_coor,getDTD=T,addAttributeNamespaces=T)

arriba_centro = xmlRoot(doc_coor_centro)

#Vemos los nombres de los campos de la tabla, convirtiendo a un Data frame

names(arriba_centro[[1]])

datos_centro=xmlToDataFrame(centro_coor)

head(datos_centro)


# como se puede ver en el resultado, las varibles que descargamso del catastro directamente no aportan las coordenadas de la las parcelas
# para mantener el resultado dentro de los terminos privados que rige este tipo de inforamción
# vamso a generar una seire de coordenadas aleatorias a aprtir del polígono de coordenadas UTM huso 30 de cada uno de los distritos
# aporta la web del catastro de Madrid, ya en trasforamdas en coordendas sexa decimales

install.packages("sp")

library("sp")

# para la zona centro

east_centro <- c(441418.95, 440996.00, 440158.96, 439260.08, 439681.65, 439288.63, 438869.13, 438627.51, 439166.34, 439059.00, 439186.96,
                439587.87, 440379.53, 441171.17, 441295.74, 441161.86, 441217.26, 441418.95)

north_centro <- c(4475157.64, 4475448.17, 4475666.59, 4475795.67, 4474982.84, 4474646.55, 4474638.35, 4473943.95, 4473933.75, 4473629.94, 
                 4473360.20, 4473146.50, 4472937.64, 4473229.22, 4473355.31, 4474075.06, 4474531.22, 4475157.64)

frame_coor_centro <- data.frame(east_centro,north_centro)



# utilizamos las funciones del paquete sp de R para trasforam coordenadas UTM de huso 30 a coordenadas hexadecimales

utmcoor_centro<-SpatialPoints(cbind(frame_coor_centro$east_centro,frame_coor_centro$north_centro), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_centro<-spTransform(utmcoor_centro,CRS("+proj=longlat"))

# volcamos al transformación en el frame de coordenadas del centro

frame_coor_centro$x <- coordinates(longlatcoor_centro)[,1]
frame_coor_centro$y <- coordinates(longlatcoor_centro)[,2]

frame_coor_centro <- frame_coor_centro[,-c(1,2)]

#library(raster)

#rasted_centro <- raster(frame_coor_centro)

#lin <- rasterToContour(is.na(r))




# El siguiente paso será generar un nube de coordenadas decimales para las posiciones de lso consumidores de la zona centro,
# dentro de los límites de poligono de coordendas de esta zona

lat.lims <- frame_coor_centro$y
lon.lims <- frame_coor_centro$x


# el número de coordendas será igual al número de consumidores de al zona Centro

n <- sum(consumption_prediction_2016$Distrito == "Centro")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamso dos columnas de lat y longitud en el data frame de predicciones àra signar los valores calculados

consumption_prediction_2016$lat <- c(1:178598)

consumption_prediction_2016$lon <- c(1:178598)

consumption_prediction_2016$lat[1:6612] <- lat.sample

consumption_prediction_2016$lon[1:6612] <- lon.sample


# bien, ahora hay que repetir el mismos código para cada distrito, que en un afase posterior reprogramaremos para que sea un función

# más limpia para simplificar más la solución

--------------------------------------------------------------------------
# Arganzuela

"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_arganda <- c(439166.32, 438805.26, 438769.90, 440358.40, 441446.74, 441905.19, 442797.20, 441273.26, 440369.92, 439138.28, 439166.32)
north_arganda <- c(4473930.85, 4473951.20, 4472474.37, 4471814.34, 4470508.23, 4470623.69, 4471801.15, 4473290.16, 4472896.81, 4473347.81, 4473930.85)

frame_coor_arganda<- data.frame(east_arganda,north_arganda)

utmcoor_arganda<-SpatialPoints(cbind(frame_coor_arganda$east_arganda,frame_coor_arganda$north_arganda), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_arganda<-spTransform(utmcoor_arganda,CRS("+proj=longlat"))

frame_coor_arganda$x <- coordinates(longlatcoor_arganda)[,1]
frame_coor_arganda$y <- coordinates(longlatcoor_arganda)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_arganda$y
lon.lims <- frame_coor_arganda$x

n <- sum(consumption_prediction_2016$Distrito == "Arganzuela")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamso dos columnas de lat y longitud en el data frame de predicciones àra signar los valores calculados


consumption_prediction_2016$lat[6613:8169] <- lat.sample

consumption_prediction_2016$lon[6613:8169] <- lon.sample
-------------------------------------------------------------
# Retiro
  
"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_retiro <- c(441268.37, 442510.49, 443970.64, 442964.58, 441254.84,441268.37)

north_retiro <- c(4474526.33, 4474690.78, 4474418.60, 4471971.13, 4473542.77, 4474526.33)
  
frame_coor_retiro<- data.frame(east_retiro,north_retiro)

utmcoor_retiro<-SpatialPoints(cbind(frame_coor_retiro$east_retiro,frame_coor_retiro$north_retiro), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_retiro<-spTransform(utmcoor_retiro,CRS("+proj=longlat"))

frame_coor_retiro$x <- coordinates(longlatcoor_retiro)[,1]
frame_coor_retiro$y <- coordinates(longlatcoor_retiro)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_retiro$y
lon.lims <- frame_coor_retiro$x

n <- sum(consumption_prediction_2016$Distrito == "Retiro")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[8170:8966] <- lat.sample

consumption_prediction_2016$lon[8170:8966] <- lon.sample

-----------------------------------------------
  
# Salamanca
  
"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_salamanca <- c(441358.09, 444010.78, 443991.72, 442611.88, 441572.60, 441358.09)

north_salamanca <- c(4474824.47, 4474722.95, 4477238.16, 4476520.56, 4476560.50, 4474824.47)

frame_coor_salamanca<- data.frame(east_salamanca,north_salamanca)

utmcoor_salamanca<-SpatialPoints(cbind(frame_coor_salamanca$east_salamanca,frame_coor_salamanca$north_salamanca), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_salamanca<-spTransform(utmcoor_salamanca,CRS("+proj=longlat"))

frame_coor_salamanca$x <- coordinates(longlatcoor_salamanca)[,1]
frame_coor_salamanca$y <- coordinates(longlatcoor_salamanca)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_salamanca$y
lon.lims <- frame_coor_salamanca$x

n <- sum(consumption_prediction_2016$Distrito == "Salamanca")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[8967:10630] <- lat.sample

consumption_prediction_2016$lon[8967:10630] <- lon.sample

--------------------------------------
  
  
  # Chamartin
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_chamartin <- c(441434.42, 443652.37, 443665.52, 442917.48, 441718.56, 441591.49, 441434.42)

north_chamartin <- c(4476764.63, 4476640.29, 4478774.76, 4480317.17, 4480346.71, 4479727.24, 4476764.63)


frame_coor_chamartin<- data.frame(east_chamartin,north_chamartin)

utmcoor_chamartin<-SpatialPoints(cbind(frame_coor_chamartin$east_chamartin,frame_coor_chamartin$north_chamartin), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_chamartin<-spTransform(utmcoor_chamartin,CRS("+proj=longlat"))

frame_coor_chamartin$x <- coordinates(longlatcoor_chamartin)[,1]
frame_coor_chamartin$y <- coordinates(longlatcoor_chamartin)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_chamartin$y
lon.lims <- frame_coor_chamartin$x

n <- sum(consumption_prediction_2016$Distrito == "Chamartin")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[10631:14156] <- lat.sample

consumption_prediction_2016$lon[10631:14156] <- lon.sample
----------------------------------------------------
  
# Tetuan
  
"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_tetuan <- c(441673.31, 439917.94, 439915.71, 441364.85, 441673.31)

north_tetuan <- c(4480515.21, 4480471.12, 4477655.49, 4477548.30, 4480515.21)


frame_coor_tetuan<- data.frame(east_tetuan,north_tetuan)

utmcoor_tetuan<-SpatialPoints(cbind(frame_coor_tetuan$east_tetuan,frame_coor_tetuan$north_tetuan), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_tetuan<-spTransform(utmcoor_tetuan,CRS("+proj=longlat"))

frame_coor_tetuan$x <- coordinates(longlatcoor_tetuan)[,1]
frame_coor_tetuan$y <- coordinates(longlatcoor_tetuan)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_tetuan$y
lon.lims <- frame_coor_tetuan$x

n <- sum(consumption_prediction_2016$Distrito == "Tetuan")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[14157:30969] <- lat.sample

consumption_prediction_2016$lon[14157:30969] <- lon.sample
------------------------------
  
# Chamberi
  
"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_chamberi <- c(439140.31, 441608.03, 441482.83, 439053.51, 439140.31)

north_chamberi <- c(4477542.84, 4477369.47, 4475505.28, 4475820.55, 4477542.84)


frame_coor_chamberi<- data.frame(east_chamberi,north_chamberi)

utmcoor_chamberi<-SpatialPoints(cbind(frame_coor_chamberi$east_chamberi,frame_coor_chamberi$north_chamberi), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_chamberi<-spTransform(utmcoor_chamberi,CRS("+proj=longlat"))

frame_coor_chamberi$x <- coordinates(longlatcoor_chamberi)[,1]
frame_coor_chamberi$y <- coordinates(longlatcoor_chamberi)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_chamberi$y
lon.lims <- frame_coor_chamberi$x

n <- sum(consumption_prediction_2016$Distrito == "Chamberi")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[30970:59301] <- lat.sample

consumption_prediction_2016$lon[30970:59301] <- lon.sample 
--------------------------------------------------------------------

# Fuencarral
  
"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_fuencarral <- c(442893.71, 442944.50, 444091.56, 444134.09, 441964.42, 438630.69, 436795.96, 436385.49, 436389.81, 438309.34,
                   439357.25, 441712.57, 441870.59, 442109.56, 442893.71)

north_fuencarral <- c(4481778.42, 4482714.25, 4483972.03, 4484991.67, 4485770.20, 4483565.70, 4481969.50, 4481265.68, 4480746.71,
                    4480582.78, 4480240.81, 4480697.55, 4481557.31, 4481601.83, 4481778.42)


frame_coor_fuencarral<- data.frame(east_fuencarral,north_fuencarral)

utmcoor_fuencarral<-SpatialPoints(cbind(frame_coor_fuencarral$east_fuencarral,frame_coor_fuencarral$north_fuencarral), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_fuencarral<-spTransform(utmcoor_fuencarral,CRS("+proj=longlat"))

frame_coor_fuencarral$x <- coordinates(longlatcoor_fuencarral)[,1]
frame_coor_fuencarral$y <- coordinates(longlatcoor_fuencarral)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_fuencarral$y
lon.lims <- frame_coor_fuencarral$x

n <- sum(consumption_prediction_2016$Distrito == "Fuencarral")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[59302:77081] <- lat.sample

consumption_prediction_2016$lon[59302:77081] <- lon.sample 

-------------------------------------------
  
# Moncloa
  
"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_moncloa <- c(438607.17, 434541.09, 434992.87, 439946.06, 439618.43, 438607.17)

north_moncloa <- c(4474306.82, 4474349.93, 4480419.51, 4479986.71, 4474365.34, 4474306.82)


frame_coor_moncloa<- data.frame(east_moncloa,north_moncloa)

utmcoor_moncloa<-SpatialPoints(cbind(frame_coor_moncloa$east_moncloa,frame_coor_moncloa$north_moncloa), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_moncloa<-spTransform(utmcoor_moncloa,CRS("+proj=longlat"))

frame_coor_moncloa$x <- coordinates(longlatcoor_moncloa)[,1]
frame_coor_moncloa$y <- coordinates(longlatcoor_moncloa)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_moncloa$y
lon.lims <- frame_coor_moncloa$x

n <- sum(consumption_prediction_2016$Distrito == "Moncloa")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[77082:79273] <- lat.sample

consumption_prediction_2016$lon[77082:79273] <- lon.sample

----------------------------------
  
  
# latina
  
"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_latina <- c(437414.87, 432167.06, 432171.40, 437307.27, 437414.87)

north_latina <- c(4472102.44, 4472098.71, 4468385.46, 4468288.57, 4472102.44)

frame_coor_latina<- data.frame(east_latina,north_latina)

utmcoor_latina<-SpatialPoints(cbind(frame_coor_latina$east_latina, frame_coor_latina$north_latina), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_latina<-spTransform(utmcoor_latina,CRS("+proj=longlat"))

frame_coor_latina$x <- coordinates(longlatcoor_latina)[,1]
frame_coor_latina$y <- coordinates(longlatcoor_latina)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_latina$y
lon.lims <- frame_coor_latina$x

n <- sum(consumption_prediction_2016$Distrito == "Latina")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[79274:81492] <- lat.sample

consumption_prediction_2016$lon[79274:81492] <- lon.sample

-------------------------------
  
# Carabanchel
  
"https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_carabanchel <- c(436213.58, 436337.47, 436943.17, 437051.52, 438638.71,438661.24, 440134.33, 439001.53,
                      438834.21, 437446.31, 436213.58)
                      
north_carabanchel <- c(4467890.13, 4470811.46, 4471447.87, 4473420.54, 4474244.54, 4472548.18, 4471851.32,
                       4470681.93, 4469102.46, 4469231.12, 4467890.13)

frame_coor_carabanchel<- data.frame(east_carabanchel,north_carabanchel)

utmcoor_carabanchel<-SpatialPoints(cbind(frame_coor_carabanchel$east_carabanchel, frame_coor_carabanchel$north_carabanchel), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_carabanchel<-spTransform(utmcoor_carabanchel,CRS("+proj=longlat"))

frame_coor_carabanchel$x <- coordinates(longlatcoor_carabanchel)[,1]
frame_coor_carabanchel$y <- coordinates(longlatcoor_carabanchel)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_carabanchel$y
lon.lims <- frame_coor_carabanchel$x

n <- sum(consumption_prediction_2016$Distrito == "Carabanchel")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[81493:83947] <- lat.sample

consumption_prediction_2016$lon[81493:83947] <- lon.sample

-----------------------------------
  
# Usera
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_usera <- c(439344.01, 441336.97, 441683.68, 438875.00, 439344.01)
                
north_usera <- c(4471047.46, 4471005.49, 4468369.47, 4468563.13, 4471047.46)

frame_coor_usera<- data.frame(east_usera,north_usera)

utmcoor_usera<-SpatialPoints(cbind(frame_coor_usera$east_usera, frame_coor_usera$north_usera), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_usera<-spTransform(utmcoor_usera,CRS("+proj=longlat"))

frame_coor_usera$x <- coordinates(longlatcoor_usera)[,1]
frame_coor_usera$y <- coordinates(longlatcoor_usera)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_usera$y
lon.lims <- frame_coor_usera$x

n <- sum(consumption_prediction_2016$Distrito == "Usera")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[83948:86521] <- lat.sample

consumption_prediction_2016$lon[83948:86521] <- lon.sample


--------------------------------------
  
  # Puente vallecas
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_puente_vallecas <- c(442881.19, 446226.80, 446256.20, 442776.36, 442881.19)

north_puente_vallecas <- c(4472499.98, 4472211.15, 4468991.59, 4469066.66, 4472499.98)

frame_coor_puente_vallecas<- data.frame(east_puente_vallecas,north_puente_vallecas)

utmcoor_puente_vallecas<-SpatialPoints(cbind(frame_coor_puente_vallecas$east_puente_vallecas, frame_coor_puente_vallecas$north_puente_vallecas), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_puente_vallecas<-spTransform(utmcoor_puente_vallecas,CRS("+proj=longlat"))

frame_coor_puente_vallecas$x <- coordinates(longlatcoor_puente_vallecas)[,1]
frame_coor_puente_vallecas$y <- coordinates(longlatcoor_puente_vallecas)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_puente_vallecas$y
lon.lims <- frame_coor_puente_vallecas$x

n <- sum(consumption_prediction_2016$Distrito == "Puente_Vallecas")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[86522:94421] <- lat.sample

consumption_prediction_2016$lon[86522:94421] <- lon.sample


--------------------------------------
  
  # Moratalaz
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_moratalaz<- c(443774.85, 447118.12, 446878.77, 443690.62, 443774.85)

north_moratalaz <- c(4473947.11, 4473827.03, 4472113.63, 4472427.11, 4473947.11)

frame_coor_moratalaz<- data.frame(east_moratalaz,north_moratalaz)

utmcoor_moratalaz<-SpatialPoints(cbind(frame_coor_moratalaz$east_moratalaz, frame_coor_moratalaz$north_moratalaz), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_moratalaz<-spTransform(utmcoor_moratalaz,CRS("+proj=longlat"))

frame_coor_moratalaz$x <- coordinates(longlatcoor_moratalaz)[,1]
frame_coor_moratalaz$y <- coordinates(longlatcoor_moratalaz)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_moratalaz$y
lon.lims <- frame_coor_moratalaz$x

n <- sum(consumption_prediction_2016$Distrito == "Moratalaz")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[94422:95571] <- lat.sample

consumption_prediction_2016$lon[94422:95571] <- lon.sample

------------------------
  
  # Ciudad lineal
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_ciudad_lineal <- c(443886.59, 446521.16, 446722.10, 444122.60, 443886.59)

north_ciudad_lineal <- c(4477586.98, 4477530.00, 4474668.37, 4474875.88, 4477586.98)


frame_coor_ciudad_lineal<- data.frame(east_ciudad_lineal,north_ciudad_lineal)

utmcoor_ciudad_lineal<-SpatialPoints(cbind(frame_coor_ciudad_lineal$east_ciudad_lineal, frame_coor_ciudad_lineal$north_ciudad_lineal), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_ciudad_lineal<-spTransform(utmcoor_ciudad_lineal,CRS("+proj=longlat"))

frame_coor_ciudad_lineal$x <- coordinates(longlatcoor_ciudad_lineal)[,1]
frame_coor_ciudad_lineal$y <- coordinates(longlatcoor_ciudad_lineal)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_ciudad_lineal$y
lon.lims <- frame_coor_ciudad_lineal$x

n <- sum(consumption_prediction_2016$Distrito == "Ciudad_Lineal")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[95572:113461] <- lat.sample

consumption_prediction_2016$lon[95572:113461] <- lon.sample 

---------------

  
  #HOrtaleza
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_hortaleza<- c(445024.33, 443856.55, 443352.65, 444664.71, 446441.33, 446567.50, 445024.33)

north_hortaleza <- c(4477888.87,4477851.13, 4482821.28, 4484373.45, 4484270.68, 4477903.80, 4477888.87) 


frame_coor_hortaleza<- data.frame(east_hortaleza,north_hortaleza)

utmcoor_hortaleza<-SpatialPoints(cbind(frame_coor_hortaleza$east_hortaleza, frame_coor_hortaleza$north_hortaleza), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_hortaleza<-spTransform(utmcoor_hortaleza,CRS("+proj=longlat"))

frame_coor_hortaleza$x <- coordinates(longlatcoor_hortaleza)[,1]
frame_coor_hortaleza$y <- coordinates(longlatcoor_hortaleza)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_hortaleza$y
lon.lims <- frame_coor_hortaleza$x

n <- sum(consumption_prediction_2016$Distrito == "Hortaleza")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[113462:139587] <- lat.sample

consumption_prediction_2016$lon[113462:139587] <- lon.sample


---------------------------
  
  
  #Villaverde
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_villaverde<- c(438876.24, 442485.47, 442411.99, 438821.64,438876.24)
                    

north_villaverde <- c(4468356.95, 4468003.21, 4464881.37, 4464871.96, 4468356.95)

frame_coor_villaverde<- data.frame(east_villaverde,north_villaverde)

utmcoor_villaverde<-SpatialPoints(cbind(frame_coor_villaverde$east_villaverde, frame_coor_villaverde$north_villaverde), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_villaverde<-spTransform(utmcoor_villaverde,CRS("+proj=longlat"))

frame_coor_villaverde$x <- coordinates(longlatcoor_villaverde)[,1]
frame_coor_villaverde$y <- coordinates(longlatcoor_villaverde)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_villaverde$y
lon.lims <- frame_coor_villaverde$x

n <- sum(consumption_prediction_2016$Distrito == "Villaverde")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[139588:161755] <- lat.sample

consumption_prediction_2016$lon[139588:161755] <- lon.sample 

--------------------------
  
  #Vallecas
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_vallecas<- c(444708.13, 450271.91, 450262.97, 444608.56, 444605.10, 444708.13)


north_vallecas <- c(4470348.96, 4470305.93, 4467710.89, 4468025.61, 4468115.98, 4470348.96)

frame_coor_vallecas<- data.frame(east_vallecas,north_vallecas)

utmcoor_vallecas<-SpatialPoints(cbind(frame_coor_vallecas$east_vallecas, frame_coor_vallecas$north_vallecas), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_vallecas<-spTransform(utmcoor_vallecas,CRS("+proj=longlat"))

frame_coor_vallecas$x <- coordinates(longlatcoor_vallecas)[,1]
frame_coor_vallecas$y <- coordinates(longlatcoor_vallecas)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_vallecas$y
lon.lims <- frame_coor_vallecas$x

n <- sum(consumption_prediction_2016$Distrito == "Vallecas")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[161756:168490] <- lat.sample

consumption_prediction_2016$lon[161756:168490] <- lon.sample 

-----------------------
  
  #Vicalvaro
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_vicalvaro<- c(447290.09, 454135.64, 454144.12, 447158.05, 447290.09)

north_vicalvaro <- c(4473548.40, 4473310.53, 4471328.09, 4471763.99, 4473548.40)

frame_coor_vicalvaro<- data.frame(east_vicalvaro,north_vicalvaro)

utmcoor_vicalvaro<-SpatialPoints(cbind(frame_coor_vicalvaro$east_vicalvaro, frame_coor_vicalvaro$north_vicalvaro), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_vicalvaro<-spTransform(utmcoor_vicalvaro,CRS("+proj=longlat"))

frame_coor_vicalvaro$x <- coordinates(longlatcoor_vicalvaro)[,1]
frame_coor_vicalvaro$y <- coordinates(longlatcoor_vicalvaro)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_vicalvaro$y
lon.lims <- frame_coor_vicalvaro$x

n <- sum(consumption_prediction_2016$Distrito == "Vicalvaro")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[168491:169489] <- lat.sample

consumption_prediction_2016$lon[168491:169489] <- lon.sample  

--------------------------------
  
  
  #San blas
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_san_blas<- c(448912.57, 448816.36, 446231.04, 446459.55, 448912.57)

north_san_blas <- c(4474768.35, 4477781.39, 4477695.23, 4474741.34, 4474768.35)

frame_coor_san_blas<- data.frame(east_san_blas,north_san_blas)

utmcoor_san_blas<-SpatialPoints(cbind(frame_coor_san_blas$east_san_blas, frame_coor_san_blas$north_san_blas), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_san_blas<-spTransform(utmcoor_san_blas,CRS("+proj=longlat"))

frame_coor_san_blas$x <- coordinates(longlatcoor_san_blas)[,1]
frame_coor_san_blas$y <- coordinates(longlatcoor_san_blas)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_san_blas$y
lon.lims <- frame_coor_san_blas$x

n <- sum(consumption_prediction_2016$Distrito == "San_Blas")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[169490:172190] <- lat.sample

consumption_prediction_2016$lon[169490:172190] <- lon.sample   


------------------
  
  
  #Barajas
  
  "https://www1.sedecatastro.gob.es/Cartografia/mapa.aspx?del=28&mun=92&refcat=28092A024000560000OK&final="

east_barajas<- c(452866.91, 448980.32, 449010.69, 452830.96, 452866.91)

north_barajas <- c(4483595.57, 4483520.91, 4477940.47, 4477891.37, 4483595.57)

frame_coor_barajas<- data.frame(east_barajas,north_barajas)

utmcoor_barajas<-SpatialPoints(cbind(frame_coor_barajas$east_barajas, frame_coor_barajas$north_barajas), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_barajas<-spTransform(utmcoor_barajas,CRS("+proj=longlat"))

frame_coor_barajas$x <- coordinates(longlatcoor_barajas)[,1]
frame_coor_barajas$y <- coordinates(longlatcoor_barajas)[,2]

# el número de coordendas será igual al número de consumidores de Arganzuela

lat.lims <- frame_coor_barajas$y
lon.lims <- frame_coor_barajas$x

n <- sum(consumption_prediction_2016$Distrito == "barajas")

lon.sample <- runif(n, min = min(lon.lims), max = max(lon.lims))

lat.lims.rads <- lat.lims / 360 * 2 * pi
h.lims <- sin(lat.lims.rads)
h.sample <- runif(n, min = min(h.lims), max = max(h.lims))

lat.sample <- asin(h.sample) / 2 / pi * 360

plot(lon.sample, lat.sample)

# creamos dos columnas de lat y longitud en el data frame de predicciones para asignar los valores calculados


consumption_prediction_2016$lat[172190:178598] <- lat.sample

consumption_prediction_2016$lon[172190:178598] <- lon.sample  

---------------------------------------
  
# guardamos el dataset de predicciones con las coordendas calculadas

  
write.csv(consumption_prediction_2016,file="consumption_pred_data_madrid_2016_by_coor.csv")
  
  
# cargamos ahora el paquete de ggmap para visualizar que tal queda

  
install.packages("ggmap")

library("ggmap")

install.packages("rgdal")

#cargamos el paquete rgdal, especialmente indicado para tratar los mapas tipo shape de http://centrodedescargas.cnig.es:

"http://centrodedescargas.cnig.es/CentroDescargas"

library("rgdal")

require(rgdal)


list.files()

map_madrid <- readOGR(dsn = ".", layer = "MANZANA")

map_madrid_vias <- readOGR(dsn = ".", layer = "PORTAL_PK")

map_call_madrid <- readOGR(dsn = ".", layer = "call2016")


class(map_madrid_vias)

class(map_call_madrid)


# convertimos el shape en un data frame

map_madrid_df <- fortify(map_madrid)

map_madrod_vias_df <- fortify(map_madrid_vias)

map_call_madrid_df <- fortify(map_call_madrid)

utmcoor_call<-SpatialPoints(cbind(map_call_madrid_df$long, map_call_madrid_df$lat), proj4string=CRS("+proj=utm +zone=30"))

longlatcoor_call<-spTransform(utmcoor_call,CRS("+proj=longlat"))

map_call_madrid_df$x <- coordinates(longlatcoor_call)[,1]
map_call_madrid_df$y <- coordinates(longlatcoor_call)[,2]

map_call_madrid_df$long <- map_call_madrid_df$x

map_call_madrid_df$lat <- map_call_madrid_df$y


# Ahora el shapefile se puede trazar como geom_path o geom_polygon.
# Los path manejan el recorte mejor. Los polígonos se pueden rellenar.
# Necesitamos la aes long, lat, y group.



map_dist_madrid <- ggplot() +  geom_path(data = map_madrid_df, aes(x = long, y = lat, group = group), color = 'grey', size = 0.1)

map_call_dist_madrid <- ggplot() +  geom_path(data = map_call_madrid_df, aes(x = long, y = lat, group = group), color = 'grey', size = 0.1)

print(map_dist_madrid)

class(water_dist_map)



water_dist_map <- map_call_dist_madrid + geom_point(data=consumption_pred_data_madrid_2016_by_coor, aes(x=consumption_pred_data_madrid_2016_by_coor$lon, 
                                  y=consumption_pred_data_madrid_2016_by_coor$lat, colour=consumption_pred_data_madrid_2016_by_coor$`2016/08`),
                                                 alpha = 0.3, show.legend = FALSE, size=0.1, color= "blue")
library("sp")

require(sp)
require(maptools)
library("maptools")

water_dist_map_sp <- shapefile(water_dist_map)

print(water_dist_map)

#es una primera aproximación a la versión gráfica, exploremos un poco más con leaflet

install.packages("leaflet")

install.packages("sp")

library ("leaflet")

#creemso un shape con los puntos de consumo

sp_consumtion_madrid<-SpatialPoints(cbind(consumption_pred_data_madrid_2016_by_coor$lat, consumption_pred_data_madrid_2016_by_coor$lon), proj4string=CRS("+proj=utm +zone=30"))


leaflet(data = map_call_madrid) %>% addTiles() %>% addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(fill = FALSE, stroke = TRUE, color = "#03F") %>% addLegend("bottomright", colors = "#03F", labels = "Consumption Water Point") %>% 
  setView(-3.7, 40.41, zoom = 12)
                                                                         
# creamos un leaflet con lso datos de matriz de consumos


mark_data <- data.frame(long= consumption_pred_data_madrid_2016_by_coor$lon[1:2000], lat= consumption_pred_data_madrid_2016_by_coor$lat[1:2000], val=consumption_pred_data_madrid_2016_by_coor$`2016/01`[1:2000])

str(mark_data)


leaflet(data = mark_data) %>% addTiles() %>% addMarkers(~long, ~lat, popup = ~as.character(val), label = ~as.character(val, clusterOptions = markerClusterOptions()))

leaflet(data = mark_data) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions())


