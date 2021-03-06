
# la base del analisis parte de un data set AguaH rescatado de la comuidad kaggle

"https://www.kaggle.com/jaeyoonpark/data-imputation-aguah-water-consumption/data"

# en algunso notebooks desarrollados hast aal fecha en torno ahas este data set, se centran sobre todo en la aproximaci�n en la sustituci�n de los
#NA/null data en base a criterios como interpolaciones o k- vecinos. Anque es un ejercicio interesante y podemos trasformar ese c�digo
# como un ejerciio auxiliar al tfm pero no como el grueso del proceso de limpieza, que optaremos por estrategias directas.


# cargamos algunas tablas auxiliares

library(readxl)
Consumos_Madrid_distrito <- read_excel("C:/Users/jmuro/Desktop/tfm/data raw/Consumos_Madrid_distrito.xls")
View(Consumos_Madrid_distrito)

write.csv(Consumos_Madrid_distrito,file="Consumos_Madrid_distrito.csv")

library(readxl)
Habitantes_Madrid_distrito <- read_excel("C:/Users/jmuro/Desktop/tfm/data raw/Habitantes_Madrid_distrito.xls")
View(Habitantes_Madrid_distrito)

write.csv(Habitantes_Madrid_distrito,file="habitantes_Madrid_distrito.csv")

#limpieza de datos

District_data<- read.csv("C:/Users/jmuro/Desktop/tfm/data raw/AguaH.csv", header=FALSE)

# Leemos el raw data de patrones de consumo

summary(District_data)
head(District_data)
class(District_data)

# Tenemos un data set donde la columnas V6-V89 corresponde a variables de una serie temporal desde enero 2009 hasta diciembre de 2015 mes a mes.
# hay otras varaibles secundarias com V1 que indica un c�digo de lectura, V2 indica el tipo de consumidor, V3 coeficiente no indentificado que puede hacer referencia a la fibilidad dela lectura del caudal�metro.
#V4 es el tipo de/modelo de caudal�metro utilizado. para comprobar al naturaleza de V5, donde hay un sesgo sisgnificativo, vamos comprobar si corresponde al sumatorio de todos los meses

library(readr)

install.packages("data.table")

library(data.table)

#una vez comprobada la poca inferencia de las columas mencionadas, paso a eliminar las coumnas que no aportan valor a los datos

dist_flow<-District_data[,-c(1,3,4,5)]

#en otro paso del analisis y sabiendo que son consumos medios aproximados los que estamos trabajando, las unidades de consumo mensual cuadran con 
#el orden de magnitud de m3/mes, sin discriminar el numero de habitantes por vivienda.

#paso a trabajar sobre los huecos en la data set, tanot nan como vacios.

# si hacemos un summary al �ltimo data set, podemos observar muchos ceros y datos perdidos, sobre toddo en los primeros a�os del muestreo

summary(dist_flow)

#la estrategia ser� rellenar esos huecos con la mediana de la serie del mes solo en las vacias, ya que es un dato asumible que un vivienda no registre consumo durante
# un mes, pero no que pierda el ping del caudal�metro (Vacio). tambien prodria tratarse de una vivienda vacia, por tanto no eliminamos ni sustituimos ceros.


str(dist_flow)

class(dist_flow[1,1])

#como se puede ver, cada columna del data frame ya es un factor, un vector de elementos que es toda la columna, que previsiblemente tiene unos niveles
#si analizamos los niveles de algunos elementos

class(dist_flow$V6[c(2)])


class(dist_flow$V6[c(1)])

#como muestra el an�lisis, los factores est�n categorizados en 306 niveles, con una codificai�n num�ricas e intrinseca en R que a priori no nos aporta ninguna informaci�n
# o al menos no tiene un sentido l�gico en la tarea que nos ocupa. Puede que sean lso ping del caudal�metro durante el resitro diario, con un cadencia determinada.

# el siguiente paso ser� trasformar todos estos factores en n�meros, con los que poder operar.Para ello Hemos de tener cuidado de cambiar primero el factor
# en caracteres y luego en num�rico, porque si no puede absorber los valores de lo niveles que no nos interesa


dist_flow[, c(2:85)] <- sapply(dist_flow[, c(2:85)],as.character)

dist_flow[, c(2:85)] <- sapply(dist_flow[, c(2:85)], as.numeric)


# como podemos observar, cada elemento de las columnas es un n�mero


class(dist_flow$V6[c(50)])

#guardamos

write.csv(dist_flow,file="dist_flow_clean_01.csv")

#Se cambia el encabezado de las variables temporales para que tengan m�s sentido
#la primera fila tambien la cambiadmos de nombre. Asigno un tipo de residencia aleatoria y en una segunda fase lo trataremos con el resto de datos.



dist_flow_clean_02$`Tipo Consumidor`[1]<- "DOMESTICO MEDIO"

colnames(dist_flow_clean_02)<-c("Tipo Consumidor", "2009/01","2009/02","2009/03","2009/04","2009/05","2009/06","2009/07", "2009/08","2009/09","2009/10","2009/11","2009/12",
                       "2010/01","2010/02","2010/03","2010/04","2010/05","2010/06","2010/07", "2010/08","2010/09","2010/10","2010/11","2010/12",
                       "2011/01","2011/02","2011/03","2011/04","2011/05","2011/06","2011/07", "2011/08","2011/09","2011/10","2011/11","2011/12",
                       "2012/01","2012/02","2012/03","2012/04","2012/05","2012/06","2012/07", "2012/08","2012/09","2012/10","2012/11","2012/12",
                       "2013/01","2013/02","2013/03","2013/04","2013/05","2013/06","2013/07", "2013/08","2013/09","2013/10","2013/11","2013/12",
                       "2014/01","2014/02","2014/03","2014/04","2014/05","2014/06","2014/07", "2014/08","2014/09","2014/10","2014/11","2014/12",
                       "2015/01","2015/02","2015/03","2015/04","2015/05","2015/06","2015/07", "2015/08","2015/09","2015/10","2015/11","2015/12")
                       
dist_flow_clean_01[, c(1)] <- sapply(dist_flow_clean_01[, c(1)],as.character)

#vemos que tal van los datos


summary(dist_flow_clean_01)

#vemos que la media es bastante elevada, probablmente porque hay maximos muy altos, que explorarremos si se trata de consumo especiales o outliner

names(dist_flow_clean_01)

write.csv(dist_flow_clean_01,file="dist_flow_clean_02.csv")

summary(dist_flow_clean_02)

dist_flow_clean_02<-dist_flow_clean_02[,-c(1)]

max(dist_flow_clean_02$`2009/01`)

summary(dist_flow_clean_02)

write.csv(dist_flow_clean_01,file="dist_flow_clean_02.csv")

median(dist_flow_clean_02$`2009/01`, na.rm=TRUE)

#del analisis de los m�ximos de la serie se extrae que los m�ximos consumos de agua se deben a consumidores industriales, lo cual es un dato
#esperable, pero bienvenido porque implica que el data set tiene sentido al menos en este punto

max(dist_flow_clean_02$`2009/01`, na.rm=TRUE)

which.max(dist_flow_clean_02$`2009/01`)

dist_flow_clean_02$`Tipo Consumidor`[2640]


# despu�s del an�lisis de los par�metros principales, observo dque los outliners son entes con sentido, ya que una industria media dentro de un distrito
#puede consumir este volumen de agua. Pasamos a pasar todas las columnas para sustituir por la mediana de la serie, un parametro que est� m�s equilibrado
#con el consumo medio del consumidos residente.

library(dplyr)

dist_flow_clean_03<-dist_flow_clean_02 %>% mutate_at(vars(starts_with("20")), funs(ifelse(is.na(.),median(., na.rm = TRUE),.)))

write.csv(dist_flow_clean_03,file="dist_flow_clean_03.csv")

#a partir de aqu� se puede comprobar que ya tenemos un dataframe m�s ordenado y limpio listo para comenzar a sacar patrones y ciencia.


head(dist_flow_clean_03)

summary(dist_flow_clean_03)

# el siguiente paso ser� calcular los pesos anuales por distrito y totalizados para poder aplicarlos a un distrito tipo de Madrid y analizar los consumos 
#con el patr�n heredado y limpio

sum(dist_flow_clean_03$`2009/01`)
Total_2009<-colSums (select (dist_flow_clean_03, contains ("2009")))
Total_2009
sum(Total_2009,1:12)
