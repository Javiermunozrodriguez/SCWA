# En este capitulo del trabajo , estudiaremo un modelo de regresión generalizada

library(readr)

#por favor, el siguinete dataset, hay que cargarlso desde al carpeta que esté alojado en su servidor

Madrid_compsuption_profile <- read_csv("C:/Users/jmuro/Desktop/tfm/step_03_data clean first analisis/Madrid_compsuption_profile.csv", 
                                       col_types = cols(X1 = col_skip()))
View(Madrid_compsuption_profile)

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


# primero volvemos a echar un vistazo sobre los datos que hemos obtenido de los trabajos en pasos anteriores


boxplot(Madrid_compsuption_profile$`2009/02`, main="2009_02", sub=paste("Outlier rows: ", boxplot.stats(Madrid_compsuption_profile$`2009/02`)$out))  # box plot for '2009/01'


qplot(Madrid_compsuption_profile$consumer_vector,Madrid_compsuption_profile$`2009/01`,data=Madrid_compsuption_profile,geom="boxplot", ylog = TRUE)

----------------------------------------------------------------

###CONSTRUCCIÓN DE LA MATRIZ DE SERIE TEMPORAL###

# vamos a considerar como series temporales cada fila de consumidor que tenemos asignada 

s1<- ts(c(Madrid_compsuption_profile[1,2:85]),frequency=12,start=c(2009,1))


# para todo el dataset, obtenemos cada serie como una columna dentro de la matriz, donde cada columna es la serie de variables independientes
# con resultados de los consumos de los 178598 para cada uno de los 84 meses entre 2009-2015

series<-apply(Madrid_compsuption_profile[-1], 1, FUN = function(x) ts(x, start = c(2009, 01), frequency = 12))


class(series)

series[,1]


par(mfrow=c(2,2))

plot( series[,1])
plot( series[,2])
plot( series[,3])
plot( series[,4])
 

# podemos estudiar un poco más estacionalidad y tendencias de las series

des_s1 <- decompose(ts(series[,1], frequency = 12, start=c(2009,01)))
des_s2 <- decompose(ts(series[,2], frequency = 12, start=c(2009,01)))
des_s3 <- decompose(ts(series[,3], frequency = 12, start=c(2009,01)))
des_s4 <- decompose(ts(series[,4], frequency = 12, start=c(2009,01)))

par(mfrow=c(2,2))

plot(des_s1)
plot(des_s2)
plot(des_s3)
plot(des_s4)

# podemos utilizar tambien el comando stl para comprender al periocidad de la serie

des_s1_stl=stl((ts(series[,1], frequency = 12, start=c(2009,01))), s.window="periodic")
str(des_s1_stl)

plot(des_s1_stl)

----------------------------------

  ###PRUEBA DE MODELOS###

#pasamso a desarrollar la regresion lineal de la varaible Como primer argumento de lm se coloca la fórmula variable dependiente ~ variable independiente. 
#Como segundo argumento se indica el conjunto de datos que se usará para construir el modelo. vamos a hacer varia aproximaciones y comparar modelso para
#estimar cual es l amejor a proxomación con lo desarrollado arriba.



# pRIMERO ensayamos un modelo lineal con algunas de las varaibles,  agrupando con la estimación de la varaible dependiente `2015/01` como

# un resultado de las varaibles independientes de los meses de enero de años anteriores

ggplot(Madrid_compsuption_profile, aes(x = `2009/01`, y = `2015/01`)) + geom_point() + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)+scale_x_log10() +scale_y_log10() 

modeloLogit=lm(cbind(Madrid_compsuption_profile$`2015/01`)~`2009/01`+`2010/01`+ `2011/01`+`2012/01`+`2013/01`+`2014/01`, data=Madrid_compsuption_profile)
summary(modeloLogit)

# convertimos en un factor la columna de consumidores 

class(Madrid_compsuption_profile$consumer_vector)

Madrid_compsuption_profile$consumer_vector=as.factor(Madrid_compsuption_profile$consumer_vector) #factorizamos el tipo de consumidor


--------------

#probamos ahora con otro modelo de regresión logística genralizada, suponiendo como variable dependiente el ultimo mes de la serie

modeloGlobal=glm(Madrid_compsuption_profile$`2015/12` ~ ., data = Madrid_compsuption_profile)
summary(modeloGlobal)

forecast_cons01_model<- forecast(modeloGlobal, newdata=Madrid_compsuption_profile, h=12)
forecast_cons1

head(Madrid_compsuption_profile$`2015/12`)

anova(modeloLogit,modeloGlobal)

-----------------
# en este punto merece la pena rescatar los modelos ARIMA de las serires temporales.Existe una amplia literatura sobre la aplicación de modelos ARIMA en series temporales. R 
# ya que cuenta con numerosos procedimientos para la estimación, validación y predicción con estos modelos, especialmente diseñados para las serires
# temporales
  
# El uso adecuado de algunas funciones que ya hemos visto (acf(), pacf() y diff()) permite al usuario identificar posibles modelos candidatos para representar una serie temporal estacionaria.

#Estos modelos pueden estimarse fácilmente en R mediante la función arima().

#Por último, señalemos que la función arima.sim() permite simular una serie temporal de acuerdo con el modelo ARIMA especificado por el usuario.

# podemos ver la tendencia de muestras aleatorias de la serie de observaciones

par(mfrow=c(2,2))


plot( series[,130000])
plot(diff(ts(series[,130000], frequency = 12, start=c(2009,01))))

plot( series[,150000])
plot(diff(ts(series[,150000], frequency = 12, start=c(2009,01))))



# podemos estimar el modelo a partir de los datos con la herramienta arima, a y ajustar lso coeficientes con el estudio de los lag y residuos


ajuste_s1=arima((ts(series[,1], frequency = 12, start=c(2009,01))),order=c(1,1,1))
ajuste_s1

#La función tsdiag() muestra que no queda estructura de autocorrelación en los residuos del ajuste_s1, tambien prbaremos con la función de auto.arima

#creamso ahora un ajuste con auto.arima (lm), recojiendo la estacionalidad que se deriba del estudio de diff del perfil del consumidor

library(forecast)

fit_proba_s1<-auto.arima((ts(series[,1], frequency = 12, start=c(2009,01))), seasonal=TRUE)

fit_proba_s1

tsdisplay(residuals(fit_proba_s1), lag.max=45, main='(1,1,1) Model Residuals')

#vamos a hacer un comparación entre las dos estrategisas para elegir modelo de ARIMA



fit_proba_s1$residuals

smoothScatter(fit_proba_s1$residuals)
hist(fit_proba_s1$residuals)
qqnorm(fit_proba_s1$residuals); qqline(fit_proba_s1$residuals,col=2)
confint(fit_proba_s1,level=0.95)

--------------------
  
  ### TRAIN & TEST####


#crememos unos grupos de test y entrenamiento para testar lo modelos aplicados, sobre la varaible dependiente 2015/12, mediante el estudio
#de modelos de regresión logística que se encuentran integrados dentro del propio paquete de auto.arima


set.seed(1234) 
SAMPLE = sample.split(Madrid_compsuption_profile$`2015/12`, SplitRatio = .75)
Madrid_consumption_train = subset(Madrid_compsuption_profile, SAMPLE == TRUE)
Madrid_consumption_test = subset(Madrid_compsuption_profile, SAMPLE == FALSE)


model_glm_mtrain=glm(`2015/12`~ . , data = Madrid_consumption_train)

summary(model_glm_mtrain)

Madrid_consumption_train$prediccion=predict(model_glm_mtrain)

Predauxiliar= prediction(Madrid_consumption_train$prediccion, Madrid_consumption_train$`2015/12`)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloLogittrain = as.numeric(auc.tmp@`2015/12`.values)
aucModeloLogittrain

# como hemos conprobado, al ser un modelo de para series temorales, no podemo smos testear el modelo de regresion econ el metodo
#arriba descrito al no tratarse de objetos predictivos de clase binaria. pasamo a desarrollar el modelo predictivo para cada serie  asumeinto del 
#paque ARIMA.

------------------------
  
  ###ELECCION DEL MODELO AUTO.ARIMA####

#Hay un patrón claro presente en ACF / PACF y las parcelas de residuales de modelo que se repiten en el retraso 4. Esto sugiere que nuestro modelo podría estar mejor con una especificación diferente, como p = 7 o q = 7.
# Podemos repetir el proceso de ajuste que permite el componente MA (36) y examinar las gráficas de diagnóstico nuevamente. Esta vez, no hay autocorrelaciones significativas presentes.
#Si el modelo no se especifica correctamente, eso generalmente se reflejará en los residuos en forma de tendencias, asimetría o cualquier otro patrón no capturado por el modelo.
# Idealmente, los residuos deben verse como ruido blanco, lo que significa que normalmente se distribuyen. Se puede usar una función de conveniencia tsdisplay () para trazar estos diagnósticos del modelo.
#Los gráficos de Residuals muestran un rango de error más pequeño, más o menos centrado alrededor de 0. Podemos observar que el AIC también es más pequeño para la estructura (1, 1, 36):

ajuste_s1=arima((ts(series[,1], frequency = 12, start=c(2009,01))),order=c(1,1,36))
ajuste_s1

tsdisplay(residuals(ajuste_s1), lag.max=45, main='(1,1,36) Model Residuals')

tsdiag(ajuste_s1)


---------------------------------

# podemos hacer predicciones de este modelo a traves de forecast() y predict() para el año 2016


library(forecast)

forecast_cons1<- forecast(fit_proba_s1, h=12)# dar robusted  por los potenciales outliers del modelo


#estudiamos las prodiedades de la predicción

forecast_cons1$method
forecast_cons1$model
forecast_cons1$mean
forecast_cons1$lower
forecast_cons1$upper

# el modelo elegido por la maquina e el "ARIMA(2,1,2)(1,0,0)[12]"

plot(forecast(fit_proba_s1), xlab="year", ylab= "m3", main ="forecast for consumer 01 ARIMA (1,1,36)")

# se puede observar en la curva de forecast que las predicciones de ARIMA son menos precisas a medida que avanza la serie a predecir
# en los meses relativos a 2016 y 2017, lo que implica que debemos mejorar el modelo en el siguiente paso, pero que es un buena primera aproximación para asignar 

# las variables dependientes. como vemos la serie tiene estacionalidad, aunque la tendencia el claramente a la baja. 


# tambien podemos aproximar la sere con la herramienta predict ()

prediction_cons01 <- predict(ajuste_s1,n.ahead=12)

prediction_cons01$pred #predicciones con predict()
prediction_cons01$se  #errores en al prediccion con predict

# como se puede ver , el porcentaje de error medio de forecast, con el valor que aceptariamos como una predicción plausible, es un serietemporal.

# venimos trabajando algunas lineas el data frame como serie temporal para poder aprovechar las herramientas como ARIME, donde encajamos un modelo de predicción 
#forecast y predic, la idea a partir de aquí es converir de nuevo en vector la media de la predicción y encajar dentro de un nuevo dataframe 
#que será el data set de predicciones en 2016

class(forecast_cons1$mean)

final_pred_s01 <- c(as.numeric(forecast_cons1$mean))

class(final_pred_s01)


# vamos a aplicar el modelo predictivo ARIMA para todas las series de consumidores, y generar las varibles dependientes
#de lo que será la matriz de predicciones, 
#que posteriormente convertiremos a un data frame y utilizaremos como test para el accurancy final

# el loop que se desarrolla a continuación, requiere de bastante potencia de computación, ya que en una sola linea traduce la serie temporal
# genera el modelo más ajustado con auto.arima, precide el vector de tiempo del 2016 y luego convierte en vector y lo asigna  a la matriz
# de consumos futuros, antes de que la traduzcamos a data frame. Por eso, la primera aproximación es partir el cálculo en dieciocho(18) matrices, y luego hacer un joint o un merge


pred_matrix_2016 <- matrix(NA,nrow=10000,ncol=12)

for(i in 5001:10000){
  pred_matrix_2016[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}


pred_matrix_2016_01 <- as.data.frame(pred_matrix_2016)

colnames(pred_matrix_2016_01)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_01 <- pred_matrix_2016_01%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_01,file="pred_matrix_2016_01.csv")


#vemos que tal ha quedado el calculo del forecasting de consumos de los 10.000 primeros consumidores


# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a lso [10001-20000]

pred_matrix_2016_02 <- matrix(NA,nrow=20000,ncol=12)

for(i in 10001:20000){
  pred_matrix_2016_02[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_02 <- as.data.frame(pred_matrix_2016_02)

colnames(pred_matrix_2016_02)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_02 <- pred_matrix_2016_02 [-c(1:10000),]

pred_matrix_2016_02 <- pred_matrix_2016_02%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_02,file="pred_matrix_2016_02.csv")


# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a lso [20001-30000]


pred_matrix_2016_03 <- matrix(NA,nrow=30000,ncol=12)

for(i in 20001:30000){
  pred_matrix_2016_03[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_03 <- as.data.frame(pred_matrix_2016_03)

colnames(pred_matrix_2016_03)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_03 <- pred_matrix_2016_03 [-c(1:20000),]

pred_matrix_2016_03 <- pred_matrix_2016_03%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_03,file="pred_matrix_2016_03.csv")

# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [30001-40000]


pred_matrix_2016_04 <- matrix(NA,nrow=40000,ncol=12)

for(i in 30001:40000){
  pred_matrix_2016_04[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_04 <- as.data.frame(pred_matrix_2016_04)

colnames(pred_matrix_2016_04)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_04 <- pred_matrix_2016_04 [-c(1:30000),]

pred_matrix_2016_04 <- pred_matrix_2016_04%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_04,file="pred_matrix_2016_04.csv")

# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [40001-50000]


pred_matrix_2016_05 <- matrix(NA,nrow=50000,ncol=12)

for(i in 40001:50000){
  pred_matrix_2016_05[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_05 <- as.data.frame(pred_matrix_2016_05)

colnames(pred_matrix_2016_05)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_05 <- pred_matrix_2016_05 [-c(1:40000),]

pred_matrix_2016_05 <- pred_matrix_2016_05%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_05,file="pred_matrix_2016_05.csv")

# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [50001-60000]


pred_matrix_2016_06 <- matrix(NA,nrow=60000,ncol=12)

for(i in 50001:60000){
  pred_matrix_2016_06[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_06 <- as.data.frame(pred_matrix_2016_06)

colnames(pred_matrix_2016_06)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_06 <- pred_matrix_2016_06 [-c(1:50000),]

pred_matrix_2016_06 <- pred_matrix_2016_06%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_06,file="pred_matrix_2016_06.csv")


# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [60001-70000]


pred_matrix_2016_07 <- matrix(NA,nrow=70000,ncol=12)

for(i in 60001:70000){
  pred_matrix_2016_07[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_07 <- as.data.frame(pred_matrix_2016_07)

colnames(pred_matrix_2016_07)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_07 <- pred_matrix_2016_07 [-c(1:60000),]

pred_matrix_2016_07 <- pred_matrix_2016_07%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_07, file = "pred_matrix_2016_07.csv")



#continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [70001-80000]


pred_matrix_2016_08 <- matrix(NA,nrow=80000,ncol=12)

for(i in 70001:80000){
  pred_matrix_2016_08[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_08 <- as.data.frame(pred_matrix_2016_08)

colnames(pred_matrix_2016_08)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_08 <- pred_matrix_2016_08 [-c(1:70000),]

pred_matrix_2016_08 <- pred_matrix_2016_08%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_08,file="pred_matrix_2016_08.csv")


# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [80001-90000]


pred_matrix_2016_09 <- matrix(NA,nrow=90000,ncol=12)

for(i in 80001:90000){
  pred_matrix_2016_09[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_09 <- as.data.frame(pred_matrix_2016_09)

colnames(pred_matrix_2016_09)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_09 <- pred_matrix_2016_09 [-c(1:80000),]

pred_matrix_2016_09 <- pred_matrix_2016_09%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_09, file = "pred_matrix_2016_09.csv")


#continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [90001-100000]


pred_matrix_2016_10 <- matrix(NA,nrow=100000,ncol=12)

for(i in 90001:100000){
  pred_matrix_2016_10[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_10 <- as.data.frame(pred_matrix_2016_10)

colnames(pred_matrix_2016_10)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_10 <- pred_matrix_2016_10 [-c(1:90000),]

pred_matrix_2016_10 <- pred_matrix_2016_10%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_10,file="pred_matrix_2016_10.csv")


# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [100001-110000]


pred_matrix_2016_11 <- matrix(NA,nrow=110000,ncol=12)

for(i in 100001:110000){
  pred_matrix_2016_11[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_11 <- as.data.frame(pred_matrix_2016_11)

colnames(pred_matrix_2016_11)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_11 <- pred_matrix_2016_11 [-c(1:100000),]

pred_matrix_2016_11 <- pred_matrix_2016_11%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_11, file = "pred_matrix_2016_11.csv")


#continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [110001-120000]


pred_matrix_2016_12 <- matrix(NA,nrow=120000,ncol=12)

for(i in 110001:120000){
  pred_matrix_2016_12[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_12 <- as.data.frame(pred_matrix_2016_12)

colnames(pred_matrix_2016_12)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_12 <- pred_matrix_2016_12 [-c(1:110000),]

pred_matrix_2016_12 <- pred_matrix_2016_12%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_12,file="pred_matrix_2016_12.csv")


# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [120001-130000]


pred_matrix_2016_13 <- matrix(NA,nrow=130000,ncol=12)

for(i in 120001:130000){
  pred_matrix_2016_13[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_13 <- as.data.frame(pred_matrix_2016_13)

colnames(pred_matrix_2016_13)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_13 <- pred_matrix_2016_13 [-c(1:120000),]

pred_matrix_2016_13 <- pred_matrix_2016_13%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_13, file = "pred_matrix_2016_13.csv")


#continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [130001-140000]


pred_matrix_2016_14 <- matrix(NA,nrow=140000,ncol=12)

for(i in 130001:140000){
  pred_matrix_2016_14[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_14 <- as.data.frame(pred_matrix_2016_14)

colnames(pred_matrix_2016_14)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_14 <- pred_matrix_2016_14 [-c(1:130000),]

pred_matrix_2016_14 <- pred_matrix_2016_14%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_14,file="pred_matrix_2016_14.csv")


# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [140001-150000]


pred_matrix_2016_15 <- matrix(NA,nrow=150000,ncol=12)

for(i in 140001:150000){
  pred_matrix_2016_15[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_15 <- as.data.frame(pred_matrix_2016_15)

colnames(pred_matrix_2016_15)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_15 <- pred_matrix_2016_15 [-c(1:140000),]

pred_matrix_2016_15 <- pred_matrix_2016_15%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_15, file = "pred_matrix_2016_15.csv")




#continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [150001-160000]


pred_matrix_2016_16 <- matrix(NA,nrow=160000,ncol=12)

for(i in 150001:160000){
  pred_matrix_2016_16[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_16 <- as.data.frame(pred_matrix_2016_16)

colnames(pred_matrix_2016_16)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_16 <- pred_matrix_2016_16 [-c(1:150000),]

pred_matrix_2016_16 <- pred_matrix_2016_16%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_16,file="pred_matrix_2016_16.csv")


#continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los consumidores [160001-170000]


pred_matrix_2016_17 <- matrix(NA,nrow=170000,ncol=12)

for(i in 160001:170000){
  pred_matrix_2016_17[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_17 <- as.data.frame(pred_matrix_2016_17)

colnames(pred_matrix_2016_17)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_17 <- pred_matrix_2016_17 [-c(1:160000),]

pred_matrix_2016_17 <- pred_matrix_2016_17%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_17,file="pred_matrix_2016_17.csv")


# continuamos con la produccion con los siguientes productos de datos aplicando el modelo a los últimos consumidores de la serie [170001-178598]


pred_matrix_2016_18 <- matrix(NA,nrow=178598,ncol=12)

for(i in 170001:178598){
  pred_matrix_2016_18[i,] <- c(as.numeric((forecast((auto.arima((ts(series[,i], frequency = 12, start=c(2009,01))), seasonal=TRUE)), h=12))$mean))
}

pred_matrix_2016_18 <- as.data.frame(pred_matrix_2016_18)

colnames(pred_matrix_2016_18)<-c("2016/01","2016/02","2016/03","2016/04","2016/05","2016/06","20116/07", "2016/08","2016/09","2016/10","2016/11","2016/12")

pred_matrix_2016_18 <- pred_matrix_2016_18 [-c(1:170000),]

pred_matrix_2016_18 <- pred_matrix_2016_18%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(round(.,digits=2)))

write.csv(pred_matrix_2016_18, file = "pred_matrix_2016_18.csv")




# juntamos ahora todos los resultados parciales en un unicco dataframe como base de predicciones para el año 2016
# cabe estudiar una forma más elegante de hacerlo, pero tras varias pruebas y por mantener limpieza en el joint, 
#calculo una comumna de indices y luego inserto y agrupo por esos indices de consumidor




id <- c(1:10000)

pred_matrix_2016_01_id_final <- cbind(id,pred_matrix_2016_01)

id <- c(10001:20000)

pred_matrix_2016_02_id_final <- cbind(id,pred_matrix_2016_02)

id <- c(20001:30000)

pred_matrix_2016_03_id_final <- cbind(id,pred_matrix_2016_03)

id <- c(30001:40000)

pred_matrix_2016_04_id_final <- cbind(id,pred_matrix_2016_04)

id <- c(40001:50000)

pred_matrix_2016_05_id_final <- cbind(id,pred_matrix_2016_05)

id <- c(50001:60000)

pred_matrix_2016_06_id_final <- cbind(id,pred_matrix_2016_06)

id <- c(60001:70000)

pred_matrix_2016_07_id_final <- cbind(id,pred_matrix_2016_07)

id <- c(70001:80000)

pred_matrix_2016_08_id_final <- cbind(id,pred_matrix_2016_08)

id <- c(80001:90000)

pred_matrix_2016_09_id_final <- cbind(id,pred_matrix_2016_09)

id <- c(90001:100000)

pred_matrix_2016_10_id_final <- cbind(id,pred_matrix_2016_10)

id <- c(100001:110000)

pred_matrix_2016_11_id_final <- cbind(id,pred_matrix_2016_11)

id <- c(110001:120000)

pred_matrix_2016_12_id_final <- cbind(id,pred_matrix_2016_12)

id <- c(120001:130000)

pred_matrix_2016_13_id_final <- cbind(id,pred_matrix_2016_13)

id <- c(130001:140000)

pred_matrix_2016_14_id_final <- cbind(id,pred_matrix_2016_14)

id <- c(140001:150000)

pred_matrix_2016_15_id_final <- cbind(id,pred_matrix_2016_15)

id <- c(150001:160000)

pred_matrix_2016_16_id_final <- cbind(id,pred_matrix_2016_16)

id <- c(160001:170000)

pred_matrix_2016_17_id_final <- cbind(id,pred_matrix_2016_17)

id <- c(170001:178598)

pred_matrix_2016_18_id_final <- cbind(id,pred_matrix_2016_18)


join_full_pred_matrix <- bind_rows(pred_matrix_2016_01_id_final, pred_matrix_2016_02_id_final, pred_matrix_2016_03_id_final,
                              pred_matrix_2016_04_id_final, pred_matrix_2016_05_id_final, pred_matrix_2016_06_id_final,
                              pred_matrix_2016_07_id_final, pred_matrix_2016_08_id_final, pred_matrix_2016_09_id_final,
                              pred_matrix_2016_10_id_final, pred_matrix_2016_11_id_final, pred_matrix_2016_12_id_final,
                              pred_matrix_2016_13_id_final, pred_matrix_2016_14_id_final, pred_matrix_2016_15_id_final,
                              pred_matrix_2016_16_id_final, pred_matrix_2016_17_id_final, pred_matrix_2016_18_id_final
                              ) %>%  group_by(id) 
  
  
# Durante el análisis se detecta que por el modelo logístico elegido, las tendencias de algunos consumos bajos se han 
# predicho hacia valores negativos. Es sin duda un síntoma de que el algoritmo de juste ha de ser depurado, pero como medida 
# en este punto vamos a poner en positivo todos estos valores además de añadir el vecor de perfiles de consumidor

consumption_prediction_2016 <- join_full_pred_matrix[,-c(1)]

consumer_vector<- (Madrid_compsuption_profile$`consumer_vector`)  

consumption_prediction_2016<-cbind(consumer_vector,consumption_prediction_2016)


consumption_prediction_2016 <- consumption_prediction_2016%>%mutate_at(.vars = vars(`2016/01`:`2016/12`),.funs = funs(abs(. )))

write.csv(consumption_prediction_2016, file = "consumption_prediction_2016.csv")


# vamos a hacer algunas comparaciones con datos reales, veamos la suma de consumo para 2016 en los datos del Ayto. De Madrid

Total_2016<-colSums (select (consumption_prediction_2016, contains ("2016")))

sum(Total_2016,1:12)

#Vemos que el consumo total se cifra en 119.575.111 m3/año sin tener en cuenta las perdidas en al red, que podemos estimar entre

#un 15 -20 %, el coste por metro cubico medio en el año 2016 fue de 1,82 ???/m3


"http://www-2.munimadrid.es/CSE6/control/seleccionDatos?numSerie=14030200040"


#la cifra de cconsumo de agua en cidad de Madrid en el año 2016 fue de 148.439.675 m3 por tanto un accurancy bruto del modelo predictivo


accurancy_pred_vs_real = (sum(Total_2016,1:12)/148439675)

accurancy_pred_vs_real


# para poder ser más fiel al patron de consumo real de la ciudad, hemos de ser capaces de acceder a lso datos por consumidores

# esto implica que la preción del algoritmo desarrollado pude aumentar o disminuir dea cuerdo a varios parametros, tales como el fraude, 

# que aportaria un incremento en el consumo de agua sin qu este fuera detectado por las predicciones, siendo un un spin off del TFM que nos ocupa.


# un 80,55 % del consumo real a la baja. Pongamos como hipotesis las pérdidas medias registradas en la red ese año (15,06 %)

# y devido a la desviación con el consumo real, demos además un margen del 20 % para reserva, el consumo final previsto según

# el modelo desarrollado sería de:


pred_mayorada <- sum(Total_2016,1:12)*1.1506*1.2

pred_mayorada

#

# esto supone un total de consumo mayorado de 165.103.889 m3 para la ciudad de Madrid. Teniendo en cuenta la disposición desde embalses y captaciones
# para ese año 

"http://www-2.munimadrid.es/CSE6/control/mostrarDatos"

# hablamos de 491.000.000 m3 que se pusieron a disposición el red de distribución, depósitos, y agua que tubo que tratarse para su disposición

# sin tener en cuenta la carga de la red de 4.000 km de la ciudad de MAdrid, aunque tubieramso un volumen de respaldo del dble de este consumo previsto y mayorado

# por reservas para al red de distrubución y el resto de poblaciones periféricas a MAdrid, siendo conservadores

porcent_diff_captation_vs_consuption <- 491000000- 2.5*pred_mayorada

porcent_diff_captation_vs_consuption

# esto supone más de 160 Hm3 que se están captando, potabilizando, bombeando y almacenando en la red y depósitos que hay que mantener

# teneindo el cuanta precio medio del m3, esto supone:


prev_ahorro_red_distribución_2016 <- porcent_diff_captation_vs_consuption*1.82

prev_ahorro_red_distribución_2016


# estamos hablando de 142 Millones de euros!!!, que la empresa de gestion del agua puede ahorrar y emplear en un pequeña parte de ese ahorro

# en fortalecer su departamento de analisis inteligente de las redes de distribución para la ciudad de MAdrid y datascience


# en el año 2017 el CYII realizó una serie de mejoras en la red que bajaraonsustancialmente las pérdidas según su s publicaciones hasta dicras de 

#3.4 % de la red, cifra aún por contrastar cuando los resultados del año 2017 estén disonibles.



# la conclusión es que este TFM se abre como un aprimera aproximación al problema de gestión de agua en las ciudades. aplicar ciencia de datos junto 

# junto con el potencial que arroja las inicia tevas como el proyecto iWESLA

"https://www.esmartcity.es/2017/02/09/gestion-inteligente-agua-madrid-proyecto-europeo-iwesla"

"http://iwesla.iot4water.com/"


# programaas de adquisición de datos como los proyectados en iwesla, pueden facilitar al aplicación de modelso como el aquí enunciado

# para futuras tecnicas de prediccion como clustering en tiempo real, ahorrarían milllones de euros al contribuyente mejorando las condiciones de suministro.