
                                                  ###  Smart City Water Analitycs  ####
                                                  
                                                  

**Abstract

The study consisted of two stages, in the first one the datasets were split by months and analyzed separately to evaluate the features presented in each one. To evaluate the representative behavior for city, the two dataset will be analyzed to find the water foot print who share the same pattern in behavior. In the second stage different regression models were applied to the data to find the relation between the demand patterns and the meter account variables. This projects shall be considerer as mixed of real data from Madrid statictis and random consumption in middle city by neighborhood groups, in order to save the public mottion of this task.

**Introducción

A partir del conocimiento del estado del arte en las técnicas de Big Data existentes y Se elegirán las herramientas más adecuadas para la predicción del consumo de redes de distribución de agua de Madrid. Se procesaron diferentes conjuntos de datos a los fines del estudio, dos de ellos correspondientes a la ciudad de madrid y el patron de consumos heredado de un aciudad tipo de acceso público, incluyendo variables vinculadas a la cuenta del consumidor asociada a los medidores. 

El estudio constó de dos etapas, en la primera, los conjuntos de datos se dividieron por temporada y Se analizan por separado para evaluar las características presentadas en cada uno. Para evaluar la comportamiento representativo y virtual de la Ciudad de Madrid, se analizaron las etiquetas de agrupamiento para encontrar los grupos que tengan el mismo patrón de comportamiento. En la segunda etapa se aplicaron modelos a los datos para encontrar la relación entre los patrones de demanda y losVariables de la cuenta del medidor.

**Descripción de los datos de entrada

la base del analisis parte de un data set AguaH rescatado de la comuidad kaggle, csv alojado en:

"https://www.kaggle.com/jaeyoonpark/data-imputation-aguah-water-consumption/data"

En algunos notebooks desarrollados hasta la fecha en torno a este data set, se centran sobre todo en la aproximación en la sustitución de los NA/null data en base a criterios como interpolaciones o k- vecinos. Anque es un ejercicio interesante y podemos trasformar ese código
como un ejerciio auxiliar al tfm, pero no como el grueso del proceso de limpieza, que se opta por estrategias directas. Los datos en bruto presentan varios problemas en la clase de los registros de consumo (son factores), en la cantidade variables qeu no aportan información, y 
en la candidad de datos perdidos, sobre todo en lsopormeros años de registro 2009-2011, como se puede ver el link de kaggle. la linpieza se ha explica paso a paso en el scrip de esata fase. El leguaje elegido ha sido R por la naturaleza potencial estadistica del problema.

**Metodología

Las serires de datos disponibles en las bases de lso organismos oficiales de Madrid aportan inforamción hasta el 2016. en base a esto se ha escalado el patron de consumo en base a lso totalizados de distritos de Madrid y adecaudo la serire temporal de datos para el periodo
de 2009-2015. En resumen se han trabajado 85 variables independientes con observaciones entorno a lso 180.000 puntos de consumo.

Durante el tramiento de datos se ha filtrado la información para conseguir el tipo de series (diccionarios) que necesita el modelo de lafase de análisis, y asignado valores a datos NA/null en base a lso detalles redactados en el código.

En la fase de análisis se ha empleado diferentes modelos generalizados de regresión logística, tratando lso datoso como series temporales, adecuadas para el tratamiento de la información con el paquete Arima, autogenerador del modelso de regresioón más adecuado para un estimación de la serie. Los detalles y trameinto de al inforación se encentran en el apartado de Regression model_water analitics.

**Resumen del resultado

Como resultado final encontramos un matriz de predicciones completa para el año 2016, que podemos testear con valores reales totalizados de la serie. La precisión de l aprediccion se ha calculado entorno a 80 %. hay derivadas interesasnte que apuntan lso datos como estimaciones a la baja en torno 10-5 %, que pueden indicar dos patrones, una restriccion en la inforamción la naturaleza de lso puntos calinetes de la ciudad( hospitales, instalaciones militares, edificios oficiles, etc) o incluso un huella de fraude de consumidores en la desviación del consumo al alza que puede ser un spin off interesante de abordar de este proyecto, con los registros oficiales y privados de la fuente (CYII).

**Manual de usuario

Se ha empleado shiny como paquete de R para la construcción de cuadros de mando web interactivos. Permite, por ejemplo, crear interfaces para algoritmos o acceder y manipular tablas de datos a través de controles de HMTL: sliders, botones, etc. el usuario puede visualizar los datos de la serie histórica y las predicciones a través de un mapa interactivo.











