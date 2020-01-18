##########################
# Prueba Técnica - Derco #
##########################

# Por: Diego Rivera

# Estudiando el fenómeno de preferencia y recambio de automóviles en Chile, por medio de un set de datos anonimizados de tenencia 
# de vehículos histórica.
# Aplicar técnicas de ETL, enriquecimiento, exploración, descubrimiento y analítica para explicar y caracterizar los fenómenos.

# setting directorio de trabajo
setwd("~/GoogleDrive/DataScience/Derco")

# librearías a utilizar
library(dplyr) # para manipulación de data frames
library(inspectdf) # para inspección en detalle del data frame
library(Amelia) # para graficar datos faltantes
library(ggplot2) # para visualización de datos
# install.packages('VIM')
library(VIM) # para clasificación de datos perdidos
library(tidyquant) # para mejorar display de ggplot
library(gridExtra) # para visualización de datos conjuntos
library(mice) # para proceso de imputación de datos
library(stringr) # para manipulacion de datos strings
library(sqldf) # para manipuar dataframe con structured query language, lógica de base de datos relacionales.
library(ggplot2) # para visualización de datos
library(tidyquant) # para mejorar display de ggplot
library(gridExtra) # para visualización de datos conjuntos
library(corrplot) # para visualización de corrplot (correlaciones)

# data Set de Derco asignado a variable "data_derco"
data_derco <- read.csv(file.choose(), header = FALSE, sep = ";")

# analizando la estructura del set de datos data_derco
names(data_derco)
head(data_derco)

# se cambiarán los nombres de las columnas del header para mejor visualización de los atributos y para facilitar el análisis 
# descriptivo por los siguientes valores:
# PATENTE;MARCA;MODELO;AÑO;ID_CLIENTE;COMUNA;REGION;SEXO;ACTIVIDAD;TASACION;FEC_TRANSFERENCIA;COLOR2;EDAD;VIGENCIA
names(data_derco) = c("PATENTE", "MARCA", "MODELO", "AÑO", "ID_CLIENTE", "COMUNA", "REGION", "SEXO", "ACTIVIDAD", "TASACION",
                      "FEC_TRANSFERENCIA", "COLOR2", "EDAD", "VIGENCIA")
names(data_derco)
head(data_derco)

# haciendo un análisis exploratorio de las variables
str(data_derco)
summary(data_derco)

# Analizando la estructura y resumen de los atributos del set de datos, se aprecia que el atributo "ACTIVIDAD" es una variable
# entera que no aporta dato alguno, ya que cuenta con min, max, media, mediana y 1er y 3er cuartil igual a cero y el resto de
# de valores son NA, por lo cual será eliminada al ser de nulo aporte estadístico y explicativo.

# Otras consideraciones como datos atípicos serán analizados una vez tratado la gran cantidad de datos faltantes.

# visualizando la presencia de datos NA, en base a la respuesta del summary y que serán tratados un poco más adelnate.
data_derco %>% inspect_na
data_derco %>% inspect_na %>% show_plot
# Se visualizan una gran cantidad de datos NA: ACTIVIDAD 809.190; FEC_TRANSFERENCIA 665.817; EDAD 283.479.

# Eliminando el ACTIVADAD, dado lo comentado anteriormente. No así FEC_TRANSFERENCIA y EDAD que serán evaluados.
data_derco <- data_derco[,-c(9)]
names(data_derco)
head(data_derco)

# Evaluando la opción de limpiar valores NA's
prueba_data_derco_naomit <- na.omit(data_derco)
dim(prueba_data_derco_naomit)
# lo datos se reducirían a 238.939 obs. de un total de 1.006.969 observaciones. Por lo cual eliminar EC_TRANSFERENCIA y EDAD no será 
# una opción a seguir de momento y se evaluará por cada atributo los casos de NA y datos en blanco.

# Los patrones de datos faltantes representan relaciones matemáticas genéricas entre los datos observados y los ausentes.

# Clasificación de los datos perdidos
# MCAR (Missing Completely At Random): La probabilidad de que una respuesta a una variable sea dato faltante es independiente 
# tanto del valor de esta variable como del valor de otras variables del conjunto de datos.

# MAR (Missing At Random): La probabilidad de que una respuesta sea dato faltante es independiente de los valores de la misma 
# variable pero es dependiente de los valores de otras variables del conjunto de datos.

# NMAR (Not Missing At Random): La probabilidad de que una respuesta a una variable sea dato faltante es dependiente de los valores 
# de la variable.
# http://rpubs.com/ydmarinb/429757

# Asumiendo FEC_TRANSFERENCIA y EDAD como MCAR (Missing Completely At Random), además de ser variables numéricas continuas por lo que 
# se realizará una imputación por la media para cada variable.
columns <- c("FEC_TRANSFERENCIA", "EDAD")
imputed_data <- mice(data_derco[,names(data_derco) %in% columns],m = 1,
                     maxit = 1, method = "mean",seed = 2018,print=F)
complete.data <- mice::complete(imputed_data)
summary(complete.data)

# La imputación media subestimará la varianza, alterará las relaciones entre las variables, sesgará casi cualquier estimación que no
# sea la media y sesgará la estimación de la media cuando los datos no sean MCAR.
par(mfrow=c(1,2))
plot(density(data_derco$FEC_TRANSFERENCIA,na.rm = T), col=2, main="FEC_TRANSFERENCIA")
lines(density(complete.data$FEC_TRANSFERENCIA),col=3)
plot(density(data_derco$EDAD,na.rm = T),col=2,main="EDAD")
lines(density(complete.data$EDAD),col=3)

derco_dataCompleta <- cbind.data.frame(complete.data, data_derco)
summary(derco_dataCompleta)

# revisando estadísticos de la imputación realizada, y viendo las respectivas variaciones en los quartiles 1 y 3.  

#   Datos Imputados                             vs                Datos Originales          
# FEC_TRANSFERENCIA         EDAD                                  FEC_TRANSFERENCIA             EDAD     
# Min.   :20091015      Min.   :  3.00                            Min.   :20091015          Min.   :  3.00  
# 1st Qu.:20151804      1st Qu.: 43.00                            1st Qu.:20140704          1st Qu.: 39.00  
# Median :20151804      Median : 48.93                            Median :20151202          Median : 48.00 
# Mean   :20151804      Mean   : 48.93                            Mean   :20151804          Mean   : 48.93 
# 3rd Qu.:20151804      3rd Qu.: 54.00                            3rd Qu.:20170116          3rd Qu.: 58.00 
# Max.   :20180111      Max.   :134.00                            Max.   :20180111          Max.   :134.00  
#                                                                 NA's   :665817            NA's   :283479

# Eliminando data original e incompleta de FEC_TRANSFERENCIA y EDAD del nuevo set de datos completos de Derco.
derco_dataCompleta <- derco_dataCompleta[,-c(12,14)]
summary(derco_dataCompleta)

# Continuando revisión para resto de los datos. PATENTE, AÑO, VIGENCIA se encuentran con datos completos y coherentes.

# Pasando campos en blanco para MARCA, MODELO a "OTRO". Dada la funcionalidad del atributo ID_CLIENTE, que es de identificación de
# cliente único, se cambiará el tipo de dato de numérico a factor y se reemplazarán los valores 0 por "OTRO".
# Para la variable dicotómica de SEXO se aplicará la misma lógica para los campos sin información

# primero pasando el tipo de dato factor a character, revisando que no hayan espacios en blanco en los campos vacios, para luego
# reemplazar éstos y posibles NA a OTRO, para luego retornar el tipo de dato a factor.
derco_dataCompleta$MARCA <- as.character(derco_dataCompleta$MARCA)
derco_dataCompleta$MARCA <- str_trim(derco_dataCompleta$MARCA)
derco_dataCompleta$MARCA[which(derco_dataCompleta$MARCA=="")] = 'OTRO'
derco_dataCompleta$MARCA[which(is.na(derco_dataCompleta$MARCA))] = 'OTRO'
derco_dataCompleta$MARCA <- as.factor(derco_dataCompleta$MARCA)
table(derco_dataCompleta$MARCA)

# idem para MODELO
derco_dataCompleta$MODELO <- as.character(derco_dataCompleta$MODELO)
derco_dataCompleta$MODELO <- str_trim(derco_dataCompleta$MODELO)
derco_dataCompleta$MODELO[which(derco_dataCompleta$MODELO=="")] = 'OTRO'
derco_dataCompleta$MODELO[which(is.na(derco_dataCompleta$MODELO))] = 'OTRO'
derco_dataCompleta$MODELO <- as.factor(derco_dataCompleta$MODELO)
table(derco_dataCompleta$MODELO)

# y o mismo para ID_CLIENTE
derco_dataCompleta$ID_CLIENTE <- as.character(derco_dataCompleta$ID_CLIENTE)
derco_dataCompleta$ID_CLIENTE <- str_trim(derco_dataCompleta$ID_CLIENTE)
derco_dataCompleta$ID_CLIENTE[which(derco_dataCompleta$ID_CLIENTE=="")] = 'OTRO'
derco_dataCompleta$ID_CLIENTE[which(derco_dataCompleta$ID_CLIENTE=="0")] = 'OTRO'
derco_dataCompleta$ID_CLIENTE[which(is.na(derco_dataCompleta$ID_CLIENTE))] = 'OTRO'
derco_dataCompleta$ID_CLIENTE <- as.factor(derco_dataCompleta$ID_CLIENTE)
table(derco_dataCompleta$ID_CLIENTE)

table(derco_dataCompleta$SEXO)
# Los datos en blaco de sexo serán asignado a "SIN DATO"
#             F      M 
# 884361  55169  67439
# idem para SEXO
derco_dataCompleta$SEXO <- as.character(derco_dataCompleta$SEXO)
derco_dataCompleta$SEXO <- str_trim(derco_dataCompleta$SEXO)
derco_dataCompleta$SEXO[which(derco_dataCompleta$SEXO=="")] = 'SIN DATO'
derco_dataCompleta$SEXO[which(is.na(derco_dataCompleta$SEXO))] = 'SIN DATO'
derco_dataCompleta$SEXO <- as.factor(derco_dataCompleta$SEXO)
table(derco_dataCompleta$SEXO)
#     F       M   SIN DATO 
#55169    67439   884361 

# Para el atributo COMUNA, los campos en blaco pasarán a "SIN COMUNA" además de los valores "\xa5U\xa5OA", y corregir 
# los siguientes valores para las siguientes comunas, en base a verificaciones en internet, aunque lo ideal sería seguir patrones 
# de acuerdo a los requerimientos del solicitante, en base a lo anterior se propone lo siguiente:

# Revisando posibles casos de alcances de nombre de comunas de distintas regiones.
# ver regiones entre PLACILLA y PLACILLA DE PENUELAS y PENUELAS
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE COMUNA = "PLACILLA" GROUP BY REGION') # Región VI ... OHIGGINS
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE COMUNA = "PLACILLA DE PENUELAS" GROUP BY REGION') # Región V DE VALPARAISO
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE COMUNA = "PENUELAS" GROUP BY REGION') # Región V DE VALPARAISO

# ver a cual comuna corresponde SAN JOSE, SAN JOSE DE M, y MARIQUINA en base a la región
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE COMUNA = "SAN JOSE"') # Región METROPOLITANA
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE COMUNA = "SAN JOSE DE M"') # Región METROPOLITANA
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE COMUNA = "MARIQUINA" GROUP BY REGION') # Región XIV DE LOS RIOS

# ver a cual comuna corresponde SAN PEDRO, SAN PEDRO DE, en base a la región
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE COMUNA = "SAN PEDRO" GROUP BY REGION') # Región METROPOLITANA
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE COMUNA = "SAN PEDRO DE" GROUP BY REGION') # Región VIII DEL BIO BIO

derco_dataCompleta$COMUNA <- as.character(derco_dataCompleta$COMUNA)
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="")] = 'SIN COMUNA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="\xa5U\xa5OA")] = 'SIN COMUNA'
derco_dataCompleta$COMUNA[which(is.na(derco_dataCompleta$COMUNA))] = 'SIN COMUNA'
derco_dataCompleta$COMUNA <- str_trim(derco_dataCompleta$COMUNA)
#   - "CALERA DE TAN" -> pasar a "CALERA DE TANGO"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="CALERA DE TAN")] = 'CALERA DE TANGO'

#   - COLINA-ESTACION" -> pasar a "COLINA" *** evisar
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="COLINA-ESTACION")] = 'COLINA'

#   - "DIEGO DE ALMA" -> pasar a "DIEGO DE ALMAGRO"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="DIEGO DE ALMA")] = 'DIEGO DE ALMAGRO'

#   - "DO\xa5IHUE"    -> pasar a "DONIHUE"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="DO\xa5IHUE")] = 'DONIHUE'

#   - "EST CENTRAL, "ESTACION CENT" -> pasar a "ESTACION CENTRAL"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="ESTACION CENT")] = 'ESTACION CENTRAL'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="EST CENTRAL")] = 'ESTACION CENTRAL'

#   - "LLAILLAY", "LLAY-LLAY", "LLAYLLAY" -> pasar a "LLAY LLAY"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="LLAILLAY")] = 'LLAY LLAY'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="LLAY-LLAY")] = 'LLAY LLAY'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="LLAYLLAY")] = 'LLAY LLAY'

#   - "PE\xa5ABLANCA" -> pasar a "PENABLANCA"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="PE\xa5ABLANCA")] = 'PENABLANCA'

#   - "PE\xa5ALOLEN"  -> pasar a "PENALOLEN"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=="PE\xa5ALOLEN")] = 'PENALOLEN'

#   - "O"HIGGINS", -> pasar a "OHIGGINS"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='O"HIGGINS')] = 'OHIGGINS'

#   - "P AGUIRRE CERDA", "PEDRO AGUIRRE", "PEDRO AGUIRRE CERD" -> pasar a "PEDRO AGUIRRE CERDA"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='P AGUIRRE CERDA')] = 'PEDRO AGUIRRE CERDA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='PEDRO AGUIRRE')] = 'PEDRO AGUIRRE CERDA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='PEDRO AGUIRRE CERD')] = 'PEDRO AGUIRRE CERDA'
      
#   - "PLACILLA DE PENUEL", "PENUELAS" -> pasar a "PLACILLA DE PENUELAS"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='PLACILLA DE PENUEL')] = 'PLACILLA DE PENUELAS'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='PENUELAS')] = 'PLACILLA DE PENUELAS'

#   - "SAN FCO DE MOSTAZA", "SAN FCO DE MOSTAZAL", "SAN FRANCISCO", "SAN FRANCISCO DE M", "MOSTAZAL" ->
#                                                                                               pasar a "SAN FRANCISCO DE MOSTAZAL"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN FCO DE MOSTAZA')] = 'SAN FRANCISCO DE MOSTAZAL'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN FCO DE MOSTAZAL')] = 'SAN FRANCISCO DE MOSTAZAL'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN FRANCISCO')] = 'SAN FRANCISCO DE MOSTAZAL'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN FRANCISCO DE M')] = 'SAN FRANCISCO DE MOSTAZAL'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='MOSTAZAL')] = 'SAN FRANCISCO DE MOSTAZAL'

#   - "SAN JOSE DE LA MARIQ", "SAN JOSE DE MARIQUIN", "MARIQUINA",  -> pasar a "SAN JOSE DE LA MARIQUINA"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN JOSE DE LA MARIQ')] = 'SAN JOSE DE LA MARIQUINA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN JOSE DE MARIQUIN')] = 'SAN JOSE DE LA MARIQUINA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='MARIQUINA')] = 'SAN JOSE DE LA MARIQUINA'

#   - "SAN JOSE MAIPO", "SAN JOSE DE M", "SAN JOSE" -> pasar a "SAN JOSE DE MAIPO"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN JOSE MAIPO')] = 'SAN JOSE DE MAIPO'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN JOSE DE M')] = 'SAN JOSE DE MAIPO'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN JOSE')] = 'SAN JOSE DE MAIPO'

#   - "SAN JUAN COSTA", "SAN JUAN DE LA COS" -> pasar a "SAN JUAN DE LA COSTA"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN JUAN COSTA')] = 'SAN JUAN DE LA COSTA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN JUAN DE LA COS')] = 'SAN JUAN DE LA COSTA'

#   - "SAN PEDRO DE ATACA" -> pasar a "SAN PEDRO DE ATACAMA"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN PEDRO DE ATACA')] = 'SAN PEDRO DE ATACAMA'

#   - "SAN PEDRO DE LA PA", "SAN PEDRO DE" -> pasar a "SAN PEDRO DE LA PAZ"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN PEDRO DE LA PA')] = 'SAN PEDRO DE LA PAZ'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN PEDRO DE')] = 'SAN PEDRO DE LA PAZ'

#   - "SAN PEDRO DE MELIP" -> pasar a "SAN PEDRO DE MELIPILLA"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN PEDRO DE MELIP')] = 'SAN PEDRO DE MELIPILLA'

#   - "SAN VICENTE", "SAN VICENTE DE TAG", "SAN VICENTE T-T", "SAN VICENTE TAGUA", "SAN VICENTE TAGUA TA" -> pasar a 
#                                                                                                           "SAN VICENTE TAGUA TAGUA"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN VICENTE')] = 'SAN VICENTE TAGUA TAGUA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN VICENTE DE TAG')] = 'SAN VICENTE TAGUA TAGUA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN VICENTE T-T')] = 'SAN VICENTE TAGUA TAGUA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN VICENTE TAGUA')] = 'SAN VICENTE TAGUA TAGUA'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='SAN VICENTE TAGUA TA')] = 'SAN VICENTE TAGUA TAGUA'

#   - "TEODORO SCHIMDT, "TEODORO SCHMI" -> pasar a "TEODORO SCHMIDT"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='TEODORO SCHIMDT')] = 'TEODORO SCHMIDT'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='TEODORO SCHMI')] = 'TEODORO SCHMIDT'

#   - "TIERRA AMARIL" -> pasar a "TIERRA AMARILLA"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='TIERRA AMARIL')] = 'TIERRA AMARILLA'

#   - "TIL-TIL", "TILTIL" -> pasar a "TIL TIL"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='TIL-TIL')] = 'TIL TIL'
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='TILTIL')] = 'TIL TIL'

#   - "TORRES DE PAINE" -> pasar a "TORRES DEL PAINE"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='TORRES DE PAINE')] = 'TORRES DEL PAINE'

#   - "TREGUACO" -> pasar a "TREHUACO"
derco_dataCompleta$COMUNA[which(derco_dataCompleta$COMUNA=='TREGUACO')] = 'TREHUACO'
derco_dataCompleta$COMUNA <- as.factor(derco_dataCompleta$COMUNA)
table(derco_dataCompleta$COMUNA)

# Para regiones, revisar blancos y valores 0 (zero) y analizar vs comuna:
derco_dataCompleta$REGION <- as.character(derco_dataCompleta$REGION)
derco_dataCompleta$REGION <- str_trim(derco_dataCompleta$REGION)
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=="")] = 'SIN REGION'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=="0")] = 'SIN REGION'
derco_dataCompleta$REGION[which(is.na(derco_dataCompleta$REGION))] = 'SIN REGION'

# para 01, 1, DE TARAPACA  -> I DE TARAPACA
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='01')] = 'I DE TARAPACA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='1')] = 'I DE TARAPACA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE TARAPACA')] = 'I DE TARAPACA'

# para 02, 2, DE ANTOFAGASTA -> II DE ANTOFAGASTA
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='02')] = 'II DE ANTOFAGASTA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='2')] = 'II DE ANTOFAGASTA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE ANTOFAGASTA')] = 'II DE ANTOFAGASTA'

# para 03, 3, DE ATACAMA -> III DE ATACAMA
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='03')] = 'III DE ATACAMA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='3')] = 'III DE ATACAMA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE ATACAMA')] = 'III DE ATACAMA'

# para 04, 4, DE COQUIMBO -> IV DE COQUIMBO
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='04')] = 'IV DE COQUIMBO'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='4')] = 'IV DE COQUIMBO'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE COQUIMBO')] = 'IV DE COQUIMBO'

# para 05, 5, DE VALPARAISO -> V DE VALPARAISO
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='05')] = 'V DE VALPARAISO'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='5')] = 'V DE VALPARAISO'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE VALPARAISO')] = 'V DE VALPARAISO'

# para 06, 6, DEL LIBERTADOR BERNARDO OHIGGINS -> VI DEL LIBERTADOR GENERAL BERNARDO OHIGGINS
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='06')] = 'VI DEL LIBERTADOR GENERAL BERNARDO OHIGGINS'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='6')] = 'VI DEL LIBERTADOR GENERAL BERNARDO OHIGGINS'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DEL LIBERTADOR BERNARDO OHIGGINS')] = 
                                                                                        'VI DEL LIBERTADOR GENERAL BERNARDO OHIGGINS'

# para 07, 7, DEL MAULE -> VII DEL MAULE
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='07')] = 'VII DEL MAULE'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='7')] = 'VII DEL MAULE'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DEL MAULE')] = 'VII DEL MAULE'

# para 08, 8, DEL BIO BIO -> VIII DEL BIO BIO
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='08')] = 'VIII DEL BIO BIO'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='8')] = 'VIII DEL BIO BIO'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DEL BIO BIO')] = 'VIII DEL BIO BIO'

# para 09, 9, DE LA ARAUCANIA -> IX DE LA ARAUCANIA
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='09')] = 'IX DE LA ARAUCANIA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='9')] = 'IX DE LA ARAUCANIA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE LA ARAUCANIA')] = 'IX DE LA ARAUCANIA'

# para 10, DE LOS LAGOS -> X DE LOS LAGOS
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='10')] = 'X DE LOS LAGOS'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE LOS LAGOS')] = 'X DE LOS LAGOS'

# para 11, AYSEN DEL GENERAL CARLOS IBANEZ -> XI DE AYSEN DEL GENERAL CARLOS IBANEZ DEL CAMPO
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='11')] = 'XI DE AYSEN DEL GENERAL CARLOS IBANEZ DEL CAMPO'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='AYSEN DEL GENERAL CARLOS IBANEZ')] = 
                                                                                    'XI DE AYSEN DEL GENERAL CARLOS IBANEZ DEL CAMPO'

# para 12, DE MAGALLANES Y ANTARTICA CHILENA  -> XII DE MAGALLANES Y ANTARTICA CHILENA
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='12')] = 'XII DE MAGALLANES Y ANTARTICA CHILENA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE MAGALLANES Y ANTARTICA CHILENA')] = 
                                                                                              'XII DE MAGALLANES Y ANTARTICA CHILENA'

# para 13, METROPOLITANA DE SANTIAGO -> METROPOLITANA DE SANTIAGO
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='13')] = 'METROPOLITANA DE SANTIAGO'

# para 14, DE LOS RIOS -> XIV DE LOS RIOS
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='14')] = 'XIV DE LOS RIOS'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE LOS RIOS')] = 'XIV DE LOS RIOS'

# para 15, DE ARICA y PARINACOTA -> XV DE ARICA y PARINACOTA
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='15')] = 'XV DE ARICA y PARINACOTA'
derco_dataCompleta$REGION[which(derco_dataCompleta$REGION=='DE ARICA y PARINACOTA')] = 'XV DE ARICA y PARINACOTA'

derco_dataCompleta$REGION <- as.factor(derco_dataCompleta$REGION)
table(derco_dataCompleta$REGION)

# validando COMUNAS sin REGION
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE REGION = "SIN REGION" GROUP BY COMUNA')

#                       COMUNA        REGION
# 1                   CAIMANES    SIN REGION --> IV DE COQUIMBO
derco_dataCompleta$REGION[which(derco_dataCompleta$COMUNA=='CAIMANES')] = 'IV DE COQUIMBO'

# 2                      CAJON    SIN REGION --> Datos permanecerán sin datos de COMUNA NI REGION, ya que no se puede asumir si 
#                                               pertenecen a la IX DE LA ARAUCANIA o METROPOLITANA DE SANTIAGO
# 3                  CHINCOLCO    SIN REGION --> V DE VALPARAISO
derco_dataCompleta$REGION[which(derco_dataCompleta$COMUNA=='CHINCOLCO')] = 'V DE VALPARAISO'

# 4                 LAS BRISAS    SIN REGION --> Datos permanecerán sin datos de COMUNA NI REGION. No se encontró respaldo 
#                                               concluyente
# 5               LAS VENTANAS    SIN REGION --> V DE VALPARAISO
derco_dataCompleta$REGION[which(derco_dataCompleta$COMUNA=='LAS VENTANAS')] = 'V DE VALPARAISO'

# 6                 PENABLANCA    SIN REGION --> V DE VALPARAISO
derco_dataCompleta$REGION[which(derco_dataCompleta$COMUNA=='PENABLANCA')] = 'V DE VALPARAISO'

# 7                     QUEULE    SIN REGION --> IX DE LA ARAUCANIA
derco_dataCompleta$REGION[which(derco_dataCompleta$COMUNA=='QUEULE')] = 'IX DE LA ARAUCANIA'

# 8  SAN FRANCISCO DE MOSTAZAL    SIN REGION --> VI DEL LIBERTADOR GENERAL BERNARDO OHIGGINS
derco_dataCompleta$REGION[which(derco_dataCompleta$COMUNA=='SAN FRANCISCO DE MOSTAZAL')] = 
                                                                                        'VI DEL LIBERTADOR GENERAL BERNARDO OHIGGINS'

# 9          SAN JOSE DE MAIPO    SIN REGION --> METROPOLITANA DE SANTIAGO
derco_dataCompleta$REGION[which(derco_dataCompleta$COMUNA=='SAN JOSE DE MAIPO')] = 'METROPOLITANA DE SANTIAGO'

# 10      SAN JUAN DE LA COSTA    SIN REGION --> X DE LOS LAGOS
derco_dataCompleta$REGION[which(derco_dataCompleta$COMUNA=='SAN JUAN DE LA COSTA')] = 'X DE LOS LAGOS'

# 11                 SANTA ANA    SIN REGION --> Datos permanecerán sin datos de COMUNA NI REGION. No se encontró respaldo
#                                               concluyente
# 12                SIN COMUNA    SIN REGION --> Datos permanecerán sin datos de COMUNA NI REGION

# revalidando COMUNAS sin REGION
sqldf('SELECT COMUNA, REGION FROM derco_dataCompleta WHERE REGION = "SIN REGION" GROUP BY COMUNA')

#       COMUNA        REGION
# 1      CAJON    SIN REGION
# 2 LAS BRISAS    SIN REGION
# 3  SANTA ANA    SIN REGION
# 4 SIN COMUNA    SIN REGION
table(derco_dataCompleta$REGION)

table(derco_dataCompleta$COLOR2)
# Dada la gran variedad de colores y de subcategorias, para facilitar un análisis con el atributo COLOR2 se reasignarán a:
# AMARILLO, AZUL, BEIGE, BLANCO, CAFE, CELESTE, DORADO, GRIS, NARANJO, NEGRO, PLATEADO, ROJO, ROSADO, VERDE, VIOLETA
derco_dataCompleta$COLOR2 <- as.character(derco_dataCompleta$COLOR2)
derco_dataCompleta$COLOR2 <- str_trim(derco_dataCompleta$COLOR2)
derco_dataCompleta$COLOR2[which(derco_dataCompleta$COLOR2=="")] = 'OTRO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "AMARILLO"))] = 'AMARILLO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "AZUL"))] = 'AZUL'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "BEIGE"))] = 'BEIGE'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "BLANCO"))] = 'BLANCO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "CAFE"))] = 'CAFE'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "CELESTE"))] = 'CELESTE'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "DORADO"))] = 'DORADO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "GRIS"))] = 'GRIS'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "NARANJO"))] = 'NARANJO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "NEGRO"))] = 'NEGRO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "PLATEADO"))] = 'PLATEADO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "ROJO"))] = 'ROJO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "ROSADO"))] = 'ROSADO'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "VERDE"))] = 'VERDE'
derco_dataCompleta$COLOR2[which(str_detect(derco_dataCompleta$COLOR2, pattern = "VIOLETA"))] = 'VIOLETA'
derco_dataCompleta$COLOR2[which(is.na(derco_dataCompleta$COLOR2))] = 'OTRO'
derco_dataCompleta$COLOR2 <- as.factor(derco_dataCompleta$COLOR2)
table(derco_dataCompleta$COLOR2)

# ************
# Visualizando Número de Vehículos sin datos de origen.
sqldf('SELECT COMUNA, REGION, COUNT(MODELO) as "Num Vehicles" FROM derco_dataCompleta WHERE REGION = "SIN REGION" GROUP BY COMUNA')

#       COMUNA        REGION   Num Vehicles
# 1      CAJON    SIN REGION              1
# 2 LAS BRISAS    SIN REGION              1
# 3  SANTA ANA    SIN REGION              1
# 4 SIN COMUNA    SIN REGION         465762

# Se aumirá esa cantidad de Vehículos sin datos de origen, ya que no se conteplarán más supuestos para el caso.
# ************

# El atributo TASACION corresponde a la tasación fiscal del vehículo que permite entre otras cosas estimar lo que cuesta; es decir, 
# el valor del vehículo, por ejemplo para fines tributarios, conocer el valor del permiso de circulación de un auto, calcular el 
# valor del impuesto a la transferencia en caso de que sea menor que el precio del auto.

# Dado lo anterior este atributo se trabajará como tipo numérico.
derco_dataCompleta$TASACION <- as.character(derco_dataCompleta$TASACION)
summary(derco_dataCompleta$TASACION)

derco_dataCompleta$TASACION <- as.numeric(derco_dataCompleta$TASACION)
summary(derco_dataCompleta$TASACION)

summary(derco_dataCompleta)
str(derco_dataCompleta)

dim(derco_dataCompleta)
# dimension derco_dataCompleta 1006969      13

sqldf('SELECT COMUNA, REGION, COUNT(MODELO) as "Num Vehicles", TASACION FROM derco_dataCompleta WHERE TASACION = 0 GROUP BY REGION')
sqldf('SELECT COUNT(MODELO) as "Num Vehicles" FROM derco_dataCompleta WHERE TASACION = 0')

# Valores 0 de Tasacion total de 270604 Vehicles repartidos de manera pareja a lo largo del país.

# Dado los tratamientos de imputaciones y reemplazos de las variables anteriores y luego del cambio de tipo de datos para el 
# atributo TASACION se decidio eliminar los valores NA's
derco_dataFinal <- derco_dataCompleta

# Limpiando valores NA's
derco_dataFinal <- na.omit(derco_dataFinal)

dim(derco_dataFinal)
# dimension derco_dataFinal 511578     13

# Modelo y marca de vehiculo de mayor tasacion
sqldf('SELECT MODELO, MARCA, MAX(TASACION) FROM derco_dataCompleta')

summary(derco_dataFinal)
str(derco_dataFinal)

# Visualizando la distribución de las variables numéricas en histogramas.
hist1 <- ggplot(data = derco_dataFinal, aes(x = FEC_TRANSFERENCIA)) +
  geom_histogram() +
  theme_tq()

hist2 <- ggplot(data = derco_dataFinal, aes(x = EDAD)) +
  geom_histogram() +
  theme_tq()

hist3 <- ggplot(data = derco_dataFinal, aes(x = AÑO)) +
  geom_histogram() +
  theme_tq()

hist4 <- ggplot(data = derco_dataFinal, aes(x = TASACION)) +
  geom_histogram() +
  theme_tq()

grid.arrange(hist1, hist2, hist3, hist4)

# Visualizando outliers para variables continuas.
Boxplot1 = derco_dataFinal %>%
  ggplot(aes(x=VIGENCIA, y=FEC_TRANSFERENCIA, fill=VIGENCIA)) + geom_boxplot()

Boxplot2 = derco_dataFinal %>%
  ggplot(aes(x=VIGENCIA, y=EDAD, fill=VIGENCIA)) + geom_boxplot()

Boxplot3 = derco_dataFinal %>%
  ggplot(aes(x=VIGENCIA, y=AÑO, fill=VIGENCIA)) + geom_boxplot()

Boxplot4 = derco_dataFinal %>%
  ggplot(aes(x=VIGENCIA, y=TASACION, fill=VIGENCIA)) + geom_boxplot()

grid.arrange(Boxplot1, Boxplot2, Boxplot3, Boxplot4, ncol=2)

summary(derco_dataFinal$EDAD)
summary(derco_dataFinal$TASACION)

###############################
# Se removeran los valores ceros en TASACION para visualizar el impacto en la data.
##############################
names(derco_dataFinal)

derco_dataFinal2 <- sqldf('SELECT FEC_TRANSFERENCIA, EDAD, PATENTE, MARCA, MODELO, AÑO, ID_CLIENTE, COMUNA, REGION, SEXO, TASACION, 
                            COLOR2, VIGENCIA FROM derco_dataFinal WHERE TASACION > 0')
summary(derco_dataFinal2)
dim(derco_dataFinal2)

# removiendo outliers de del set final.
derco_dataFinal3 <- sqldf('SELECT FEC_TRANSFERENCIA, EDAD, PATENTE, MARCA, MODELO, AÑO, ID_CLIENTE, COMUNA, REGION, SEXO, TASACION, 
                            COLOR2, VIGENCIA FROM derco_dataFinal2 WHERE EDAD BETWEEN 18 AND 75')

summary(derco_dataFinal3)
dim(derco_dataFinal3)

summary(derco_dataFinal3$EDAD)
summary(derco_dataFinal3$TASACION)

# Visualizando distribucion y comportamiento de datos atípicos de variables numericas.
# Visualizando la distribución de las variables numéricas en histogramas.
hist1_2 <- ggplot(data = derco_dataFinal2, aes(x = FEC_TRANSFERENCIA)) +
  geom_histogram() +
  theme_tq()

hist2_2 <- ggplot(data = derco_dataFinal2, aes(x = EDAD)) +
  geom_histogram() +
  theme_tq()

hist3_2 <- ggplot(data = derco_dataFinal2, aes(x = AÑO)) +
  geom_histogram() +
  theme_tq()

hist4_2 <- ggplot(data = derco_dataFinal2, aes(x = TASACION)) +
  geom_histogram() +
  theme_tq()

grid.arrange(hist1_2, hist2_2, hist3_2, hist4_2)

# Visualizando outliers para variables continuas.
Boxplot1_2 = derco_dataFinal2 %>%
  ggplot(aes(x=VIGENCIA, y=FEC_TRANSFERENCIA, fill=VIGENCIA)) + geom_boxplot()

Boxplot2_2 = derco_dataFinal2 %>%
  ggplot(aes(x=VIGENCIA, y=EDAD, fill=VIGENCIA)) + geom_boxplot()

Boxplot3_2 = derco_dataFinal2 %>%
  ggplot(aes(x=VIGENCIA, y=AÑO, fill=VIGENCIA)) + geom_boxplot()

Boxplot4_2 = derco_dataFinal2 %>%
  ggplot(aes(x=VIGENCIA, y=TASACION, fill=VIGENCIA)) + geom_boxplot()

grid.arrange(Boxplot1_2, Boxplot2_2, Boxplot3_2, Boxplot4_2, ncol=2)

# visualizando correlaciones
names(derco_dataFinal3)
corrplot(cor(derco_dataFinal3[,c(1,2,6,11)]))
dev.off()
corrplot(cor(derco_dataFinal3[,c(1,2,6,11)]), method='number')




