
# ###########################################################
#                     Tarea 1
#        Modelos de regresión lineal simples
#   Fecha de entrega: 11 de septiembre de 2025
#             Lizzeth Gómez Rodríguez
# ###########################################################

# Instrucciones:
# 1.Tomar como referencia el Gujarati y resumir los supuestos del modelo clásico
# de la regresión lineal

# 2.Realizar un modelo de doble logarítmo (log-log)
# (Variable dependiente e independient tienen logarítmo)
# Utilizar otra variable dependiente distinto a la clase

# 3. Replicar scripts de la clase
# 4. Incorporar la interpretación: Gujarati Cap. 6 se explica la interpretación
# de modelos con logarítmo
#-------------------------------------------------------------------

# Supuestos del Modelo Clásico de Regresión Lineal
# 1. Linealidad en los parámetros
#    El modelo debe ser lineal respecto a los parámetros (coeficientes),
#    es decir, sus exponentes deben ser 1:
#    Y = B0 + B1X1 + .... + BnXn + u
# 3. Esperanza condicional del error igual a cero
#    E(ui|Xi) = 0
# 4. Varianza constante de los errores (Homoscedasticidad)
#    La varianza de los errores es la misma para todos los valores de x
#    var(ui|Xi) =s2
# 5. Normalidad en los errores: Los errores se distribuyen como una normal
#    ui∼N(0,s2)
# 6. No autocorrelación de los errores:
#    Cov(ui,uj) = 0 para i distinto de j


# -----------------------------------------------------------------
# Replica de Script y modelo de doble logaritmo (log-log)
getwd()
setwd("./Clases")

library(tidyverse)

# Cargamos base de datos
library(readxl)
MARCADOS <- read_excel("MARCADOS.xlsx")

# Para evitar notación cientifica de exponencial (e) utilizamos options()
options(scipen = 9000)

# Observamos los extremos de los datos
head(MARCADOS)
tail(MARCADOS)

# Librería para modificar/manipular el objeto
# install.packages('textshape')
library(textshape)

# Creamos una columna para usarla como index
datosb <- textshape::column_to_rownames(MARCADOS,1)
head(datosb) #la fecha ahora es el índice

# Variables de la base de datos:
# MEX10A: Bono M a 10 años
# IPC_MXX: BMV
# MXNUSD: Tipo de cambio por dolar
# WTI: Precio del Petróleo West Texas Intermediate

# -------------------------------------------------------------
# Análisis estadístico
# -------------------------------------------------------------
# Para esta tarea utilizaremos la variable del precio del barril de petróleo (WTI)
mean(datosb$WTI)
median(datosb$WTI)
var(datosb$WTI) #Varianza
sd(datosb$WTI) #Desviación estándar

# Resumen descriptivo de los datos
summary(datosb)
summary(datosb$WTI)
# Notemos que le precio del barril tiene un promedio de 69.91 dólares


# --------------------------------------------------------------
# Introducción a los modelos lineales
# --------------------------------------------------------------
# Paquetes para modelos lineales
# install.packages('lmtest')
# install.packages('tseries')
# install.packages('moments')

library(lmtest)
library(moments)
library(tseries)

kurtosis(datosb$WTI)
skewness(datosb$WTI)

# Notemos que la kurtosis es < 3 lo que indica que la distribución es Platocúrtica
# es decir que la mayoría de los datos se concentran en el centro, hay menos valores extremos
# Por otro lado podemos decir que los datos son ligeramente simétricos,
# ya que la asimetría está muy ligeramente arriba de 0 indicando un muy pequeñlo sesgo positivo
# con cola ligeramente alargada a la derecha


# La Kurtosis = 3
# Tanto el sesgo como la kurtosis se obtienen a partir de los momentos alrededor de la media

# 1er momento: Media
# 2do momento: varianza (var)
# 3er momento: desviación estándar (sd)

# Si Kurtosis = 3, distribución Mesocúrtica
# Si kurtosis > 3, distribución Leptocúrtica
# Si kurtosis < 3, distribución Platocúrtica

# Pruba Jarque Bera
# Prueba de Bondad de ajuste para probar si los datos de una muestra, provienen de una distribución normal
# La hipótesis se distribuye con media cero y varianza constante. 
# HO: Hipótesis nula con asimetría = 0 y kurtosis = 3
jarque.bera.test(datosb$WTI)
# Notemos que el P-value < 0.05, por lo que se rechaza la H0, es decir,
# los datos no son normales


# ---------------------------------------------------------------------
# GRÁFICOS
# ---------------------------------------------------------------------  

# Gráfico de caja
boxplot(datosb$WTI,
        horizontal = T,
        col = 'pink')
grid()
# Rango intercuantil(IQR) = Q3 - Q4

# Histograma de frecuencia
hist(datosb$WTI,
     main = 'Precios del petróleo',
     col = "pink",
     xlab = "WTI",
     ylab = "Frecuencia",
     las = 1)

# Notemos que no se distribuye como una normal, comprobando la prueba de 
# Jarque-Bera de arriba donde se rechaza la H0 al ser P-value < 0.05



# Curva de densidad
# Kdensity (la densidad son probabilidades, de 0 a 1)
# la densidad la agregamos en el vector ylim, que es el límite o rango del eje "y"
hist(datosb$WTI,
     main = '',
     col = "lightgreen",
     xlab = "Precios del petróleo (WTI)",
     ylab = "Densidad",
     las = 1,
     freq = F,
     xlim = c(5,140))
# Agregamos línea de densidad
lines(density(datosb$WTI),col = "black",lwd = 3)

# --------------------------------------------------------------
# Series de tiempo
# --------------------------------------------------------------
wti_t <- ts(datosb$WTI,
               start = c(2009, 1), #inicia el primer mes en 2009
               frequency = 12)
ts.plot(wti_t)

#Personalización
# cex.main es tamaño del título
plot(wti_t, 
     ylab = "",
     xlab = "",
     col = "blue",
     lwd = 2,
     las = 1,
     main = "Precios del petróleo (WTI)",
     cex.main = 1)
grid()

# Nótese una fuerte caída en el precio del petróleo a partir del 2014-2016, 
# derivado de un exceso en la oferta


# Nuevamente realizamos la prueba de normalidad
# Pruba de dsitribución de Bondad de ajuste
jarque.bera.test(datosb$WTI)

# --------------------------------------------------------------------
# Estadísticos de variabiliad
# --------------------------------------------------------------------
# COVARIANZA
# cov(x,y) mide la independencia o dependencia de las variables
# Si cov(x,y) = 0 : son independientes
# Si cov(x,y) =!0 : con dependientes
# El signo de la covarianza indica si la relación es 
# directa(positiva) o inversa(negativa) de las variables
# e: coeficiente de correlación

# Matriz de covarianza
mat_cov <- cov(datosb)
mat_cov
# observemos ahora la covarianza entre nuestra variable WTI y el TC
mat_cov2 <- cov(datosb$WTI,datosb$MXNUSD)
mat_cov2
# Notemos que existe una relación negativa entre las variables


# Matriz de correlación
mat_cor <- cor(datosb)
mat_cor
mat_cor2 <- cor(datosb$WTI,datosb$MXNUSD)
mat_cor2

# -------------------------------------------------------------------
# Gráficos
# ------------------------------------------------------------------
#install.packages('ggcorrplot')
library(ggcorrplot)
library(ggplot2)

# Gráfico de correlación entre las variables
ggcorrplot(mat_cor)

# Gráfico de correlación (De componentes principales)
#install.packages('corrplot')
library(corrplot)

corrplot(mat_cor,
         method = 'number')


# Diagrama de dispersión
# Supongamos que nuestra variable WTI está en función del TC
# WTI = f(MXNUSD)

# Observese una relación negativa en el gráfico
plot(datosb$MXNUSD, datosb$WTI,
     xlab = 'MXNUSD', ylab = 'WTI',
     las = 1,
     col = "darkorange",
     lwd = 2)
grid()


# Trazamos la linea de regresión
# Definimos la linea estimada y usamos los mínimos cuadrados ordinarios
y_est <- lm(WTI~MXNUSD, data = datosb)

#lty=1 línea no continua
abline(y_est, lwd = 2, lty = 2, col = 'blue')

# Ejercicio
# WTI en función del Bono 10M
plot(datosb$MEX10A,datosb$WTI, 
     xlab = "Bono10M", ylab = "WTI",
     col = "darkgreen",
     las = 1,
     lwd = 2)
grid()

y_est2 <- lm(WTI~MEX10A, data = datosb)
abline(y_est2, lwd =2,col = "darkorange",lty = 2)

# ####################################################################
#                        MODELO 1 
#               Modelo simple sin logaritmo
#         WTI = f(MXNUSD, MEX10A, IPC_MXX)
# ####################################################################

mod1 <- lm(WTI ~ MXNUSD + MEX10A + IPC_MXX, data = datosb)
mod1

# WTI = 115.67 -6.68*MXNUSD + 2.95*MEX10A -0.00*IPC_MXX
# Y = b0 + b1x1 + b2x2 +..

# Interpretación 
# El incremento del tipo de cambio en una unidad,
# hará que el precio del barril caiga en 6.68 dólares
# el fortalecimiento del dolar provocará una disminución del precio del petróleo

# En el caso del Bono a 10 años, si incrementa el bono en una unidad, 
# el barril del petróleo incrementará en 2.95 unidades

# Si la inflación incrementa en una unidad, 
# el precio del barril disminuirá en 2.95 dólares.

# Resumen estadístico del modelo
summary(mod1)
# Nótese que se generan los residuales:
# Y = b0 + b1x1 + b2x2 +... + u, donde las bx son la y estimada: ye, u:residual
# Y = ye + u
# En el gráfico, los residuales son la distancia entre las observaciones
# y la y estimada = la recta de la gráfica

# Si tomamos el valor estimado y lo dividimos entre la desviación estándar
# obtendremos el valor t
# ejercicio de comprobación con datos del Tipo de cambio
-6.6800064/0.4177282
# Nótese además en la tabla, que los p-values son menores a 0.05

# ---------------------------------------------------------------
# Matriz Varianza-Covarianza de los Estimadores
vcov(mod1)
round(vcov(mod1),7)

# El error estándar se obtiene de la raiz cuadrada de la varianza
# verificamos con el intercepto
sqrt(135.6487890) #11.6468360
summary(mod1)


# -----------------------------------------------------------------
# Pruebas de Hipótesis 
# Ayudan a saber si los resultados son verdaderos o falsos
# H0: b1 = 0, x no explica a y
# H1: b1 =!0, x sí explica a y
# Se busca es rechazar la Hipotesis nula
# P-value < 0.05, es un nivel de confianza del 5%, es decir es el margen de error
# Notese que para el intercepto,el p-value está por debajo de 0.05 
# por lo que se rechaza la H0 y se acepta la hipótesis alternativa

# CONCLUSIÓN
# Todas las variables rechazan la H0, por lo que podemos concluir que:
# SÍ EXPLICAN AL PRECIO DEL BARRIL DE PETRÓLEO

# -----------------------------------------------------------------
# Varianza de la regresión
# Var = suma de los residuales al cuadrado entre los grados de libertad 
# donde grados de libertad = (n-k)
# var = u2/(n-k)

# Notemos:
# +(u) = +(y-ye) = 0
# +(u2) = +(y-ye)2 -> minimizar la distancia MLO (minimos cuadrados ordinarios)
# El modelo marca un R2 = 0.6887
# Es decir que las variables explican el 68% del precio del Barril de petróleo

# Prueba Global o Prueba F, en donde:
# H0: b0 = b1 = b2 ...
# H1: b0 =! b1 =! b2
# Con el P-value se observa que las variables sí explican a la variable dependiente


# -----------------------------------------------------------------
# Tabla ANOVA
library(lmtest)
anova(mod1)

# Columna de Sum sq: Suma de los cuadrados indica la variable que mayor información aporta para
# explicar la variable dependiente, en este caso es el WTI
# Columna Mean sq, es la suma de los cuadrados dividido entre los grados de libertad

# ###############################################################
#                         Modelo 2
#         Regresión con doble logaritmo (log-log)
#   log(WTI) = f(log(MXNUSD), log(MEX10A), log(IPC_MXX))
#
#        log(Y) = β0 + β1log(X1) + β2log(X2) + ... + u
# ##############################################################

# Aplicamos logaritmos a las variables

# Vamos a incorporar el logaritmo al objeto de datos (a la base)
# el log permite hommogeneizar la serie
# variable de nivel se llama cuando tenemos la serie sin ninguna transformació (Propiedades marginales)
# Cuando se tiene logaritmo (se interpretan como elasticidades)
# Si la dependiente está en log, se interpreta como tasa de crecimeinto
datosb$lBM10 <- log(datosb$MEX10A)
datosb$lipc <- log(datosb$IPC_MXX)
datosb$ltc <- log(datosb$MXNUSD)
datosb$lwti <- log(datosb$WTI)

# Revisamos que ya tengamos las variables logarítmicas en la base
ls(datosb)
mod2 <- lm(lwti ~ ltc + lBM10 + lipc, data = datosb)
summary(mod2)
anova(mod2)
# Notemos que la R2 es de 0.75, lo que significa que el75% del precio del barrl
# es explicado por las variables independientes
# Notemos en la tabla Anova que el valor de la F es alta para el tipo de cambio y el IPC
# sin embargo, para el bono a 10 años es muy pequeña y no es estaditiscamente significativo 
# ya que el valor es > 0.05

# ------------------------------------------------------------------------
mod2
# Interpretación de los datos
# log(WTI)= − 0.2239 − 1.8426log(TC) + 0.6347log(BM10) + 0.7806log(IPC) + u

# Si el TC aumenta en 1%, entonces el precio del barril disminuirá en 1.84%, en promedio.
# existe una relación negativa que indica que si el dólar se fortalece, entoces
# el precio del petróleo tiende a bajar

# Si el valor del Bono a 10 años umenta en 1%, el precio del barril de petróleo 
# incrementará en 0.63%

# Si la inflación aumenta en 1%, el precio del petróleo aumentará 0.78%

# --------------------------------------------------------------
# Criterios de intermediación
# AIC: Akaike
# BIC: Bayesiano

# El mejor modelo es aquel que tiene el menor criterio de información,
# i.e, el que se acerca a cero

AIC(mod1)
AIC(mod2)

BIC(mod1)
BIC(mod2)

# Notemos que estandarizando la serie, es decir, aplicando logaritmo a nuestro modelo
# Ajusta mucho mejor y claramente el valor está uy por debajo del primer modelo

# --------------------------------------------------------------
# Pruba de especificación
# Obtener los residuales

error <- mod2$residuals

# type = "l" de line
plot(error, type = "l", col = "red", las = 1)
grid()
# línea que cruza por el cero, h: de horizontal
abline(h = 0)

#----------------------------------------------------
# HISTOGRAMA
# Histograma para ver si existe normalidad
# Con frecuencia en false, porque queremos calcular la densidad (se ajusta en el eje y)
hist(error, col = 'lightgreen', freq = F, las = 1)
lines(density(error),col = 'red', lwd = 2)

# Notemos que se compporta ligeramente como una normal, pero sí se obserba cola alargada por la izquierda

# ---------------------------------------------------
# GRÁFICO DE CAJAS
boxplot(error, col = "pink", horizontal = T)

#----------------------------------------------------
# Jarque-Bera
# H0: distribución normal N(0,var2), media cero y varianza mínima
# H1: No se distribuye como una normal
# recordar que si P-Value < 0.005 se rechaza la H0
jarque.bera.test(error) # los residuales no se distribuyen como una normal, ya que P-value < 0.05
kurtosis(error) # Probelma de varianza
skewness(error) # está sesgada

# Notemos que la Jarque Bera da un p-value > 0.05 por lo que se acepta la H0
# es decir que se distribuye como una normal
# La Kurtosis es < 3 lo que indica una distribución Platocurtica
# El sesog/asimetría es < 0; indicando un sesgo negativo, es decir, con cola alargada a la izquierda


# ------------------------------------------------------------------
# MULTICOLINEALIDAD
# ------------------------------------------------------------------

# Multicolinealidad sucede cuando las variables que explican, aportan la misma información
# Por ejemplo para el Consumo, se usa Ingreso disponible, PNB, PNN (producto nacional neto)
# Pasamos de un Modelo Estático a uno Dinámico y así rompemos la multicolinealidad

# La prueba VIF se usa para que cada variable explicatova se considera como independiente
# En esta prueba se busca que la R2 sea baja ya que habrá menos correlación entre las variables
# VIF considera 3 niveles:
# VIF <= 5: Multicolinealidad débil
# VIF > 5: Multicolinealidad fuerte
# VIF = 100: Multicolinealidad exacta (la información es exactamente la misma)

# VIF = 1/(1-R2)

#install.packages('olsrr')
library(olsrr)
ols_vif_tol(mod2)
# Notemos que el resultado es menor a 5, por lo que no hay multicolinealidad

# -----------------------------------------------------------------
# AUTOCORRELACIÓN 
# -----------------------------------------------------------------
# Cuando se maneja tiempo, siempre se referirá a Autcorrelación
# El tiempo como una v.a. discreta, será Determinística
# El tiempo como var aleatoria continua, será Estocástica

# Si se considera un periodo para considerar la explicación de una variable (es decir, un rezago)
# entonces se considera de 1er orden y así para cada rezago i de orden i
# Por ejemplo el TC de hoy puede estar determinado por el TC del periodo anterior, 
# es decir par el TC de ayer, y por lo cual se dice que tiene Autocorrelación

# Si Cov(TCi, TCj) = 0 -> No hay Autocorrelación
# Si Cov(TCi, TCj) =! 0 -> Sí hay Autocorrelación (cuando el pasado empieza a afectar/explicar los precios del futuro)

# ------------------------------------------------------------
# Lo que nos interesa es saber si los Errores no presentan Autocorrelación
# Dos pruebas:
# 1. Durbin Watson -> Pruba No Paramétrica (i.e. que la probabilidad no interviene de manera directa)
# No es prueba robusta, se toma el valor central
# H0: u no tienen autocorrelación
# H1: u sí tienen autocorrelación (cuando se aleja del valor central 0:4 -> valor central es 2)
# Si la prueba da 1.7 o 2.3, se rechaza H0 y se acepta H1
# Para aceptar H0 se requiere P-value > 0.05

# ---------------------------------------------------------------
# PRUEBAS DE AUTOCORRELACIÓN
# ---------------------------------------------------------------

# Durbin Watson
dwtest(mod2)

# Breush-Godfrey_LM
bgtest(mod2)

# Am,bas pruebas muestran que sí hay autocorrelación 
# P-value < 0.05 se rechaza H0

# Heteroscedasticidad / Varianza constante
# Homocedasticidad:
# En cada punto de la regresión (para cada obs) la varianza es constante
# Cuando la varianza no es constante, entonces hay Heteroscedasticidad
# H0: La varianza es Homocedástica
# H1: La varianza es Heteroscedastica
# P-Value > 0.05 para aceptar H0

# Prueba de Breush-Pagan para Heteroscedasticidad
bptest(mod2)
# la prueba es < 0.05 indicando que se rechaza H0, por lo que existe Heterocedasticidad

# ###############################################################
#                         Modelo 3
#  Regresión con doble logaritmo (log-log) quitando el intercepto
#   log(WTI) = f(log(MXNUSD), log(MEX10A), log(IPC_MXX))
#
#        log(Y) = β1log(X1) + β2log(X2) + ... + u
# ##############################################################

mod3 <- lm(lwti ~ 0 + ltc + lBM10 + lipc, data = datosb)
summary(mod3)
anova(mod3)
# R2 de 0.99, dado que practicamente indica que el precio del barril es explicado por el
# 99% de las variables independientes, puede existir una sobreestimación de nuestro modelo.

# --------------------------------------------------------------
# Criterios de intermediación
# AIC: Akaike
# BIC: Bayesiano

AIC(mod2)
AIC(mod3)

BIC(mod2)
BIC(mod3)

# Los resultados indican que ambos son bastante bajos, sin membargo, el modelo 2
# es el que mejor se ajusta al estar por debajo del modelo 3

# --------------------------------------------------------------
# Pruba de especificación
# Obtener los residuales

error <- mod3$residuals

# Graficamos
plot(error, type = "l", col = "red", las = 1)
grid()
abline(h = 0)

#----------------------------------------------------
# HISTOGRAMA
hist(error, col = 'lightgreen', freq = F, las = 1)
lines(density(error),col = 'red', lwd = 2)

# ---------------------------------------------------
# GRÁFICO DE CAJAS
boxplot(error, col = "pink", horizontal = T)

#----------------------------------------------------
# Jarque-Bera
# H0: distribución normal N(0,var2), media cero y varianza mínima
# H1: No se distribuye como una normal
# recordar que si P-Value < 0.005 se rechaza la H0
jarque.bera.test(error) # los residuales se distribuyen como una normal, ya que P-value > 0.05
kurtosis(error) # k < 3, es platocurtica
skewness(error) # está sesgada

# ------------------------------------------------------------------
# MULTICOLINEALIDAD
# ------------------------------------------------------------------
# VIF <= 5: Multicolinealidad débil
# VIF > 5: Multicolinealidad fuerte
# VIF = 100: Multicolinealidad exacta (la información es exactamente la misma)

# VIF = 1/(1-R2)
ols_vif_tol(mod3)
# Notemos que el resultado es menor a 5, por lo que no hay multicolinealidad

# -----------------------------------------------------------------
# AUTOCORRELACIÓN 
# -----------------------------------------------------------------
# Si Cov(TCi, TCj) = 0 -> No hay Autocorrelación
# Si Cov(TCi, TCj) =! 0 -> Sí hay Autocorrelación (cuando el pasado empieza a afectar/explicar los precios del futuro)

# ---------------------------------------------------------------
# PRUEBAS DE AUTOCORRELACIÓN

# Durbin Watson
dwtest(mod3)

# Breush-Godfrey_LM
bgtest(mod3)

# Ambas pruebas muestran que sí hay autocorrelación 
# P-value < 0.05 se rechaza H0

# Prueba de Breush-Pagan para Heteroscedasticidad
bptest(mod3)
# la prueba es < 0.05 indicando que se rechaza H0, por lo que existe Heterocedasticidad

  
