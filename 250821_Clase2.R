getwd()
#dir.create("./Documents/Especialidad/Econometría/RData")

#-----------------------------------------------------------
#         Jueves 21 de agosto - 2025
#                 Práctica 1
#           Lizzeth Gómez Rodríguez
# ----------------------------------------------------------

setwd("./Documents/Especialidad/Econometría/RData")
install.packages('tidyverse')
library(tidyverse)

# Cargamos base de datos
library(readxl)
MARCADOS <- read_excel("MARCADOS.xlsx")
view(MARCADOS)

#Para evitar notación cientifica de exponencial (e) utilizamos options()
options(scipen = 9000)

head(MARCADOS)
tail(MARCADOS)


#Librería para modificar/manipular el objeto
install.packages('textshape')
library(textshape)

#Queremos una columna para usarla como index
datosb <- textshape::column_to_rownames(MARCADOS,1)
head(datosb) #la fecha ahora es el índice

# Variables de la base de datos:
# MEX10A: Bono M a 10 años
# IPC_MXX: BMV
# MXNUSD: Tipo de cambio por dolar
# WTI: Precio del oetróleo West texas intermediate

# ----------------------------------------------------
# Análisis estadístico
mean(datosb$MEX10A)
median(datosb$MXNUSD)
var(datosb$WTI) #Varianza
sd(datosb$WTI) #Desviación estándar

#Resumen descriptivo de los datos
summary(datosb)
summary(datosb$MXNUSD)

#Prueba de modelos lineales
install.packages('lmtest')
install.packages('tseries')
install.packages('moments')


library(lmtest)
library(moments)
library(tseries)
kurtosis(datosb$MEX10A)
skewness(datosb$MEX10A)

#Por definición, la Kurtosis = 3
#Tanto el sesgo como la kurtosis, se obtienen a partir de los momentos alrededor de la media

#1er momento: Media
#2do momento: varianza (var)
#3er momento: desviación estándar (sd)

#Si Kurtosis = 3, distribución Mesocúrtica
#Si kurtosis > 3, distribución Leptocúrtica
#Si kurtosis < 3, distribución Platocúrtica

# Pruba Jarque Bera
# prueba de Bondad de ajuste para probar si los datos de una muetsra, provienen de una distribución normal
# La hipótesis se distribuye coomo media cero y varianza constante. 
# HO: Hipótesis nula con asimetría =0 y kurtosis = 3
((140/6)*((skewness(datosb$MEX10A)**2))+((1/4)*((skewness(datosb$MEX10A)-3)**2)))
jarque.bera.test(datosb$MEX10A)


# ---------------------------------------------------------------------
# GRÁFICOS

#Gráfico de caja
boxplot(datosb$MEX10A,
        horizontal = T,
        col = 'pink')
grid()
# Rango intercuantil(IQR) = Q3 - Q4

# Histograma de frecuencia
hist(datosb$MEX10A)

#Persolaización
# No poner título dentro del gráfico
# parámetro: las con 1,2,3 para cambiar/voltear los número del eje (1,2,3)
# lab es de label, es decir de etiqueta para ejes x, y 
hist(datosb$MEX10A,
     main = '',
     col = "pink",
     xlab = "Bono M (BM10)",
     ylab = "frecuencia",
     las = 1)

# Curva de densidad
# Kdensity (la densidad son probabilidades, de 0 a 1)
# la densidad la agregamos en el vector ylim, que es el límite o rango del eje "y"
hist(datosb$MEX10A,
     main = '',
     col = "lightgreen",
     xlab = "Bono M (BM10)",
     ylab = "Densidad",
     las = 1,
     freq = F,
     ylim = c(0,0.5),
     xlim = c(3,10))
# Agregamos línea de densidad
lines(density(datosb$MEX10A),col = "black",lwd = 3)


# Series de tiempo
mex10a_t <- ts(datosb$MEX10A,
               start = c(2009, 1), #inicia el primer mes en 2009
               frequency = 12)
ts.plot(mex10a_t)

#Personalización
# cex.main es tamaño del título
plot(mex10a_t, 
     ylab = "",
     xlab = "",
     col = "blue",
     lwd = 2,
     las = 1,
     main = "Rendimiento del Bono m a 10 años",
     cex.main = 1)
grid()

# -----------------------------------------------------------------
# Ejercicio de distribución con Tipo de Cambio

#Gráfico de caja
boxplot(datosb$MXNUSD,
        horizontal = T,
        col = 'pink')
grid()

#Histográma (Distribución bimodal, tiene dos máximos)
hist(datosb$MXNUSD,
     main = '',
     col = "lightgreen",
     xlab = "Tipo de cambio",
     ylab = "Densidad",
     las = 1,
     freq = F,
     ylim = c(0,0.25),
     xlim = c(8,30))

# Agregamos línea de densidad
lines(density(datosb$MXNUSD),col = "black",lwd = 3)

#Series de tiempo
mxnusd_t <- ts(datosb$MXNUSD,
               start = c(2009, 1), #inicia el primer mes en 2009
               frequency = 12)
ts.plot(mxnusd_t)

plot(mxnusd_t, 
     ylab = "",
     xlab = "",
     col = "blue",
     lwd = 2,
     las = 1,
     main = "Tipo de cambio",
     cex.main = 1)
grid()

# Pruba de dsitribución de Bondad de ajuste
((140/6)*((skewness(datosb$MXNUSD)**2))+((1/4)*((skewness(datosb$MXNUSD)-3)**2)))
jarque.bera.test(datosb$MXNUSD)