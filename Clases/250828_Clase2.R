# -----------------------------------------------------------------
#                   28 de agosto de 2025
#                        Práctica 2
#                  Lizzeth Gómez Rodríguez
# ------------------------------------------------------------------

getwd()
setwd("./Documents/Especialidad/Econometría/RData")

# Cargamos base de datos
library(readxl)
MARCADOS <- read_excel("MARCADOS.xlsx")

#Para evitar notación cientifica de exponencial (e) utilizamos options()
options(scipen = 9000)

#Para manipular el objeto
library(textshape)
library(tidyverse)

#Queremos una columna para usarla como index
datosb <- textshape::column_to_rownames(MARCADOS,1)

# ---------------------------------------------------------------------
# La Covarianza cov(x,y) mide la independencia o dependencia de las variables
# Si cov(x,y) = 0 : son independientes
# Si cov(x,y) =!0 : con dependientes
# El signo de la covarianza indica si la relación es 
# directa(positiva) o inversa(negativa) de las variables
# e: coeficiente de correlación


# Matriz de covarianza
mat_cov <- cov(datosb)
mat_cov


# Matriz de correlación
mat_cor <- cor(datosb)
mat_cor

# -------------------------------------------------------------------
# Gráficos
install.packages('ggcorrplot')
library(ggcorrplot)
library(ggplot2)

# Gráfico de correlación entre las variables
ggcorrplot(mat_cor)

# Gráfico de correlación (De componentes principales)
install.packages('corrplot')
library(corrplot)

corrplot(mat_cor,
         method = 'number')


# Diagrama de dispersión
# MXNUSD = f(WTI)

#Observese una relación negativa en el gráfico
plot(datosb$WTI, datosb$MXNUSD,
     xlab = 'WTI', ylab = 'MXNUSD',
     las = 1,
     col = "darkorange",
     lwd = 2)
grid()

# Trazamos la linea de regresión
# Definimos la linea estimada y usaremos los mínimos cuadrados ordinarios
y_est <- lm(MXNUSD~WTI, data = datosb)

#lty=1 línea no continua
abline(y_est, lwd = 2, lty = 2, col = 'blue')


# Ejercicio
# Bono en función del IPC
plot(datosb$MEX10A,datosb$IPC_MXX, 
     xlab = "BonoM", ylab = "IPC",
     col = "darkgreen",
     las = 1,
     lwd = 2)
grid()

y_est2 <- lm(IPC_MXX ~ MEX10A, data = datosb)
abline(y_est2, lwd =2,col = "darkorange",lty = 2)

# --------------------------------------------------------------------
# MODELO 1
# MXNUSD = f(MEX10A, IPC_MXX, WTI)
# --------------------------------------------------------------------

ls(datosb)
mod1 <- lm(MXNUSD ~ MEX10A + IPC_MXX + WTI, data = datosb)
mod1

# MXUSD = 9.69 + 0.78*MEX10A + 0.00019*IPC -0.97*WTI
# Y = b0 + b1x1 + b2x2 +..
# El incremento del rendimiento del bono en una unidad, en este caso, en 1%,
# entonces el tipo de cambio se aprecia en 78 centavos, ya que aumenta 0.78 pesos por dólar
# por cada unidad de incremento del Bono

# Para el barril de WTI, si aumenta el precio del barril en una unidad, 
# el TC se deprecia en 0.097 centavos

# ---------------------------------------------------------------------
# Resumen estadístico del modelo
summary(mod1)
# Nótese que se generan los residuales:
# Y = b0 + b1x1 + b2x2 +... + u, donde las bx son la y estimada: ye, u:residual
# Y = ye + u
# En el grpafico, los residuales son la distancia entre las observaciones
# y la y estimada = la recta de la gráfica

# Recordar que la mediana es el 2do cuartil
# Q1, Q2=Median, Q3

# Si tomamos el valor estimado y lo dividimos entre la desviación estpandar
# obtendremos el valor t
# ejercicio de comprobación con datos del Bono
0.78684632/0.14658603


#---------------------------------------------------------------
# Matriz Varianza-Covarianza de los Estimadores
vcov(mod1)
round(vcov(mod1),7)

# El error estándar se obtiene de la raiz cuadrada de la varianza
# verificamos con el intercepto
sqrt(2.7332391) #1.653251
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
# SÍ EXPLICAN AL TIPO DE CAMBIO


# -----------------------------------------------------------------
# Varianza de la regresión
# Var = suma de los residuales al cuadrado entre los grados de libertad 
# donde grados de libertad = (n-k)
# var = u2/(n-k)

# Notemos:
# +(u) = +(y-ye) = 0
# +(u2) = +(y-ye)2 -> minimizar la distancia MLO (minimos cuadrados ordinarios)
# El modelo marca un R2 = 0.78
# Es decir que las variables explican el 78% del tipo de cambio

# Prueba Global o Prueba F, en donde:
# H0: b0 = b1 = b2 ...
# H1: b0 =! b1 =! b2
# Con el P-value se observa que las variables sí explican a la variable dependiente


# ------------------------------------------------------------------
# Tabla ANOVA
library(lmtest)
anova(mod1)

# Columna de Sum sq: Suma de los cuadrados indica la variable que mayor información aporta para
# explicar la variable dependiente, en este caso es el WTI

#Columna Mean sq, es la suma de los cuadrados dividido entre los grados de libertad

# Coeficiente de determinación o Ji-cuadrada -> Indicador de linealidad
# R2 = 1-(+u2/st2) con st: desviación estándar (REVISAR)


# ------------------------------------------------------------
# MODELO 2
# Le quitamos el intercepto con el cero
# -------------------------------------------------------------
ls(datosb)
mod2 <- lm(MXNUSD ~ 0 + MEX10A + IPC_MXX + WTI, data = datosb)
summary(mod2)
anova(mod2)

# Un modelo aceptable es que la suma explicada de los cuadrados es mayor a
# la suma de los residuales al cuadrado
# Si la R2 es cercana a 1, se tendrá una F estadística alta
# Problema de especificación, cuando la F es grande, pero la R2 es baja

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


# --------------------------------------------------------------
# Pruba de especificación
# Obtener a los resicuales

error <- mod1$residuals

# type = "l" de line
plot(error, type = "l", col = "red", las = 1)
grid()
# línea que cruza por el cero, h: de horizontal
abline(h = 0)

#-----------------------------------------------------
# HISTOGRAMA
# Histograma para ver si existe normalidad
# Con frecuencia en false, porque queremos calcular la densidad (se ajusta en el eje y)
hist(error, col = 'lightgreen', freq = F, las = 1)
lines(density(error),col = 'red', lwd = 2)

# ---------------------------------------------------
# GRÁFICO DE CAJAS
#Nótese en el gráfico que se tienen valores atípicos
boxplot(error, col = "pink", horizontal = T)

#----------------------------------------------------
# Jarque-Bera
# H0: distribución normal N(0,var2), media cero y varianza mínima
# H1: No se distribuye como una normal
# recordar que si P-Value < 0.005 se rechaza la H0
jarque.bera.test(error) # los residuales no se distribuyen como una normal, ya que P-value < 0.05
kurtosis(error) # Probelma de varianza
skewness(error) # está sesgada

# ------------------------------------------------------------------
# MULTICOLINEALIDAD

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

install.packages('olsrr')
library(olsrr)
ols_vif_tol(mod1)
# Notemos que el resultado es menor a 5, por lo que no hay multicolinealidad
