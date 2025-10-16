# ###############################################
#               Econometría
#               2 de Octubre
#             Series de tiempo
# ###############################################
getwd()
setwd("../Econometría/Clases")

# Cargamos base de datos
TC_diario <- read.csv("TC_diario.csv")
head(TC_diario)

# -------------------------------------------------------
# Descomposición de series de tiempo
# Medias Moviles
options(scipen = 999)
installed.packages("TTR")
library(TTR)

# Describir la serie a partir de su componente de tendencia,
# Se emplea el método aditivo, ya que este es muy común para suavizar o emplear
# el método de la media móvil

# Transformamos la serie, en una serie de tiempo
tc_ts <- ts(TC_diario$MXNUSD,start = c(2021,4), frequency = 260)
# 260 días,iniciando en el mes 4 del año 2021
summary(tc_ts)

# Graficamos la serie de tiempo
ts.plot

# Construir las medias móvilees: 50, 40, 20, 10 (son días)
# s: estacional, M: movil, A: average
tc_tsma50 <- SMA(tc_ts, n = 50)
tc_tsma40 <- SMA(tc_ts, n = 40)
tc_tsma20 <- SMA(tc_ts, n = 20)
tc_tsma10 <- SMA(tc_ts, n = 10)

# Graficamos la serie original con las medias móviles
ts.plot(tc_ts,tc_tsma10, tc_tsma20, tc_tsma40, tc_tsma50,
        col = c('black', 'darkgreen', 'blue', 'red', 'orange'),
        lwd = c(0.5,1,1,1,1))
grid()

ts.plot(tc_ts,tc_tsma10, tc_tsma50,
        col = c('black','blue', 'red'),
        lwd = c(1,1,1,1,1))

# Agregamos leyendas
# 'bottomright, 'bottom', 'bottomleft', 'left
# 'topleft, 'toprroght', 'topleft'
# 'center', 'right'

legend(x = 'bottomleft', legend = c('Original', 'MA10', 'MA50'),
       col = c('black', 'blue', 'red'),
       lty = c(1,1,1),
       inset = 0.05,
       bty = 'n')

# --------------------------------------------------
# TAREA: Replicar TC Mensual (Freq = 12)
# Utilizar archivo con datos mensuales "Datos_mensual_TC"
# MA = 3; MA = 6, MA = 12 (meses)
# -------------------------------------------------

# Utilizamos el archivo "publico" de escel que contiene ingreso y gasto
library(readxl)
publico <- read_excel('publico.xlsx')

ingreso_ts <- ts(publico$ingreso, start = c(1977,1), frequency = 12)
ts.plot(ingreso_ts)
# Se muestrea que la serie tiene un compnente estaional fuerte

# Desestacionalizamos la serie
install.packages('seasonal')
library(seasonal)

des_ingreso <- seas(ingreso_ts, x11 = '')
plot(des_ingreso)

gasto_ts <- ts(publico$gasto, start = c(1977, 1), frequency = 12)
ts.plot(gasto_ts)

des_gasto <- seas(gasto_ts, x11 = '')
plot(des_gasto)

# Extraemos los datos
des_gasto2 <- data.frame(des_gasto)
head(des_gasto2)

des_ingreso2 <- data.frame(des_ingreso)
head(des_ingreso2)

plot(des_ingreso2$final, type = 'l')
plot(des_gasto2$final, type = 'l')

# ---------------------------------------------------
# Otro método de descomposición de la serie
ingreso_desc <- decompose(ingreso_ts)
plot(ingreso_desc)

# Mejor tomamos la serie desestacionalizada
# primero la volvemos a convertir en serie de teimpo, ya que es un df
ingreso2_ts <- ts(des_ingreso2$final,start = c(1997,1), frequency = 12)

# descomponemos la serie en los 4 comoponentes:
# 1. Tendencia, 2. Estacionalidad, 3. Iregularidad
ingreso_desc <- decompose(ingreso_ts)
plot(ingreso_desc)

# Extraemos el componente de estaciobalidad para crear el índice de estacionalidad
plot(ingreso_desc$figure, type = 'b',
     xlab = 'Mes',
     ylab = 'índice',
     main = 'índice estacional del ingreso',
     cex.main = 0.8,
     col = 'purple', las = 1, lwd = 2)
grid()

# Repolicamos para gasto
gasto_desc <- decompose(gasto_ts)
plot(gasto_desc)

# Extraemos el componente de estacionalidad para crear el índice de estacionalidad
plot(gasto_desc$figure, type = 'b',
     xlab = 'Mes',
     ylab = 'índice',
     main = 'índice estacional del ingreso',
     cex.main = 0.8,
     col = 'darkgreen', las = 1, lwd = 2)
grid()

# #############################################
#                   Parte 2
# ############################################

# Filtro Holdrick Prescott
install.packages("mFilter")
library(mFilter)

options(scipen = 999)

d_publico <- data.frame(des_ingreso2$final, des_gasto2$final)
ts.plot(d_publico$des_ingreso2.final)

ingreso2 <- ts(d_publico$des_ingreso2.final, start = c(1977,1), frequency = 12)
ts.plot(ingreso2)

# ----------------------------------------------------------
#Filtro Holdrick Prescott

# Para el caso del filtro Holdrick-Prescott se define 
# el siguiente parametro lambda

# lambda = 100 -> datos anuales
# lambda = 1600 -> datos trimestrales
# lambda = 14400 -> datos mensuales

# Caso ingreso
ingreso2_HP <- hpfilter(ingreso2, freq = 14400)
plot(ingreso2_HP)
ingreso2_HP <- data.frame(ingreso2_HP$trend)


# ----------------------------------
# Caso del gasto
gasto2 <- ts(d_publico$des_gasto2.final, start = c(1977,1), frequency = 12)
ts.plot(gasto2)

gasto2_HP <- hpfilter(gasto2, freq = 14400)
plot(gasto2_HP)

gasto2_HP <- data.frame(gasto2_HP$trend)

# ---------------------------------------------------------
# Filtro Cristiano Fitzgerald

#Caso ingreso
ingreso2_CF <- cffilter(ingreso2)
plot(ingreso2_CF)
ingreso2_CF <- data.frame(ingreso2_CF$trend)


# Caso gasto
gasto2_CF <- cffilter(gasto2)
plot(gasto2_CF)
gasto2_CF <-data.frame(gasto2_CF$trend)

# Crear dataframe 
datos_publico <- data.frame(publico$mes,
                             d_publico$des_ingreso2.final,
                             d_publico$des_gasto2.final,
                             ingreso2_HP$Series.1,
                             ingreso2_CF$Series.1,
                             gasto2_CF$Series.1,
                            gasto2_HP$Series.1)


# Guardamos Dataframe



