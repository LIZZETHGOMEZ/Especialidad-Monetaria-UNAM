# ############################################
#             Econometría
#           Series de tiempo
#               Tarea
#       Lizzeth Gómez Rodríguez
# #############################################


# TAREA: Replicar TC Mensual (Freq = 12)
# Utilizar archivo con datos mensuales "Datos_mensual_TC"
# MA = 3; MA = 6, MA = 12 (meses)

getwd()
setwd("../Tareas")

# Cargamos datos
tc_mensual <- read.csv('Datos_mensual_TC.csv')
head(tc_mensual)

options(scipen = 999)
# installed.packages("TTR")
library(TTR)

# Tranformamos a serie de tiempo
tcm_ts <- ts(tc_mensual$TCMXN,start = c(2018,1), frequency = 12)
summary(tcm_ts)

# Graficamos
ts.plot(tcm_ts, lwd = 2, col = 'blue')

# Construimos medias móviles
tcm_tsma3 <- SMA(tcm_ts, n = 3)
tcm_tsma6 <- SMA(tcm_ts, n = 6)
tcm_tsma12 <- SMA(tcm_ts, n = 12)

# Graficamos la serie original con las medias móviles
ts.plot(tcm_ts,tcm_tsma3, tcm_tsma6, tcm_tsma12,
        col = c('black', 'darkgreen', 'blue', 'red'),
        lwd = c(2,1,1,1))
grid()

# Agregamos leyendas:
legend(x = 'topright', legend = c('Original', 'MA3', 'MA6', 'MA12'),
       col = c('black', 'darkgreen', 'blue', 'red'),
       lty = c(1,1,1),
       inset = 0.05,
       bty = 'n',
       cex = 0.8)




