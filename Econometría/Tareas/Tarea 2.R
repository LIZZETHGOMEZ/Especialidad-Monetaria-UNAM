
# ######################################################################
#                         Econometría  
#                           Tarea 2
#                   Modelo de Probabilidades
#                   Lizzeth Gómez Rodríguez
# ######################################################################
# Replicar el script para estimar el modelo MPL y Logit (hasta eficiencia del modelo)
# Utilizando como variable dependiente (respuesta) a la accion de netflix
# netflix = f(Tlevisa, BMV, ACMX)


# install.packages("tseries")
library(tseries)

# Descargamos los datos
# Frecuencia mensual : = "m"

# Televisa
TLVISA_m <- get.hist.quote(instrument = 'TLEVISACPO.MX',
                           start = '2021-1-4',
                           end = '2025-9-18',
                           quote = c('Open', 'Close', 'High', 'Low'),
                           provider = c('yahoo'),
                           compression = 'm')

# ACMX
ACMX_m <- get.hist.quote(instrument = 'AC.MX',
                           start = '2021-1-4',
                           end = '2025-9-18',
                           quote = c('Open', 'Close', 'High', 'Low'),
                           provider = c('yahoo'),
                           compression = 'm')

# IPC BMV
BMV_m <- get.hist.quote(instrument = '^MXX',
                        start = '2021-1-4',
                        end = '2025-9-18',
                        quote = c('Open', 'Close', 'High', 'Low'),
                        provider = c('yahoo'),
                        compression = 'm')

# NETFLIX MEXICO
NTFLIX_m <- get.hist.quote(instrument = 'NFLX.MX',
                           start = '2021-1-4',
                           end = '2025-9-18',
                           quote = c('Open', 'Close', 'High', 'Low'),
                           provider = c('yahoo'),
                           compression = 'm')


# -------------------------------------------------------------------------
# Generamos tasa de variación (rendimientos)
NTFLIX_m$t_var <- ((NTFLIX_m$Close/lag(NTFLIX_m$Close,1))-1)*100

# Generamos variable dummy:
# dum = 1 cuando t_var > 0
# dum = 0 cuando t_var > 0

NTFLIX_m$dum1 <- ifelse(NTFLIX_m$t_var > 0, 1, 0)
head(NTFLIX_m)

# Generamos datos logit
datos_logit <- data.frame(NTFLIX_m$dum1, TLVISA_m$Close, BMV_m$Close, ACMX_m$Close)
head(datos_logit)
str(datos_logit)

# Definimos nombres de columnas
names(datos_logit) <- c('Netflix', 'Televisa', 'BMV', "ACMX")
names(datos_logit)

# Eliminamos NAs
datos_logit <- na.omit(datos_logit)

# matriz de correlación y covarianza
cor(datos_logit)
cov(datos_logit)


# --------------------------------------------------------------
# Modelo 1. Modelo Probabilidad Lineal (MPL)
# Pr(Netflix = 1 | Televisa, BMV, ACMX)

options(scipen = 999)
MPL <- lm(Netflix ~ Televisa + BMV + ACMX, 
          data = datos_logit)
summary(MPL)

# Obtener las probabilidades del MPL
# Prob(Netflix = 1 | Televisa, BMV) = -0.93079179 + 0.00256812*Televisa + 0.00004303*BMV -0.00591190*ACMX

# Considerar precio promedio de Televisa, BMV y ACMX:
summary(datos_logit)
-0.93079179 + (0.00256812*22.73) + (0.00004303*52348) - (0.00591190*159.62)

# Si el índice bursátil de la bolsa mexicana está en su promedio y netflix también,
# la probabilidad de que el rendimiento de televisa sea positivo es de 53%

# Considerando valores minimos:
-0.93079179 + (0.00256812*6.91) + (0.00004303*44593) - (0.00591190*94.77)

# Considerando maximos
-0.93079179 + (0.00256812*57.02) + (0.00004303*58709) - (0.00591190*213.99)


# Modelos lineales generalizados
Logit1 <- glm(Netflix ~ Televisa + BMV + ACMX,
              data = datos_logit,
              family = 'binomial')

Logit1

# Calculamos probabilidades individuales
pr_indv <- predict(Logit1, type = 'response')
head(pr_indv)
pr_indv
summary(pr_indv)
# Las probabilidades rondan en promedio en 40%  de que los rendimientos 
# de Netflix sean positivos


# odds ratio (El ratio de probabilidad)
OR <- exp(cbind(OR = coef(Logit1), confint(Logit1)))
OR 
# los factores dan 1 a 1 con Televisa y  BMV,
# por lo que ambos dan igual ya que la probabilidad de que el rendimiento de Netflix = 1 pues es la misma


# Eficiencia del modelo
clasificacion <- predict(Logit1, type = 'response')
eficiencia <- ifelse(clasificacion < 0.5, 0, 1) #El umbral es la proba, si los valores son >0.5 es 0, si no, es 1

table(eficiencia)
# El modelo indica que 41 veces tuvo rendimiento negativo y 14 veces rendimiento positivo

# Comprobamos con datos reales:
table(datos_logit$Netflix)
# indica que 31 fueron negativos y 24 positivo



