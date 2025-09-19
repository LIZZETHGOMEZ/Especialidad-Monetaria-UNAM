
# ------------------------------------------------------------------
#                   18 de septiembre de 2025
#                        Práctica 3
#                  Lizzeth Gómez Rodríguez
# ------------------------------------------------------------------

getwd()

install.packages("tseries")
library(tseries)

# Descargamos los datos de Arca Continental
# Frecuencia mensual : = "m"

ACMX_m <- get.hist.quote(instrument = 'AC.MX',
                         start = '2021-1-4',
                         end = '2025-9-18',
                         quote = c('Open', 'Close', 'High', 'Low'),
                         provider = c('yahoo'),
                         compression = 'm')


# Graficamos
plot(ACMX_m, type = 'l', col = 'red')

plot(ACMX_m$Close, type = 'l', col = "green", las = 1)
grid()

# Televisa
TLVISA_m <- get.hist.quote(instrument = 'TLEVISACPO.MX',
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

# NVIDIA MEXICO
NVDIA_m <- get.hist.quote(instrument = 'NVDA',
                         start = '2021-1-4',
                         end = '2025-9-18',
                         quote = c('Open', 'Close', 'High', 'Low'),
                         provider = c('yahoo'),
                         compression = 'm')

plot(NVDIA_m$Close, type = 'l', las=1)
grid()

# -------------------------------------------------------------------
# Generar variables dummy (1,0)
# 1. Se utiliza los precios al cierre para obtener la tasa de variación porcentual


# Tasa de variación 
# Tasa var = ((xt / xt-1)-1)*100

# Agregamos variación al dataset de ACMX (el lag es para los rezagos a aplicar)
ACMX_m$t_var <- ((ACMX_m$Close/lag(ACMX_m$Close,1))-1)*100
head(ACMX_m$t_var)
summary(ACMX_m)

# Aplicamos condicionales
# dum = 1 cuando t_var > 0
# dum = 0 cuando t_var > 0

# Definimos dummys 

# ARCA CONTINENTAL
ACMX_m$dum1 <- ifelse(ACMX_m$t_var > 0, 1, 0)
head(ACMX_m)

ACMX_m$dum2 <- ifelse(ACMX_m$t_var >= 0, 'Hola', 'Mundo')
head(ACMX_m,20)

ACMX_m$dum3 <- ifelse(ACMX_m$t_var > 180, 'Compro', 'Vendo')
head(ACMX_m$dum3)



# TELEVISA
TLVISA_m$t_var <- ((TLVISA_m$Close/lag(TLVISA_m$Close,1))-1)*100
head(TLVISA$t_var)

TLVISA_m$dum1 <- ifelse(TLVISA_m$t_var > 0, 1, 0)
head(TLVISA_m)


# NETFLIX
NTFLIX_m$t_var <- ((NTFLIX_m$Close/lag(NTFLIX_m$Close,1))-1)*100

NTFLIX$dum1 <- ifelse(NTFLIX$t_var > 0, 1, 0)
head(NTFLIX)

# BMV
BMV_m$t_var <- ((BMV_m$Close/lag(BMV_m$Close,1))-1)*100

BMV_m$dum1 <- ifelse(BMV_m$t_var > 0, 1, 0)
head(BMV_m)

# NVDIA
NVDIA_m$t_var <- ((NVDIA_m$Close/lag(NVDIA_m$Close,1))-1)*100

NVDIA_m$dum1 <- ifelse(NVDIA_m$t_var > 0, 1, 0)
head(NVDIA_m)

# ------------------------------------------------------------------
# Probabilidad de que la acción de Televisa = 1 dado la BMV y Netflix
# recordar que si = 1 -> rendimientos positivos
# Prob(TLVISA = 1 | BMV, NTFLIX)

datos_logit <- data.frame(TLVISA_m$dum1,
                          BMV_m$Close,
                          NTFLIX_m$Close)
head(datos_logit)
str(datos_logit)

# Definimos nombres de columnas
names(datos_logit) <- c('Tlvisa', 'BMV', 'Netflix')

# matriz de correlación
cor(datos_logit)


# --------------------------------------------------------------
# Modelo 1. Modelo Probabilidad Lineal (MPL)
# Pr(Tlvisa = 1 | BMV, NTFLIX)

options(scipen = 999)
MPL <- lm(Tlvisa ~ BMV + Netflix, 
          data = datos_logit)
summary(MPL)

# Obtener las probabilidades del MPL
# Prob(Tlvisa = 1 | BMV, Netflix) = -0.23221604 + 0.00001848*BMV - 0.00001848* Netflix

# Considerar precio promedio de BMV y Netflix:
summary(datos_logit)
 -0.23221604 + (0.00001848*52513) - (0.00001557*11228)

# Si el índice bursátil de la bolsa mexicana está en su promedio y netflix también,
# la probabilidad de que el rendimiento de televisa sea positivo es de 53%

# Considerando valores minimos:
-0.23221604 + (0.00001848*44593) - (0.00001557*3485)

# Considerando maximos
-0.23221604 + (0.00001848*61596) - (0.00001557*25090)


# Modelos lineales generalizados
  Logit1 <- glm(Tlvisa ~ BMV + Netflix,
              data = datos_logit,
              family = 'binomial')

Logit1

# Calculamos probabilidades individuales
pr_indv <- predict(Logit1, type = 'response')
head(pr_indv)
pr_indv
summary(pr_indv)
# Las probabilidades rondan en 50% de que los rendimientos de televisa sean positivos


# odds ratio (El ratio de probabilidad)
OR <- exp(cbind(OR = coef(Logit1), confint(Logit1)))
OR 
# los factores dan 1 a 1 con BMV y 0.99 de Netflix,
# por lo que ambos dan igual ya que la pronbabilidad de que el rendimiento de Televisa = 1 pues es la misma


# Eficiencia del modelo
clasificacion <- predict(Logit1, type = 'response')
eficiencia <- ifelse(clasificacion < 0.5, 0, 1) #El umbral es la proba, si los valores son >0.5 es 0, si no, es 1

table(eficiencia)
# El modelo indica que 15 veces tuvo rendimiento negativo y 41 veces rendimiento positivo

# Comprobamos con datos reales:
table(datos_logit$Tlvisa)
# indica que 24 fueron negativos y 31 positivo

# --------------------------------------------------------------------------
# Tarea
# Replicar el script con Netflix hasta el OR


