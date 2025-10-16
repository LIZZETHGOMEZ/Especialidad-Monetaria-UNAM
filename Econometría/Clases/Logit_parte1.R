
# ESPECIALIDAD EN ECONOMIA MONETARIA Y FINANCIERA
# ECONOMETRIA MONETARIA Y FINANCIERA

## MODELO LOGIT

install.packages('tseries')
library(tseries)
install.packages('zoo')
library(zoo) 

# Arca Continental
# Frecuencia mensual := 'm'

ACMX_m <- get.hist.quote(instrument = 'AC.MX',
                         start = '2021-1-4',
                         end = '2025-8-29',
                         quote = c('Open', 'Close', 'High', 'Low'),
                         provider = c('yahoo'),
                         compression = 'm')
                         
plot(ACMX_m,
     type = 'l',
     col = 'red')


plot(ACMX_m$Close,
     type = 'l',
     col = 'red',
     las = 1)  
grid()

# TELEVISA

TLVISA <-get.hist.quote(instrument = 'TLEVISACPO.MX',
                        start = '2021-1-4',
                        end = '2025-8-29',
                        quote = c('Open', 'Close', 'High', 'Low'),
                        provider = c('yahoo'),
                        compression = 'm')

# IPC BMV

BMV_m <- get.hist.quote(instrument = '^MXX',
                        start = '2021-1-4',
                        end = '2025-8-29',
                        quote = c('Open', 'Close', 'High', 'Low'),
                        provider = c('yahoo'),
                        compression = 'm')

# NETFLIX MEXICO

NTFLIX_m <- get.hist.quote(instrument = 'NFLX.MX',
                           start = '2021-1-4',
                           end = '2025-8-29',
                           quote = c('Open', 'Close', 'High', 'Low'),
                           provider = c('yahoo'),
                           compression = 'm')


NVIDIA <-get.hist.quote(instrument = 'NVDA',
                        start = '2021-1-4',
                        end = '2025-8-29',
                        quote = c('Open', 'Close', 'High', 'Low'),
                        provider = c('yahoo'),
                        compression = 'm')


plot(NVIDIA$Close,
     type = 'l',
     col = 'red',
     las = 1)  
grid()

### Generar las variable dummy (1,0)  

# Primero se utiliza los precios al cierre para obtener la
# tasa de variación porcentual


# t_var = ((Xt / Xt-1)-1)*100

# ACMX_m

ACMX_m$t_var <- ((ACMX_m$Close / lag(ACMX_m$Close,1))-1)*100
head(ACMX_m)

summary(ACMX_m$t_var)

# Con el comando ifelse se utilizan los valores logicos / condicionales
## > (mayor que); < (menor que); == (igual a );
## >= (mayor igual); <= (menor igual)

# Despues de la condición logica, se introduce los valores de la variable
# que va adoptar (estos numericos o de texto)

# En el caso del ejemplo
# dum = 1 cuando t_var > 0
# dum = 0 cuando tvar < 0

ACMX_m$dum1 <- ifelse(ACMX_m$t_var > 0, 1, 0)
head(ACMX_m, 20)

#ACMX_m$dum2 <-ifelse(ACMX_m$t_var >= 0, 'Hola', 'Mundo')
#head(ACMX_m,20) 

summary(ACMX_m$Close)


dum3 <-ifelse(ACMX_m$t_var > 180, 'Compro', 'Vendo')
head(dum3)  

# TLVISA

TLVISA$t_var <- ((TLVISA$Close / lag(TLVISA$Close,1))-1)*100
head(TLVISA)

TLVISA$dum1 <- ifelse(TLVISA$t_var > 0, 1, 0)
head(TLVISA, 20)

# NTFLIX MEXICO

NTFLIX_m$t_var <- ((NTFLIX_m$Close / lag(NTFLIX_m$Close,1))-1)*100
head(NTFLIX_m, 20)

NTFLIX_m$dum1 <- ifelse(NTFLIX_m$t_var > 0, 1, 0)
head(NTFLIX_m, 20)

# IPC BMV

BMV_m$t_var <- ((BMV_m$Close / lag(BMV_m$Close,1))-1)*100
head(BMV_m)

BMV_m$dum1 <- ifelse(BMV_m$t_var > 0, 1, 0)
head(BMV_m, 20)

# Crear un nuevo dataframe

datos_logit <- data.frame(TLVISA$dum1,
                          ACMX_m$Close,
                          BMV_m$Close,
                          NTFLIX_m$Close)
head(datos_logit)

# Prob(TLVISA = 1 | ACMX, BMV, NTFLIX)

str(datos_logit)

View(datos_logit)

# Cambio de nombre a las variables
names(datos_logit) <- c('Tlvisa',
                        'ACMX',
                        'BMV',
                        'Netflix')
names(datos_logit)

cov(datos_logit)
cor(datos_logit)

#### Eliminar datos no observados "NA"

datos_logit <- na.omit(datos_logit)

cor(datos_logit)
cov(datos_logit)

cor(datos_logit[,2:4])

# Modelo 1. Modelo Probabilidad Lineal (MPL)

options(scipen = 999)

# Pr(Tlvisa = 1 | ACMX, BMV, Netflix)

MPL <- lm(Tlvisa ~ ACMX + BMV + Netflix,
          data = datos_logit)

summary(MPL)

# Obtener las probabilidades del MPL

# Prob(Tlvisa = 1 | BMV, Netflix) = -0.23221604 + 0.00092893*ACMX + 0.00001848*BMV -0.00001557*Netflix

#Calcular las probabilidades de los medios medios

summary(datos_logit)


0.09075082 + (0.00092893*(159.04)) + (0.00001025*(52230)) -(0.00002020*(10811))

# Bajo este escenario de los valores medios en BMV y Netflix, la Prob de que el
# rendimiento de televisa > 0 es de %

# Valores minimos = 56.54%

0.09075082 + (0.00092893*(159.04)) + (0.00001025*(52230)) -(0.00002020*(10811))

# Valores maximos = 37.55%

0.09075082 + (0.00092893*(159.04)) + (0.00001025*(52230)) -(0.00002020*(10811))

# Modelo 2. Modelo Logit


Logit1 <- glm(Tlvisa ~ ACMX + BMV + Netflix,
              data = datos_logit,
              family = binomial(link = 'logit'))

Logit1

pr_indv <- predict(Logit1, type = 'response')
head(pr_indv)
pr_indv

summary(pr_indv)

# odds ratio


OR <- exp(cbind(OR = coef(Logit1),
                confint(Logit1)))
OR


# Efciencia del modelo

clasificacion <- predict(Logit1, type = 'response')
eficiencia <- ifelse(clasificacion < 0.5, 0, 1)

table(eficiencia)

table(datos_logit$Tlvisa)

table(datos_logit$Tlvisa, eficiencia)

# De acuerdo a la tabla anterior
## De las veces que Tlvisa = 0; fueron correctamente clasificados 9,
## mientras que 7 estan marcados como rendimiento negativo cuando estos son
## periodos con rendimiento positivo

## Mientras tanto, cuando Tlvisa = 1; 23 estan correctamente clasificados
## 15 son falsos positivos o que el modelo considera que hubo un rendimiento
## pero hubo un rendimiento negativo

### Curva ROC
## Compara la sensibilidad y especificidad del modelo
## sensibilidad -> el modelo clasifica correctamente
## especificidad -> que tanto el modelo clasifica de manera incorrecta

install.packages('ROCR')
library(ROCR)

curva_ROC <- prediction(fitted(Logit1), datos_logit$Tlvisa)
plot(performance(curva_ROC, 'tpr', 'fpr'),
     col = 'brown3')
abline(0,1,
       lty = 12,
       col = 'darkgreen')
grid()

# Grafica logit

plot(x = datos_logit$ACMX,
     y = datos_logit$Tlvisa,
     main = 'Rendimiento de Televisa',
     xlab = 'ACMX',
     ylab = 'Televisa',
     pch = 20,
     las = 1,
     ylim = c(-0.5, 1.5),
     xlim = c(90, 220),
     cex.main = 0.9)
grid()

summary(datos_logit$ACMX)

# agregar una linea en los puntos donde y = {0, 1}
abline(h = 1, lty = 2, col = 'darkred')
abline(h = 0, lty = 2, col = 'darkred')
text(180, 0.9, cex = 0.8, 'Rendimiento Positivo')
text(180, -0.1, cex = 0.8, 'Rendimiento Negativo')

Logit2 <- glm(Tlvisa ~ ACMX,
              data = datos_logit, 
              family = binomial(link = 'logit'))

x <- seq(90, 220, 0.001)
y_logit <- predict(Logit2, 
                   list(ACMX = x),
                   type = 'response')
lines(x, y_logit,
      lwd = 1.5,
      col = 'red',
      lty = 2)

# Efecto Marginal para los valores medios

install.packages('mfx')
library(mfx)

logitmfx(formula = Tlvisa ~ ACMX + BMV + Netflix,
         data = datos_logit)


head(datos_logit)


0.000941738*100
0.000010515*100
-0.00002047*100

# Tarea
# Replicar el script para estimar el modelo MPL y Logit (hasta el Efecto Marginal)
# Utilizando como variable dependiente (respuesta) a la accion de netflix
# netflix = f(Tlevisa, BMV, ACMX)
 
