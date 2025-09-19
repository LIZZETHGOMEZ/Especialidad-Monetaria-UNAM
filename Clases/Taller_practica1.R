# -----------------------------------------------------------------
#                   17 de septiembre 2025
#                        Práctica 1 en R
#                           Taller
#                  Lizzeth Gómez Rodríguez
# ------------------------------------------------------------------

getwd()
setwd("./clases")

# Cargamos Matriz de Retornos
library(readxl)
R <- read_excel("Taller_Data_activos_clase.xlsx")

# Convertimos a Matriz y quitamos la fecha
R <- as.matrix(R[,-1])

# Vector de retorno anual (integra a todos los activos)
mu <- colMeans(R)*252 #Promedio anualizado
mu

# Matriz de varianzas-covarianzas
sigma <- var(R)*252

# Volatilidad Desviacipon estándar
# apply permite operar con: (base, 2 = columnas, aplicar la desviaión estándar)
# 1 = aplica a filas
SD_i <- apply(R,2,sd)*sqrt(252)
SD_i

# Matriz de correlaciones
cor(R)

# Métricas
n <- ncol(R) # número de activos
n
w_naive = rep(1/n,n) # Portafolio sum(w) =1 pesos iguales
w <- w_naive
# w <- c(0.37,0.10,0.23,0.37,0.4,0,0)

# creamos una funcionl, con solo un argumento, no se requieren las llaves
# Funciones retorno y volatilidad
p_r <- function(w) sum(w*mu) #Retorno del portafolio
p_sd <- function(w) sqrt(t(w)%*%sigma%*%w) # volatilidad del portafolio (usamos transpuesta)

# Matrices portafolio
p_r(w)
p_sd(w)

# Ratio de diversificación
div_ratio = p_sd(w) /SD_i[1]
div_ratio

# beta
beta = cov(R%*%w, R[,1]) / var(R[,1]) #logica de columna Q
beta

# betas individuales
beta_i <- apply(R,2,function(x) cov(x,R[,1]) / var(R[,1]))
beta_i
beta = sum(w*beta_i)

#Ratios ajustados si:
rf = 0.08
sharpe = (p_r)



