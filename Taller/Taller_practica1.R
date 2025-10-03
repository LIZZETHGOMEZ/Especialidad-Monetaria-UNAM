  # -----------------------------------------------------------------
  #                   17 de septiembre 2025
  #                        Práctica 1 en R
  #                           Taller
  #                  Lizzeth Gómez Rodríguez
  # ------------------------------------------------------------------
  
  getwd()
  setwd("./Taller/")
  
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
  
  # Ratios ajustados si:
  rf = 0.08
 

  # --------------------------------------------------------------
  # 24 de sep
  
  # ratiso ajustados si:
  rf = 0.08
  sharpe = (p_r(w) - rf) / p_sd(w) 
  # Que tanto está cernano a la tasa libre de riesgo (se busca una tasa alta)
  # Cunato se obtienen de retorno por unidad de riesgo
  
  
  Traynor = (p_r(w) - rf) / beta
  
  # Capital AsseT Pricing Model
  CAPM = rf + beta*(p_r(w) -rf)
  
  # Mínima varianza global
  # Multiplicamos la inversa de sigma, %*% es una operación matricial
  w_mvg <- solve(sigma)%*%rep(1,n) / as.numeric(t(rep(1,n))%*% solve(sigma)%*%rep(1,n))

  # Mínima varianza retorno target  
  p_MV <- function(mu_T) {
    v1 <- rep(1,n)
    A <- (t(v1)%*%solve(sigma)%*%v1)[1]
    B <- (t(mu)%*%solve(sigma)%*%v1)[1]
    C <- (t(v1)%*%solve(sigma)%*%mu)[1]
    D <- (t(mu)%*%solve(sigma)%*%mu)[1]
    delta <- A*D - B*C
    lambda1 <- (D-mu_T*C) / delta
    lambda2 <- (D-mu_T*A - B) / delta
    w_MV <- lambda1*solve(sigma)%*%v1 + lambda2*solve(sigma)%*%mu
    return(w_MV)
  }
  
  # Portafolio de mínima varianza global (MVG)
  p_MV(0.15)
  
  # Graficamos el portafolio
  # Espacio sd. E(r)
  plot(p_sd(w_mvg),p_r(w_mvg),
       xlim = c(0,1), ylim = c(0,1),
       xlab = "var",
       ylab = "E(r)",
       main = "Set portafolio")
  
  # Generamos la frontera
  mu_i = seq(-1,1,0.01)
  sd_i = sapply(mu_i, function(x) p_sd(p_MV(x)))
  lines(sd_i, mu_i)
  
  points(SD_i[7], mu[7])
  points(p_MV(mu[7]))
  
  # Métricas 1
  x = p_sd(w)
  y = p_r(w)
  points(x, y, cex = 0.3)
  
  r_f=0.08
  # Tangencia (REVISAR)
  p_tan<-function (r_f) {
    A = t(rep(1, n))%*%(solve(sigma)%*%rep(1, n))
    B = t (mu)%*%(solve(sigma)%*%rep (1, n))
    w_t = solve(sigma)%*%(mu-r_f*rep(1, n))/(B - A*r_f) [1]
    return (w_t)
  }
  p_tan()
  
  
  
  
  
  
  
  
  
  
  
  
  

