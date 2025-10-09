#Práctica 2 Portafolios
#start <- "2015-08-26"
#end   <- "2025-08-26"
#Funciones librería quantmod
#install.packages('quantmod')
#library(quantmod)
#tickers <- c("SPY", "NVDA", "MSFT", "WMT", "VBR", "GMEXICOB.MX", "NAFTRACISHRS.MX")
#getSymbols(tickers, src = "yahoo", from = start, to = end)
#getSymbols("MXN=X", src = "yahoo", from = start, to = end)
#P = do.call(merge, lapply(tickers, function(x) Ad(get(x))))
#USDMXN<-Cl(`MXN=X`)
#P <- na.omit(merge(Ad(SPY)*USDMXN,Ad(NVDA)*USDMXN,Ad(MSFT)*USDMXN,Ad(WMT)*USDMXN,
#                   Ad(VBR)*USDMXN,Ad(`NAFTRACISHRS.MX`),Ad(`GMEXICOB.MX`)))
#R = na.omit(ROC(P, type ="continuous"))
#colnames(R)<-c("Market", "Nvda", "Msft", "Wmt", "Small", "GMexico", "MX Market")
#plot(R)
#Funciones:
p_r<-function(w) sum(w*mu)                
p_sd<-function(w) sqrt(t(w)%*%Sigma%*%w) 
p_MV<-function(mu_T) {
  v1 <- rep(1, n)
  A <-(t(v1)%*%solve(Sigma)%*%v1)[1]
  B <-(t(mu)%*%solve(Sigma)%*%v1)[1]
  C <-(t(v1)%*%solve(Sigma)%*%mu)[1]
  D <-(t(mu)%*%solve(Sigma)%*%mu)[1]
  delta <- A*D - B*C
  lambda1 <- (D-mu_T*C) / delta
  lambda2 <- (mu_T*A - B) / delta
  w_MV <- lambda1*solve(Sigma)%*%v1 + lambda2*solve(Sigma)%*%mu
  return(w_MV)
}  
p_tan<-function(r_f) {
  A = t(rep(1, n))%*%(solve(Sigma)%*%rep(1, n))
  B = t(mu)%*%(solve(Sigma)%*%rep(1, n))
  w_t = solve(Sigma)%*%(mu-r_f*rep(1, n))/(B - A*r_f)[1]
  return(w_t)
} 
#Fuente de datos:
R<-as.matrix(read.csv("Retornos2025_T3.csv", header=T)[,-1])
#Inputs modelo
mu<-colMeans(R)*252   
mu<-c(0.12,0.37,0.24,0.16,0.10,0.10,0.11)   
Sigma<-var(R)*252   
#Definiciones
r_f=0.02
n<-ncol(R)   
#Frontera de portafolios eficientes con activos riesgosos:
plot(
  x = sapply(seq(-1, 1, 0.01), function(m) p_sd(p_MV(m))),
  y = seq(-1, 1, 0.01),
  type = "l",
  xlim = c(0, 0.8), ylim = c(0, 0.8),
  xlab = "σ",
  ylab = "E(r)",
  main = "Set portafolios")
#Espacio
# portafolios aleatorios 
nsim <- 100000
res <- matrix(NA, nsim, 2)
for (i in 1:nsim) {
  w <- runif(n, -1000, 1000)
  w <- w / sum(w)
  res[i,1] <- p_r(w)
  res[i,2] <- p_sd(w)
}
points(res[,2], res[,1], pch=20, cex=0.05, col="gray")
#No portafolios apalancados
nsim <- 100000
res <- matrix(NA, nsim, 2)
for (i in 1:nsim) {
  w <- rexp(n) # > 0
  w <- w / sum(w) # = 1
  res[i,1] <- p_r(w)
  res[i,2] <- p_sd(w)
}
points(res[,2], res[,1], pch=20, cex=0.05)
#Activos
SD_i<-apply(R,2,sd)*sqrt(252) 
points(SD_i[1:n], mu[1:n])
#Capital Allocation Line CAL
Sharpe= (p_r(p_tan(r_f))- r_f) / p_sd(p_tan(r_f))
curve(r_f + Sharpe*x, from=0, to=1, add=T)
                                 points(p_sd(p_tan(r_f)), p_r(p_tan(r_f)))
#Minima varianza global
w_mvg<-solve(Sigma)%*%rep(1, n) / (t(rep(1, n)) %*% solve(Sigma) %*% rep(1, n))[1] # Solución analítica
points(p_sd(w_mvg), p_r(w_mvg))
# Restricciones no lineales
require("quadprog")
# ?solve.QP
# Retorno objetivo
mu_T=0.20
# Solve sin restricciones
solve_MV <- function(mu_T) {
  # Objetos función
  Dmat<-Sigma                        # Matriz en la función cuadrática
  dvec<-rep(0, n)                    # vector en la función cuadrácita
  # Restricciones:
  Amat<-t(rbind(rep(1, n), mu))      # sum(w)=1, sum(w*mu)=mu_T 
  bvec<-c(1, mu_T)                   # b = 0
  meq=2                              # Primeras 2 restricciones de igualdad, default = 0
  # w solve
  w<-solve.QP(Dmat, dvec, Amat, bvec, meq)$solution
  return(w)
}
# Long Only: Restricciones no Venta corta
solve_MV_long <- function(mu_T) {
  Dmat <- Sigma
  dvec <- rep(0, n)
  # Restricciomes:
  # (1) sum(w)=1
  # (2) sum(w*mu)=mu_T
  # (3) No short-selling: w_i >= 0
  Amat<-t(rbind(rep(1, n), mu, diag(n)))
  bvec<-c(1, mu_T, rep(0, n))
  meq=2
  w <- solve.QP(Dmat, dvec, Amat, bvec, meq)$solution
  return(w)
}
# Frontera restringida de portafolio eficientes:
mu_i <- seq(min(mu), max(mu), 0.01)
sd_i <- sapply(mu_i, function(m) {
  w <- try(solve_MV_long(m), silent = T)
  if (!inherits(w, "try-error")) p_sd(w) else NA
})
lines(sd_i, mu_i, lwd=1, lty=2, col="red")

# Caso 2: Short sell > -0.10%, max 100%
solve_MV_Caso2 <- function(mu_T) {
  Dmat<-Sigma
  dvec<-rep(0, n)
  # Restricciomes:
  # 1) sum(w)=1
  # 2) sum(w*mu)=mu_T
  # 3) > -0.10%
  # 4) < 30%
  Amat<-t(rbind(rep(1, n), mu, diag(n), -diag(n)))
  bvec<-c(1, mu_T,rep(-0.30, n), rep(-1, n))
  meq=2
  # Solve 
  w <- solve.QP(Dmat, dvec, Amat, bvec, meq)$solution
  return(w)
}
sd_i <- sapply(mu_i, function(x) if (!inherits(try(res <- solve_MV_Caso2(x), silent = TRUE), "try-error")) p_sd(res) else NA)
lines(na.omit(sd_i), mu_i[!is.na(sd_i)],lwd=1, lty=2, col="red")
#Con libre de riesgo
w_optimo_con_rf <- function(mu_P) {
  v1 <- rep(1, n)
  A <- (t(v1) %*% solve(Sigma) %*% v1)[1]
  B <- (t(mu) %*% solve(Sigma) %*% v1)[1]
  D <- (t(mu) %*% solve(Sigma) %*% mu)[1]
 lambda <- (mu_P - r_f) / (D - 2*r_f*B + r_f^2*A)
  w_rf <- lambda*solve(Sigma)%*%(mu - r_f*v1)
  w_0 <- 1 - sum(w_rf)
  w_rff <- c(w_0, drop(w_rf))
  names(w_rff) <- c("Rf", colnames(R))
  return(w_rff)
}
w=w_optimo_con_rf(mu_T)[-1]
points(p_sd(w),mu_T)
#Función de utilidad
r_t=mu_T
# utilidad
ut <- function(A,U0) U0 + 0.5*A*p_sd(w)^2

A <- 10
U0 <- r_t - 0.5*A*s0^2
x <- seq(0,1,len=200)
lines(x, ut(x,A,U0), col=2)
points(s0,m0)

# Caso 3: Min 5%, max 30%
solve_MV_Caso3 <- function(mu_T) {
  Dmat <- Sigma
  dvec <- rep(0, n)
  # Restricciomes:
  # 1) sum(w) = 1
  # 2) sum(w * mu) = mu_T
  # 3) min 10
  # 4) max 30
  Amat <- t(rbind(rep(1, n), mu, diag(n), -diag(n)))
  bvec <- c(1, mu_T,rep(0.05, n), rep(-0.30, n))
  meq=2  
  w <- solve.QP(Dmat, dvec, Amat, bvec, meq)$solution
  return(w)
}

sd_i <- sapply(mu_i, function(x) if (!inherits(try(res <- solve_MV_Caso3(x), silent = TRUE), "try-error")) p_sd(res) else NA)
lines(na.omit(sd_i), mu_i[!is.na(sd_i)], lwd=1, lty=2, col="purple")
#points(na.omit(sd_i), mu_i[!is.na(sd_i)], col="purple", pch=16, cex=0.2)
w_min10_max30=solve_MV_Caso3(min(mu_i[!is.na(sd_i)]))
w_min10_max30=solve_MV_Caso3(max(mu_i[!is.na(sd_i)]))

points(p_sd(w_min10_max30), p_r(w_min10_max30), cex=0.5, col="purple")


#Traynor
Traynor= (p_r(w) - r_f) / beta
#Capital Asset Pricing Model
r_CAPM= r_f + beta*(mu[1] - r_f)
#Series portafolio
r_p <- as.numeric(R%*%w)
# VaR al 5% (histórico y paramétrico, diario)
VaR5_expost <- -quantile(r_p, 0.05)
VaR5_norm <- -(mean(r_p) + qnorm(0.05)*sd(r_p))
# Tracking Error (diario y anualizado)
T_er <- sd(r_p - mu[1])*sqrt(252) 
# Histograma 
hist(r_p, breaks=20, freq=F,
     col="lightgray", border="white",
     main="Portafolio", xlab="Retorno")

curve(dnorm(x, mean=mean(r_p), sd=sd(r_p)),
      add=T, lwd=2)
# Simulación
n_days <- 252*10     
n_sim <- 100       
mu_port <- p_r(w) / 252  
sd_port <-p_sd(ww_final) / sqrt(252)  

# Simulación de Monte Carlo
set.seed(123)  
simulated_returns <- matrix(rnorm(n_days * n_sim, mean = mu_port, sd = sd_port), ncol = n_sim)

simulated_prices <- apply(simulated_returns, 2, function(x) cumprod(1 + x))

matplot(simulated_prices, type = "l", col = rgb(0.8, 0.8, 0.8, 0.5), lty = 1,
        xlab = "Días", ylab = "Retorno acumulado", main = "Simulación Portafolio")
lines(rowMeans(simulated_prices), lwd = 2)  # Promedio de todas las simulaciones



