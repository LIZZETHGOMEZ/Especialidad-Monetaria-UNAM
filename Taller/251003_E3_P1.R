#Práctica R E1 Espacio de portafolios factibles
start <- "2015-08-26"
end   <- "2025-08-26"
#Funciones librería quantmod
install.packages('quantmod')
library(quantmod)
tickers <- c("SPY", "NVDA", "MSFT", "WMT", "VBR", "GMEXICOB.MX", "NAFTRACISHRS.MX")
getSymbols(tickers, src = "yahoo", from = start, to = end)
getSymbols("MXN=X", src = "yahoo", from = start, to = end)
#P = do.call(merge, lapply(tickers, function(x) Ad(get(x))))
USDMXN<-Cl(`MXN=X`)
P <- na.omit(merge(Ad(SPY)*USDMXN,Ad(NVDA)*USDMXN,Ad(MSFT)*USDMXN,Ad(WMT)*USDMXN,
                   Ad(VBR)*USDMXN,Ad(`NAFTRACISHRS.MX`),Ad(`GMEXICOB.MX`)))
R = na.omit(ROC(P, type ="continuous"))
colnames(R)<-c("Market", "Nvda", "Msft", "Wmt", "Small", "GMexico", "MX Market")
#plot(P,R)
#Excel 
#R<-as.matrix(read.csv("Retornos2025_T1.csv", header=T)[,-1])
mu<-colMeans(R)*252   
Sigma<-var(R)*252   
#activos
SD_i<-apply(R,2,sd)*sqrt(252) 
corr=cor(R)
#w portafolio
n<-ncol(R)         
w=rep(1/n, n)   
#w<-c(0.30,0.25,0.25,0.25,0.10,0.0,0.0)
p_r<-function(w) sum(w*mu)                
p_sd<-function(w) sqrt(t(w)%*%Sigma%*%w) 
#Metricas
#p_r(w)
#p_sd(w)
#Ratios
div_ratio=p_sd(w) / SD_i[1]
#Miníma varianza global
w_mvg<-solve(Sigma)%*%rep(1, n) / (t(rep(1, n)) %*% solve(Sigma) %*% rep(1, n))[1] # Solución analítica
#Minima varianza retorno target
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
  # w_MV <- as.numeric(w_MV)
  return(w_MV)
}              
#Tangencia
p_tan<-function(r_f) {
  A = t(rep(1, n))%*%(solve(Sigma)%*%rep(1, n))
  B = t(mu)%*%(solve(Sigma)%*%rep(1, n))
  w_t = solve(Sigma)%*%(mu-r_f*rep(1, n))/(B - A*r_f)[1]
  return(w_t)
}            
# Espacio sd, E(r) 
plot(p_sd(w_mvg), p_r(w_mvg),
     xlim = c(0, 1), ylim = c(0,1),
     xlab = "σ",
     ylab = "E(r)",
     main = "Set portafolios")
#beta
beta = cov(R%*%w, R[, 1]) / var(R[, 1])
beta = sum(w*apply(R, 2, function(x) cov(x, R[, 1]) / var(R[, 1])))
# Frontera de portafolio eficientes:
mu_i=seq(-1, 1, 0.01)                            
sd_i=sapply(mu_i, function(x) p_sd(p_MV(x)))     
lines(sd_i, mu_i)
#Tasa libre-riesgo
r_f=0.03
#Ratios
Sharpe= (p_r(p_tan(r_f)) - r_f) / p_sd(p_tan(r_f))
Sharpe

# Graficamos la frontera libre de riesgo 
# La pendiente es el índice de Sharpe
# Recta eficiente (CML): mu = r_f + Sharpe * sigma
curve(r_f + Sharpe*x, from=0, to=1, add=T)


#Traynor
Traynor= (p_r(w) - r_f) / beta
#Capital Asset Pricing Model
r_CAPM= r_f + beta*(mu[1] - r_f)
#Portafolios
points(SD_i[1:n], mu[1:n])
points(p_sd(p_tan(0.08)), p_r(p_tan(0.08)))
#Función de utilidad
r_t=0.25
# punto objetivo
s0 <- p_sd(p_MV(r_t))
m0 <- p_r(p_MV(r_t))
# utilidad
ut <- function(s,A,U0) U0 + 0.5*A*s^2
# calcular U0 para que pase por (s0,m0)

# Aversión al riesgo
A <- 10

U0 <- m0 - 0.5*A*s0^2
# trazar
x <- seq(0,1,len=200)
lines(x, ut(x,A,U0), col=2)
points(s0,m0)
#Series portafolio
r_p <- as.numeric(R%*%w)
# VaR al 5% (histórico y paramétrico, diario)
VaR5_expost <- -quantile(r_p, 0.05, na.rm = T)
VaR5_norm <- -(mean(r_p, na.rm=T) + qnorm(0.05)*sd(r_p, na.rm=T))*sqrt(252)
# Tracking Error (diario y anualizado)
T_er <- sd(r_p - r_bk, na.rm=T)*sqrt(252) 












