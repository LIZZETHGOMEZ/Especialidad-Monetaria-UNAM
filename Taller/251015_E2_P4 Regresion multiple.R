
# #######################################################
# Práctica 3 APT betas prime risk
# ######################################################

getwd()
setwd("../Taller")


R<-as.matrix(read.csv("Retornos2025_T4.csv", header=T)[,-1])
#Beta
beta<-cov(R, R[,3]) / var(R[,1])
#Dispersiones #pairs(R), boxplot(R), plot.ts(R[,1]), hist(R[,1])             
#Betas múltiples
Y<-R[, 1]         
X<-cbind(1, R[, -1]) 
betas<-solve(t(X) %*% X, t(X) %*% Y)
betas <-solve(t(X) %*% X) %*% t(X) %*% Y
#Con linear model
R<-as.data.frame(R)
coef(lm(R[,1] ~ ., data=R[,-1]))
#Mejor modelo
modelo <- step(lm(MKT ~ ., data=R), trace=0)
summary(step(lm(MKT ~ ., data=R), trace=0))
#Backtest
plot(R$MKT, 
     predict(modelo), 
     xlab="MKT obs", 
     ylab="MKT est"); abline(0,1)


# ------------------------------------------------
#Parte 2
mu<-colMeans(R)*12 
Sigma<-var(R)*12  


#Fuente:
start <- "2015-01-01"
end   <- "2025-08-01"
library(quantmod)
tickers <- c("WMT","VTI","VB","VV","VTV","VUG","BIL")
getSymbols(tickers, src = "yahoo", from = start, to = end)

# Calculo del retorno mensual
P = do.call(merge, lapply(tickers, function(x) Ad(get(x))))
R <- na.omit(do.call(merge, lapply(tickers, function(t) monthlyReturn(Ad(get(t)), type="log"))))

# Renombramos columnas
colnames(R) <- c("A","MKT","S","B","V","G","RF")

# Construimos matriz, primera columna es el Asset price etc..
R<-as.matrix(R)
#R<-as.data.frame(R)
R <- cbind(A=R[,"A"]-R[,"RF"],MKT=R[,"MKT"]-R[,"RF"], SMB=R[,"S"]-R[,"B"], HML=R[,"V"]-R[,"G"], Rf=R[,"RF"])

