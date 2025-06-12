#Estatística: Aplicação ao Sensoriamento Remoto - SER204, INPE, 2025
#http://urlib.net/8JMKD2USNRW34T/4D6DMD2
#https://cdrenno.github.io/Estatistica/

# Entrada dos dados
dados <- read.csv("RegrDados.dat", header = TRUE, sep="\t", dec = ".", na.strings = NA)

# Plotando gráficos de dispersão e correlações
upanel <- function(x, y, ...)
{
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.5, format(cor(x, y), digits=2), cex = 1.5)
}
pairs(dados,upper.panel=upanel)

#avaliando as variáveis independentes quanto a linearidade
plot(Y~X1,data=dados)
plot(Y~X2,data=dados)
plot(Y~X3,data=dados)
plot(Y~X4,data=dados)
plot(Y~X5,data=dados)

#avaliando a variável X3
plot(Y~X3,data=dados)
plot(Y~log10(X3),data=dados)
#linearizando a variável X3
dados$X3 <- log10(dados$X3)
names(dados)[4]<-"logX3"
pairs(dados,upper.panel=panel.cor)


#calculando VIF
reg<-lm(Y ~ X1 + X2 + logX3 + X4 + X5, data=dados)
library(car)
round(vif(reg),digits=2)

#aplicando-se o Stepwise
library(MASS)
reg<-lm(Y ~ X1 + X2 + logX3 + X4 + X5, data=dados)
regsel<-stepAIC(reg,direction="both")

#modelo selecionado
summary(regsel)

#avaliando modelo selecionado
#normalidade dos resíduos
shapiro.test(regsel$residuals)
#Y x resíduos padronizados
erropadr <- (summary(regsel))$sigma
plot(dados$Y,regsel$residuals/erropadr,xlab="Y",ylab="standard error")
#homoscedasticidade
library(lmtest)
bptest(regsel)

#cada variável do modelo x resíduos padronizados
plot(dados$X1,regsel$residuals/erropadr,xlab="X1",ylab="standard error")
plot(dados$logX3,regsel$residuals/erropadr,xlab="logX3",ylab="standard error")
plot(dados$X4,regsel$residuals/erropadr,xlab="X4",ylab="standard error")
plot(dados$X2,regsel$residuals/erropadr,xlab="X2",ylab="standard error")
plot(dados$X5,regsel$residuals/erropadr,xlab="X5",ylab="standard error")

#eliminando amostras 7, 34 e 35 uma a uma
#dados completos
reg<-lm(Y ~ X1 + X2 + logX3 + X4 + X5, data=dados)
regsel<-stepAIC(reg,direction="both",trace=FALSE)
print(paste("Dados Completos   ","  AIC:", extractAIC(regsel),"   modelo final:",deparse(formula(regsel))))
#dados sem amostras
for (i in c(7,34,35)) {
  reg<-lm(Y ~ X1 + X2 + logX3 + X4 + X5, data=dados[-i,])
  regsel<-stepAIC(reg,direction="both",trace=FALSE)
  print(paste("Dados sem amostra",i,"  AIC:", extractAIC(regsel),"   modelo final:",deparse(formula(regsel))))
}

#eliminado amostra 35
reg<-lm(Y ~ X1 + X2 + logX3 + X4 + X5, data=dados[-35,])
regsel<-stepAIC(reg,direction="both",trace=FALSE)
summary(regsel)
shapiro.test(regsel$residuals)
erropadr <- (summary(regsel))$sigma
plot(dados[-35,]$Y,regsel$residuals/erropadr,xlab="Y",ylab="standard error")
bptest(regsel)
plot(dados[-35,]$X1,regsel$residuals/erropadr,xlab="X1",ylab="standard error")
plot(dados[-35,]$X2,regsel$residuals/erropadr,xlab="X2",ylab="standard error")
plot(dados[-35,]$logX3,regsel$residuals/erropadr,xlab="logX3",ylab="standard error")
plot(dados[-35,]$X4,regsel$residuals/erropadr,xlab="X4",ylab="standard error")
plot(dados[-35,]$X5,regsel$residuals/erropadr,xlab="X5",ylab="standard error")

#eliminado as amostras 7, 34 e 35
reg<-lm(Y ~ X1 + X2 + logX3 + X4 + X5, data=dados[-c(7,34,35),])
regsel<-stepAIC(reg,direction="both",trace=FALSE)
summary(regsel)

#eliminado as amostras 7, 34 e 35 mas excluindo as variáveis X2 e X5
regsel<-lm(Y ~ X1 + logX3 + X4, data=dados[-c(7,34,35),])
summary(regsel)
shapiro.test(regsel$residuals)
erropadr <- (summary(regsel))$sigma
plot(dados[-c(7,34,35),]$Y,regsel$residuals/erropadr,xlab="Y",ylab="standard error")
bptest(regsel)
plot(dados[-c(7,34,35),]$X1,regsel$residuals/erropadr,xlab="X1",ylab="standard error")
plot(dados[-c(7,34,35),]$logX3,regsel$residuals/erropadr,xlab="logX3",ylab="standard error")
plot(dados[-c(7,34,35),]$X4,regsel$residuals/erropadr,xlab="X4",ylab="standard error")
plot(dados[-c(7,34,35),]$X2,regsel$residuals/erropadr,xlab="X2",ylab="standard error")
plot(dados[-c(7,34,35),]$X5,regsel$residuals/erropadr,xlab="X5",ylab="standard error")


#fazendo busca exaustiva
library(leaps)
#usando todas as amostras
leaps<-regsubsets(Y ~ X1 + X2 + logX3 + X4 + X5,data=dados,nbest=6)
plot(leaps,scale="adjr2")
#descartando as amostras 7, 34 e 35
leaps<-regsubsets(Y ~ X1 + X2 + logX3 + X4 + X5,data=dados[-c(7,34,35),],nbest=6)
plot(leaps,scale="adjr2")

