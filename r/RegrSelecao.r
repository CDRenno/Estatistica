#Estatística: Aplicação ao Sensoriamento Remoto - SER204, INPE, 2024
#http://www.dpi.inpe.br/~camilo/estatistica

## Entrada dos dados
dados <- read.csv("RegrDados.dat", header = TRUE, sep="\t", dec = ".", na.strings = NA)

## Plotando gráficos de dispersão e correlações
upanel <- function(x, y, ...)
{
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.5, format(cor(x, y), digits=2), cex = 1.5)
}
plot(dados,upper.panel=upanel)

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
plot(dados,upper.panel=panel.cor)

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
#cada variável do modelo x resíduos padronizados
plot(dados$X1,regsel$residuals/erropadr,xlab="X1",ylab="standard error")
plot(dados$logX3,regsel$residuals/erropadr,xlab="logX3",ylab="standard error")
plot(dados$X4,regsel$residuals/erropadr,xlab="X4",ylab="standard error")


#fazendo busca exaustiva
library(leaps)
leaps<-regsubsets(Y ~ X1 + X2 + logX3 + X4 + X5,data=dados,nbest=6)
plot(leaps,scale="adjr2")

