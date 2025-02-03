#Estatística: Aplicação ao Sensoriamento Remoto - SER204, INPE, 2024
#http://www.dpi.inpe.br/~camilo/estatistica

#Teorema do Limite Central

#valores aleatórios de distribuição exponencial com média 1

limcentral.expo<-function(n,rep=5000)
    {
    #soma de n valores (rep repetições) de exponencial com média=1
    x<-double(rep)
    for (i in 1:rep) x[i]<-sum(rexp(n,rate=1))
    x<-sort(x)
    plot(x,(0:(rep-1))/(rep-1),type="l",xlab="x",ylab="F(x)",main=paste("Soma de",n,"exponenciais")) #frequência acumulada
    lines(qnorm((0:(rep-1))/(rep-1),mean=n,sd=sqrt(n),lower.tail=T),(0:(rep-1))/(rep-1),col="red") #frequência acumulada se fosse distribuição normal
    return(x)
    }

par(mfrow=c(1,3))
x<-limcentral.expo(2)
x<-limcentral.expo(5)
x<-limcentral.expo(50)