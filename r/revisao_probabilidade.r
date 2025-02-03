#Estatística: Aplicação ao Sensoriamento Remoto - SER204, INPE, 2024
#http://www.dpi.inpe.br/~camilo/estatistica

#exemplo 1 - lançamento de dados

dado<-function(n)
{
valores<-1:6
res<-sample(valores,n,replace=T)
hist(res,breaks=seq(.5,6.5,1),plot=T,freq=F,main="Histograma",xlab="valores",ylab="frequência relativa",ylim=c(0,1))
return(res)
}

x<-dado(10)
x<-dado(10000)

#exemplo 2 - 2 urnas

sorteio_cr<-function(n) #com reposição (valor exato 28/75=0.37333333)
{
urna1<-c("R","R","R","G","B","B")
urna2<-c("R","G","G","B")
res<-integer(n) #vetor com n elementos
for (i in 1:n)
  {
  urna2nova<-c(urna2,sample(urna1,1))
  amostra<-sample(urna2nova,2,replace=T)
  res[i]<-amostra[1]==amostra[2]
  }
  return(mean(res))
}

sorteio_cr(100)
sorteio_cr(10000)

sorteio_sr<-function(n) #sem reposição (valor exato 13/60=0.21666667)
{
urna1<-c("R","R","R","G","B","B")
urna2<-c("R","G","G","B")
res<-integer(n) #vetor com n elementos
for (i in 1:n)
  {
  urna2nova=c(urna2,sample(urna1,1))
  amostra<-sample(urna2nova,2,replace=F)
  res[i]<-amostra[1]==amostra[2]
  }
  return(mean(res))
}

sorteio_sr(100)
sorteio_sr(10000)