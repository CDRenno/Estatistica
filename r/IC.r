#Estatística: Aplicação ao Sensoriamento Remoto - SER204, INPE, 2024
#http://www.dpi.inpe.br/~camilo/estatistica

#Significado do IC

#construindo IC para média populacional de uma normal padrão a partir de amostras de n valores
#considera-se de a variância populacional é conhecida, ou seja, igual a 1)

estim.mediaN01<-function(n,rep=1000)
    {
    res=double(rep)
    for (i in 1:rep) {
      sample<-rnorm(n,mean=0,sd=1)
      res[i]<-mean(sample)
      }
    plot(c(1,rep),c(-2,2),type="n",ylab="média populacional",xlab="",xaxt="n")
    for (i in 1:rep) lines(c(i,i),c(res[i]-1.96/sqrt(n),res[i]+1.96/sqrt(n)),col=if ((res[i]-1.96/sqrt(n)) > 0) "red" else if ((res[i]+1.96/sqrt(n)) < 0) "red" else "black")
    return(res)
    }

xbarra<-estim.mediaN01(10)
