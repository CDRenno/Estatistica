#Estatística: Aplicação ao Sensoriamento Remoto - SER204, INPE, 2024
#http://www.dpi.inpe.br/~camilo/estatistica

#Estimadores da média e variância

#amostras de n valores de uma normal padrão

estim.media<-function(n,rep=10000)
    {
    #estimador 1: (min+max)/2
    #estimador 2: mediana
    #estimador 3: média
    res=list(estim1=double(rep),estim2=double(rep),estim3=double(rep),esperanca=double(3),variancia=double(3))
    for (i in 1:rep) {
      sample<-rnorm(n,mean=0,sd=1)
      res$estim1[i]<-(min(sample)+max(sample))/2
      res$estim2[i]<-median(sample)
      res$estim3[i]<-mean(sample)
      }
    res$esperanca[1]<-mean(res$estim1)
    res$esperanca[2]<-mean(res$estim2)
    res$esperanca[3]<-mean(res$estim3)
    res$variancia[1]<-var(res$estim1)
    res$variancia[2]<-var(res$estim2)
    res$variancia[3]<-var(res$estim3)
    return(res)
    }

res<-estim.media(10)
res$esperanca
res$variancia

estim.variancia<-function(n,rep=10000)
    {
    #estimador 1: variancia com denominador n
    #estimador 2: variancia com denominador n-1
    res=list(estim1=double(rep),estim2=double(rep),esperanca=double(2),variancia=double(2))
    for (i in 1:rep) res$estim2[i]<-var(rnorm(n,mean=0,sd=1))
    res$estim1<-res$estim2*(n-1)/n
    res$esperanca[1]<-mean(res$estim1)
    res$esperanca[2]<-mean(res$estim2)
    res$variancia[1]<-var(res$estim1)
    res$variancia[2]<-var(res$estim2)
    return(res)
    }

res<-estim.variancia(10)
res$esperanca
res$variancia

