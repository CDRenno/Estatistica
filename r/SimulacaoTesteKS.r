#Estatística: Aplicação ao Sensoriamento Remoto - SER204, INPE, 2024
#http://www.dpi.inpe.br/~camilo/estatistica

#Determinando o valor-P e o KDcrítico pata o Teste de Kolmogorov-Smirnov para 2 amostras

regA<-c(81,78,61,89,69,58,64,84,89,83,88,56,87,95,75)
regB<-c(56,55,76,54,83,97,85,66,78,80,61,69,71,55,91)
min<-min(regA,regB)
max<-max(regA,regB)
dif<-rep(0,max-min+1)
for (i in min:max) dif[i-min+1]<-abs(length(which(regA <= i))-length(which(regB <= i)))
KDobs<-max(dif)
KDobs
regAB<-c(regA,regB)
n<-10000
pKD<-rep(0,16)
ValorP<-0
for (k in 1:n) {
regAB<-sample(regAB)
regAt<-regAB[1:length(regA)]
regBt<-regAB[length(regA)+1:length(regAB)]
dif<-rep(0,max-min+1)
for (i in min:max) dif[i-min+1]<-abs(length(which(regAt <= i))-length(which(regBt <= i)))
KD<-max(dif)
if (KD <= KDobs) ValorP<- ValorP+1
pKD[KD+1]<- pKD[KD+1]+1 }
pKDcum <-rev(cumsum(rev(pKD))/n)
KDcrit<-min(which(pKDcum < 0.05))-1
KDcrit
ValorP<-ValorP/n
ValorP
