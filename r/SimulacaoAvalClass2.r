#Estatística: Aplicação ao Sensoriamento Remoto - SER204, INPE, 2026
#https://cdrenno.github.io/Estatistica/

# install.packages("raster")
# install.packages("ggplot2")
# install.packages("scales")

library(raster)

setwd("D:/Camilo/OneDrive/Documentos/estatistica2025/r")
imgref <- as.matrix(raster("class_referencia.tif"))
imgclass <- as.matrix(raster("class_final.tif"))

mcref <- table(imgclass, imgref)
classes <- c("Floresta", "Desmat", "Agua")

colnames(mcref) <- classes
rownames(mcref) <- classes

img <- expand.grid(row = 1:nrow(imgref), col = 1:ncol(imgref))
img$ref <- as.vector(imgref)
img$class <- as.vector(imgclass)
vclass <- unique(img$class)
img$ref <- factor(img$ref,vclass)
img$class <- factor(img$class,vclass)
vclass <- levels(img$class)

propclass <- prop.table(table(img$class))

nsimul <- 1000
res <- data.frame()
n <- c(100,300,1000)
for (s in 1:nsimul) {
  for (i in 1:length(n)) {
    nprop <- floor(n[i]*propclass+.5)
    nigual <- floor(n[i]*rep(1/length(propclass),length(propclass))+.5)
    
    amostras <- img[sample(nrow(img), n[i], replace=T), ]
    amostrasprop <- do.call(rbind, lapply(1:length(classes), function(cl) {
      subset_classe <- img[img$class == vclass[cl], ]
      subset_classe[sample(nrow(subset_classe), nprop[cl], replace=T), ]
    }))
    amostrasigual <- do.call(rbind, lapply(1:length(classes), function(cl) {
      subset_classe <- img[img$class == vclass[cl], ]
      subset_classe[sample(nrow(subset_classe), nigual[cl], replace=T), ]
    }))
    tmp <- do.call(c, lapply(list(amostras,amostrasprop,amostrasigual), function(am) {
      mc <- table(am$class,am$ref)
      mccorr <- sum(mc)*mc*matrix(rep(propclass, each = 1, times = length(classes)), nrow = length(classes))/matrix(rep(rowSums(mc), each = 1, times = length(classes)), nrow = length(classes))
      c(sum(diag(mc)) / sum(mc),as.numeric(1 - diag(mc) / rowSums(mc)),as.numeric(1 - diag(mc) / colSums(mc)),sum(diag(mccorr)) / sum(mccorr),as.numeric(1 - diag(mccorr) / rowSums(mccorr)),as.numeric(1 - diag(mccorr) / colSums(mccorr)))
    }))
    tmp[is.na(tmp)] <- 0

    indice <- c("ag",rep("eu",length(classes)),rep("ep",length(classes)))
    indice <- c(indice,paste0(indice,"corr"))
    indice <- rep(indice,3)
    df <- data.frame(
      indice = indice,
      classe = rep(c("",rep(classes,2)),6),
      tipo = c(rep("Aleatorio",4*length(classes)+2),rep("Estrat Prop",4*length(classes)+2),rep("Estrat Igual",4*length(classes)+2)),
      n = rep(n[i],6*(2*length(classes)+1)),
      valor = tmp
    )
    res <- rbind(res, df)
  }
}

library(ggplot2)
library(scales)

#Analise erro do usuário
limites <- c(0.02,0.05,0.3)
for (i in 1:length(classes)) {
  df <- res[(res$indice == "eu") & (res$classe == classes[i]),]
  df <- df[, !(names(df) %in% c("indice","classe"))]
  df$grupo <- paste0(df$tipo, " n = ", df$n)
  df$grupo <- factor(df$grupo, levels = c(paste0("Aleatorio"," n = ",n),paste0("Estrat Prop"," n = ",n),paste0("Estrat Igual"," n = ",n)))

  p <- ggplot(df, aes(x = grupo, y = valor, fill = factor(n))) +
    geom_boxplot() +
    scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
    coord_cartesian(ylim = c(0, limites[i])) +
    scale_fill_manual(values = c("100" = "#0000ff",   # azul
                                "300" = "#ff0000",   # vermelho
                                "1000" = "#00ff00")) +  # verde
    labs(x = classes[i], y = "Erro do Usuario", fill = "n") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

#Analise erro do produtor
limites <- c(0.12,0.1,0.1)
for (i in 1:length(classes)) {
  df <- res[(res$indice == "ep") & (res$classe == classes[i]),]
  df <- df[, !(names(df) %in% c("indice","classe"))]
  df$grupo <- paste0(df$tipo, " n = ", df$n)
  df$grupo <- factor(df$grupo, levels = c(paste0("Aleatorio"," n = ",n),paste0("Estrat Prop"," n = ",n),paste0("Estrat Igual"," n = ",n)))
  
  p <- ggplot(df, aes(x = grupo, y = valor, fill = factor(n))) +
    geom_boxplot() +
    scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
    coord_cartesian(ylim = c(0, limites[i])) +
    scale_fill_manual(values = c("100" = "#0000ff",   # azul
                                 "300" = "#ff0000",   # vermelho
                                 "1000" = "#00ff00")) +  # verde
    labs(x = classes[i], y = "Erro do Produtor", fill = "n") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

#Analise acuracia global
df <- res[res$indice == "ag",]
df <- df[, !(names(df) %in% c("indice","classe"))]
df$grupo <- paste0(df$tipo, " n = ", df$n)
df$grupo <- factor(df$grupo, levels = c(paste0("Aleatorio"," n = ",n),paste0("Estrat Prop"," n = ",n),paste0("Estrat Igual"," n = ",n)))
  
ggplot(df, aes(x = grupo, y = valor, fill = factor(n))) +
  geom_boxplot() +
  scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
  coord_cartesian(ylim = c(0, limites[i])) +
  scale_fill_manual(values = c("100" = "#0000ff",   # azul
                                "300" = "#ff0000",   # vermelho
                                "1000" = "#00ff00")) +  # verde
  labs(x = "", y = "Exatidao Global", fill = "n") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Analise erro do usuário corrigido
limites <- c(0.02,0.05,0.3)
for (i in 1:length(classes)) {
  df <- res[(res$indice == "eucorr") & (res$classe == classes[i]),]
  df <- df[, !(names(df) %in% c("indice","classe"))]
  df$grupo <- paste0(df$tipo, " n = ", df$n)
  df$grupo <- factor(df$grupo, levels = c(paste0("Aleatorio"," n = ",n),paste0("Estrat Prop"," n = ",n),paste0("Estrat Igual"," n = ",n)))
  
  p <- ggplot(df, aes(x = grupo, y = valor, fill = factor(n))) +
    geom_boxplot() +
    scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
    coord_cartesian(ylim = c(0, limites[i])) +
    scale_fill_manual(values = c("100" = "#0000ff",   # azul
                                 "300" = "#ff0000",   # vermelho
                                 "1000" = "#00ff00")) +  # verde
    labs(x = classes[i], y = "Erro do Usuario", fill = "n") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

#Analise erro do produtor corrigido
limites <- c(0.02,0.04,0.08)
for (i in 1:length(classes)) {
  df <- res[(res$indice == "epcorr") & (res$classe == classes[i]),]
  df <- df[, !(names(df) %in% c("indice","classe"))]
  df$grupo <- paste0(df$tipo, " n = ", df$n)
  df$grupo <- factor(df$grupo, levels = c(paste0("Aleatorio"," n = ",n),paste0("Estrat Prop"," n = ",n),paste0("Estrat Igual"," n = ",n)))
  
  p <- ggplot(df, aes(x = grupo, y = valor, fill = factor(n))) +
    geom_boxplot() +
    scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
    coord_cartesian(ylim = c(0, limites[i])) +
    scale_fill_manual(values = c("100" = "#0000ff",   # azul
                                 "300" = "#ff0000",   # vermelho
                                 "1000" = "#00ff00")) +  # verde
    labs(x = classes[i], y = "Erro do Produtor", fill = "n") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

#Analise acuracia global
df <- res[res$indice == "ag",]
df <- df[, !(names(df) %in% c("indice","classe"))]
df$grupo <- paste0(df$tipo, " n = ", df$n)
df$grupo <- factor(df$grupo, levels = c(paste0("Aleatorio"," n = ",n),paste0("Estrat Prop"," n = ",n),paste0("Estrat Igual"," n = ",n)))

ggplot(df, aes(x = grupo, y = valor, fill = factor(n))) +
  geom_boxplot() +
  scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
  coord_cartesian(ylim = c(0.95, 1)) +
  scale_fill_manual(values = c("100" = "#0000ff",   # azul
                               "300" = "#ff0000",   # vermelho
                               "1000" = "#00ff00")) +  # verde
  labs(x = "", y = "Exatidao Global", fill = "n") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Analise acuracia global corrigida
df <- res[res$indice == "agcorr",]
df <- df[, !(names(df) %in% c("indice","classe"))]
df$grupo <- paste0(df$tipo, " n = ", df$n)
df$grupo <- factor(df$grupo, levels = c(paste0("Aleatorio"," n = ",n),paste0("Estrat Prop"," n = ",n),paste0("Estrat Igual"," n = ",n)))

ggplot(df, aes(x = grupo, y = valor, fill = factor(n))) +
  geom_boxplot() +
  scale_y_continuous(labels = percent_format(accuracy = 0.5)) +
  coord_cartesian(ylim = c(0.95, 1)) +
  scale_fill_manual(values = c("100" = "#0000ff",   # azul
                               "300" = "#ff0000",   # vermelho
                               "1000" = "#00ff00")) +  # verde
  labs(x = "", y = "Exatidao Global", fill = "n") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

a<-NA
mctot <- matrix(data=0,nrow=3,ncol=3)
for (i in 1:1000) {
  amostrasigual <- do.call(rbind, lapply(1:length(classes), function(cl) {
    subset_classe <- img[img$class == vclass[cl], ]
    subset_classe[sample(nrow(subset_classe), nigual[cl], replace=T), ]
  }))
  mc<-table(amostrasigual$class,amostrasigual$ref)
  mctot <- mctot + mc
  ep<-1-diag(mc)/colSums(mc)
  a<-c(a,ep[1])
}
a<-a[-1]
median(a)


