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

propclass <- prop.table(table(img$class))

nsimul <- 1000
res <- data.frame()
n <- c(100,300,1000)

for (s in 1:nsimul) {
  for (i in 1:length(n)) {
    nprop <- floor(n[i]*propclass+.5)
    nigual <- floor(n[i]*rep(1/length(propclass),length(propclass))+.5)

    mca <- matrix(rmultinom(n=1,size=n[i],prob=mcref/sum(mcref)),nrow=length(classes),ncol=length(classes))
    mcp <- t(do.call(cbind, lapply(1:length(classes), function(j) {
      rmultinom(n=1,size=nprop[j],prob=mcref[j,]/sum(mcref[j,]))
    })))
    mci <- t(do.call(cbind, lapply(1:length(classes), function(j) {
      rmultinom(n=1,size=nigual[j],prob=mcref[j,]/sum(mcref[j,]))
    })))
    tmp <- do.call(c, lapply(list(mca,mcp,mci), function(mc) {
      c(sum(diag(mc)) / sum(mc),as.numeric(1 - diag(mc) / rowSums(mc)),as.numeric(1 - diag(mc) / colSums(mc)))
    }))
    tmp[is.na(tmp)] <- 0
    tmp <- data.frame(
      indice = rep(c("ag",rep("eu",length(classes)),rep("ep",length(classes))),3),
      classe = rep(c("",rep(classes,2)),3),
      tipo = c(rep("Aleatorio",2*length(classes)+1),rep("Estrat Prop",2*length(classes)+1),rep("Estrat Igual",2*length(classes)+1)),
      n = rep(n[i],3*(2*length(classes)+1)),
      valor = tmp
    )
    res <- rbind(res, tmp)
    
    #corrigindo mc segundo prop classes
    mca <- sum(mca)*mca*matrix(rep(propclass, each = 1, times = length(classes)), nrow = length(classes))/matrix(rep(rowSums(mca), each = 1, times = length(classes)), nrow = length(classes))
    mcp <- sum(mcp)*mcp*matrix(rep(propclass, each = 1, times = length(classes)), nrow = length(classes))/matrix(rep(rowSums(mcp), each = 1, times = length(classes)), nrow = length(classes))
    mci <- sum(mci)*mci*matrix(rep(propclass, each = 1, times = length(classes)), nrow = length(classes))/matrix(rep(rowSums(mci), each = 1, times = length(classes)), nrow = length(classes))
    tmp <- do.call(c, lapply(list(mca,mcp,mci), function(mc) {
      c(sum(diag(mc)) / sum(mc),as.numeric(1 - diag(mc) / rowSums(mc)),as.numeric(1 - diag(mc) / colSums(mc)))
    }))
    tmp[is.na(tmp)] <- 0
    tmp <- data.frame(
      indice = rep(c("agcorr",rep("eucorr",length(classes)),rep("epcorr",length(classes))),3),
      classe = rep(c("",rep(classes,2)),3),
      tipo = c(rep("Aleatorio",2*length(classes)+1),rep("Estrat Prop",2*length(classes)+1),rep("Estrat Igual",2*length(classes)+1)),
      n = rep(n[i],3*(2*length(classes)+1)),
      valor = tmp
    )
    res <- rbind(res, tmp)
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
  coord_cartesian(ylim = c(0.95,1)) +
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
  coord_cartesian(ylim = c(0.95,1)) +
  scale_fill_manual(values = c("100" = "#0000ff",   # azul
                               "300" = "#ff0000",   # vermelho
                               "1000" = "#00ff00")) +  # verde
  labs(x = "", y = "Exatidao Global", fill = "n") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


