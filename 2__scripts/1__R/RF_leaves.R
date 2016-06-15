library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot)

setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("ech_apprentissage.csv", header=T)
maif_test < - fread("ech_test.csv", header=T)


d <- density(maif_train$prime_tot_ttc)
plot(d)

output <- maif_train$prime_tot_ttc

# CREATION DES CLUSTERS :
# ::::::::::::::::::::::::::::

# définir le nombre de clusters N : 
N <- 30
q <- c(0,quantile(output, probs = seq(0, 1, by = 1/N)),1e9)
# clus <- cut(output,
#             breaks=q,
#             labels=as.character(c(1:(N+2))))
clus <- cut(output,
            breaks=seq(0,max(output+1),length.out=N+1),
            labels=as.character(c(1:(N))))

d_clus <- maif_train
d_clus[,clus:=as.character(clus)]



