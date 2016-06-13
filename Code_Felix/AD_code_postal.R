library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)

setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("Brut_Train.csv", header=T)
maif_test <- fread("Brut_Test.csv", header=T)

code <- maif_train$codepostal
code_num <- as.numeric(code)
  
# combien de code postaux dfférents peut-on trouver ? 
length(unique(maif_train$codepostal))   # 23696
t <- table(maif_train$codepostal)
t

# certains codes ne peuvent pas être transformés en entier : 
ind <- which(is.na(code_num))
code[ind]   #  valeurs "NR" et "ARMEE"

# on remplace les "NR" par des NA 
set(maif_train, which(maif_train[["codepostal"]]=='NR' ), "codepostal", NA)



# codepostal correspond t-elle bien à des codes postaux ou est-elle cryptée ? 
# ::::::::::::::::::::::::

# on calcule le nombre de char 
tab_code <- maif_train[,.(codepostal)][,nb:=nchar(codepostal), by='codepostal']
tab_code
table(tab_code[,.(nb)])
# on voit parfoir 2, parfois 4, parfois 5 char : 
tab_code[nb==2]  # les NA normal 
summary(as.numeric(tab_code[nb==4]$codepostal))
summary(as.numeric(tab_code[nb==5]$codepostal))
code_postal

# les codes postaux à 4 chiffres doivent correspondre aux codes postaux commencant
# par 0 sans le 0 
# on le rajoute donc

tab_code[,cp:=codepostal]
tab_code[nb==4, cp:=paste("0",codepostal,sep="")]

table(nchar(tab_code$cp))

