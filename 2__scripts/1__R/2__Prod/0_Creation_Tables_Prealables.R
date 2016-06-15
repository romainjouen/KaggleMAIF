library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)

# setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/Projet_maif/")

maif_train <- fread("1__data/1__input/Brut_Train.csv", header=T)
maif_test <- fread("1__data/1__input/Brut_Test.csv", header=T)

# creation prix_brut
maif_train[,prix_ref:=100*prime_tot_ttc/crm,by=id]






##########################################################
###
##      CALCUL DES INDICES DEPARTEMENT 
###
##########################################################



# on remplace les "NR" par des NA 
set(maif_train, which(maif_train[["codepostal"]]=='NR' ), "codepostal", NA)

# on calcule le nommbre de char pour voir auquel dep ajouter 0 
maif_train[,nb:=nchar(codepostal), by='codepostal']
maif_train[,cp:=codepostal, by='codepostal']

# ajout des 0 pour les code postal des d?partements < 10 
maif_train[nb==4, cp:=paste("0",codepostal,sep="")]

# on groupe par d?partement : 
maif_train[,dep:=substr(cp,0,2), by='cp']

# moyennes par dep 
maif_train[,mean_dep:=mean(prix_ref),by='dep']

# on stocke dans une table 
dep_indices <- unique(maif_train[,.(dep,mean_dep)])

# calcul du prix de ref francais moyen 
prix_ref_fra <- mean(maif_train$prix_ref)

# calcul des indices de prix par departement :     (pour prix_ref_fra = prix_ref_dep/indice)
dep_indices[,indice_dep:=mean_dep/prix_ref_fra,by='dep']

# on stocke les infos dans une valeur
indices_dep <- dep_indices[,.(dep, indice_dep)]

# enregistrement des indices par dep 
save(indices_dep, file="1__data/2__output/Indices_Prix_Departement.Rda")




##########################################################
###
##      CALCUL DES INDICES DE CODE POSTAL 
###
##########################################################


# on remplace les "NR" par des NA 
set(maif_train, which(maif_train[["codepostal"]]=='NR' ), "codepostal", NA)

# on calcule le nommbre de char pour voir auquel dep ajouter 0 
maif_train[,nb:=nchar(codepostal), by='codepostal']
maif_train[,cp:=codepostal, by='codepostal']

# ajout des 0 pour les code postal des d?partements < 10 
maif_train[nb==4, cp:=paste("0",codepostal,sep="")]

# effectifs par code postal
maif_train[,count_cp:=.N, by='cp']

# moyennes par cp
maif_train[count_cp>=50,mean_cp:=mean(prix_ref),by='cp']

# on stocke dans une table 
cp_indices <- unique(maif_train[,.(cp,mean_cp)])

# calcul du prix de ref francais moyen 
prix_ref_fra <- mean(maif_train$prix_ref)

# calcul des indices de prix par departement :     (pour prix_ref_fra = prix_ref_dep/indice)
cp_indices[,indice_cp:=mean_cp/prix_ref_fra,by='cp']

# on stocke les infos dans une valeur
indices_cp <- cp_indices[,.(cp, indice_cp)]

# enregistrement des indices par dep 
save(indices_cp, file="1__data/2__output/Indices_Prix_CodePostal.Rda")



rm(cp_indices,
   dep_indices,
   indices_cp,
   indices_dep,
   maif_test,
   maif_train,
   prix_ref_fra)


#################################################
##
##    CHOIX DU NOMBRE D'OBS MINIMAL POUR PRENDRE LINDICE DU CODE POSTAL
##
#################################################

# Choix du nombre d'obs minimum sur le code postal pour prendre l'indice du cp 

# d <- unique(maif_train[,.(cp,count_cp)])
# sum(d[count_cp>=15]$count_cp)/300000   # 73.9 %
# sum(d[count_cp>=20]$count_cp)/300000   # 69.7 %
# sum(d[count_cp>=25]$count_cp)/300000   # 65.6 %
# sum(d[count_cp>=30]$count_cp)/300000   # 62.6 %
# sum(d[count_cp>=35]$count_cp)/300000   # 60.3 %
# sum(d[count_cp>=40]$count_cp)/300000   # 58.3 %
# sum(d[count_cp>=45]$count_cp)/300000   # 56.6 %
# sum(d[count_cp>=50]$count_cp)/300000   # 55.0 %

# on prend ceux avec au moins 15 observations (73% des observations totales)
# on prend ceux avec au moins 20 observations (69.7% des observations totales)