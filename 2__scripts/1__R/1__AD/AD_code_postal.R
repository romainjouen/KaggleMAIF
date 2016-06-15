library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)

setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("Brut_Train.csv", header=T)
maif_test <- fread("Brut_Test.csv", header=T)

# creation prix_brut
maif_train[,prix_ref:=100*prime_tot_ttc/crm,by=id]



########################################
###
##      ANALYSE
#



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
maif_train[,nb:=nchar(codepostal), by='codepostal']
maif_train[,cp:=codepostal]
maif_train[nb==4, cp:=paste("0",codepostal,sep="")]
table(nchar(maif_train$cp))  # niquel 


# on groupe par département : 
maif_train[,dep:=substr(cp,0,2), by='cp']

# moyennes par dep 
maif_train[,mean_dep:=mean(prix_ref),by='dep']

# effectifs par dep
maif_train[,count_dep:=.N,by='dep']


# On regarde les prix par groupes
depart <- unique(maif_train[,.(dep,mean_dep,count_dep)])
depart


# on va caluler un prix par région : 
# on calcule le prix de référence moyen : 
prix_ref_mean <- mean(maif_train$prix_ref)

# calcul des indices par departement : 
depart[,indice_dep:=prix_ref_mean/mean_dep,by='dep']
depart

# code R de Yoan 






# pour egarder dans tableau 
write.csv2(depart, file="prix_par_dep.csv",row.names=F)








### On a fait les prix par departement ; 

# peut-on regarder le prix par code postal :


##############################################################"




code <- maif_train$codepostal
code_num <- as.numeric(code)


# combien de code postaux dfférents peut-on trouver ? 
length(unique(maif_train$codepostal))   # 23696
t <- table(maif_train$codepostal)
t


# on remplace les "NR" par des NA 
set(maif_train, which(maif_train[["codepostal"]]=='NR' ), "codepostal", NA)

# les codes postaux à 4 chiffres doivent correspondre aux codes postaux commencant
# par 0 sans le 0 
# on le rajoute donc
maif_train[,nb:=nchar(codepostal), by='codepostal']
maif_train[,cp:=codepostal]
maif_train[nb==4, cp:=paste("0",codepostal,sep="")]
table(nchar(maif_train$cp))  # niquel 

t <- table(maif_train$codepostal)
t
tt <- table(t)
tt

  



#
##        COMPARAISON CODE POSTAUX TEST-TRAIN
###
#####################################################


# on remplace les "NR" par des NA 
set(maif_train, which(maif_train[["codepostal"]]=='NR' ), "codepostal", NA)
set(maif_test , which(maif_test [["codepostal"]]=='NR' ), "codepostal", NA)


# les codes postaux à 4 chiffres doivent être aux codes postaux commencant
# par 0 sans le 0 : on le rajoute 
maif_train[,nb:=nchar(codepostal), by='codepostal']
maif_train[,cp:=codepostal]
maif_train[nb==4, cp:=paste("0",codepostal,sep="")]
table(nchar(maif_train$cp))  # niquel 

maif_test[,("codepostal"):=lapply(.SD, as.character),.SDcols="codepostal"] # chgt de type 

maif_test[,nb:=nchar(codepostal), by='codepostal']
maif_test[,cp:=codepostal]
maif_test[nb==4, cp:=paste("0",as.character(codepostal),sep="")]
table(nchar(maif_test$cp))  # niquel 



cp_train <- unique(maif_train[,.(cp)])
cp_test  <- unique(maif_test [,.(cp)])

setkey(cp_train,cp)
setkey(cp_test,cp)

setdiff(cp_test$cp,cp_train$cp)

t <- merge(cp_test,cp_train,by='cp')
nrow

# que 3 valeurs en commun : 01034 ; 97107 ; 97109


maif_train[,dep:=substr(cp,0,2), by='cp']
table(maif_train$dep)
length(unique(maif_train$cp))



maif_test[,dep:=substr(cp,0,2), by='cp']
table(maif_test$dep)
length(unique(maif_test$cp))


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

# ajout des 0 pour les code postal des départements < 10 
maif_train[nb==4, cp:=paste("0",codepostal,sep="")]

# on groupe par département : 
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
save(indices_dep, file="Indices_Prix_Departement.Rda")




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

# ajout des 0 pour les code postal des départements < 10 
maif_train[nb==4, cp:=paste("0",codepostal,sep="")]

# effectifs par code postal
maif_train[,count_cp:=.N, by='cp']

# moyennes par cp
maif_train[count_cp>=15,mean_cp:=mean(prix_ref),by='cp']

# on stocke dans une table 
cp_indices <- unique(maif_train[,.(cp,mean_cp)])

# calcul du prix de ref francais moyen 
prix_ref_fra <- mean(maif_train$prix_ref)

# calcul des indices de prix par departement :     (pour prix_ref_fra = prix_ref_dep/indice)
cp_indices[,indice_cp:=mean_cp/prix_ref_fra,by='cp']

# on stocke les infos dans une valeur
indices_dep <- cp_indices[,.(cp, indice_cp)]

# enregistrement des indices par dep 
save(indices_dep, file="Indices_Prix_CodePostal.Rda")



# Choix du nombre d'obs minimum sur le code postal pour prendre l'indice du cp 

d <- unique(maif_train[,.(cp,count_cp)])
setkey(d,count_cp)
d
tail(d,100)
sum(d$count_cp)
sum(d$count_cp[20481:23696])/sum(d$count_cp)
d$count_cp[20480]
d$count_cp[20481]
# on prend ceux avec au moins 15 observations (73% des observations totales)