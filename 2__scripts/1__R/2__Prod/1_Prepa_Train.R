library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)

# setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/Projet_maif/")

maif_train <- fread("1__data/1__input/Brut_Train.csv", header=T)




        ################################################
        ####                                        ####
        ###      MISE AU PRIX DE REFERENCE           ###
        ####                                        ####
        ################################################

maif_train[,prix_ref:=100*prime_tot_ttc/crm,by=id]


        
        ##########################################################
        ####                                                  ####
        ###      MISE AU PRIX DE REF PAR DEPARTEMENT           ###
        ####                                                  ####
        ##########################################################

# on remplace les "NR" par des NA 
set(maif_train, which(maif_train[["codepostal"]]=='NR' ), "codepostal", NA)

# on calcule le nombre de char pour voir auquel dep ajouter 0 
maif_train[,nb:=nchar(codepostal), by='codepostal']
maif_train[,cp:=codepostal, by='codepostal']

# ajout des 0 pour les code postal des d?partements < 10 
maif_train[nb==4, cp:=paste("0",codepostal,sep="")]

# on groupe par d?partement : 
maif_train[,dep:=substr(cp,0,2), by='cp']

# on charge les donn?es d'indices par département et de code postal
load(file="1__data/2__output/Indices_Prix_Departement.Rda")  # indices_dep
# load(file="1__data/2__output/Indices_Prix_CodePostal.Rda")   # indices_cp


# on les rajoute dans la matrice train 
train_dep <- merge(maif_train, indices_dep[,.(dep,indice_dep)], by="dep")
# train_dep <- merge(train_dep,   indices_cp[,.(cp,  indice_cp)], by="cp")

# calcul du prix de ref par individu 
setkey(train_dep,NULL)
train_dep[,  prix_ref_geo:=prix_ref/indice_dep, by='id']

# # si on veut affecter un poids par code postal plutot que par dept : 
# train_dep[!(is.na(indice_cp)), prix_ref_geo:=prix_ref/indice_cp, by='id']
# train_dep[  is.na(indice_cp),  prix_ref_geo:=prix_ref/indice_dep,by='id']


rm(indices_dep)




          ##########################################################
          ####                                                  ####
          ###          CREATION DES VARIABLES DE DATE            ###
          ####                                                  ####
          ##########################################################

train_dep[,age_permis := annee_permis-annee_naissance, by='id']
train_dep[,duree_permis := 2016 - annee_permis,by='id']











          
          ##########################################################
          ####                                                  ####
          ###          CREATION MATRICE APPRENTISSAGE            ###
          ####                                                  ####
          ##########################################################


train_mat <- train_dep[,.(prix_ref_geo,
                          age_permis,
                          duree_permis,
                          # marque,
                          puis_fiscale,
                          anc_veh,
                          # codepostal,
                          energie_veh,
                          kmage_annuel,
                          profession,
                          var2,
                          var3,
                          var4,
                          var5,
                          var6,
                          var8,
                          var13,
                          var14,
                          var15,
                          var16,
                          var17,
                          var20,
                          var21,
                          var22
                          )]


rm(train_dep)

