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
        ###   SUPPRESSION DES VARIABLES ABERRANTES   ###
        ####                                        ####
        ################################################

maif_train <- maif_train[annee_permis<=2016]     # -294 :  299 706 / 300 000  
maif_train <- maif_train[annee_naissance<=1998]  #    0 

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

# age auquel l'individu a passé le permis
train_dep[,age_permis := annee_permis-annee_naissance, by='id']

# durée depuis l'obtention du permis en années
train_dep[,duree_permis := 2016 - annee_permis,by='id']


          
          
          ##########################################################
          ####                                                  ####
          ###       CREATION DE CATEG POUR LES VAR QUANTI        ###
          ####                                                  ####
          ##########################################################

# puissance_fiscale :  clusters basés sur le prix de l'assurance  : 
train_dep[,puis_fiscale_cat:=cut(puis_fiscale, 
                                 breaks=c(-1,7,1000),
                                 labels=c("puis_croit","puis_stagne"))]

# anc_veh : clusters basés sur la distribution des valeurs  : 
train_dep[,anc_veh_cat:=cut(anc_veh, 
                            breaks=c(-1,15,31,70,110),
                            labels=c("age1","age2","age3","age4"))]

# anc_veh : clusters basés sur le prix de l'assurance  : 
train_dep[,anc_veh_cat_2:=cut(anc_veh, 
                              breaks=c(-1,3,7,15,60,120),
                              labels=c("age2_1","age2_2","age2_3","age2_4","age2_5"))]

# kmage_annuel : clusters basés sur le prix de l'assurance et la distribution des valeurs
train_dep[,kmage_annuel_cat:=cut(kmage_annuel, 
                             breaks=c(-1,5000,9500,16500,30000),
                             labels=c("km1","km2","km3","km4"))]



          
          ##########################################################
          ####                                                  ####
          ###          CREATION MATRICE APPRENTISSAGE            ###
          ####                                                  ####
          ##########################################################



train_mat  <- train_dep[,.(prix_ref_geo,
                            age_permis,
                            duree_permis,
                            # marque,
                            puis_fiscale, 
                            puis_fiscale_cat,
                            anc_veh,
                            anc_veh_cat,
                            anc_veh_cat_2,
                            # codepostal,
                            energie_veh,
                            kmage_annuel,
                            kmage_annuel_cat,
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

