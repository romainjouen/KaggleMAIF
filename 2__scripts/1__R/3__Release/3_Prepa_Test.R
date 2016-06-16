library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)

# setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_test <- fread("1__data/1__input/Brut_Test.csv", header=T)


      
      
      ##########################################################
      ####                                                  ####
      ###          CREATION DES VARIABLES DE DATE            ###
      ####                                                  ####
      ##########################################################

# age auquel l'individu a passé le permis
maif_test[,age_permis := annee_permis-annee_naissance, by='id']

# durée depuis l'obtention du permis en années
maif_test[,duree_permis := 2016 - annee_permis, by='id']


      
      ##########################################################
      ####                                                  ####
      ###       CREATION DE CATEG POUR LES VAR QUANTI        ###
      ####                                                  ####
      ##########################################################

# puissance_fiscale 
maif_test[,puis_fiscale_cat:=cut(puis_fiscale, 
                                 breaks=c(-1,7,1000),
                                 labels=c("puis_croit","puis_stagne"))]

# anc_veh : cluster basés sur la distribution des valeurs  : 
maif_test[,anc_veh_cat:=cut(anc_veh, 
                            breaks=c(-1,15,31,70,110),
                            labels=c("age1","age2","age3","age4"))]

# anc_veh : cluster basés sur le prix de l'assurance  : 
maif_test[,anc_veh_cat_2:=cut(anc_veh, 
                               breaks=c(-1,3,7,15,60,120),
                               labels=c("age2_1","age2_2","age2_3","age2_4","age2_5"))]

# kmage_annuel
maif_test[,kmage_annuel_cat:=cut(kmage_annuel, 
                             breaks=c(-1,5000,9500,16500,30000),
                             labels=c("km1","km2","km3","km4"))]



      
      ##########################################################
      ####                                                  ####
      ###              CREATION MATRICE DE TEST              ###
      ####                                                  ####
      ##########################################################


test_mat  <- maif_test[,.(# prix_ref_geo,
                          age_permis,
                          duree_permis,
                          marque,
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


