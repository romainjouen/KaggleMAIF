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

maif_test[,age_permis := annee_permis-annee_naissance, by='id']
maif_test[,duree_permis := 2016 - annee_permis, by='id']

      
      
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


