library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)





# changement de type
maif_test[,("codepostal"):=lapply(.SD, as.character),.SDcols="codepostal"]

        
        ##########################################################
        ####                                                  ####
        ###                APPLICATION DU MODELE               ###
        ####                                                  ####
        ##########################################################



# prediction :
pred <- predict(mod, test_mat)
pred





        
        ##########################################################
        ####                                                  ####
        ###             MISE AU PRIX DE REFERENCE              ###
        ####                                                  ####
        ##########################################################

maif_test[,pred:=pred]
maif_test[,pred_ref_fra:=pred*crm/100, by=id]



        
        ##########################################################
        ####                                                  ####
        ###      MISE AU PRIX DE REF PAR DEPARTEMENT / CP      ###
        ####                                                  ####
        ##########################################################


# on remplace les "NR" par des NA 
set(maif_test, which(maif_test[["codepostal"]]=='NR' ), "codepostal", NA)

# on calcule le nommbre de char pour voir auquel dep ajouter 0 
maif_test[,nb:=nchar(codepostal), by='codepostal']
maif_test[,cp:=codepostal, by='codepostal']

# ajout des 0 pour les code postal des d?partements < 10 
maif_test[nb==4, cp:=paste("0",codepostal,sep="")]

# on groupe par d?partement : 
maif_test[,dep:=substr(cp,0,2), by='cp']

# on charge les donn?es d'indices par d?partement et de code postal
load(file="Indices_Prix_CodePostal.Rda")   # indices_cp
load(file="Indices_Prix_Departement.Rda")  # indices_dep

# on les rajoute dans la matrice de test
test_geo <- merge(maif_test, indices_dep[,.(dep,indice_dep)], by="dep", all.x=T)
test_geo <- merge(test_geo,   indices_cp[,.(cp,  indice_cp)], by="cp",  all.x=T)

# sum(is.na(test_geo$indice_dep))  # doit ?tre 0
# sum(is.na(test_geo$indice_cp))   # doit ?tre assez ?lev?

# calcul du prix de ref par individu par departement
setkey(test_geo,NULL)
test_geo[ !(is.na(indice_cp)) , pred_ref_geo:=pred_ref_fra*indice_cp, by='id']
test_geo[   is.na(indice_cp)  , pred_ref_geo:=pred_ref_fra*indice_dep,by='id']



        
        ##########################################################
        ####                                                  ####
        ###               IMPRESSION DES RESULTATS             ###
        ####                                                  ####
        ##########################################################

# impression r?sultats :
res <- data.table(ID=test_geo$id, COTIS=test_geo$pred_ref_geo)
write.table(res,
            "mon_resultat2.csv",
            sep=';',
            row.names=F)

sum(is.na(res$ID))
sum(is.na(res$COTIS))

res[which(is.na(res$COTIS)), COTIS:=prix_ref_fra]

