library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)








##########################################################
####                                                  ####
###                  ARBRE DE DECISION                 ###
####                                                  ####
##########################################################



# model fit
mod <- tree(prix_ref_geo~., data=train_mat)
mod




##########################################################
####                                                  ####
###                  REGRESSION LINEAIRE               ###
####                                                  ####
##########################################################



reg<-lm(app$prime_cor~
          app$annee_permis+
          app$anc_veh+
          app$annee_naissance+
          app$puis_fiscale+
          app$anc_veh+
          app$kmage_annuel+
          app$var1+
          app$var2+
          app$var3+
          app$var4+
          app$var5+
          app$var6+
          app$var7+
          app$var8+
          app$var9+
          app$var10+
          app$var11+app$var12+app$var13+app$var14+app$var15+app$var16+app$var17+app$var18+app$var19+app$var20+app$var21+app$var22,family=gaussian)


mod <- lm(prix_ref_dep~., data=train_mat)

# model fit
mod <- tree(prix_ref_dep~., data=train_mat)
mod








# FORET ALEATOIRE :
# :::::::::::::::::

# changement des character en factor pour l'inputation des NA 
typ <- train_mat[,lapply(.SD, function(x) {class(x)}), .SDcols=colnames(train_mat)]
typ <- t(as.vector(typ))
cols <- rownames(typ)[which(typ=="character")]
train_mat[,(cols):=lapply(.SD, as.factor),.SDcols=cols]


# imputation de valeurs dans les NA 
t1 <- Sys.time()
foret <- rfImpute(prix_ref_dep~., data=train_mat, ntrees=5)
t2 <- Sys.time()
t2-t1

# model fit
t1 <- Sys.time()
foret <- randomForest(prix_ref_dep~., data=train_mat, ntrees=5)
t2 <- Sys.time()
t2-t1


# prediction :
pred <- predict(foret, maif_test)
pred

# impression résultats :
res <- data.table(ID=maif_test$id, COTIS=pred)
write.table(res,
            "foret.csv",
            sep=';',
            row.names=F)







