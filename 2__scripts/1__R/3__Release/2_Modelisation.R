library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)
library(randomForestSRC)








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







##########################################################
####                                                  ####
###                  FORET ALEATOIRE                   ###
####                                                  ####
##########################################################



# suppression des NA :
train_mat
100-train_mat[,lapply(.SD, function(x) round(100*sum(is.na(x))/nrow(train_mat),2) )
              ,.SDcols=colnames(train_mat)]   # % remplissage
train_mat[,id:=seq(1:nrow(train_mat))]
train_mat[,to_omit:=sum(is.na(c(age_permis,duree_permis))),by='id']
table(train_mat[,.(to_omit)])

train_mat <- train_mat[to_omit==0]
train_mat[,to_omit:=NULL]
train_mat[,id:=NULL]



# echantillonage: on prend n parmi les 300 000 
n <- nrow(train_mat)
N <- sample(c(1:300000),size=n,replace=F)
train_mat_2 <- train_mat[N]



# changement des character en factor pour le package randomForestSRC
typ <- train_mat_2[,lapply(.SD, function(x) {class(x)}), .SDcols=colnames(train_mat_2)]
typ <- t(as.vector(typ))
cols <- rownames(typ)[which(typ=="character")]
train_mat_2[,(cols):=lapply(.SD, as.factor),.SDcols=cols]


# construction de la forêt
t1 <- Sys.time()
foret <- rfsrc(prix_ref_geo~., 
               data=train_mat_2, 
               ntree=50,
               mtry=9)
t2 <- Sys.time()
t2-t1
foret
mod <- foret

save(foret, file="1__data/2__output/Random_Forest_50t_all.Rda")

load(file="1__data/2__output/Random_Forest.Rda")

mod <- foret

##########################################################
####                                                  ####
###                  FORET ALEATOIRE                   ###
####                                                  ####
##########################################################

# on enleve tous les NA :
train_mat
100-train_mat[,lapply(.SD, function(x) round(100*sum(is.na(x))/nrow(train_mat),2) )
              ,.SDcols=colnames(train_mat)]   # % remplissage
train_mat[,id:=seq(1:nrow(train_mat))]
train_mat[,to_omit:=sum(is.na(c(age_permis,duree_permis))),by='id']
table(train_mat[,.(to_omit)])

train_mat <- train_mat[to_omit==0]
train_mat[,to_omit:=NULL]
train_mat[,id:=NULL]


# on prend 200 000 AU PIF 
N <- sample(c(1:300000),size=500,replace=F)
train_mat_2 <- train_mat[N]

t1 <- Sys.time()
foret <- randomForest(prix_ref_geo~., 
                      data=train_mat_2, 
                      ntrees=5,
                      mtry=12,
                      na.action=na.omit)
t2 <- Sys.time()
t2-t1
foret

t1 <- Sys.time()
foret <- randomForest(prix_ref_geo~., 
                      data=train_mat_2, 
                      ntrees=5,
                      mtry=12)
t2 <- Sys.time()
t2-t1
foret


# imputation de valeurs manquantes : 
na.roughfix(train_mat)


# changement des character en factor pour l'inputation des NA 
typ <- train_mat[,lapply(.SD, function(x) {class(x)}), .SDcols=colnames(train_mat)]
typ <- t(as.vector(typ))
cols <- rownames(typ)[which(typ=="character")]
train_mat[,(cols):=lapply(.SD, as.factor),.SDcols=cols]


# on prend 200 000 AU PIF 
N <- sample(c(1:300000),size=50000,replace=F)

# 
train_mat_2 <- train_mat[N]
# imputation de valeurs dans les NA 
t1 <- Sys.time()
foret <- rfImpute(prix_ref_geo~., data=train_mat_2, ntrees=50)
t2 <- Sys.time()
t2-t1
foret


# model fit
t1 <- Sys.time()
foret <- randomForest(prix_ref_dep~., data=train_mat, ntrees=5)
t2 <- Sys.time()
t2-t1


# prediction :
pred <- predict(foret, maif_test)
pred

# impression r?sultats :
res <- data.table(ID=maif_test$id, COTIS=pred)
write.table(res,
            "foret.csv",
            sep=';',
            row.names=F)







