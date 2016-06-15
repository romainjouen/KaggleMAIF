#############################################
#############################################
#####                                ########
#####        INITIALISATION          ########
#####                                ########
#############################################
#############################################

#Vider l'environnement
rm(list=ls())

source("RTools.R")

Instal_Required("data.table")
Instal_Required("ade4")
Instal_Required("tree")
Instal_Required("lda")
Instal_Required("ggplot")
Instal_Required("randomForest")
Instal_Required("stringr")

#setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("../1-Data/0-Input/ech_apprentissage.csv", header=T)
maif_test <- fread("../1-Data/0-Input/ech_test.csv", header=T)


plot(density(maif_train$prime_tot_ttc))

#Missing Value
is.na(maif_train) <- maif_train=="NR"
is.na(maif_test) <- maif_test=="NR"
source("RTools.R")
#Synthese = Synt_Df(maif_train, out="TRAIN")
#Synthese2 = Synt_Df(maif_test, out="TEST")
comp = Comp_train_test(train = maif_train, test = maif_test)


plot(aggregate(prime_tot_tcc~var17,maif_test,mean))

# changement de type
maif_test$codepostal = as.factor(maif_test$codepostal)
maif_test$energie_veh = as.factor(maif_test$energie_veh)
maif_test$marque = as.factor(maif_test$marque)
maif_test$var6 = as.factor(maif_test$var6)
maif_test$var8 = as.factor(maif_test$var8)
maif_test$var14 = as.factor(maif_test$var14)
maif_test$profession = as.factor(maif_test$profession)

maif_train$codepostal = as.factor(maif_test$codepostal)
maif_train$energie_veh = as.factor(maif_test$energie_veh)
maif_train$marque = as.factor(maif_test$marque)
maif_train$var6 = as.factor(maif_test$var6)
maif_train$var8 = as.factor(maif_test$var8)
maif_train$var14 = as.factor(maif_test$var14)
maif_train$profession = as.factor(maif_test$profession)

# # remplacement des 0 
# for (j in names(tab_fam)[-1]){
#   set(tab_fam, which( is.na( tab_fam[[j]] ) ), j, 0)
# }
# str(tab_fam)



# ARBRE DE DECISION :
# :::::::::::::::::::::::::


# model fit
arbre <- tree(prime_tot_ttc~., data=maif_train)
arbre

# prediction :
pred <- predict(arbre, maif_test)
pred

# impression résultats :
res <- data.table(ID=maif_test$id, COTIS=pred)

#############################################
#############################################
#####                                ########
#####            SUBMSSION           ########
#####                                ########
#############################################
#############################################

Submission(res,"First")



# FORET ALEATOIRE :
# :::::::::::::::::

# changement des character en factor pour l'inputation des NA 
d_train <- maif_train
typ <- d_train[,lapply(.SD, function(x) {class(x)}), .SDcols=colnames(d_train)]
typ <- t(as.vector(typ))
cols <- rownames(typ)[which(typ=="character")]
d_train[,(cols):=lapply(.SD, as.factor),.SDcols=cols]


# imputation de valeurs dans les NA 
foret <- rfImpute(prime_tot_ttc~., data=d_train, ntrees=50)

# model fit
foret <- randomForest(prime_tot_ttc~., data=maif_train, ntrees=50)
foret

# prediction :
pred <- predict(foret, maif_test)
pred

# impression résultats :
res <- data.table(ID=maif_test$id, COTIS=pred)
write.table(res,
            "foret.csv",
            sep=';',
            row.names=F)


?rfImpute
