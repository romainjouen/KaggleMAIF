
library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)

setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("Brut_Train.csv", header=T)
maif_test <- fread("Brut_Test.csv", header=T)


d <- density(maif_train$prime_tot_ttc)
plot(d)



# % de remplissage
100-maif_train[,lapply(.SD, function(x) round(100*sum(is.na(x))/nrow(maif_train),2) )
         ,.SDcols=colnames(maif_train)]
100-maif_test[,lapply(.SD, function(x) round(100*sum(is.na(x))/nrow(maif_test),2) )
               ,.SDcols=colnames(maif_test)]


# type des colonnes 
maif_train[,lapply(.SD, function(x) {class(x)}), .SDcols=colnames(maif_train)]
maif_test[,lapply(.SD, function(x) {class(x)}), .SDcols=colnames(maif_test)]
# changement de type
maif_test[,("codepostal"):=lapply(.SD, as.character),.SDcols="codepostal"]

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
write.table(res,
            "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/result.csv",
            sep=';',
            row.names=F)



# ARBRE DE DECISION SUR PRIX BRUT:
# :::::::::::::::::::::::::::::::::

# creation prix_brut
maif_train[,prix_brut:=100*prime_tot_ttc/crm,by=id]

# choix des variables
train <- maif_train
train[,c("id","prime_tot_ttc","crm"):=NULL]

# model fit
arbre <- tree(prix_brut~., data=train, minsize=20)
arbre

# changement de type
maif_test[,("codepostal"):=lapply(.SD, as.character),.SDcols="codepostal"]


# prediction :
pred <- predict(arbre, maif_test)
pred

maif_test[,pred:=pred]
maif_test[,p_final:=pred*crm/100, by=id]

# impression résultats :
res <- data.table(ID=maif_test$id, COTIS=maif_test$p_final)
write.table(res,
            "arbre_brut.csv",
            sep=';',
            row.names=F)


# var1 , anc_veh, kmage_annuel


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



# variables à 2 valeurs : var 3, 5 , 20 et 21
table(maif_train$var20, maif_train$var21)
table(maif_train$var20, maif_train$var5)
table(maif_train$var5, maif_train$var21)
table(maif_train$var3, maif_train$var21)
table(maif_train$var20, maif_train$var3)
table(maif_train$var5, maif_train$var3)


#  croisement de variables 
# kmage_annuel et var12
t <- table(maif_train$kmage_annuel, maif_train$var12)
sum(t)
sum(diag(t))


head(maif_train[,.(var12,kmage_annuel)])

pairs(maif_train[,.(var12,kmage_annuel)])
plot(maif_train$kmage_annuel,maif_train$var12,
     col='red', type='l')

# croisement graphique avec les autres variables 
g4 <- ggplot(maif_train, aes(kmage_annuel,var12)) +
  geom_point(aes(group=as.character(var17), 
                 colour=as.character(var17), 
                 fill=as.character(var17))) +
  ggtitle(paste("Variable 3 en couleur"))
g4

pairs(maif_train)

multiplot(g1,g2,g3,g4,cols=2)


# variable 11
table(maif_train$var11)/3000
round(table(maif_train$puis_fiscale)/3000,4)

maif_train[puis_fiscale==58]


t <- maif_train[,mean(prime_tot_ttc), by="crm"]
setkey(t,crm)
t
t[,prix_brut:=V1*100/crm]
t

plot(t$crm,t$prix_brut, col='blue')
points(t$crm,t$V1, col='red')


# profession
t <- maif_train[,mean(prime_tot_ttc), by="profession"]
t


maif_train[,prix_brut:=prime_tot_ttc*100/crm,by=id]
maif_train[,mean(prix_brut), by="profession"]

