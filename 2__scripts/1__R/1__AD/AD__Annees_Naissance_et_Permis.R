
library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)
library(boot)

# setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("1__data/1__input/Brut_Train.csv", header=T)
maif_test <- fread("1__data/1__input/Brut_Test.csv", header=T)

# mise au prix de reference
maif_train[,prix_ref:=prime_tot_ttc*100/crm, by=id]
maif_test[,prime_tot_ttc:=prime_tot_ttc*100/crm, by=id]


N_indiv <- nrow(maif_train)

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




# 
##    annee_naissance
###
##############################

plot(maif_train$annee_naissance, maif_train$prix_ref )

d <- maif_train[,.(annee_naissance, prix_ref)]
d[,prime_annee:=mean(prix_ref), by=.(annee_naissance)]
d <- unique(d[,.(annee_naissance,prime_annee)])
setkey(d,annee_naissance)
d
plot(d$annee_naissance,d$prime_annee, col='blue')




# 
##    annee_permis
###
##############################

plot(maif_train$annee_permis, maif_train$prix_ref )

d2 <- maif_train[,.(annee_permis, prix_ref)]
d2[,prime_annee:=mean(prix_ref), by=.(annee_permis)]
d2 <- unique(d2[,.(annee_permis,prime_annee)])
setkey(d2,annee_naissance)
d2
plot(d2$annee_permis,d2$prime_annee, col='red')



# 
##    annee_permis  VS  annee_naissance
###
##############################

par(mfrow=c(2,1))
plot(d$annee_naissance,d$prime_annee, col='blue',
     main='Ann?e de naissance')
plot(d2$annee_permis,d2$prime_annee, col='red',
     main='Ann?e du permis')

corr(cbind(maif_train$annee_naissance, maif_train$annee_permis))
mod <- lm(maif_train$annee_permis~maif_train$annee_naissance)
str(mod)



# 
##    New Variables d'age
###
##############################

# creation des variables
d3 <- maif_train[,.(annee_naissance, annee_permis,prix_ref)]
d3[,age_permis := annee_permis-annee_naissance, by=.(annee_naissance, annee_permis)]
d3[,age_actuel := 2016 - annee_naissance]
d3[,duree_permis := age_actuel - age_permis]

# calcul des primes moyennes et effectifs par valeurs de variables
d3[,age_permis_mean   := mean(prix_ref), by=.(age_permis)]
d3[,age_actuel_mean   := mean(prix_ref), by=.(age_actuel)]
d3[,duree_permis_mean := mean(prix_ref), by=.(duree_permis)]
d3[,age_permis_count   := .N/N_indiv, by=.(age_permis)]
d3[,age_actuel_count   := .N/N_indiv, by=.(age_actuel)]
d3[,duree_permis_count := .N/N_indiv, by=.(duree_permis)]

# cr?ation des sous tables pour les graphes
d3a <- unique(d3[,.(age_permis,  age_permis_mean)])
d3b <- unique(d3[,.(age_actuel,  age_actuel_mean)])
d3c <- unique(d3[,.(duree_permis,duree_permis_mean)])
d3d <- unique(d3[,.(age_permis,  age_permis_count)])
d3e <- unique(d3[,.(age_actuel,  age_actuel_count)])
d3f <- unique(d3[,.(duree_permis,duree_permis_count)])

# ordonnancement
setkey(d3a,age_permis)
setkey(d3b,age_actuel)
setkey(d3c,duree_permis)
setkey(d3d,age_permis)
setkey(d3e,age_actuel)
setkey(d3f,duree_permis)


# graphes  des valeurs moyennes
par(mfrow=c(3,1))
plot(d3a$age_permis,  d3a$age_permis_mean, col='blue', type='b',
     main='Age obtention du permis')
plot(d3b$age_actuel,  d3b$age_actuel_mean, col='red', type='b',
     main='Age actuel')
plot(d3c$duree_permis,d3c$duree_permis_mean, col='blue', type='b',
     main='Duree depuis le permis (ann?es)')


# graphes  des effectifs
par(mfrow=c(3,1))
plot(d3d$age_permis,  d3d$age_permis_count, col='blue', type='b',
     main='Age obtention du permis (Proportion)')
plot(d3e$age_actuel,  d3e$age_actuel_count, col='red', type='b',
     main='Age actuel (Proportion)')
plot(d3f$duree_permis,d3f$duree_permis_count, col='blue', type='b',
     main='Dur?e depuis le permis (ann?es) (Proportion)')


d3c
# on voit certaines durées de permis de -2 : on regarde 
nrow(maif_train[annee_permis>2016])  # on les enleve
d3 <- d3[duree_permis>=0]




d3c
# on voit des prix d'assurance bizarre pour duree permis = 8 et 9 ans 
# on va remplacer les valeurs d'assurance par qqch de plus cohérent : 
plot(d3c$duree_permis,d3c$duree_permis_mean, col='blue', type='b',
     main='Duree depuis le permis (ann?es)')
mod <- lm(d3c$duree_permis_mean[c(5:10,13:28)]~d3c$duree_permis[c(5:10,13:28)])
mod
abline(mod$coef[1],mod$coef[2],col='red')

# les valeurs moyennes pour 8 et 9 devraient être :
val_8_moy <- mod$coef[1] + 8 * mod$coef[2]
val_9_moy <- mod$coef[1] + 8 * mod$coef[2]

# calcul des indices : prix_moyen_classe / prix_moyen
indices_duree_permis <- c(val_8_moy,val_9_moy)
mean(maif_train$prix_ref)
indices_duree_permis <- indices_duree_permis/mean(maif_train$prix_ref)

save(indices_duree_permis, file="1__data/2__output/Indices_Duree_Permis.Rda")


# FAIRE LES BOXPLOT PAR VALEUR POUR VOIR SI QUELQUES VALEURS BIZARRES OU TOUTES CELLE DE 8-9 BIZARRES

g <- ggplot(d3,aes(as.character(duree_permis),prix_ref))
g + geom_boxplot(aes(color=as.character(duree_permis)))





