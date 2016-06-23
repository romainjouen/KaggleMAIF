source("2__scripts/1__R/3__Release/RTools.R")

Instal_Required("data.table")
Instal_Required("ade4")
Instal_Required("tree")
Instal_Required("lda")
Instal_Required("ggplot2")
Instal_Required("randomForest")
Instal_Required("boot")

# setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("1__data/1__input/Brut_Train.csv", header=T)
maif_test <- fread("1__data/1__input/Brut_Test.csv", header=T)




# mise au prix de reference
maif_train[,prix_ref:=prime_tot_ttc*100/crm, by=id]


Out = Synthese(data_train = maif_train,
               data_test = maif_test,
               export=TRUE, 
               rapport=TRUE, 
               target = "prix_ref",
               without=c("id","prime_tot_ttc"))


##########################################################
####                                                  ####
###                    PROFESSION                      ###
####                                                  ####
##########################################################

source("2__scripts/1__R/3__Release/RTools.R")
# # type cat?gorique 
# val <- "profession"
# maif_train[,val:=profession]
# maif_test [,val:=profession]
# 
# # Valeurs TRAIN / TEST identiques ???    yes 
# # ::::::::::::::::::::::::::::::::::::::::::::::::
# v_train <- unique(maif_train[,.(val)])
# v_test  <- unique(maif_test [,.(val)])
# setkey(v_train,val)
# setkey(v_test,val)
# identical(v_test,v_train)
# 
# # effectifs TRAIN/TEST identiques ??? 
# # ::::::::::::::::::::::::::::::::
# par(mfrow=c(2,1))
# maif_train[,count_val:=round(100*.N/300000,2),by='val']
# maif_test [,count_val:=round(100*.N/30000 ,2),by='val']
# tr <- unique(maif_train[,.(val,count_val)])
# te <- unique(maif_test [,.(val,count_val)])
# setkey(tr,val)
# setkey(te,val)
# prop <- merge(tr,te,all=T, by='val')
# prop[,rapp:=count_val.y/count_val.x]
# View(prop)
# 
# # train et test semblent avoir des ?chantillons ? peu pr?s identiques
# 
# 
# # Prix par Classe : 
# # ::::::::::::::::::::::::::::::::
# g <- ggplot(maif_train,aes(profession,prix_ref))
# g + geom_boxplot(aes(color=profession))
# 
# 
# # prix moyens par classes 
# # ::::::::::::::::::::::::::::::::
# round(100*table(maif_train$profession)/300000,2)
# maif_train[,mean_prof:=mean(prix_ref),by='profession']
# 
# prof <- unique(maif_train[,.(profession,mean_prof)])
# prof

Out = func_AD(data_train = maif_train,data_test = maif_test,target = "prix_ref",AD_val = "var9")
Out$plotly



##########################################################
####                                                  ####
###                    MARQUE                          ###
####                                                  ####
##########################################################



# type cat?gorique
maif_train[,val:=marque]
maif_test [,val:=marque]

# Valeurs TRAIN / TEST identiques ???    no 
# ::::::::::::::::::::::::::::::::::::::::::::::::
v_train <- unique(maif_train[,.(val)])
v_test  <- unique(maif_test [,.(val)])
setkey(v_train,val)
setkey(v_test,val)
identical(v_test$val,v_train$val)
intersect(v_train$val , v_test$val)
setdiff(v_train$val , v_test$val)
setdiff(v_test$val  , v_train$val)


# train a 155 marques
# test a 74 marques dont 72 dans train 

# nouvelles marques de train : "GEELY"   "SOVAM"



# effectifs TRAIN/TEST identiques ??? 
# ::::::::::::::::::::::::::::::::
par(mfrow=c(2,1))
maif_train[,count_val:=round(100*.N/300000,2),by='val']
maif_test [,count_val:=round(100*.N/30000 ,2),by='val']
tr <- unique(maif_train[,.(val,count_val)])
te <- unique(maif_test [,.(val,count_val)])
setkey(tr,val)
setkey(te,val)
prop <- merge(tr,te,all=T, by='val')
prop[,rapp:=count_val.y/count_val.x]
View(prop)



# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(val,prix_ref))
g + geom_boxplot(aes(color=val))

# on voit des diff?rences : faire des clusters de marque ? 



# prix moyens par classes 
# ::::::::::::::::::::::::::::::::
round(100*table(maif_train$val)/300000,2)
maif_train[,mean_val:=mean(prix_ref),by='val']

prof <- unique(maif_train[,.(val,mean_val)])
prof





##########################################################
####                                                  ####
###               energie_veh                          ###
####                                                  ####
##########################################################



# type cat?gorique
maif_train[,val:=energie_veh]
maif_test [,val:=energie_veh]

# Valeurs TRAIN / TEST identiques ???    no 
# ::::::::::::::::::::::::::::::::::::::::::::::::
v_train <- unique(maif_train[,.(val)])
v_test  <- unique(maif_test [,.(val)])
setkey(v_train,val)
setkey(v_test,val)
identical(v_test$val,v_train$val)
intersect(v_train$val , v_test$val)
setdiff(v_train$val , v_test$val)
setdiff(v_test$val  , v_train$val)

# train et test ont les m?mes valeurs 



# effectifs TRAIN/TEST identiques ??? 
# ::::::::::::::::::::::::::::::::
par(mfrow=c(2,1))
maif_train[,count_val:=round(100*.N/300000,2),by='val']
maif_test [,count_val:=round(100*.N/30000 ,2),by='val']
tr <- unique(maif_train[,.(val,count_val)])
te <- unique(maif_test [,.(val,count_val)])
setkey(tr,val)
setkey(te,val)
prop <- merge(tr,te,all=T, by='val')
prop[,rapp:=count_val.y/count_val.x]
View(prop)

# effectifs ? peu pr?s similaires 



# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(val,prix_ref))
g + geom_boxplot(aes(color=val))

# on voit des diff?rences : faire des clusters de marque ? 



# prix moyens par classes 
# ::::::::::::::::::::::::::::::::
round(100*table(maif_train$val)/300000,2)
maif_train[,mean_val:=mean(prix_ref),by='val']

prof <- unique(maif_train[,.(val,mean_val)])
prof


# il semble que ca ait du sens de garder energie veh tel quel 








##########################################################
####                                                  ####
###                    puis_fiscale                    ###
####                                                  ####
##########################################################



# type cat?gorique
maif_train[,val:=puis_fiscale]
maif_test [,val:=puis_fiscale]

# Valeurs TRAIN / TEST identiques ???    no 
# ::::::::::::::::::::::::::::::::::::::::::::::::
v_train <- unique(maif_train[,.(val)])
v_test  <- unique(maif_test [,.(val)])
setkey(v_train,val)
setkey(v_test,val)
identical(v_test$val,v_train$val)
intersect(v_train$val , v_test$val)
setdiff(v_train$val , v_test$val)
setdiff(v_test$val  , v_train$val)

# toutes les valeurs de test sont dans train 

# 36 valeurs sur les 51 de train sont dans test


summary(maif_train$val)
summary(maif_test $val)
plot(density(maif_train$val), col='blue', freq='F')
lines(density(maif_test$val), col='red' , freq='F')


# effectifs TRAIN/TEST identiques ??? 
# ::::::::::::::::::::::::::::::::
par(mfrow=c(2,1))
maif_train[,count_val:=round(100*.N/300000,3),by='val']
maif_test [,count_val:=round(100*.N/30000 ,3),by='val']
tr <- unique(maif_train[,.(val,count_val)])
te <- unique(maif_test [,.(val,count_val)])
setkey(tr,val)
setkey(te,val)
prop <- merge(tr,te,all=T, by='val')
prop[,rapp:=count_val.y/count_val.x]
View(prop)

# effectifs ? peu pr?s similaires 
# 


# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(as.character(val),prix_ref))
g + geom_boxplot(aes(color=as.factor(val)))

# on voit des diff?rences : faire des clusters de marque ? 



# prix moyens par classes 
# ::::::::::::::::::::::::::::::::
round(100*table(maif_train$val)/300000,2)
maif_train[,mean_val:=mean(prix_ref),by='val']

means <- unique(maif_train[,.(val,mean_val)])
means
plot(means$val,means$mean_val,ylim=c(0,700),col='blue')
plot(means$val,means$mean_val,col='blue',xlim=c(0,25))


# le prix semble augmenter avec la puis fiscale pour les puis fiscales < 8 ce qui 
# correspond ? la plupart des voitures

# cela a du sens de mettre la variable en quanti

maif_train[,puis_fiscale_cat:=cut(puis_fiscale, 
                                  breaks=c(-1,7,1000),
                                  labels=c("gr1_croit","gr2_plat"))]






##########################################################
####                                                  ####
###                    kmage_annuel                    ###
####                                                  ####
##########################################################



# type cat?gorique
maif_train[,val:=kmage_annuel]
maif_test [,val:=kmage_annuel]

# Valeurs TRAIN / TEST identiques ???     
# ::::::::::::::::::::::::::::::::::::::::::::::::
summary(maif_train$val)
summary(maif_test $val)
plot(density(maif_train$val), col='blue', freq='F')
lines(density(maif_test$val), col='red' , freq='F')

# minimum et maximum tr?s proches, distribution quasi identique 

maif_train[,mean_val:=mean(prix_ref),by='val']

means <- unique(maif_train[,.(val,mean_val)])
means
plot(means$val,means$mean_val)



# Prix par Valeur : 
# ::::::::::::::::::::::::::::::::
plot(maif_train$val, maif_train$prix_ref)

# il semble int?ressant de faire 4 clusters pour regarder les valeurs par groupe 
# creation des clusters
maif_train[,km_group:=cut(kmage_annuel, 
                          breaks=c(-1,5000,9500,16500,30000),
                          labels=c("km1","km2","km3","km4"))]
maif_train[,val:=km_group]

# prix moyens par classes 
# ::::::::::::::::::::::::::::::::
round(100*table(maif_train$val)/300000,2)
maif_train[,mean_val:=mean(prix_ref),by='val']

means <- unique(maif_train[,.(val,mean_val)])
means
plot(means$val,means$mean_val)


# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(as.character(val),prix_ref))
g + geom_boxplot(aes(color=as.factor(val)))

# Il semble pertinent de faire des classes. 
# Comme on a vu que certaines  valeurs de test d?passent un peu des valeurs de train , 
# on peut ainsi d?finir ? quel niveau de la variable quanti l'arbre coupe en le d?finissant nous-m?me











##########################################################
####                                                  ####
###                       anc_veh                      ###
####                                                  ####
##########################################################



# type quantitative
maif_train[,val:=anc_veh]
maif_test [,val:=anc_veh]

# Valeurs TRAIN / TEST identiques ???     
# ::::::::::::::::::::::::::::::::::::::::::::::::
summary(maif_train$val)
summary(maif_test $val)
plot(density(maif_train$val), col='blue')
lines(density(maif_test$val), col='red' )

# minimum et maximum tr?s proches, distribution quasi identique 


# Prix par Valeur : 
# ::::::::::::::::::::::::::::::::
plot(maif_train$val, maif_train$prix_ref)
maif_train[,mean_val:=mean(prix_ref), by='val']
d <- unique(maif_train[,.(val,mean_val)])
setkey(d,val)
lines(d$val,d$mean_va, col='red')

plot(d$val,d$mean_val)

# il semble int?ressant de faire 4 clusters pour regarder les valeurs par groupe 
# clusters_basés sur la distribution des valeurs :
maif_train[,anc_veh_cat:=cut(anc_veh, 
                             breaks=c(-1,15,31,70,110),
                             labels=c("age1","age2","age3","age4"))]

#  cluster basés sur les moyennes : 
maif_train[,anc_veh_cat_2:=cut(anc_veh, 
                               breaks=c(-1,3,7,15,60,120),
                               labels=c("age2_1","age2_2","age2_3","age2_4","age2_5"))]

# choix de la variable à regarder
maif_train[,val:=anc_veh_cat]
maif_train[,val:=anc_veh_cat_2]


# prix moyens par classes 
# ::::::::::::::::::::::::::::::::
round(100*table(maif_train$val)/300000,2)
maif_train[,mean_val:=mean(prix_ref),by='val']

means <- unique(maif_train[,.(val,mean_val)])
means
plot(means$val,means$mean_val)


# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(as.character(val),prix_ref))
g + geom_boxplot(aes(color=as.factor(val)))

# Il semble pertinent de faire des classes. 
# Comme on a vu que certaines  valeurs de test d?passent un peu des valeurs de train , 
# on peut ainsi d?finir ? quel niveau de la variable quanti l'arbre coupe en le d?finissant nous-m?me











##########################################################
####                                                  ####
###                       anc_veh                      ###
####                                                  ####
##########################################################



# type quantitative
maif_train[,val:=anc_veh]
maif_test [,val:=anc_veh]

# Valeurs TRAIN / TEST identiques ???     
# ::::::::::::::::::::::::::::::::::::::::::::::::
summary(maif_train$val)
summary(maif_test $val)
plot(density(maif_train$val), col='blue')
lines(density(maif_test$val), col='red' )

# minimum et maximum tr?s proches, distribution quasi identique 


# Prix par Valeur : 
# ::::::::::::::::::::::::::::::::
plot(maif_train$val, maif_train$prix_ref)
maif_train[,mean_val:=mean(prix_ref), by='val']
d <- unique(maif_train[,.(val,mean_val)])
setkey(d,val)
lines(d$val,d$mean_va, col='red')

# il semble int?ressant de faire 4 clusters pour regarder les valeurs par groupe 
# creation des clusters
maif_train[,km_group:=cut(anc_veh, 
                          breaks=c(-1,15,31,70,110),
                          labels=c("age1","age2","age3","age4"))]
maif_train[,val:=km_group]

# prix moyens par classes 
# ::::::::::::::::::::::::::::::::
round(100*table(maif_train$val)/300000,2)
maif_train[,mean_val:=mean(prix_ref),by='val']

means <- unique(maif_train[,.(val,mean_val)])
means
plot(means$val,means$mean_val)


# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(as.character(val),prix_ref))
g + geom_boxplot(aes(color=as.factor(val)))

# Il semble pertinent de faire des classes. 
# Comme on a vu que certaines  valeurs de test d?passent un peu des valeurs de train , 
# on peut ainsi d?finir ? quel niveau de la variable quanti l'arbre coupe en le d?finissant nous-m?me










##########################################################
####                                                  ####
###                     var3--var5                     ###
####                                                  ####
##########################################################




plot(maif_train$var3, maif_train$var5)
table(maif_train$var3, maif_train$var5)




##########################################################
####                                                  ####
###                         var7                       ###
####                                                  ####
##########################################################





nrow(maif_train[var7=="NR"])

maif_2 <- maif_train[var7!="NR"]

cols <- c("var7")
maif_2[,(cols):=lapply(.SD, as.numeric),.SDcols=cols]

plot(density(maif_2$var7))
maif_2[,mean_var7:=mean(prix_ref), by="var7"]

m <- unique(maif_2[,.(var7,mean_var7)])
setkey(m,var7)

plot(m$var7,m$mean_var7)




##########################################################
####                                                  ####
###                         var10                       ###
####                                                  ####
##########################################################

maif_train$var11

plot(density(maif_train$var18),col='red')
lines(density(maif_test$var18),col='blue')


plot(maif_train$var9,maif_train$var12)
abline(0,10,col='red')


100*table(maif_train$var14)/nrow(maif_train)

round(100*table(maif_train$var17)/nrow(maif_train),2)



maif_train[,var:=var17]
maif_train[,mean_var:=mean(prix_ref), by="var"]
m <- unique(maif_train[,.(var,mean_var)])
setkey(m,var)

plot(m$var,m$mean_var)




maif_train[,age:=2016-annee_naissance]
summary(maif_train$age)
summary(maif_train$var18)



maif_train[,age_permis := annee_permis-annee_naissance, by=.(annee_naissance, annee_permis)]
summary(maif_train$age_permis)

plot(density(maif_train$var18),col='red')
lines(density(na.omit(maif_train$age)),col='blue')
lines(density(na.omit(maif_train$age_permis)),col='green')
