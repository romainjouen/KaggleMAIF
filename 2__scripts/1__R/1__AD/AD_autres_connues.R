
library(data.table)
library(ade4)
library(tree)
library(lda)
library(ggplot2)
library(randomForest)
library(boot)

setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("Brut_Train.csv", header=T)
maif_test <- fread("Brut_Test.csv", header=T)




# mise au prix de reference
maif_train[,prix_ref:=prime_tot_ttc*100/crm, by=id]



          ##########################################################
          ####                                                  ####
          ###                    PROFESSION                      ###
          ####                                                  ####
          ##########################################################


# type catégorique 
val <- "profession"
maif_train[,val:=profession]
maif_test [,val:=profession]

# Valeurs TRAIN / TEST identiques ???    yes 
# ::::::::::::::::::::::::::::::::::::::::::::::::
v_train <- unique(maif_train[,.(val)])
v_test  <- unique(maif_test [,.(val)])
setkey(v_train,val)
setkey(v_test,val)
identical(v_test,v_train)

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

# train et test semblent avoir des échantillons à peu près identiques


# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(profession,prix_ref))
g + geom_boxplot(aes(color=profession))


# prix moyens par classes 
# ::::::::::::::::::::::::::::::::
round(100*table(maif_train$profession)/300000,2)
maif_train[,mean_prof:=mean(prix_ref),by='profession']

prof <- unique(maif_train[,.(profession,mean_prof)])
prof




          
          ##########################################################
          ####                                                  ####
          ###                    MARQUE                          ###
          ####                                                  ####
          ##########################################################



# type catégorique
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

# on voit des différences : faire des clusters de marque ? 



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
              


# type catégorique
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

# train et test ont les mêmes valeurs 



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

# effectifs à peu près similaires 



# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(val,prix_ref))
g + geom_boxplot(aes(color=val))

# on voit des différences : faire des clusters de marque ? 



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
              


# type catégorique
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

# effectifs à peu près similaires 
# 


# Prix par Classe : 
# ::::::::::::::::::::::::::::::::
g <- ggplot(maif_train,aes(as.character(val),prix_ref))
g + geom_boxplot(aes(color=as.factor(val)))

# on voit des différences : faire des clusters de marque ? 



# prix moyens par classes 
# ::::::::::::::::::::::::::::::::
round(100*table(maif_train$val)/300000,2)
maif_train[,mean_val:=mean(prix_ref),by='val']

means <- unique(maif_train[,.(val,mean_val)])
means
plot(means$val,means$mean_val)


# le prix semble augmenter avec la puis fiscale pour les puis fiscales < 9 ce qui 
# correspond à la plupart des voitures

# cela a du sens de mettre la variable en quanti




            
            
            
            ##########################################################
            ####                                                  ####
            ###                    kmage_annuel                    ###
            ####                                                  ####
            ##########################################################
            


# type catégorique
maif_train[,val:=kmage_annuel]
maif_test [,val:=kmage_annuel]

# Valeurs TRAIN / TEST identiques ???     
# ::::::::::::::::::::::::::::::::::::::::::::::::
summary(maif_train$val)
summary(maif_test $val)
plot(density(maif_train$val), col='blue', freq='F')
lines(density(maif_test$val), col='red' , freq='F')

# minimum et maximum très proches, distribution quasi identique 


# Prix par Valeur : 
# ::::::::::::::::::::::::::::::::
plot(maif_train$val, maif_train$prix_ref)

# il semble intéressant de faire 4 clusters pour regarder les valeurs par groupe 
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
# Comme on a vu que certaines  valeurs de test dépassent un peu des valeurs de train , 
# on peut ainsi définir à quel niveau de la variable quanti l'arbre coupe en le définissant nous-même









          
          
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

# minimum et maximum très proches, distribution quasi identique 


# Prix par Valeur : 
# ::::::::::::::::::::::::::::::::
plot(maif_train$val, maif_train$prix_ref)
maif_train[,mean_val:=mean(prix_ref), by='val']
d <- unique(maif_train[,.(val,mean_val)])
setkey(d,val)
lines(d$val,d$mean_va, col='red')

# il semble intéressant de faire 4 clusters pour regarder les valeurs par groupe 
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
# Comme on a vu que certaines  valeurs de test dépassent un peu des valeurs de train , 
# on peut ainsi définir à quel niveau de la variable quanti l'arbre coupe en le définissant nous-même











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

# minimum et maximum très proches, distribution quasi identique 


# Prix par Valeur : 
# ::::::::::::::::::::::::::::::::
plot(maif_train$val, maif_train$prix_ref)
maif_train[,mean_val:=mean(prix_ref), by='val']
d <- unique(maif_train[,.(val,mean_val)])
setkey(d,val)
lines(d$val,d$mean_va, col='red')

# il semble intéressant de faire 4 clusters pour regarder les valeurs par groupe 
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
# Comme on a vu que certaines  valeurs de test dépassent un peu des valeurs de train , 
# on peut ainsi définir à quel niveau de la variable quanti l'arbre coupe en le définissant nous-même



