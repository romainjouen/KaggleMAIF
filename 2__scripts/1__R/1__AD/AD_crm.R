source("2__scripts/1__R/3__Release/RTools.R")

Instal_Required("data.table")
Instal_Required("ade4")
Instal_Required("tree")
Instal_Required("lda")
Instal_Required("ggplot2")
Instal_Required("randomForest")

#setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif_train <- fread("1__data/1__input/Brut_Train.csv", header=T)
maif_test <- fread("1__data/1__input/Brut_Test.csv", header=T)



# analyse de la donn?es crm 
# peut-on l'utiliser pour obtenir facilement le prix d'achat de base : 


# distribution de CRM:
d <- density(maif_train$crm)
plot(d, col='red')
lines(density(maif_test$crm), col='blue')


hist(maif_train$crm, freq=F, ylim=c(0,0.09), main="R?partition de CRM")
lines(density(maif_train$crm), col='red')

t <- round(100*table(maif_train$crm)/nrow(maif_train),2)
t
summary(maif_train$crm)
# va jusqu'? 270 mais cas tr?s particuliers :
# moins de 0.01% de valeurs au dessus de 195


# comparaison des prix bruts et des prix finaux

# prime totale
d <- maif_train[,.(crm,prime_tot_ttc)]
d
plot(d$crm,d$prime_tot_ttc)

# prime brute avant crm
d[,prime_brute:=100*prime_tot_ttc/crm]
plot(d$crm,d$prime_brute)


# calcul des moyennes 
d[,mean_prime_tot:=mean(prime_tot_ttc), by='crm']
d[,mean_prime_brute:=mean(prime_brute), by='crm']


# repr?sentation graphique 
d2 <- unique(d[,.(crm,mean_prime_tot,mean_prime_brute)])
setkey(d2,crm)
plot(d2$crm, d2$mean_prime_tot , type='b', col='blue', ylim=c(0,1300))
points(d2$crm, d2$mean_prime_brute , type='b', col='red')

# test
d2[,prime_mean:=crm*mean_prime_tot/100]
d2
points(d2$crm,d2$prime_mean, type='b', col='green')



# peut-?tre que le facteur d'application du crm n'est pas exactement prix*crm/100

# on va faire une r?gression lin?aire pour d?terminer le rapport entre le crm e le prix 

# sur les moyennes des prix (non pond?r?)
mod1 <- lm(d2$mean_prime_tot~d2$crm)
summary(mod1)

# sur l'ensemble des prix 
mod2 <- lm(maif_train$prime_tot_ttc~maif_train$crm)
summary(mod2)

# 
mod3 <- lm( rep(m,300000) ~ maif_train$prime_tot_ttc * maif_train$crm -1 )
summary(mod3)


# ne fontionne pas 




# on va consid?rer que le crm s'applique bien comme un bonus malus et qu'on retrouve le prix brut de : 
# Prix de Base =   prime_totale_ttc * 100 / CRM 





