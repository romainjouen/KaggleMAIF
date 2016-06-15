library(data.table)
library(ade4)
library(tree)

setwd(dir = "C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/")

maif <- fread("C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/ech_apprentissage.csv",
              header=T)
test <- fread()

maif_test <- fread("C:/Users/felix.rougier/Documents/Challenge/DataScienceNet/maif/ech_test.csv",
                   header=T)

data


mod <- lm(prime_tot_ttc~., data=maif)



acp <- dudi.pca


# % de remplissage
100-maif[,lapply(.SD, function(x) round(100*sum(is.na(x))/nrow(maif),2) )
         ,.SDcols=colnames(maif)]

# type des colonnes 
maif[,lapply(.SD, function(x) {class(x)}, 2), .SDcols=colnames(maif)]
apply(maif,2,class)
apply(maif_test,2,class)

# chnagement de type
maif_test[,("codepostal"):=lapply(.SD, as.character),.SDcols="codepostal"]

# remplacement des 0 
for (j in names(tab_fam)[-1]){
  set(tab_fam, which( is.na( tab_fam[[j]] ) ), j, 0)
}
str(tab_fam)



# model fit
arbre <- tree(prime_tot_ttc~., data=maif)
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

