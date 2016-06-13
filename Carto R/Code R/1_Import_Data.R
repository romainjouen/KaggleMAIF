###########################
## 1. Import des données ##
###########################

# Dans cette partie, nous allons importer les données nécessaires à notre analyse.
# L'utilisateur doit changer le chemin pour pointer sur son fichier de données et potentiellement
# modifier certains paramètres (type de fichier, délimitateur, ...) pour que l'import fonctionne

# Import des données : Exemple avec des données réelles (TOTAL Lux)
donnees_geo = read.csv("//frlyssfs01/Common/Big Data & Analytics/Analytics/02- Ateliers/10- Mission SNCF et carto R/Carto R/Données exemple/STATION_MAP_LUX.csv", header = TRUE, sep=";", dec=".", quote = "\\")
donnees_geo = subset(donnees_geo, Lat!=0 & Lng!=0)
