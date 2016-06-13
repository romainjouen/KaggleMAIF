##############################
## 3. Lancement de la carte ##
##############################

# Dans cette partie, nous allons créer la carte finale
# L'utilisateur doit changer les paramètres de la fonction

# Objets : Individus à placer sur la carte
# Chiffres : Valeurs affichées sur la carte (taille, couleurs des cercles)
# Categories : Filtres qualitatifs de la carte
# Localisateur : Type de localisateur - "pointeur" ou "cercle"
# Rad : Echelle globale des cercles (par défaut à 2, à modifier si besoin)
mymap = Creation.Carte(Table = donnees_geo,
                       Latitude = donnees_geo$Lat,
                       Longitude = donnees_geo$Lng,
                       Objets = donnees_geo$STATION,
                       Chiffres = donnees_geo$CA_TOT, 
                       Categories = donnees_geo$TYPE, 
                       Localisateur = "cercle",
                       rad = 2)
mymap