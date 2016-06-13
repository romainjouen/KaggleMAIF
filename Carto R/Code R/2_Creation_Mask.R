###########################
## 2. Création du masque ##
###########################

# Dans cette partie, nous allons créer le fond de carte.
# L'utilisateur n'a rien à modifier dans cette partie.


if (!require("leaflet")) install.packages("leaflet")
library(leaflet)

# Fonction qui permet la création de la carte associée aux données
Creation.Carte <- function(Table, Latitude, Longitude, Objets, Chiffres, Categories, Lien_HTML = "", Localisateur="pointeur", rad = 2)
{

  if (Localisateur == "pointeur") { 
    # Création d'icones personnalisés
    leafIcons <- icons(
      iconUrl = ifelse(Chiffres < quantile(Chiffres,0.3),
                       "http://leafletjs.com/docs/images/leaf-green.png",
                       ifelse(Chiffres < quantile(Chiffres,0.6),
                              "http://leafletjs.com/docs/images/leaf-orange.png",
                              "http://leafletjs.com/docs/images/leaf-red.png")),
      iconWidth = 38, iconHeight = 95,
      iconAnchorX = 22, iconAnchorY = 94,
      shadowUrl = "http://leafletjs.com/docs/images/leaf-shadow.png",
      shadowWidth = 50, shadowHeight = 64,
      shadowAnchorX = 4, shadowAnchorY = 62
    )
  }
  
  
  # Création de la map et des modules annexes
  mymap = leaflet(data = Table) %>% addTiles() %>% addProviderTiles("Stamen.Toner") 
if (Localisateur == "pointeur") { 
  mymap = mymap %>%
    addMarkers(~Longitude, ~Latitude, 
               popup=paste(sep = "<br/>",
                           paste(sep="","<b><a href=",Lien_HTML,">",Objets,"</a></b>"),
                           Chiffres),
               group = Categories,
               icon = leafIcons) 
  }
if (Localisateur == "cercle") { 
  mymap = mymap %>% 
    addCircles(~Longitude, ~Latitude, weight = 1,
               radius = ~sqrt(Chiffres) * rad, # A modifier selon l'echelle des données
               color = ifelse(Chiffres < quantile(Chiffres,0.3),
                              "green",
                              ifelse(Chiffres < quantile(Chiffres,0.6),
                                     "orange",
                                     "red")),
               group = Categories,
               popup=paste(sep = "<br/>",
                           paste(sep="","<b><a href=",Lien_HTML,">",Objets,"</a></b>"),
                           Chiffres)) 
  }
mymap = mymap %>%
    addLayersControl(
      overlayGroups = Categories,
      options = layersControlOptions(collapsed = FALSE),
      position = "bottomright"
    )  
    
 return(mymap) 
}

