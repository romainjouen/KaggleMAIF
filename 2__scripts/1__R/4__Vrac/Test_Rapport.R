library( ReporteRs )


  doc <- pptx(template="Template.pptx" )
  layouts = slide.layouts(doc)
  layouts
  # loop over layout names to plot each slide style
  for(i in layouts ){
    slide.layouts(doc, i )
    title(sub = i )
    #if( interactive() ) readline(prompt = "show next slide layout")
  } 
  
  
  plot(prop.table(table(maif_train$var6)), type = 'p', col='red')
  lines(prop.table(table(maif_test$var6)), type = 'p', col='blue')
  
  
  # Diapo 1 : Diapositive de titre
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Titre")
  doc <- addTitle(doc,"Rapport automatisé RSTUDIO")
  doc <- addParagraph(doc, "Réalisé par Lejaille Thibault")
  
  # Diapo 2 : Ajouter de la synthèse
  #+++++++++++++++++++++++ 
  doc <- addSlide(doc, "Sous rubriques")
  doc <- addTitle(doc,"Synthèse")
  doc <- addSlide(doc, "Titre seul")
  MyFTable <- vanilla.table(a)
  MyFTable <- setZebraStyle(MyFTable, odd = '#eeeeee', even = 'white')
  MyFTable = setFlexTableBackgroundColors( MyFTable, i = 1,colors = "gray", to = "header" )
  
  doc = addFlexTable(doc,MyFTable)

  # Diapo 3 : Ajouter les plots
  #+++++++++++++++++++++++
  doc <- addSlide(doc, "Sous rubriques")
  doc <- addTitle(doc,"Analyses descriptives")
    
  for(i in 1:3){

  doc <- addSlide(doc, "Plots")
  doc <- addTitle(doc,paste("Var",i))
  #par(mar = rep(2, 4))
  plotFunc<- function(){
    barplot(VADeaths, beside = TRUE,
            col = c("lightblue", "mistyrose", "lightcyan",
                    "lavender", "cornsilk"),
            legend = rownames(VADeaths), ylim = c(0, 100))
    title(main = "Death Rates in Virginia", font.main = 4)
  }
  test = function(){Out$marque$plotly}
  doc <- addPlot(doc, test )
  t = function(){plot(iris$Petal.Length, iris$Petal.Width, main="Edgar Anderson's Iris Data")}
  doc <- addPlot(doc, t)
  doc <- addParagraph(doc, "Commentaires :")
  }
  
  # Enregistrer le document 
  writeDoc(doc, "r-reporters-powerpoint-from-template.pptx" )
  