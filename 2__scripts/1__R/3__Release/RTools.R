#############################################
#############################################
#####                                ########
#####        INITIALISATION          ########
#####                                ########
#############################################
#############################################


#Creation d'une fonction permettant d'installer les packages plus facilement
#Etapes:
#- Verification si le package est deja installe
#   - Si oui, library simule puis OK
#   - Si non, installe puis simule library
#Interet : permet d'avoir une commande pour installer et lancer package sans problème d'erreur

Instal_Required <- function(package){
  if(eval(parse(text=paste("require(",package,")")))) 
  {
    return("Chargement OK !")
  }
  else{
    install.packages(package)
    eval(parse(text=paste("require(",package,")")))
    return("Chargement OK !")
  }
}

#############################################
#############################################
#####                                ########
#####            PACKAGES            ########
#####                                ########
#############################################
#############################################

#Installation des packages
Instal_Required("stringr")
Instal_Required("xlsx")

Instal_Required("devtools")
Instal_Required("r2excel")

#Si devtools/r2excel ne marche pas, passer par ça :
#install.packages("devtools")
#devtools::install_github("kassambara/r2excel")
#library(r2excel)

Instal_Required("plotly")

Instal_Required("webshot")
#webshot::install_phantomjs()
Instal_Required("htmlwidgets")

Instal_Required("ReporteRs")



#############################################
#############################################
#####                                ########
#####        NUAGE de MOTS           ########
#####                                ########
#############################################
#############################################

#OUTDATE !!
Nuage_de_mots <- function(variable){
  
  req <- paste("tmp_data <- data.frame(text = Data$", variable, ', stringsAsFactors=FALSE)', sep="")
  eval(parse(text=req))
  
  req <- paste("review_text <- paste(Data$", variable, ', collapse=" ")', sep="")
  eval(parse(text=req))
  
  export <- paste("****",paste(Data$Texte, collapse=" **** "))
  review_source <- VectorSource(review_text)
  corpus <- Corpus(review_source)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("fr"))
  #corpus <- tm_map(corpus, stemDocument)
  dtm <- DocumentTermMatrix(corpus)
  dtm2 <- as.matrix(dtm)
  tdm <- TermDocumentMatrix(Corpus(DataframeSource(tmp_data)))
  frequency <- colSums(dtm2)
  
  
  frequency <- sort(frequency, decreasing=TRUE)
  frequency_pourc <- frequency/length(frequency)
  wf=data.frame(term=names(frequency_pourc),occurrences=frequency_pourc)
  top <-  head(frequency)
  words <- names(frequency)
  
  p <- ggplot(subset(wf, frequency_pourc>0.05), aes(term, occurrences))+ geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=45, hjust=1))
  set.seed(50)
  wordcloud(words[1:50], frequency[1:50],colors=brewer.pal(5,"Dark2"))
  
  png(paste("NGM_",variable,".png",sep = ""), width=350,height=350)
  wordcloud(words[1:50], frequency[1:50], scale=c(5,0.1), ,min.freq=0,random.order=F, rot.per=0, use.r.layout=T, colors=brewer.pal(8, "Dark2"))
  dev.off()
  
  liste_return <- list("frequency" = frequency, "top" = top, "words" = words, "dtm" = dtm, "dtm2" = dtm2, "corpus" = corpus, "tdm" = tdm, "wf" = wf, "plot" = p, "frequency_pourc" = frequency_pourc)
  
  return(liste_return)
}

#############################################
#############################################
#####                                ########
#####           PERCENT              ########
#####                                ########
#############################################
#############################################

#Creation d'une fonction permettant de transformer un numeric en % avec le symbole
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#############################################
#############################################
#####                                ########
#####           Synthèse             ########
#####                                ########
#############################################
#############################################

#Creation d'une fonction permettant de synthetiser un jeu de donne
#Etapes:
#- Pour chaque varaible :
#   - donne le format
#   - count le nombre de valeurs
#   - count le nombre de valeurs manquantes
#   - count le pourcentage de valeurs manquantes
#   - count le nombre de valeurs/modalités différentes
#   - donne un exemple(5) de valeurs/modalités différentes
#Interet : A lancer sur tout jeu de donnee

Synt_Df <- function(dataframe) {
  options(scipen=999)
  m <- sapply(dataframe, function(x) {
    data.frame(
      n=length(x),
      nmiss=sum(is.na(x)), 
      propmiss=sum(is.na(x))/length(x),
      valdiff=length(unique(x))
    )
  })
  d <- data.frame(t(m))
  d <- sapply(d, unlist)
  d <- as.data.frame(d)
  d$variable <- row.names(d)
  row.names(d) <- NULL
  d <- cbind(d[ncol(d)],d[-ncol(d)])
  d$type = sapply(dataframe, class)
  d$propmiss = percent(d$propmiss)
  d$valex = lapply(dataframe, function(x){str_c(head(unique(x[!is.na(x)])), collapse = "/")})
  d = d[c("variable","type","n","nmiss","propmiss","valdiff","valex")]
  return(d)
}

#############################################
#############################################
#####                                ########
#####     COMPARAISON Synthèse       ########
#####                                ########
#############################################
#############################################

#Creation d'une fonction permettant de comparer deux jeu de donnee (Train/Test)
#Etapes:
#- Pour chaque jeu :
#   - utilise la fonction précedente (synthèse)
#- Pour le jeu Test, compare les donnee avec le jeu train afin de detecter
# les nouvelles valeurs:
#   - nombre de nouvelles valeurs
#   - exemple (5) de nouvelles valeurs
# Et compte le nombre de type different
# Interet : Export xsl !! => A realiser sur tout jeu de donnee

Comp_train_test = function(train,test){
  
  synth_train = Synt_Df(train)
  synth_test = Synt_Df(test)
  df = data.frame(var=character(), nb_new = character(), value_new = character(), stringsAsFactors = FALSE)
  i=0
  Colnames <- names(df)
  for (item in colnames(test)){
    i = i+1
    eval(parse(text = paste("df[",i,",] = c('",item,"',length(match(test$",
                            item,",train$",item,", nomatch = 0)[match(test$",
                            item,",train$",item,", nomatch = 0)==0]),if(length(match(test$",
                            item,",train$",item,", nomatch = 0)[match(test$",
                            item,",train$",item,", nomatch = 0)==0])!=0){str_c(head(unique(test[row(as.data.frame(match(test$",
                            item,",train$",item,", nomatch = 0)))[match(test$",
                            item,",train$",item,", nomatch = 0)==0],]$",item,",)), collapse = '/')}else{'O'})", sep = "")))
    
    names(df) <- Colnames
  }
  df = merge(synth_test,df, by.x = "variable", by.y = "var")
  file <- paste(getwd(), "/99__Documents/Data_synthese2.xlsx", sep="")
  
  wb <- createWorkbook(type="xlsx")
  sheet <- createSheet(wb, sheetName = "TRAIN")
  # Ajouter un titre
  xlsx.addHeader(wb, sheet, value="Synthese TRAIN",level=1,color="black")
  xlsx.addLineBreak(sheet, 1)
  # Ajouter une table : data.frame
  xlsx.addTable(wb, sheet, synth_train)
  
  sheet <- createSheet(wb, sheetName = "TEST")
  # Ajouter un titre
  xlsx.addHeader(wb, sheet, value="Synthese TEST",level=1,color="black")
  xlsx.addLineBreak(sheet, 1)
  # Ajouter une table : data.frame
  xlsx.addTable(wb, sheet, df)
  
  sheet <- createSheet(wb, sheetName = "TYPE")
  # Ajouter un titre
  xlsx.addHeader(wb, sheet, value="Synthese TYPE",level=1,color="black")
  xlsx.addLineBreak(sheet, 1)
  # Ajouter une table : data.frame
  df_type = as.data.frame(table(sapply(train, class)))
  xlsx.addTable(wb, sheet, df_type)
  
  saveWorkbook(wb, file)
  return(df)
}

#############################################
#############################################
#####                                ########
#####      Analyse descriptive       ########
#####                                ########
#############################################
#############################################

#Creation d'une fonction permettant de generer une analyse descriptive en fonction
# d'une variable target
#Etapes:
# - val train et val test identiques ?
# - effectif val train et effectif val test identiques ?
# - boxplot en fonction de target
# - prix moyen
# - Graphique des moyennes de la var en fonction de la cible
# Interet : realiser en data.table !!


func_AD = function(data_train,
                   data_test,
                   target="none",
                   AD_val="none",
                   limit=1000,
                   export=TRUE){
  
  # type cat?gorique 
  eval(parse(text = paste("data_train[,AD_val:=",AD_val,"]")))
  eval(parse(text = paste("data_test[,AD_val:=",AD_val,"]")))
  
  # AD_valeurs TRAIN / TEST identiques ???    yes 
  # ::::::::::::::::::::::::::::::::::::::::::::::::
  v_train <- unique(data_train[,.(AD_val)])
  v_test  <- unique(data_test[,.(AD_val)])
  setkey(v_train,AD_val)
  setkey(v_test,AD_val)
  Test_Identiq = identical(v_test,v_train)
  
  # effectifs TRAIN/TEST identiques ??? 
  # ::::::::::::::::::::::::::::::::
  #par(mfrow=c(2,1))
  data_train[,count_AD_val:=round(100*.N/nrow(data_train),2),by='AD_val']
  data_test[,count_AD_val:=round(100*.N/nrow(maif_test) ,2),by='AD_val']
  tr <- unique(data_train[,.(AD_val,count_AD_val)])
  te <- unique(data_test[,.(AD_val,count_AD_val)])
  setkey(tr,AD_val)
  setkey(te,AD_val)
  prop <- merge(tr,te,all=T, by='AD_val')
  prop[,rapp:=count_AD_val.y/count_AD_val.x]
  
  # train et test semblent avoir des ?chantillons ? peu pr?s identiques
  
  
  # Prix par Classe : 
  # ::::::::::::::::::::::::::::::::
  eval(parse(text = paste("g = ggplot(data_train,aes(",AD_val,",",target,"))")))
  eval(parse(text = paste("g = g + geom_boxplot(aes(color=",AD_val,"))")))
  
  
  # prix moyens par classes 
  # ::::::::::::::::::::::::::::::::
  #round(100*table(data_train$AD_val)/nrow(data_train),2)
  eval(parse(text = paste("prof = data_train[, list(sum_",target," = length(",target,"), mean_",target," = mean(",target,")), by = '",AD_val,"']", sep = "")))
  #Graphique !!
  eval(parse(text=paste(
    "p = plot_ly(data = prof[sum_",target,">limit]
          ,x=",AD_val,"
          ,y=mean_",target,"
          ,mode = 'markers'
          ,marker = list(size = 20)
          ,name='Mean') %>%
  layout(title = 'Moyenne des ",target," en fonction de ",AD_val," (limit = ",limit,")')", sep = "")))
  if(export){
    saveWidget(as.widget(p), "temp.html")
    webshot("temp.html", file = paste("99__Documents/",AD_val,"_Graphe_Mean.png", sep = ""), cliprect = "viewport")
    file.remove("temp.html")
    
    if(eval(parse(text=paste("is.integer(data_train$",AD_val,")",sep="")))){
      eval(parse(text=paste("plot(density(na.omit(data_train$",AD_val,")), col='red')",sep = "")))
      eval(parse(text=paste("lines(density(na.omit(data_test$",AD_val,")), col='blue')",sep = "")))
      d <- recordPlot()
      png(filename=paste("99__Documents/",AD_val,"_Graphe_Density.png", sep = ""))
      print(d)
      dev.off()
      #rm(d)
    }else{
      eval(parse(text=paste("plot(prop.table(table(data_train$",AD_val,")), type = 'p', col='red')",sep = "")))
      eval(parse(text=paste("lines(prop.table(table(data_test$",AD_val,")), type = 'p', col='blue')",sep = "")))
      d <- recordPlot()
      png(filename=paste("99__Documents/",AD_val,"_Graphe_Density.png", sep = ""))
      print(d)
      dev.off()
      #rm(d)
    }
  }
  Out <- list("Test_Identiq" = Test_Identiq, "prop" = prop, "boxplot" = g, "prof" = prof,"plotly"=p, "density"=d)
  return(Out)
}

Synthese = function(data_train,
                    data_test,
                    target="none",
                    AD_val="none",
                    limit=1000,
                    export=TRUE,
                    rapport=TRUE,
                    without=c())
{
  
  print("*******  Fonction synthese START *****")
  
  synth_train = Synt_Df(data_train)
  print("* synthese TRAIN => OK")
  synth_test = Synt_Df(data_test)
  
  df = data.frame(var=character(), nb_new = character(), value_new = character(), stringsAsFactors = FALSE)
  i=0
  Colnames <- names(df)
  for (item in colnames(data_test)){
    i = i+1
    eval(parse(text = paste("df[",i,",] = c('",item,"',length(match(data_test$",
                            item,",data_train$",item,", nomatch = 0)[match(data_test$",
                            item,",data_train$",item,", nomatch = 0)==0]),if(length(match(data_test$",
                            item,",data_train$",item,", nomatch = 0)[match(data_test$",
                            item,",data_train$",item,", nomatch = 0)==0])!=0){str_c(head(unique(data_test[row(as.data.frame(match(data_test$",
                            item,",data_train$",item,", nomatch = 0)))[match(data_test$",
                            item,",data_train$",item,", nomatch = 0)==0],]$",item,",)), collapse = '/')}else{'O'})", sep = "")))
    
    names(df) <- Colnames
  }
  synth_test = merge(synth_test,df, by.x = "variable", by.y = "var")
  print("* synthese TEST => OK")
  synth_type = as.data.frame(table(sapply(data_train, class)))
  print("* synthese TYPE => OK")
  
  Out <- list("synthese_train"=synth_train,
              "synthese_test"=synth_test,
              "synthese_type"=synth_type)
  
  
  if(export){
    file <- paste(getwd(), "/99__Documents/Data_synthese.xlsx", sep="")
    
    wb <- createWorkbook(type="xlsx")
    sheet <- createSheet(wb, sheetName = "TRAIN")
    # Ajouter un titre
    xlsx.addHeader(wb, sheet, value="Synthese TRAIN",level=1,color="black")
    xlsx.addLineBreak(sheet, 1)
    # Ajouter une table : data.frame
    xlsx.addTable(wb, sheet, synth_train)
    
    sheet <- createSheet(wb, sheetName = "TEST")
    # Ajouter un titre
    xlsx.addHeader(wb, sheet, value="Synthese TEST",level=1,color="black")
    xlsx.addLineBreak(sheet, 1)
    # Ajouter une table : data.frame
    xlsx.addTable(wb, sheet, synth_test)
    
    sheet <- createSheet(wb, sheetName = "TYPE")
    # Ajouter un titre
    xlsx.addHeader(wb, sheet, value="Synthese TYPE",level=1,color="black")
    xlsx.addLineBreak(sheet, 1)
    # Ajouter une table : data.frame
    
    xlsx.addTable(wb, sheet, synth_type)
    
    saveWorkbook(wb, file)
    print("* Export => OK")
  }
  
  if(rapport){
    # Chargement du template
    doc <- pptx(template="99__Documents/Template.pptx" )
    
    
    # Diapo 1 : Diapositive de titre
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Titre")
    doc <- addTitle(doc,"Rapport automatisé RSTUDIO")
    doc <- addParagraph(doc, "Réalisé par Lejaille Thibault")
    
    # Diapo 2 : Ajoute sous rubriques
    #+++++++++++++++++++++++ 
    doc <- addSlide(doc, "Sous rubriques")
    doc <- addTitle(doc,"Synthèse")
    
    # Diapo 3 : Ajoute table synthèse TRAIN
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Titre+contenu")
    doc <- addTitle(doc,"Synthèse TRAIN")
    MyFTable <- vanilla.table(synth_train)
    MyFTable <- setZebraStyle(MyFTable, odd = '#eeeeee', even = 'white')
    MyFTable = setFlexTableBackgroundColors( MyFTable, i = 1,colors = "gray", to = "header" )
    doc = addFlexTable(doc,MyFTable)
    
    # Diapo 4 : Ajoute table synthèse TEST
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Titre+contenu")
    doc <- addTitle(doc,"Synthèse TRAIN")
    MyFTable <- vanilla.table(synth_test)
    MyFTable <- setZebraStyle(MyFTable, odd = '#eeeeee', even = 'white')
    MyFTable = setFlexTableBackgroundColors( MyFTable, i = 1,colors = "gray", to = "header" )
    doc = addFlexTable(doc,MyFTable)
    
    # Diapo 5 : Ajoute table synthèse TYPE
    #+++++++++++++++++++++++
    doc <- addSlide(doc, "Titre+contenu")
    doc <- addTitle(doc,"Synthèse TRAIN")
    MyFTable <- vanilla.table(synth_type)
    MyFTable <- setZebraStyle(MyFTable, odd = '#eeeeee', even = 'white')
    MyFTable = setFlexTableBackgroundColors( MyFTable, i = 1,colors = "gray", to = "header" )
    doc = addFlexTable(doc,MyFTable)
    
    # Enregistrer le document 
    writeDoc(doc, "99__Documents/Rapport.pptx" )
    print("* Rapport synthese=> OK")
  }
  
  for(var in colnames(data_train)){
    if(is.element(var, without)==FALSE){
      AD = list(func_AD(data_train,data_test,target,var,limit))
      assign(paste(var),AD)
      eval(parse(text=paste("Out = c(Out,'",var,"'=",var,")", sep = )))
      if(rapport){
        doc <- addSlide(doc, "Plots")
        doc <- addTitle(doc,paste("Variable :",var))
        doc <- addImage(doc,paste("99__Documents/",var,"_Graphe_Density.png", sep = ""))
        doc <- addImage(doc,paste("99__Documents/",var,"_Graphe_Mean.png", sep = ""))
        doc <- addParagraph(doc, "Commentaires :")
        writeDoc(doc, "Rapport.pptx" )
        print(paste("* synthese & Rapport",var,"=> OK"))
      }else{
        print(paste("* synthese",var,"=> OK"))
      }
    }
  }
  
  
  
  # ,
  #             "Test_Identiq" = Test_Identiq,
  #             "prop" = prop,
  #             "boxplot" = g,
  #             "prof" = prof,
  #             "plotly"=p,
  #             "density"=d)
  
  print("*******  Fonction synthese END  *****")
  
  return(Out)
}
