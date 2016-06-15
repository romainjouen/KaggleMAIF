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

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

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
  file <- paste(getwd(), "/Data_synthese.xlsx", sep="")
  
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


Instal_Required("stringr")
Instal_Required("xlsx")

Instal_Required("devtools")
Instal_Required("r2excel")

#install.packages("devtools")
#devtools::install_github("kassambara/r2excel")
#library(r2excel)

