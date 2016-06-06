library(SnowballC,tm)
library(tm)
library(rlist)

calculateRating <- function(row){
  if(checkQuerySequenceInTitle(row) && queryHasMoreThanOneWord(row)){
    return(4)
  }
  else{
    return(checkTokenPrecent(row))
  }
}

checkTokenPrecent <- function(row){
  
  queryWords <- removeStopWords(row$query)
  titleWords <- removeStopWords(row$product_title)
  
  queryWords <- stemSentence(queryWords, language = "english")
  titleWords <- stemSentence(titleWords, language = "english")

  return(1 + titleTokenPrecentage(queryWords,titleWords))
}

queryHasMoreThanOneWord <- function(row){
  if(length(grep(" ", toupper(row$query)))>0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

checkQuerySequenceInTitle<-function(row){
  if(length(grep(toupper(row$query),toupper(row$product_title)))>0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

stemSentence <- function(x, language){
  x <- strsplit(x, "[[:blank:]]")[[1]]
  x <- wordStem(x, language)
  return(x)
}

titleTokenPrecentage <- function(query, title){
  inTitle <- 0
  query <- lapply(query, toupper)
  title<-lapply(title, toupper)
  if(length(title)>0){
    for(i in 1:length(query)){
      if(query[i] %in% title){
        inTitle <- inTitle + 1
      }
    }
  }
  precent <- round((inTitle/length(query))*3)
  return(precent)
}

removeStopWords <- function(str){
  
  myCorpus <- Corpus(VectorSource(str))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers) 
  myCorpus <- tm_map(myCorpus, tolower) 
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  return(myCorpus[[1]])
}