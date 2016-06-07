---
title: "Assignment 4 - Crowdflower Search Results Relevance"   
author: "Tomer Belzer & Tomer Segal"   
date: "June 6, 2016"   
output: html_document
---

#Predict the relevance of search results from eCommerce sites 
##Code documentation:
This code is meant to predict the relevance of search results from eCommerce sites.

The first step was to read the data from the given CSV file

```{r}
source("calculateRating.R")

test.data <- read.csv('test.csv', header=TRUE,colClasses = c("numeric","character","character","character"))

len <- length(test.data[,1])

calculatedRating <- data.frame(id = 1:len, 
                          prediction = 1:len, stringsAsFactors = FALSE)
```

After that we calculated the relevance of each row in the data set.

```{r}
for(i in 1:len){
  calculatedRating[i,"id"] <- test.data$id[i]
  calculatedRating[i,"prediction"] <- calculateRating(test.data[i,])
}
```

We did that by:
Checking if the sequence of the query Is contained in the title of the product. 
If so, it got the highest record of 4.
```{r}
calculateRating <- function(row){
  if(checkQuerySequenceInTitle(row) && queryHasMoreThanOneWord(row)){
    return(4)
  }
  else{
    return(checkTokenPrecent(row))
  }
}
  
#this function checks if the query has more than one word
queryHasMoreThanOneWord <- function(row){
  if(length(grep(" ", toupper(row$query)))>0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

#this function checks if the character sequence from the query can be found in the title
checkQuerySequenceInTitle<-function(row){
  if(length(grep(toupper(row$query),toupper(row$product_title)))>0){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
```
If not We cleaned the query of  Punctuation, numbers and stop words.
```{r}
checkTokenPrecent <- function(row){
  
  queryWords <- removeStopWords(row$query)
  titleWords <- removeStopWords(row$product_title)
  
  queryWords <- stemSentence(queryWords, language = "english")
  titleWords <- stemSentence(titleWords, language = "english")

  return(1 + titleTokenPrecentage(queryWords,titleWords))
}

#this function removes from a given string punctuation, numbers and stop words
#turns each character in the string to lower case letter
removeStopWords <- function(str){
  myCorpus <- Corpus(VectorSource(str))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers) 
  myCorpus <- tm_map(myCorpus, tolower) 
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
  return(myCorpus[[1]])
}
```


Then we Transformed the remaining words in the query to their stem.

```{r}
#this function transforms each word in the given string to its stem
stemSentence <- function(x, language){
  x <- strsplit(x, "[[:blank:]]")[[1]]
  x <- wordStem(x, language)
  return(x)
}
```
Then we calculated the percentage of the remaining words in the title
,and then we gave each row a record between 1 to 4 according to the percentage.

```{r}
#this function calculates the precentage of significant words 
#from the query found in the title 
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
```

Then we copied the results into a CSV file

```{r}
write.csv(calculatedRating,file = 'submission.csv', row.names = FALSE)
```

##Our score:
Our score is 0.483826

![](https://github.com/tomer0912/predictRelevance/blob/master/images/score.PNG)
