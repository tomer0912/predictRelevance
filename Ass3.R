source("calculateRating.R")

test.data <- read.csv('test.csv', header=TRUE,colClasses = c("numeric","character","character","character"))

len <- length(test.data[,1])

calculatedRating <- data.frame(id = 1:len, 
                          prediction = 1:len, stringsAsFactors = FALSE)

for(i in 1:len){
  calculatedRating[i,"id"] <- test.data$id[i]
  calculatedRating[i,"prediction"] <- calculateRating(test.data[i,])
}

write.csv(calculatedRating,file = 'sampleSub.csv', row.names = FALSE)