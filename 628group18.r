rm(list=ls())
library(rjson)
library(jsonlite)
library(tidytext)
library(dplyr)
library(stringr)
library(plyr)

user <- jsonlite::stream_in(file("user_city.json"))
tip <- jsonlite::stream_in(file("tip_city.json"))
review <- jsonlite::stream_in(file("review_city.json"))
business <- jsonlite::stream_in(file("business_city.json"))

Chinese_food <- grep(pattern = c('Chinese'),business$categories[business$is_open==1])
#Chinese resteraunt still open
Chinese_food_business_ID <-business$business_id[Chinese_food]
#get business ID
Chinese_food_review <- subset(review,review$business_id%in%Chinese_food_business_ID)
#get related review
Chinese_food_tip <- subset(tip,tip$business_id%in%Chinese_food_business_ID)
#get related tips

text<-Chinese_food_review$text
text<-gsub(pattern = c('[0-9[:punct:]]+?'),replacement ="",text)
#remove number and punctuations
id<-Chinese_food_review$review_id
review_and_id <- data.frame(line = id, text = text)


k<-unnest_tokens(review_and_id,word,text)
frequency_of_words<-count(k$word)


all_word <- frequency_of_words$x[frequency_of_words$freq>=10]
#remove the words whose frequency < 10
all_word <-all_word[all_word%in%stop_words$word==FALSE]
#remove stopwords
k<-subset(k,k$word%in%all_word)
#remove the words whose frequency < 10

p<-length(all_word)
n<-length(text)

stat<-Sys.time()
row_name<-list()
for (i in 1:n){
  
  index <-which(all_word%in%k$word[(k$line==id[i])])
  vector<-rep(0,p)
  vector[index] <- count(k$word[(k$line==id[i])])$freq
  row_name[[i]]<-vector
}
end<-Sys.time()
end-stat


stars=c()
stars$stars=Chinese_food_review$stars
stars$uniqueID=Chinese_food_review$review_id
matrix=matrix(data=0,nrow=n,ncol=p)
for(i in 1:n){
  index <-which(all_word%in%k$word[(k$line==id[i])])
  matrix[i,index] <- count(k$word[(k$line==id[i])])$freq
}
dimnames(matrix)=list(review_and_id$line,all_word)

plotWordStar <- function(stars,DTM,wordList,mfrow = c(4,4)) {
  par(mfrow = mfrow)
  col_DTM = colnames(DTM)
  for(i in 1:length(wordList)) {
    index = which(col_DTM == wordList[i])
    if(length(index) == 0) {
      warning(paste(wordList[i],"not detected"))
      next
    } 
    dtm_vec = as.numeric(DTM[,index])
    namescol = rownames(DTM)[dtm_vec>0]
    starsY = rep(0,5)
    for(j in 1:5) {
      element = sum(namescol%in%(stars$uniqueID[which(stars$stars == j)]))
      starsY[j]  = element / sum(stars$stars == j)
    }
    barplot(starsY,main=wordList[i],xlab="Stars",ylab="Word Freq")
  }  
}
wordList=c("bad","worst","awful","horrible","mediocre","average","moderate","ordinary","decent","warm","nice","affordable","fantastic","excellent","wonderful","terrific")
plotWordStar(stars,matrix,wordList)
