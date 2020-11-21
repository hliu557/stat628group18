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

# unnest_tokens
text<-Chinese_food_review$text
text<-gsub(pattern = c('[0-9[:punct:]]+?'),replacement ="",text)
#remove number
id<-Chinese_food_review$review_id
review_and_id <- data.frame(line = id, text = text)

k<-unnest_tokens(review_and_id,word, text)
word <- count(k$word)$x
p<-length(word)
n<-length(text)

reviw_to_word<-matrix(ncol = p,nrow = n)
stat<-Sys.time()
row_name<-rep(0,p)
for (i in 1:n){
  
  index <-which(word%in%k$word[(k$line==id[i])])
  vector<-rep(0,p)
  vector[index] <- count(k$word[(k$line==id[i])])$freq
  row_name<-rbind(row_name,vector)
}
end<-Sys.time()
end-stat
