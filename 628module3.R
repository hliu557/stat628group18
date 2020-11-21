library(tidytext)
library(dplyr)
library(stringr)
library(plyr)
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
review_id <- data.frame(line = id, text = text)

k<-unnest_tokens(review_id,word, text)
word <- count(k$word)$x
p<-length(word)
n<-length(text)

reviw_to_word<-matrix(ncol = p,nrow = n)
stat<-Sys.time()
for (i in 1:length(text)){

  index <-which(word%in%k$word[(k$line==id[i])])
  reviw_to_word[i,index] <- count(k$word[(k$line==id[i])])$freq
}
end<-Sys.time()
end-stat
