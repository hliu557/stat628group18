
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
Open <- business[business$is_open==1,]
Chinese_food <- grep(pattern = c('Chinese|chinese'),Open$categories)
#Chinese resteraunt still open
Chinese_food_business_ID <-business$business_id[Chinese_food]
#get business ID
Chinese_food_review <- subset(review,review$business_id%in%Chinese_food_business_ID)
#get related review
Chinese_food_tip <- subset(tip,tip$business_id%in%Chinese_food_business_ID)
#get related tips

Review_text<-Chinese_food_review$text
Review_text<-gsub(pattern = c('[0-9]'),replacement ="",Review_text)
Review_text<- tolower(Review_text)
Review_text<-gsub(pattern = "I'm",replacement ="I am",Review_text)
Review_text<-gsub(pattern = "you're",replacement ="you are",Review_text)
Review_text<-gsub(pattern = "he's",replacement ="he is",Review_text)
Review_text<-gsub(pattern = "she's",replacement ="she is",Review_text)
Review_text<-gsub(pattern = "n't",replacement =" not",Review_text)
Review_text<-gsub(pattern = c('[[:punct:]]'),replacement ="",Review_text)
Review_text<-gsub(pattern = c("not |no |never "),replacement ="non",Review_text)
Review_text<-gsub(pattern = c("'"),replacement ="",Review_text)
#remove number and punctuations


id<-Chinese_food_review$review_id
stars<-Chinese_food_review$stars
user<-Chinese_food_review$user_id
review_and_id <- data.frame(line = id,stars=stars,user=user, text = Review_text)


k<-unnest_tokens(review_and_id,word,text)
frequency_of_words<-count(k$word)


all_word <- frequency_of_words$x[frequency_of_words$freq>=10]
#remove the words whose frequency < 10
all_word <-all_word[all_word%in%stop_words$word==FALSE]
#remove stopwords
k<-subset(k,k$word%in%all_word)


p<-length(all_word)
n<-length(review_and_id$line)

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

review_vs_word_matix <- matrix(ncol = p,nrow = n)
for ( i in 1:n){
  review_vs_word_matix[i,]<-row_name[[i]]
}



stars_id=c()
stars_id$stars=Chinese_food_review$stars
stars_id$uniqueID=Chinese_food_review$review_id
# matrix=matrix(data=0,nrow=n,ncol=p)
# for(i in 1:n){
#   index <-which(all_word%in%k$word[(k$line==id[i])])
#   matrix[i,index] <- count(k$word[(k$line==id[i])])$freq
# }
dimnames(review_vs_word_matix)=list(review_and_id$line,all_word)

plotWordStar <- function(stars,DTM,wordList,mfrow = c(2,2)) {
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
plotWordStar(stars_id,review_vs_word_matix,wordList)

propo_calculate <- function(vector,target=review_and_id$stars){
  type<-unique(target);
  type<-type[order(type)]
  propo<-vector(length=length(type))
  if(class(vector)=="numeric"){
    all<-sum(vector)
    for(i in 1:length(type)){
      propo[i]=sum(vector[target==type[i]])/all
    }
  }else{
    all<-length(vector)
    for(i in 1:length(type)){
      propo[i]=sum(target==type[i])/all
    }
    
  }
  return(propo)
}

word_propo<-apply(review_vs_word_matix,2,propo_calculate)
#s<-apply(word_propo,2,sum)

star_rate<-propo_calculate(vector=review_and_id$stars,target=review_and_id$stars)
names(star_rate)<-unique(review_and_id$stars)[order(unique(review_and_id$stars))]


pearson_test <- function(p_j0,p_j=star_rate){
  f_j<-p_j0*n;
  e_j<-p_j*n;
  Q<- sum((f_j-e_j)^2/e_j);
  #p_value<-pchisq(Q,df=(length(e_j)-1),lower.tail = FALSE)
  return(Q)
  #return(p_value)
}
#get
word_Q_value<-apply(word_propo,MARGIN = 2,pearson_test)
affect_of_word<-all_word[order(word_Q_value,decreasing = TRUE)]






#select the word with >80% 5star
#colnames(word_propo)[which(word_propo[5,]>0.7)]

#select the word with >80% 1star
#colnames(word_propo)[which(word_propo[1,]>0.8)]

all_word_1 <- frequency_of_words$x[frequency_of_words$freq>=800]
all_word_1 <-all_word_1[all_word_1%in%stop_words$word==FALSE]
all_word_1

#Choose the following words mainly based on the frequency.
taste_word=c("sweet","spicy","bitter","salty","bland","sour","crispy","greasy")
food_word=c("bread","burger","cheese","chicken","cream","pizza","sushi","steak")
drink_word=c("beer","coffee","soup","tea","milk","wine","water")
plotWordStar(stars_id,review_vs_word_matix,taste_word,mfrow=c(2,4))
plotWordStar(stars_id,review_vs_word_matix,food_word,mfrow=c(2,4))
plotWordStar(stars_id,review_vs_word_matix,drink_word,mfrow=c(2,4))


index_taste=c()
for(i in 1:length(taste_word)) {
  index_taste = append(index_taste,which(all_word == taste_word[i]))
}
data=review_vs_word_matix[,index_taste]
for(i in 1:length(index_taste)){
  for(j in 1:length(data[,i])){
    if(data[j,i]>0) data[j,i]=1
  }
}
data1=rbind(t(stars),t(data))
data1=t(data1)
data1=as.data.frame(data1)
model1=lm(V1~.,data=data1)
anova(model1)
#good:sweet,spicy,bland,crispy
#bad:salty,greasy
m1=lm(V1~sweet+spicy+salty+bland+crispy+greasy,data=data1)
anova(m1)
par(mfrow=c(2,2))
plot(m1)

index_food=c()
for(i in 1:length(food_word)) {
  index_food = append(index_food,which(all_word == food_word[i]))
}
data=review_vs_word_matix[,index_food]
for(i in 1:length(index_food)){
  for(j in 1:length(data[,i])){
    if(data[j,i]>0) data[j,i]=1
  }
}
data1=rbind(t(stars),t(data))
data1=t(data1)
data1=as.data.frame(data1)
model2=lm(V1~.,data=data1)
anova(model2)
m2=lm(V1~burger+chicken+cream+sushi+steak,data=data1)
anova(m2)
par(mfrow=c(2,2))
plot(m2)
#good:cream,sushi,steak
#bad:burger,chicken

index_drink=c()
for(i in 1:length(drink_word)) {
  index_drink = append(index_drink,which(all_word == drink_word[i]))
}
data=review_vs_word_matix[,index_drink]
for(i in 1:length(index_drink)){
  for(j in 1:length(data[,i])){
    if(data[j,i]>0) data[j,i]=1
  }
}
data1=rbind(t(stars),t(data))
data1=t(data1)
data1=as.data.frame(data1)
model3=lm(V1~.,data=data1)
anova(model3)
#good:coffee,tea,wine
#bad:water
m3=lm(V1~coffee+tea+milk+wine+water,data=data1)
anova(m3)
par(mfrow=c(2,2))
plot(m3)

#final model choose

combo_word<- c("sweet","spicy","bitter","salty","bland","sour","crispy","chicken","fish","beef","soup","tea")
plotWordStar(stars_id,review_vs_word_matix,combo_word,mfrow=c(3,4))
index_combo=c()
for(i in 1:length(combo_word)) {
  index_combo = append(index_combo,which(all_word == combo_word[i]))
}
data=review_vs_word_matix[,index_combo]
for(i in 1:length(index_combo)){
  for(j in 1:length(data[,i])){
    if(data[j,i]>0) data[j,i]=1
  }
}
data4=rbind(t(stars),t(data))
data4=t(data4)
data4=as.data.frame(data4)
model4=lm(V1~.*.,data=data4)
k<-anova(model4)
q<-summary(model4)
index_final<-unique(c(c(1:13),as.numeric(which((k$`Pr(>F)`<0.25)&(abs(q$coefficients[,4])<0.5)))))
q$coefficients[index_final,]                  
k$`Pr(>F)`[index_final]
par(mfrow = c(2,2))
plot(model4)


#2 sample T-test part 
compare_plot <- function(words){
  
  have_not<-mean(stars[which(review_vs_word_matix[,which(colnames(review_vs_word_matix)==words)]==0)])
  have <-mean(stars[which(review_vs_word_matix[,which(colnames(review_vs_word_matix)==words)]!=0)])
  
  #barplot(have,have_not)
  return(c(have_not,have))
}

result<-lapply(combo_word,compare_plot)
result <- matrix(unlist(result),ncol=12,nrow=2)
dimnames(result)=list(c("not included","included"),combo_word)

mean(stars)
#pplot the differences
barplot(result[,1:3],beside=TRUE,col=c("yellow","blue"),ylim = c(0,5),legend.text = TRUE,args.legend = list(cex=0.8,x=9,y=5.5),ylab =c( "Ratings"))
text(x=c(1,2,4,5,7,8)+0.5,y=c(result[,1],result[,2],result[,3]),pos = 3,labels=round(c(result[,1],result[,2],result[,3]),3),col='red',cex=1.2)
barplot(result[,4:6],beside=TRUE,col=c("yellow","blue"),ylim = c(0,5),legend.text = TRUE,args.legend = list(cex=0.8,x=9,y=5.5),ylab = "Ratings")
text(x=c(1,2,4,5,7,8)+0.5,y=c(result[,4],result[,5],result[,6]),pos = 3,labels=round(c(result[,4],result[,5],result[,6]),3),col='red',cex=1.2)
barplot(result[,7:9],beside=TRUE,col=c("yellow","blue"),ylim = c(0,5),legend.text = TRUE,args.legend = list(cex=0.8,x=9,y=5.5),ylab = "Ratings")
text(x=c(1,2,4,5,7,8)+0.5,y=c(result[,7],result[,7],result[,9]),pos = 3,labels=round(c(result[,7],result[,8],result[,9]),3),col='red',cex=1.2)
barplot(result[,10:12],beside=TRUE,col=c("yellow","blue"),ylim = c(0,5),legend.text = TRUE,args.legend = list(cex=0.8,x=9,y=5.5),ylab = "Ratings")
text(x=c(1,2,4,5,7,8)+0.5,y=c(result[,10],result[,11],result[,12]),pos = 3,labels=round(c(result[,10],result[,11],result[,12]),3),col='red',cex=1.2)

#test
p_values<-vector()
for (i in 1:dim(result)[2]) {
  words=colnames(result)[i]
  p[i]<-t.test(stars[which(review_vs_word_matix[,which(colnames(review_vs_word_matix)==words)]==0)],stars[which(review_vs_word_matix[,which(colnames(review_vs_word_matix)==words)]!=0)])$p.value
  
}
p_values







getparameter1=function(id){
  index=which(id==Chinese_food_review$business_id)
  if(length(index)==0){
    return("Your given id is not a Chinese restaurant!")
  }
  else{
    matrix_combo=review_vs_word_matix[index,index_combo]
    r=c()
    temp=c()
    for(i in 1:12){
      temp[i]=sum(matrix_combo[,i])
    }
    #good: corresponding to 1,2,7,10,11,12
    #bad:corresponding to 3,4,5,6,8,9
    tempgood=temp[c(1,2,7,10,11,12)]
    tempbad=temp[c(3,4,5,6,8,9)]
    r$good=c(1,2,7,10,11,12)[which(tempgood%in%sort(tempgood)[1:2])[1:2]]
    if(max(tempbad)==0) r$bad=0
    else r$bad=c(3,4,5,6,8,9)[which(tempbad%in%sort(tempbad,decreasing=TRUE)[1:2])[1:2]]
  }
  return(r)#for each components, a value is corresponding to a word, 0 means no bad word need to be improved.
}

suggestion1=function(r1){
  print(paste("more ",combo_word[r1$good],sep=""))
  if(sum(r1$good%in%c(1,2))==2) print("But do not mix spicy and sweet food together.")
  if(sum(r1$good%in%c(2,10))==2) print("But do not mix spicy and beef together.")
  if(sum(r1$bad)!=0) print(paste("less ",combo_word[r1$bad],sep=""))
  if(sum(r1$bad%in%c(3,4))==2) print("But you can try to mix bitter and salty food together.")
}

id=Chinese_food_business_ID[4]
r1=getparameter1(id)
suggestion1(r1)
