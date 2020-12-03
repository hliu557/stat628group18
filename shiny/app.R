# Shiny App
rm(list=ls())
library(shiny)
#library(ggplot2)
#library(shinythemes)

Chinese_food_review=read.csv("Chinese_food_review.csv")
#row.names(Chinese_food_review)=Chinese_food_review$X
review_vs_word_matix=read.csv("review_vs_word_matrix.csv")
Chinese_food_business_ID=unique(Chinese_food_review$business_id)
Chinese_food_information=read.csv("Chinese_food_information.csv")

combo_word<- c("sweet","spicy","bitter","salty","bland","sour","crispy","chicken","fish","beef","soup","tea")


getparameter=function(id){
  index=which(id==Chinese_food_review$business_id)
  if(length(index)==0){
    return("Your given id is not a Chinese restaurant!")
  }
  else{
    matrix_combo=review_vs_word_matix[index,]
    r=c()
    temp=c()
    for(i in 1:12){
      temp[i]=sum(matrix_combo[,i+1])
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

suggestion=function(r1){
  print(paste("more ",combo_word[r1$good],sep=""))
  if(sum(r1$good%in%c(1,2))==2) print("But do not mix spicy and sweet food together.")
  if(sum(r1$bad)!=0) print(paste("less ",combo_word[r1$bad],sep=""))
  if(sum(r1$bad%in%c(3,4))==2) print("But you can try to mix bitter and salty food together.")
  if(sum(r1$bad%in%c(4,8))==2) print("But you can try to mix chicken with bland taste together.")
  if(sum(r1$bad%in%c(4,9))==2) print("But you can try to mix fish with bland taste together.")
}

#example
#id=Chinese_food_business_ID[3]
#r=getparameter(id)
#suggestion(r)


ui <- fluidPage(
  titlePanel("Advices for Chinese restaurants"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("index", 
                  label = "Choose chinese food restaurant index 1-590",
                  min = 1, max = 590, value = 4)
    ),
    
    mainPanel(
      h3("information of the restaurant"),
      textOutput("name"),
      textOutput("address"),
      textOutput("city"),
      textOutput("state"),
      textOutput("postalcode"),
      textOutput("latitude"),
      textOutput("longtitude"),
      textOutput("averagestars"),
      h3("Advices for the restaurant"),
      textOutput("advice"),
      h3("plot the histogram of the number of stars : "),
      plotOutput("star", width = 300, height = 300),
      h3("plot the barplot of word counts in reviews: "),
      plotOutput("word", width = 700, height = 300)
    )
  )
)

server <- function(input, output) {
  output$name = renderText({
    paste0("Name: ",Chinese_food_information$name[input$index])
  })
  output$address = renderText({
    paste0("Address: ",Chinese_food_information$address[input$index])
  })
  output$city = renderText({
    paste0("City: ",Chinese_food_information$city[input$index])
  })
  output$state = renderText({
    paste0("State: ",Chinese_food_information$state[input$index])
  })
  output$postalcode = renderText({
    paste0("Postal code: ",Chinese_food_information$postal_code[input$index])
  })
  output$latitude = renderText({
    paste0("Latitude: ",Chinese_food_information$latitude[input$index])
  })
  output$longtitude = renderText({
    paste0("Longtitude: ",Chinese_food_information$longitude[input$index])
  })
  output$averagestars  = renderText({
    id=Chinese_food_business_ID[input$index]
    index=which(id==Chinese_food_review$business_id)
    paste0("Average of Stars : ",round(mean(Chinese_food_review$stars[index]),2))
  })
  
  output$advice <- renderPrint({ 
    id=Chinese_food_business_ID[input$index]
    r=getparameter(id)
    suggestion(r)
  })
  
  output$star = renderPlot({
    id=Chinese_food_business_ID[input$index]
    index=which(id==Chinese_food_review$business_id)
    hist(Chinese_food_review$stars[index],xlab = "stars",ylab = "number of reviews",main = "stars histogram")
  })
  
  output$word = renderPlot({
    id=Chinese_food_business_ID[input$index]
    index=which(id==Chinese_food_review$business_id)
    matrix_combo=review_vs_word_matix[index,]
    temp=c()
    for(i in 1:12){
      temp[i]=sum(matrix_combo[,i+1])
    }
    #worddata=c()
    #for(i in 1:12){
    #  worddata=c(worddata,rep(combo_word,temp[i]))
    #}
    #plot(as.factor(worddata),ylim = c(0,max(temp)))
    #rownames(temp)=combo_word
    barplot(temp,names.arg = combo_word,xlab = "word",ylab = "frequency",main = "Frequency of words in reviews",col = "blue")
  })
}

shinyApp(ui = ui, server = server)