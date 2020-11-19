rm(list=ls())
library(rjson)
library(jsonlite)


user <- jsonlite::stream_in(file("user_city.json"))
tip <- jsonlite::stream_in(file("tip_city.json"))
review <- jsonlite::stream_in(file("review_city.json"))
business <- jsonlite::stream_in(file("business_city.json"))

business_chinese <- business[c('chinese')%in%business$categories]
