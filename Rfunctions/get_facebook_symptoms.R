library(tidyverse)
library(httr)
library(jsonlite)


get_facebook_symptoms <- function (path){
      
      # request data from api
      request <- GET(url = path)
      
      # make sure the content is encoded with 'UTF-8'
      response <- content(request, as = "text", encoding = "UTF-8")
      
      # now we can have a dataframe for use!
      facebook_symptoms <- fromJSON(response, flatten = TRUE) %>% data.frame()
}


#Can only pull 3600 rows at a time, so am pulling month by month and then appending all rows together


facebook_symptoms_2020_04 <- get_facebook_symptoms("https://covidmap.umd.edu/api/resources?indicator=covid&type=daily&country=all&daterange=20200423-20200430")


facebook_symptoms_2020_05 <- get_facebook_symptoms("https://covidmap.umd.edu/api/resources?indicator=covid&type=daily&country=all&daterange=20200501-20200531")

facebook_symptoms_2020_06 <- get_facebook_symptoms("https://covidmap.umd.edu/api/resources?indicator=covid&type=daily&country=all&daterange=20200601-20200630")



facebook_symptoms_2020_all <- bind_rows(facebook_symptoms_2020_04, facebook_symptoms_2020_05, facebook_symptoms_2020_06)
