function(rfunctions.dir){

  # Creating basic functions to show top few rows of data
  View50 <- function(x){View(x[1:50,])}
  View100 <- function(x){View(x[1:100,])}
  
  # Creating the 'not in' function
  `%ni%` <- Negate(`%in%`) 
  
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- function(x){
    for( i in x ){
      #  require returns TRUE invisibly if it was able to load package
      if( ! require( i , character.only = TRUE ) ){
        #  If package was not able to be loaded then re-install
        install.packages( i , dependencies = TRUE )
        #  Load package after installing
        require( i , character.only = TRUE )
      }
    }
  }
  
  
  # Loading the packages
  ldpkg(c("tidyverse"))
  
  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}

  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Setting up folders for data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # dir.root<- ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")),
  #                   paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/"),
  #                   ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/")),
  #                          paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/"),
  #                          "Directory does not exist"))
  # 
  # 
  # rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  
  fun_date <- dget(paste0(rfunctions.dir, "get_country_date.R"))
  
  w <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors=FALSE, encoding="UTF-8")
  # w <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSe-8lf6l_ShJHvd126J-jGti992SUbNLu-kmJfx1IRkvma_r4DHi0bwEW89opArs8ZkSY5G2-Bc1yT/pub?gid=0&single=true&output=csv", stringsAsFactors=FALSE, encoding="UTF-8")
  
  w$Country <- if_else(w$Country=="Kosovo[1]", "Kosovo", w$Country)
  w$Country_code <- if_else(w$Country=="Namibia", "NA", w$Country_code)
  #w$date <- as.Date(w$Date_reported)
  w$date <- as.Date(w[,1])
  w$iso2code <- w$Country_code
  #w$Country[is.na(w$iso2code)] <- "OT"
  w$Country_code <- if_else(w$Country=="Other", "OT", w$Country_code)
  
  #Expand the time series so all countries have the same number of records
  # Create data frame with all countries and all dates
  dfframe <- fun_date(rfunctions.dir) %>% 
    filter(date<=max(w$date))%>% 
    filter(date>=min(w$date)) %>%
    filter(iso2code %in% unique(w$iso2code))
  
  
  # Get WHO data by ISO code and case count data
  wx <- w %>% select(iso2code, date, New_cases, New_deaths, Cumulative_cases, Cumulative_deaths)
  
  
  df <- left_join(dfframe,wx) %>% 
    rename(cases_new      = New_cases,
           cases_cum      = Cumulative_cases,
           deaths_new     = New_deaths,
           deaths_cum     = Cumulative_deaths
           ) %>%  
    select(country, 
           date, 
           cases_new, 
           cases_cum,
           deaths_new,
           deaths_cum,
           who_region,
           pop_2020yr,
           iso3code,
           ou_date_match,
           iso2code) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))  %>%
    filter(date != Sys.Date()) %>%
    #there is no Taiwan data in the WHO data set but it is in the JHU dataset
    filter(iso3code %ni% c("TWN")) 
  
  return(df)
}