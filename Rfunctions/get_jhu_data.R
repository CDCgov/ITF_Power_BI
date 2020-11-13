# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it reads in the Johns Hopkins public Coronavirus data set, and another function called "get_country_date.R" 
# it returns a cleaned file for analysis in Power BI alongside the same data from the WHO
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_country_date.R", is stored


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(rfunctions.dir){
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
ldpkg(c("tidyverse", "passport"))

library(tidyverse)
library(passport)
  
 
# Take out all NAs in the dataset and replace with zero
remove_nas <- function(df) { 
  df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ Setting up folders for data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# dir.root<- ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")),
#                   paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/"),
#                   ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/")),
#                          paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/"),
#                          "Directory does not exist"))
# 
# rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")


fun_frame <- dget(paste0(rfunctions.dir, "get_country_date.R"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ Function to generate datasets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", as.is=TRUE, stringsAsFactors = FALSE, check.names=FALSE)
  
  #Convert to Long Data
  cases.long <- cases %>% 
    tidyr::gather("Date.Orig", "Cumulative Cases", c(5:length(names(cases))))

  deaths.long <- deaths %>% 
    tidyr::gather("Date.Orig", "Cumulative Deaths", c(5:length(names(cases))))

  
  #Clean up Date and Time
  cases.long$Date <- as.Date(as.character(cases.long$Date.Orig), format="%m/%d/%y")
  deaths.long$Date <- as.Date(as.character(deaths.long$Date.Orig), format="%m/%d/%y")
  
  #Combine Case and Death Data
  data.long <- merge(cases.long, deaths.long, by=c("Province/State","Country/Region","Lat","Long","Date"), all=TRUE)
  # Remove the redundant date column
  data.long[c("Date.Orig.x","Date.Orig.y")] <- NULL
  
  #Calculate Daily New Cases and New Deaths, Then Make 0 if negative)
  data.long$Country.Province <- paste0(data.long$`Country/Region`," - ", data.long$`Province/State`)
  data.long$`New Cases` <- ave(data.long$`Cumulative Cases`, factor(data.long$Country.Province), FUN=function(x) c(NA,diff(x)))
  data.long$`New Deaths` <- ave(data.long$`Cumulative Deaths`, factor(data.long$Country.Province), FUN=function(x) c(NA,diff(x)))
  data.long$`New Cases`[data.long$`New Cases` <0] <- 0
  data.long$`New Deaths`[data.long$`New Deaths` <0] <- 0
  
  
  #Aggregate data to the country level
  #Keep Taiwan, Hong Kong, and Macao separate from Mainland China
  data.long$Country <- data.long$`Country/Region`
  data.long$Country[data.long$`Province/State` %in% c("Hong Kong","Macau")] <- data.long$`Province/State`[data.long$`Province/State` %in% c("Hong Kong","Macau")]
  data.long$Country <- gsub("*","",data.long$Country, fixed=TRUE)
  data.countries <- aggregate(data.long[c("New Cases","New Deaths","Cumulative Cases","Cumulative Deaths")], by=list(Country=data.long$Country, Date=data.long$Date), sum)
  data.countries$Country.clean <- parse_country(data.countries$Country, to="en-iso")
  data.countries$Country.clean[is.na(data.countries$Country.clean)] <- data.countries$Country[is.na(data.countries$Country.clean)]
  data.countries$Country <- data.countries$Country.clean
  data.countries$Country[data.countries$Country %in% c("Diamond Princess", "MS Zaandam")] <- "International Conveyance"
  data.countries <- data.countries[c("Country","Date","New Cases","Cumulative Cases","New Deaths","Cumulative Deaths")]
  data.countries <- data.countries[order(data.countries$Country, data.countries$Date),]
  

df <- data.countries %>% 
  rename(country   = Country,
         date      = Date,
         cases_new = `New Cases`,
         cases_cum = `Cumulative Cases`,
         deaths_new = `New Deaths`,
         deaths_cum = `Cumulative Deaths`
           ) %>% 
  mutate(iso3code = case_when(
    country %in% c("International Conveyance") ~ "OTH", 
    country %in% c("Eswatini") ~
      parse_country("Swaziland", to = "iso3c", how = c("regex", "google"), language = c("en")),
      country %in% c("Kosovo") ~ "XKX", 
      TRUE ~  parse_country(country, to = "iso3c", how = c("regex", "google"),language = c("en")))) %>% 
  filter(!is.na(iso3code)) %>% 
#  filter(country %ni% c("International Conveyance")) %>% 
  select(-country) %>% 
  group_by(iso3code) %>% 
  # Adding back all the first cases
  mutate(firstcase = if_else(date == min(date), 1, 0)) %>% 
  ungroup() %>% 
  mutate(cases_new = if_else(firstcase==1, cases_cum, as.integer(cases_new))) %>% 
  mutate(deaths_new = if_else(firstcase==1, deaths_cum, as.integer(deaths_new))) %>%
  select(-firstcase) %>%
  #sum over cruise ships
  group_by(iso3code,date) %>%
    summarise(cases_new=sum(cases_new),
              deaths_new=sum(deaths_new),
              cases_cum=sum(cases_cum),
              deaths_cum=sum(deaths_cum)) %>%
  ungroup()


# Adding population from the World Bank and JHU
dfframe <- fun_frame(rfunctions.dir) %>% 
  filter(date<=max(df$date)) %>% 
  filter(date>=min(df$date)) %>%
  filter(iso3code %in% unique(df$iso3code))


# Getting the basic final dataset
base_data <- left_join(dfframe, df) %>% 
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
  mutate_if(is.numeric, ~replace(., is.na(.), 0))  
    
  
base_data}
