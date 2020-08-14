# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it reads in the WHO public Coronavirus data set, and another function called "get_country_date.R" 
# it returns a cleaned file for analysis in Power BI alongside the same data from Johns Hopkins Coronavirus Resource Center
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
  ldpkg(c("tidyverse"))
  
  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}


  fun_date <- dget(paste0(rfunctions.dir, "get_country_date.R"))
  
w <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors=FALSE, encoding="UTF-8")

w$Country <- if_else(w$Country=="Kosovo[1]", "Kosovo", w$Country)
w$Country_code <- if_else(w$Country=="Namibia", "NA", w$Country_code)
w$Country_code <- if_else(w$Country=="Other", "OT", w$Country_code)
#w$date <- as.Date(w$Date_reported)
w$date <- as.Date(w[,1])
w$iso2code <- w$Country_code

#Expand the time series so all countries have the same number of records
# Create data frame with all countries and all dates
dfframe <- fun_date(rfunctions.dir) %>% 
  filter(date<=max(w$date)) %>% 
  filter(date>=min(w$date)) %>% 
  filter(iso2code %in% unique(w$iso2code))

# Get WHO data by ISO code and case count data
wx <- w %>% select(iso2code, date, New_cases, New_deaths, Cumulative_cases, Cumulative_deaths)


df <- left_join(dfframe, wx) %>% 
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
  mutate_if(is.numeric, ~replace(., is.na(.), 0))   %>%
  filter(date != Sys.Date()) 


return(df)
}
