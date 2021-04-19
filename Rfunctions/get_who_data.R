# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it reads in the WHO public Coronavirus data set, and another function called "get_country_date.R" 
# it returns a cleaned file for analysis in Power BI alongside the same data from Johns Hopkins Coronavirus Resource Center
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_country_date.R", is stored

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(rfunctions.dir, df_country_date){

  # Creating the 'not in' function
  `%ni%` <- Negate(`%in%`) 
  
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  
  # Loading the packages
  ldpkg(c("tidyverse"))
  
  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}


  # If country & date dataframe not present as input, then call the script to generate it
  if (missing(df_country_date)) {
    # Function to get the country metadata 
    fun_country_date <- dget(paste0(rfunctions.dir, "get_country_date.R"))
    df_country_date <- fun_country_date(rfunctions.dir)
  }
  
  w <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors=FALSE, encoding="UTF-8")
  
  w$Country <- if_else(w$Country=="Kosovo[1]", "Kosovo", w$Country)
  w$Country <- if_else(w$Country=="Bonaire","Bonaire, Sint Eustatius, and Saba", w$Country)
  w$Country <- if_else(w$Country=="Sint Eustatius","Bonaire, Sint Eustatius, and Saba", w$Country)
  w$Country <- if_else(w$Country=="Saba","Bonaire, Sint Eustatius, and Saba", w$Country)
  w$Country_code <- if_else(w$Country=="Namibia", "NA", w$Country_code)
  w$Country_code <- if_else(w$Country=="Other", "OT", w$Country_code)
  w$Country_code <- if_else(w$Country=="Bonaire, Sint Eustatius, and Saba", "BQ", w$Country_code)
    
  w$date <- w[,1]
  
  #Collapse Bonaire, Sint Eustatius, and Saba to a single Country
  w <- w %>%
    group_by_if(is.character) %>%
    summarize_all(list(~sum(., na.rm=T))) %>%
    ungroup()
  
  w <- w %>%
    mutate(date = as.Date(date),
           iso2code = Country_code)
  
  #Expand the time series so all countries have the same number of records
  # Create data frame with all countries and all dates
  dfframe <- df_country_date %>% 
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
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    filter(date != Sys.Date()) %>%
    #there is no Taiwan data in the WHO data set but it is in the JHU dataset
    filter(iso3code %ni% c("TWN")) 


  return(df)
}
