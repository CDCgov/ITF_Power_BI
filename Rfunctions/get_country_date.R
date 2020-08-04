# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it reads in exclusively public data sets and another function called "get_country.R" 
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_country.R", is stored


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
  ldpkg(c("maps", 
          "tidyverse",
          "countrycode",
          "passport"))
  
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
  # rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  # 
  # 
  # # function to get the country metadata 
  fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))

  geodf <- fun_country()

# Setting up dates for merge
start_date <- as.Date("2020-01-01")
end_date   <- as.Date(Sys.Date())

all_dates <- seq(from=start_date, to = end_date, by=1)

# Joining countries and dates to get cartesia product
base_frame <- merge(geodf, all_dates, by=NULL) %>% 
  dplyr::select(country, iso3code, iso2code, who_region, pop_2020yr, Continent_Name, y) %>% 
  dplyr::rename(date = y) %>%
  dplyr::mutate(ou_date_match = paste(iso3code, date, sep="_")) %>% 
  dplyr::select(ou_date_match, country, date, iso3code, iso2code, 
                who_region, pop_2020yr, Continent_Name) %>% 
  unique()

return(base_frame)
}