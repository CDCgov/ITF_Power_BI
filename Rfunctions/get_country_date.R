# Function to generate core dataframe by country and date
function(){
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
  
dir.root <- paste0("C:/Users/",
                            Sys.getenv("USERNAME"),
                            "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")
  
  
  #Define Directories
  dir.data <- paste0(dir.root,"Data/")
  dir.who.regions <- paste0(dir.root,"Data/WHO Member States/")
  
  rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  
  
  # # function to get the country metadata 
  fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))

  geodf <- fun_country()

# Setting up dates for merge
start_date <- as.Date("2020-01-01")
end_date   <- as.Date(Sys.Date())

all_dates <- seq(from=start_date, to = end_date, by=1)

# Joining countries and dates to get cartesia product
base_frame <- merge(geodf, all_dates, by=NULL) %>% 
  dplyr::select(country, iso3code, iso2code, who_region, pop_2020yr, y) %>% 
  dplyr::rename(date = y) %>%
  dplyr::mutate(ou_date_match = paste(iso3code, date, sep="_")) %>% 
  dplyr::select(ou_date_match, country, date, iso3code, iso2code, 
                who_region, pop_2020yr) %>% 
  unique()

return(base_frame)
}