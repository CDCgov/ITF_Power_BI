#' Code for CDC Tracker Power BI file
#' This function reads in exclusively public data
#' @param functions.dir The directory where the other code resides
#' @param df_country The data frame produced by "get_country.R" script (optional)
#' @return A dataframe of country metadata crossed with date span (2020-01-01 to today's date)


function(rfunctions.dir, df_country){
  # Creating basic functions to show top few rows of data
  View50 <- function(x){View(x[1:50,])}
  View100 <- function(x){View(x[1:100,])}
  
  # Creating the 'not in' function
  `%ni%` <- Negate(`%in%`)
  
  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))
  }
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  # Loading the packages
  ldpkg(c("maps", 
          "tidyverse",
          "countrycode",
          "passport"))
  

  # If country metadata dataframe not present as input, then call the script to generate it
  if (missing(df_country)) {
    # Function to get the country metadata 
    fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
    df_country <- fun_country(rfunctions.dir)
  }

  
  # Setting up dates for merge
  start_date <- as.Date("2020-01-01")
  end_date   <- as.Date(Sys.Date())
  
  all_dates <- seq(from=start_date, to = end_date, by=1)
  
  # Joining countries and dates to get cartesia product
  base_frame <- merge(df_country, all_dates, by=NULL) %>% 
    dplyr::select(country, iso3code, iso2code, who_region, pop_2020yr, Continent_Name, y) %>% 
    dplyr::rename(date = y) %>%
    dplyr::mutate(ou_date_match = paste(iso3code, date, sep="_")) %>% 
    dplyr::select(ou_date_match, country, date, iso3code, iso2code, 
                  who_region, pop_2020yr, Continent_Name) %>% 
    unique()
  
  return(base_frame)
}