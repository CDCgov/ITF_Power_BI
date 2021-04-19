
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(rfunctions.dir, df_country, df_gmob_raw){
  
  # Creating the 'not in' function
  `%ni%` <- Negate(`%in%`) 
  
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  
  # Loading the packages
  ldpkg(c("Hmisc", 
          "tidyverse",
          "openxlsx",
          "passport",
          "readxl"))
  
  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Setting up folders for data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # If country metadata not present as input, then call the script to generate it
  if (missing(df_country)) {
    # Function to get the country metadata 
    fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
    df_country <- fun_country(rfunctions.dir)
  }
  
  # If google mobility not present as input, download it from google
  if (missing(df_gmob_raw)) {
    df_gmob_raw <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", encoding="UTF-8")
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Pulling in the Google mobility data  
  gmob <- df_gmob_raw
  
  # Correcting Namibia's ISO code
  gmob$country_region_code[gmob$country_region=="Namibia"] <- "NA"
  
  ## read in Johns Hopkins Data
  #Import Country Time Series
  df <- df_country %>% 
    select(iso2code, iso3code) %>% unique()

  gmob1 <- gmob %>% 
    mutate(Date = as.Date(date)) %>% 
    # only keep the Country-level data
    filter(sub_region_1 %in% c("")) %>% 
    select(Date, country_region, country_region_code,
           retail_and_recreation_percent_change_from_baseline:
             residential_percent_change_from_baseline) %>% 
    rename(iso2code = country_region_code)
  
  
  # setdiff(unique(gmob1$iso2code), unique(df$iso2code))
  
  # Join the Google mobility data to the JHU data for incidence
  dfx <- left_join(gmob1, df) %>% 
    dplyr::mutate(ou_date_match = paste(iso3code, Date, sep="_")) 
  
  return(dfx)
}
