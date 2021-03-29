# File created by Aspen Flynn on 09/01/20

### Objective: Create one data frame with:
# JHU and WHO case and death data
# Testing data 
# Excess death data
function(x, rfunctions.dir) {
  
  #Establish directories 
  # dir.root<- ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")),
  #                   paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/"),
  #                   ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/")),
  #                          paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/"),
  #                          "Directory does not exist"))
  # 
  # dir.rfunctions <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  
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
  
  ldpkg("tidyverse")
  
  # Read in JHU and WHO case and death data. 
  # Data has already been cleaned by previous functions (get_jhu_data.R, get_who_data.R, get_ncov_data.R, get_riskmatrix_v2.R).
  get_mat <- dget(paste0(rfunctions.dir, "get_riskmatrix_v2.R"))
  matrix <- get_mat(typex="date") #date = all dates, cross = just most recent date
  
  #Read in testing data. Already cleaned by covid_testing.R
  get_testing <- dget(paste0(rfunctions.dir, "covid_testing.R"))
  testing <- get_testing("long") #long = all dates, cross = just most recent date
  ## This file contains testing information from both OWID and FIND. 
  ## OWID also provides a CSV on their git that is just testing
  ## FIND provides information on number of tests and number of cases, but their case info comes from Hopkins, so I think we can remove this.
  #Let's get this down to a more reasonable size
  testing <- testing %>% select(iso3code, date, new_cases_smoothed, new_tests_smoothed,total_tests, total_tests_per_thousand, new_tests_per_thousand, new_tests_smoothed,
                                new_tests_smoothed_per_thousand, tests_per_case, perc_positive_testing, tests_units, ou_date_match,
                                source)
  testing<- testing %>% rename(`test source` = source) #rename source column to specify that it's just for tests (deaths came from jhu)
  testing$Date <- as.Date(testing$date)
  testing$`Country Code` <- testing$iso3code
  
  # Merge testing data with cases and deaths.
  matrix.testing <- left_join(matrix, testing, by = c("Country Code", "Date"))
  
  #Select relevant variables (can  combine this with above select command)
  matrix.testing <- matrix.testing %>% select(-country_code, -date, -iso3code, -iso2code)
  
  #return(matrix.testing)
    country_spec <- matrix.testing %>% filter(`Country Code` == x)
    
    if (x == "all"){
      return(matrix.testing)} else if (x %in% unique(matrix.testing$`Country Code`)){
        return(country_spec)}

}

#--------------------------DATA CHECKS DONE BEFORE MERGING-------------------------------#

#What can we remove from case and death data?
# setdiff(matrix$country_code, matrix$`Country Code`) #no differences in either direction, can remove one of them.
# 
# 
# # Are there countries that are in case/death data that are not in testing data? 
# ## I've already confirmed that the case/death data and the testing data both use iso3 codes, so we are comparing the same variables here.
# setdiff(matrix$`Country Code`, testing$iso3code) #"BLM" "GLP" "GUF" "MAF" "MTQ" "MYT" "REU" "SPM" "ESH" "TWN"
# setdiff(testing$iso3code, matrix$`Country Code`) #none 
# 
# setdiff(testing$date, matrix$Date) #testing data started 5 days before death/case data



#--------------------------EXPLORE EXCESS DEATH DATA-------------------------------------#

#Read in excess death data. There isn't a function for this, just regular code.
#This data only goes through July 13. Will need to find more recent source of excess death data. 
# URL.ft <- "https://github.com/Financial-Times/coronavirus-excess-mortality-data/raw/master/data/ft_excess_deaths.csv"
# ex_death <- read.csv(URL.ft, header = TRUE, stringsAsFactors = FALSE)
# ex_death$date <- as.Date(ex_death$date, "%Y-%m-%d")
# ex_death <- ex_death %>% 
  #rename(Country = country) %>%
  # filter(Country == region) %>%
  # filter(date >= "2020-01-01")

#Explore excess death data
# length(unique(ex_death$Country)) #21
# length(unique(matrix.testing$Country)) #220
# Not sure it's worth combining these data sets 

