# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(z){
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
  
  
  #SETTING DIRECTORY FOR INTERNATIONAL TASK FORCE - if James, defaults to his own account, otherwise appends users' name to the path
dir.root <- paste0("C:/Users/",
                            Sys.getenv("USERNAME"),
                            "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")
  


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Getting Testing data from OurWorldinData ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  testing <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", stringsAsFactors = FALSE)
  
  testing$Date <- as.Date(testing$date)
  
  tstx <- testing %>% 
    filter(location %ni% c("World", "International")) %>% 
  mutate(ou_date_match = paste(iso_code, Date, sep="_")) %>% 
    select(-Date) %>% 
    mutate(adj_total_cases = if_else(!is.na(total_tests), total_cases, NA_real_)) %>% 
    rename(iso3code = iso_code)
  
  tst_cross <- tstx %>% 
    filter(!is.na(adj_total_cases)) %>% 
    dplyr::group_by(location, iso3code) %>% 
    dplyr::summarise(last_date = max(date, na.rm=T),
              cum_cases = max(adj_total_cases, na.rm=T),
              cum_tests = max(total_tests, na.rm=T),
              cum_test_100thousand = max(total_tests_per_thousand, na.rm=T)*100,
              cum_cases_100thousand = max(total_cases_per_million, na.rm=T)*0.1
              ) %>% 
    ungroup()
  
  test_type <- tstx %>% 
    filter(!is.na(total_tests) & !is.na(tests_units)) %>% 
    arrange(iso3code, desc(date)) %>%
    group_by(iso3code) %>% 
    mutate(snum = row_number()) %>% 
    ungroup() %>%  
    filter(snum==1) %>% 
    select(iso3code, tests_units) 
  
  tst_crossx <- left_join(tst_cross, test_type)
  
  if(z=="cross"){
    return(tst_crossx)
  } else if(z=="long"){
    return(tstx)}
}