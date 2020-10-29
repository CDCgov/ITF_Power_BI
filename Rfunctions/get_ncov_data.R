# Hello World
# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it returns a cleaned file for analysis in Power BI with country-level case and death data from WHO and the Johns Hopkins Coronavirus Resource Center
# it requires an object called "rfunctions.dir" which is the directory where the other codes, "get_jhu_date.R" and "get_who_data.R", is stored

function(rfunctions.dir){

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

  
  jhu <- dget(paste0(rfunctions.dir, "get_jhu_data.R"))
  who <- dget(paste0(rfunctions.dir, "get_who_data.R"))

  
  jdf <- jhu(rfunctions.dir) 
  jdf$data_source = "JHU"
  
  wdf <- who(rfunctions.dir) 
  wdf$data_source = "WHO"
  
  df <- rbind(jdf, wdf) %>% 
    rename( Country             =   "country"    ,
            Date                =   "date"       ,
           `New Cases`          =   "cases_new"  ,
           `Cumulative Cases`   =   "cases_cum"  ,
           `New Deaths`         =   "deaths_new" ,
           `Cumulative Deaths`  =   "deaths_cum" ,
           `WHO Region`         =   "who_region" ,
           `Population 2018.x`  =   "pop_2020yr" ,
           `Country Code`       =   "iso3code") %>%
    mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) %>% 
    mutate(country_code = `Country Code`) %>% 
    select(Country, Date, 
           `New Cases`,
           `Cumulative Cases`,
           `New Deaths`,
           `Cumulative Deaths`,
           `WHO Region`,
           `Population 2018.x`,
           `Country Code`,
           country_code,
           ou_date_match,
           ou_date_src_match,
           data_source,
           iso2code
    ) %>%  
    mutate_if(is.numeric, ~replace(., is.na(.), 0))

  
  return(df)
  }
