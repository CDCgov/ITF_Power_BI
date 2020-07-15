function(){

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
    
  dir.root <- paste0("C:/Users/",
                     Sys.getenv("USERNAME"),
                     "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")
  
  rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  
  
  
  jhu <- dget(paste0(rfunctions.dir, "get_jhu_data.R"))
  who <- dget(paste0(rfunctions.dir, "get_who_data.R"))

  
  
  
  jdf <- jhu() 
  jdf$data_source = "JHU"
  
  wdf <- who() 
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
           data_source
    ) %>%  
    mutate_if(is.numeric, ~replace(., is.na(.), 0))

  
  return(df)
  }