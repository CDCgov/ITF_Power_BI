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
  ldpkg(c("tidyverse"))
  
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
  dir.who.regions <- paste0(dir.root,"Data/WHO Member States")
  
  rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  
  
  fun_frame <- dget(paste0(rfunctions.dir, "get_country.R"))
  
  
w <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSe-8lf6l_ShJHvd126J-jGti992SUbNLu-kmJfx1IRkvma_r4DHi0bwEW89opArs8ZkSY5G2-Bc1yT/pub?gid=0&single=true&output=csv", stringsAsFactors=FALSE, encoding="UTF-8")

w$Country <- if_else(w$ADM0_NAME=="Kosovo[1]", "Kosovo", w$ADM0_NAME)
w$date <- as.Date(substr(w$date_epicrv,1,10))
w$iso3code <- w$ISO_3_CODE


# Adding population from the World Bank and JHU
iso <- fun_frame() %>% select(country, iso3code, 
                              who_region, pop_2020yr) %>% unique()

# chex <- iso %>% group_by(iso3code) %>% 
#   mutate(ranx= row_number()) %>% 
#   ungroup()

wx <- w %>% select(ISO_3_CODE, ADM0_NAME) %>% unique()

# Cross-checking all countries in both datasets
whodiffx <- setdiff(unique(wx$ISO_3_CODE), unique(iso$iso3code)) 

who <- w %>% filter(iso3code != "")

df <- left_join(who, iso) %>% 
  rename(cases_new      = NewCase,
         cases_cum      = CumCase,
         deaths_new     = NewDeath,
         deaths_cum     = CumDeath
         ) %>%  
  select(country, 
         date, 
         cases_new, 
         cases_cum,
         deaths_new,
         deaths_cum,
         who_region,
         pop_2020yr,
         iso3code) %>%  
  mutate(ou_date_match = paste(iso3code, date, sep="_")) 

return(df)
}