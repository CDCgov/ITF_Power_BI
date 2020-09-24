# Code for CDC Tracker Power BI file
# this code is called by an R script run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it will output two possible files: a cross-sectional file (requires "cross" for typex) or a longitudinal file (requires "long" for typex)
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_ncov_data.R" and "traj_sub_functions.R", is stored
# This code calls applies an algorithm to identify the trajectory status of epidemic curves for cumulative incidence or mortality 
# This algorithm was developed by the CDC Case Based Surveillance Task Force and will potentially be updated in the future
# Last update to the trajectory algorithm: 9/24/2020
# See traj_sub_functions.R code for details on trajectory updates


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function(typex, rfunctions.dir){
  
final_hotspot  <- function(typex, dfsource){

  # Take out all NAs in the dataset and replace with zero
remove_nas <- function(df) { 
  df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}

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
ldpkg(c("tidyverse",
        "passport",
        "readxl"))

# function to get the base JHU data with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ===== Beginning of Epi curve segmentation Function ===============
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#read in the getcategory and hotspot functions
source(paste0(rfunctions.dir,"traj_sub_functions.R"))

#pull in the JHU or WHO data
df.Countriesx <- fun_ncov(rfunctions.dir) # Pulling in case and death data
df.Countries.Daily <- df.Countriesx %>% filter(data_source %in% dfsource) 

# format the data for the hotspot code

#consolidate the df.Countries.Daily dataset as some areas have multiple observations
df.cases_fin <- df.Countries.Daily[c("country_code","Date","Population 2018.x","Cumulative Cases")] %>%   
  group_by(country_code, Date,`Population 2018.x`) %>% 
  summarize(Cumulative_Cases = sum(`Cumulative Cases`, na.rm=T)) %>% 
  ungroup() %>% 
  filter(`Population 2018.x`>0 & !is.na(`Population 2018.x`)) %>% 
  mutate(Cumulative_Cases=if_else(Cumulative_Cases<0,0,if_else(is.na(Cumulative_Cases),0,Cumulative_Cases))) %>%
  #select(-data_source,-country_code) %>%
  #rename to align with CBS code names
  rename(id=country_code,
         cases=Cumulative_Cases,
         pop=`Population 2018.x`,
         date=Date)

#IF you want also to do it for deaths, uncomment the following:
# # df.deaths_fin  <- df.Countriesx.Daily[c("country_code","Date","Population 2018.x","Cumulative Deaths")] %>%   
#   group_by(country_code, Date,`Population 2018.x`) %>% 
#   summarize(Cumulative_Cases = sum(`Cumulative Deaths`, na.rm=T)) %>% 
#   ungroup() %>% 
#   filter(`Population 2018.x`>0 & !is.na(`Population 2018.x`)) %>% 
#   mutate(Cumulative_Cases=if_else(Cumulative_Cases<0,0,if_else(is.na(Cumulative_Cases),0,Cumulative_Cases))) %>%
#   #select(-data_source,-country_code) %>%
#   #rename to align with CBS code names
#   rename(id=country_code,
#          cases=Cumulative_Cases,
#          pop=`Population 2018.x`,
#          date=Date)

### Run hotspot trajectory on formatted case data - use national data
#rate.cut =  ifelse(base.data == "case", 10, 0.5)
#slope.cut = ifelse(base.data == "case", 0.1, 0.005) 
#hb.cut = ifelse(base.data == "case", 100, 5)

alldata <- hotspot(df.cases_fin,"case", 0.1, 10, 100, 7, 0.6, dsource=dfsource)

#if you want to use multiple cut-points
# alldata <- bind_rows(purrr::map2(.x = c(rep(c(.01,.05,.1,.5),4)), 
#                      .y = sort(rep(c(1,5,10,15),4)), .f = ~hotspot("Case", .x, .y, 100, dsource=dfsource)))



allthedata <- alldata %>%
  mutate(data_source = dfsource) %>% 
  mutate(ou_date_match = paste(id, dates, sep="_")) %>% 
  #Creating variable that is not date dependent
  mutate(ou_cut_match = paste(id, slope_cut, rate_cut, sep="_")) %>% 
  rename(country_code = id) %>% 
  rename(date = dates) %>% 
  mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) %>% 
  mutate(ou_cut_src_match = paste(ou_cut_match, data_source, sep="_")) %>% 
  select(-ou_cut_match)
  
gisdata <- allthedata %>% select(-ou_date_match, -ou_date_src_match) %>% 
  filter(date %in% max(date))

if (typex == "date"){
return(allthedata)} else if (typex == "map"){
return(gisdata)}
  }
  
whodf <- final_hotspot(typex, "WHO") 
jhudf <- final_hotspot(typex, "JHU") 

trajdf <- bind_rows(whodf, jhudf) 

return(trajdf)
}

