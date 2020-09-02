# Code for CDC Tracker Power BI file
# this code is called by an R script run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it will output two possible files: a cross-sectional file (requires "cross" for typex) or a longitudinal file (requires "long" for typex)
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_ncov_data.R" and "traj_sub_functions.R", is stored
# This code calls applies an algorithm to identify the trajectory status of epidemic curves for cumulative incidence or mortality 
# This algorithm was developed by the CDC Case Based Surveillance Task Force and will potentially be updated in the future
# Last update to the trajectory algorithm: 7/23/2020


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
source(paste0(rfunctions.dir,"traj_sub_functions_noHmisc.R"))

#pull in the JHU or WHO data
df.Countriesx <- fun_ncov(rfunctions.dir) # Pulling in case and death data
df.Countries.Daily <- df.Countriesx %>% filter(data_source %in% dfsource) 

# format the data for the hotspot code

#consolidate the df.Countries.Daily dataset as some areas have multiple observations
df.cases_fin <- df.Countries.Daily[
  c("country_code","Date",
    "WHO Region","Population 2018.x",
    "Cumulative Cases")] %>%   
  dplyr::group_by(country_code, Date,
                  `WHO Region`, `Population 2018.x`) %>% 
  dplyr::summarize(Cumulative_Cases = sum(`Cumulative Cases`, na.rm=T)) %>% 
  ungroup() %>% 
  filter(`Population 2018.x`>0 & !is.na(`Population 2018.x`)) %>% 
  spread(Date, Cumulative_Cases) %>% 
  mutate_if(is.numeric, ~replace(., .<0, 0)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


#IF you want also to do it for deaths, uncomment the following:
# df.deaths_fin <- df.Countries.Daily[
#   c("country_code","Date",
#     "WHO Region","Population 2018.x",
#     "Cumulative Deaths")] %>%   
#   dplyr::group_by(country_code, Date,
#                   `WHO Region`, `Population 2018.x`) %>% 
#   dplyr::summarize(Cumulative_Deaths = sum(`Cumulative Deaths`, na.rm=T)) %>% 
#   ungroup() %>% 
#   filter(`Population 2018.x`>0 & !is.na(`Population 2018.x`)) %>% 
#   spread(Date, Cumulative_Deaths) %>% 
#   mutate_if(is.numeric, ~replace(., .<0, 0)) %>% 
#   mutate_if(is.numeric, ~replace(., is.na(.), 0))

### Run hotspot trajectory on formatted case data - use national data

alldata <- hotspot(df.cases_fin,"Case", 0.1, 10, 100, dsource=dfsource)

#if you want to use multiple cut-points
# alldata <- bind_rows(purrr::map2(.x = c(rep(c(.01,.05,.1,.5),4)), 
#                      .y = sort(rep(c(1,5,10,15),4)), .f = ~hotspot("Case", .x, .y, 100, dsource=dfsource)))


allthedata <- alldata %>%
  mutate(data_source = dfsource) %>% 
  mutate(ou_date_match = paste(name, dates, sep="_")) %>% 
  #Creating variable that is not date dependent
  mutate(ou_cut_match = paste(name, slope_cut, rate_cut, sep="_")) %>% 
  rename(country_code = name) %>% 
  rename(date = dates) %>% 
  mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) %>% 
  mutate(ou_cut_src_match = paste(ou_cut_match, data_source, sep="_")) %>% 
  select(-ou_cut_match)
  
alldf <- allthedata

gisdata <- alldf %>% select(-ou_date_match, -ou_date_src_match) %>% 
  filter(date %in% max(date))

if (typex == "date"){
return(alldf)} else if (typex == "map"){
return(gisdata)}
  }
  
whodf <- final_hotspot(typex, "WHO") 
jhudf <- final_hotspot(typex, "JHU") 

trajdf <- bind_rows(whodf, jhudf) 

return(trajdf)
}

