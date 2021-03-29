# Hotspots for cumulative incidence or mortality
# Based on v4
# Original code by Kathryn Winglee
# Modified by Adam Vaughan & Michelle Schmitz / James Fuller / Imran Mujawar
# May 1, 2020 
# This version adopts the script for Power BI
# updated 9/24 with new algorithm from CBS GitHub page: https://github.com/cdcent/covid-response-analyses - Andrea Stewart
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function(typex, rfunctions.dir){

final_hotspot  <- function(typex, dfsource){

  # Take out all NAs in the dataset and replace with zero
remove_nas <- function(df) { 
  df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}

# dir.root<- ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")),
#                   paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/"),
#                   ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/")),
#                          paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/"),
#                          "Directory does not exist"))


# Power BI script that pulls in all the analytic datasets in long format by Country and Date
# Folder path for all the Power BI scripts
# rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")


# function to get the base JHU data with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data_vDASH.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ===== Beginning of Epi curve segmentation Function ===============
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
ldpkg(c("tidyverse",
        "passport",
        "readxl",
        "data.table",
        "zoo"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ NEW hotspot algorithm (from CBS github 9/24) ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(paste0(rfunctions.dir,"traj_sub_functions_vDASH.R"))

df.Countriesx <- fun_ncov(rfunctions.dir) # Pulling in case and death data
df.Countries.Daily <- df.Countriesx %>% filter(data_source %in% dfsource) 

#consolidate the df.Countries.Daily dataset as some areas have multiple observations
df.cases_fin <- df.Countries.Daily[c("country_code","Date","Population 2020","Cumulative Cases")] %>%   
  group_by(country_code, Date,`Population 2020`) %>% 
  summarise(Cumulative_Cases = sum(`Cumulative Cases`, na.rm=T)) %>% 
  ungroup() %>% 
  filter(`Population 2020`>0 & !is.na(`Population 2020`)) %>% 
  mutate(Cumulative_Cases=if_else(Cumulative_Cases<0,0,if_else(is.na(Cumulative_Cases),0,Cumulative_Cases))) %>%
  #select(-data_source,-country_code) %>%
  #rename to align with CBS code names
  rename(id=country_code,
         cases=Cumulative_Cases,
         pop=`Population 2020`,
         date=Date)

#DEATHS
df.deaths_fin <- df.Countries.Daily[
  c("country_code","Date","Population 2020",
    "Cumulative Deaths")] %>%   
  dplyr::group_by(country_code, Date, `Population 2020`) %>% 
  dplyr::summarise(Cumulative_Deaths = sum(`Cumulative Deaths`, na.rm=T)) %>% 
  ungroup() %>% 
  filter(`Population 2020`>0 & !is.na(`Population 2020`)) %>% 
  mutate(Cumulative_Deaths=if_else(Cumulative_Deaths<0,0,if_else(is.na(Cumulative_Deaths),0,Cumulative_Deaths))) %>%
  #select(-data_source,-country_code) %>%
  #rename to align with CBS code names
  rename(id=country_code,
         cases=Cumulative_Deaths,
         pop=`Population 2020`,
         date=Date)

alldata <- hotspot(df.cases_fin,"case", 0.1, 10, 100, 7, 0.6, dsource=dfsource)


allthedata <- alldata %>%
  mutate(data_source = dfsource,
         datatype="Case") %>%
  mutate(ou_date_match = paste(id, dates, sep="_")) %>%
  #Creating variable that is not date dependent
  mutate(ou_cut_match = paste(id, slope_cut, rate_cut, sep="_")) %>%
  rename(country_code = id) %>%
  rename(date = dates) %>%
  mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) %>%
  mutate(ou_cut_src_match = paste(ou_cut_match, data_source, sep="_")) %>%
  select(-ou_cut_match)

alldf <- allthedata

gisdata <- alldf %>% select(-ou_date_match, -ou_date_src_match) %>%
  #filter(date == "2020-12-06")
  filter(date %in% max(date))

if (typex == "z"){
return(alldf)} else if (typex == "map"){
return(gisdata)}
  }
  
whodf <- final_hotspot(typex, "WHO") 
jhudf <- final_hotspot(typex, "JHU") 

trajdf <- bind_rows(whodf, jhudf) 

return(trajdf)
}

