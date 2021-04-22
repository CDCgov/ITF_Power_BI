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

function(typex, rfunctions.dir, df_ncov){
  
  # If base data frame is missing, then call the script to generate it
  if (missing(df_ncov)) {
    # Function to get base dataframe with cases and deaths daily/cumulative
    fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
    df_ncov <- fun_ncov(rfunctions.dir)
  }
  
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  # Loading the packages
  ldpkg(c("tidyverse",
          "passport",
          "readxl",
          "data.table",
          "zoo"))

final_hotspot  <- function(typex, dfsource){

  # Take out all NAs in the dataset and replace with zero
remove_nas <- function(df) { 
  df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ NEW hotspot algorithm (from CBS github 9/24) ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(paste0(rfunctions.dir,"traj_sub_functions.R"))

df.Countriesx <- df_ncov # Pulling in case and death data
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

