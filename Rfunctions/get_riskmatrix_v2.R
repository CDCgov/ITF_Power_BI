# Code for CDC Tracker Power BI file
# this code is called by an R script run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it will output two possible files: a cross-sectional file (requires "cross" for typex) or a longitudinal file (requires "long" for typex)
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_ncov_data.R", is stored

function(typex, rfunctions.dir){

 # Function to load
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

# function to get the base JHU data with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))

# Load/install packages 
ldpkg(c("tidyverse"))


# Getting the datasets
ncov <- fun_ncov(rfunctions.dir)  # The cases and deaths

df <- ncov %>% select(-ou_date_match)

df1 <- df %>% 
  mutate(cases_cum = if_else(is.na(`Cumulative Cases`), 0, `Cumulative Cases`)) %>% 
  mutate_if(is.numeric, ~replace(., .<0, 0)) %>% 
  group_by(data_source, country_code) %>%
  arrange(Date) %>% 
  mutate(wkcase =  cases_cum - lag(cases_cum, 7)) %>% 
  mutate(prev_wkcase =  lag(cases_cum, 7)-lag(cases_cum, 14)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, ~replace(., .<0, NA_real_)) %>% 
  mutate(case_diff = wkcase-prev_wkcase) %>% 
  mutate(wkcase_change = if_else(prev_wkcase>0,
                                 (case_diff)/prev_wkcase,
                                 NA_real_)) %>% 
  mutate(inci = if_else(`Population 2018.x`>0, 
                        ((wkcase/`Population 2018.x`)/7)*100000, 
                        NA_real_)) %>% 
  mutate(Rt = 1) %>%
  mutate(ou_src_match = paste(country_code, data_source, sep="_"))

crossx <- df1 %>% select(-ou_date_src_match) %>% 
  group_by(data_source) %>% 
  mutate(datex = max(Date)) %>% 
  ungroup() %>% 
  filter(Date == datex) %>% 
  select(-datex)

if (typex == "date"){
  return(df1)} else if (typex == "cross"){
    return(crossx)}
}





