# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(rfunctions.dir) {
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
          "zoo",
          "passport",
          "stringr"))

  #Get Case Data
  fun_get_data <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
  ncov_data_raw <- fun_get_data(rfunctions.dir)
  ncov_data <- ncov_data_raw %>%
    select(!`Country Code`) %>%
    rename_all(tolower) %>%
    rename_all(gsub, pattern=" ", replacement="_") %>%
    rename(population_2020 = population_2018.x) %>%
    mutate(new_cases = case_when(new_cases < 0 ~ 0, new_cases >= 0 ~ new_cases),
           new_deaths = case_when(new_deaths < 0 ~ 0, new_deaths >= 0 ~ new_deaths)) %>%
    mutate(mort = 100000 * new_deaths / population_2020,
           inc = 100000 * new_cases / population_2020,
           mort_cum = 100000 * cumulative_deaths / population_2020,
           inc_cum = 100000 * cumulative_cases / population_2020) %>%
    mutate(ou_date_match = paste(country_code, date, sep="_")) %>%
    filter(population_2020>0) %>%
    arrange(country, data_source, date) %>%
    group_by(country, data_source) %>%
    mutate(inc_ma7 = rollmean(inc, 7, align='right',fill=NA),
           mort_ma7 = rollmean(mort, 7, align='right',fill=NA)) %>%
    filter(date >= min(date)+6) %>%
    ungroup() %>%
    select(date, country, country_code, ou_date_match, data_source, inc_ma7, mort_ma7) %>%
    pivot_longer(cols=c("inc_ma7", "mort_ma7"), names_to="cases_death_type", values_to="cases_death_value")

  #Get Policy Stringency Data
  df.oxford.raw <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")
  df.oxford <- df.oxford.raw %>%
    rename_all(tolower) %>%
    filter(jurisdiction == "NAT_TOTAL") %>%
    select(countryname, countrycode, date, stringencyindex) %>%
    mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
    mutate(ou_date_match = paste(countrycode, date, sep="_")) %>%
    rename(country_name = countryname,
           country_code = countrycode) 


  #Get Movement Data
  df.movement.raw <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", encoding="UTF-8")
  df.movement <- df.movement.raw %>%
    rename_all(tolower) %>%
    select_all(~gsub("_percent_change_from_baseline","",.)) %>%
    filter(sub_region_1=="" & sub_region_2=="" & metro_area=="") %>%
    rename(country = country_region) %>%
    mutate(date = as.Date(date)) %>%
    mutate(country_code = parse_country(country, to="iso3c")) %>%
    mutate(ou_date_match = paste(country_code, date, sep="_")) %>%
    arrange(country_code, date) %>%
    group_by(country_code) %>%
    mutate(retail_and_recreation = rollmean(retail_and_recreation, 7, align="right", fill=NA)) %>%
    mutate(grocery_and_pharmacy = rollmean(grocery_and_pharmacy, 7, align="right", fill=NA)) %>%
    mutate(parks = rollmean(parks, 7, align="right", fill=NA)) %>%
    mutate(transit_stations = rollmean(transit_stations, 7, align="right", fill=NA)) %>%
    mutate(workplaces = rollmean(workplaces, 7, align="right", fill=NA)) %>%
    mutate(residential = rollmean(residential, 7, align="right", fill=NA)) %>%
    filter(date >= min(date)+6) %>%
    ungroup() %>%
    pivot_longer(cols=c("retail_and_recreation", "grocery_and_pharmacy", "parks", "transit_stations", "workplaces", "residential"), names_to="mobility_type", values_to="mobility_value") %>%
    select(date, country, country_code, ou_date_match, mobility_type, mobility_value)
    
  overlay_list <- list("cases_deaths" = ncov_data, "stringency" = df.oxford, "mobility" = df.movement)
  
  return(overlay_list)
}
