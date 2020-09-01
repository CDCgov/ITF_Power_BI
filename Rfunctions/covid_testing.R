# Code for CDC Tracker Power BI file
# this code is called by an R script run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it will output two possible files: a cross-sectional file (requires "cross" input) or a longitudinal file (requires "long")
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_country.R", is stored

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(z,rfunctions.dir){
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
  ldpkg(c("tidyverse","zoo"))
  
  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Getting Testing data from OurWorldinData ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  testing <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", stringsAsFactors = FALSE)
  
  testing$Date <- as.Date(testing$date)
  
  tstx <- testing %>% 
    filter(location %ni% c("World", "International")) %>% 
  mutate(ou_date_match = paste(iso_code, Date, sep="_"),
         last30days=if_else(Sys.Date()-Date<30,1,0),
         iso_code=if_else(location=="Kosovo","XKX",iso_code)) %>% 
    select(-Date) %>% 
    mutate(adj_total_cases = if_else(!is.na(total_tests), total_cases, NA_real_),
           cvd_death_rate=cardiovasc_death_rate) %>% 
    rename(iso3code = iso_code)
  
  
  #get data from FIND and format in the same way as the OWID
  find<-read_csv("https://raw.githubusercontent.com/dsbbfinddx/FIND_Cov_19_Tracker/master/input_data/cv_data_download.csv")
  
  findlong<-find %>%
    filter(!is.na(alpha3)) %>%
    mutate(date=as.character(date),
           Date=as.Date(date),
           ou_date_match=paste(alpha3,Date,sep="_"),
           last30days=if_else(Sys.Date()-Date<30,1,0)) %>%
    #Rolling average to match OWID 
    arrange(alpha3, country, date) %>%
    group_by(alpha3, country) %>%
    #if the case data haven't been updated for the current date yet, but there is testing data, drop the latest testing data until the case data get updated
    filter(!(date==max(date) & !is.na(tests_cumulative) & is.na(cases))) %>%
    # #here is code if you want to replace with the day before- not recommended and could be misleading and confusing
    # mutate(adj_total_cases= if_else(!is.na(tests_cumulative) & !is.na(lag(cases,n=1)), 
    #                                   case_when(is.na(cases) & date==max(date)~lag(cases,n=1),TRUE~cases),
    #                                   NA_real_)) %>%
    mutate(adj_total_cases=if_else(!is.na(tests_cumulative),cases,NA_real_),
           new_tests_smoothed = rollmean(new_tests, k = 7, fill = NA),
           new_tests_smoothed_per_thousand=1000*new_tests_smoothed/population,
           total_tests_per_thousand=1000*tests_cumulative/population,
           new_tests_per_thousand=1000*new_tests/population,
           total_cases_per_million=1000000*cases/population,
           total_deaths_per_million=1000000*deaths/population,
           new_cases_per_million=1000000*new_cases/population,
           new_deaths=deaths-lag(deaths,n=1),
           new_deaths_per_million=1000000*new_deaths/population) %>%
    ungroup() %>%
    rename(total_cases=cases,
           total_tests=tests_cumulative,
           total_deaths=deaths,
           iso3code=alpha3) %>%
    select(-country,-Continent,-testsPer100k,-source,-deathsPer100k,-casesPer100k,-`Income group`,-Date,-Region) %>%
    mutate(source="FIND",
           tests_units="FIND data- Unknown")
  
  owid<-tstx %>%
    select(iso3code:tests_units,population,ou_date_match,adj_total_cases,last30days,-location,-continent) %>%
    mutate(source="OWID")
  
  owid_covs<- tstx %>%
    select(ou_date_match,stringency_index,population_density:life_expectancy,cvd_death_rate) %>%
    unique()
    
  testinglong<-bind_rows(owid,findlong) %>%
    left_join(owid_covs,by="ou_date_match") %>%
    mutate(ou_src_match=paste(iso3code,source,sep="_"))
    
  
  #get cumulative testing results for just the past 30 days- number of tests, tests per pop, number of cases
  tst_cross30<-testinglong %>%
    filter(last30days==1) %>%
    group_by(ou_src_match,population) %>%
    summarise(cum_tests_last_30=sum(new_tests,na.rm=TRUE),
              cum_cases_last_30=sum(new_cases,na.rm=TRUE)) %>%
    mutate(cum_tests_last_30_100thousand=100000*cum_tests_last_30/population) %>%
    ungroup() %>%
    select(ou_src_match,cum_tests_last_30,cum_cases_last_30,cum_tests_last_30_100thousand)

  #get summary statistics for cross-sectional reporting
  tst_cross <- testinglong %>% 
    filter(!is.na(adj_total_cases)) %>% 
    group_by(iso3code, source) %>% 
    summarise(last_date = max(date, na.rm=T),
                     cum_cases = max(adj_total_cases, na.rm=T),
                     cum_tests = max(total_tests, na.rm=T),
                     cum_test_100thousand = max(total_tests_per_thousand, na.rm=T)*100,
                     cum_cases_100thousand = max(total_cases_per_million, na.rm=T)*0.1,
                     population=max(population)) %>%
                ungroup() 
  
  #get the type of tests that have been reported (OWID only)
  test_type <- testinglong %>% 
    filter(!is.na(total_tests) & !is.na(tests_units)) %>% 
    arrange(ou_date_match, desc(date)) %>%
    group_by(iso3code,source) %>% 
    mutate(snum = row_number()) %>% 
    ungroup() %>%  
    filter(snum==1) %>% 
    select(iso3code, source,ou_src_match,tests_units) 
  
  #get the last date that any new tests were reported in the data (FIND data sometimes has new cases but no new tests)
  tst_lastdate_newtests<- testinglong %>%
    filter(!is.na(new_tests) & new_tests>0) %>% 
    group_by(ou_src_match) %>% 
    summarise(last_date_new_tests = max(date, na.rm=T)) %>%
    ungroup()
  
  
  tst_crossx <- left_join(tst_cross, test_type) %>%
    left_join(tst_cross30,by="ou_src_match") %>%
    left_join(tst_lastdate_newtests,by="ou_src_match") %>%
    #if new tests weren't reported, use the last date with a total reported:
    mutate(last_date_new_tests=if_else(is.na(last_date_new_tests),last_date,last_date_new_tests))
  
  #get standardized country and continent names
  #function to get the country metadata 
  fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
  geodf <- fun_country() %>%
    select(iso3code,country,Continent_Name, who_region) %>%
    rename(location=country,
           continent=Continent_Name)
  
  tst_crossx<-left_join(tst_crossx, geodf)
  
  testinglong<-left_join(testinglong,geodf)

  if(z=="cross"){
    return(tst_crossx)
  } else if(z=="long"){
    return(testinglong)}
}
