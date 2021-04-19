# Code for CDC Tracker Power BI file
# this code is called by an R script run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it will output two possible files: a cross-sectional file (requires "cross" input) or a longitudinal file (requires "long")
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_country.R", is stored

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(z,rfunctions.dir, df_country){
   # Creating the 'not in' function
  `%ni%` <- Negate(`%in%`) 
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  # Loading the packages
  ldpkg(c("tidyverse","zoo"))
  
  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}
  
  # If country metadata dataframe not present as input, then call the script to generate it
  if (missing(df_country)) {
    # Function to get the country metadata 
    fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
    df_country <- fun_country(rfunctions.dir)
  }


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
  find<-read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")
  
  iso3<-maps::iso3166 %>%
    mutate(a3=case_when(ISOname=="Kosovo"~"XKX", TRUE~a3),
           a2=case_when(ISOname=="Namibia"~"NA", TRUE~a2)) %>%
    select(a2,a3) %>%
    rename(unit=a2,
           alpha3=a3)
  
  findlong<-filter(find,set=="country") %>%
    mutate(unit=if_else(name=="Namibia","NA",unit)) %>%
    left_join(iso3) %>%
    mutate(date=as.character(time),
           Date=as.Date(time),
           ou_date_match=paste(alpha3,Date,sep="_"),
           last30days=if_else(Sys.Date()-Date<30,1,0)) %>%
    #Rolling average to match OWID 
    arrange(unit, name, date) %>%
    group_by(unit, name) %>%
    #if the case data haven't been updated for the current date yet, but there is testing data, drop the latest testing data until the case data get updated
    filter(!(date==max(date) & !is.na(all_cum_tests) & is.na(all_cum_cases))) %>%
    mutate(adj_total_cases=if_else(!is.na(all_cum_tests),all_cum_cases,NA_real_),
           new_tests_smoothed = pop_100k*cap_new_tests,
           new_tests_smoothed_per_thousand=1000*cap_new_tests,
           total_tests_per_thousand=1000*all_cum_tests/(100000*pop_100k),
           new_tests_per_thousand=1000*new_tests_orig/(100000*pop_100k),
           total_cases_per_million=1000000*all_cum_cases/(100000*pop_100k),
           total_deaths_per_million=1000000*all_cum_deaths/(100000*pop_100k),
           new_cases_per_million=1000000*new_cases_orig/(100000*pop_100k),
           new_deaths=all_cum_deaths-lag(all_cum_deaths,n=1),
           tests_per_case=new_tests_smoothed/new_tests_orig,
           new_deaths_per_million=1000000*new_deaths/(100000*pop_100k),
           new_cases_smoothed=pop_100k*cap_new_cases,
           population=100000*pop_100k) %>%
    ungroup() %>%
    rename(total_cases=all_cum_cases,
           total_tests=all_cum_tests,
           total_deaths=all_cum_deaths,
           iso3code=alpha3,
           new_tests=new_tests_orig,
           new_cases=new_cases_orig) %>%
    select(-set,-name,-unit,-time,-pop_100k,-cap_cum_cases,-cap_new_cases,-cap_cum_deaths,-cap_new_deaths,-cap_cum_tests,-cap_new_tests,
           -all_new_cases,-all_new_deaths,-new_deaths_orig,-cap_cum_tests,-all_new_tests,-pos,-Date) %>%
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
   
  testinglong <- testinglong %>%
    mutate(perc_positive_testing=(new_cases_smoothed/new_tests_smoothed)*100) %>%
    mutate(tests_per_case=ifelse((is.nan(tests_per_case) | is.infinite(tests_per_case)),NA,tests_per_case)) %>%
    mutate(perc_positive_testing=ifelse((is.nan(perc_positive_testing) | is.infinite(perc_positive_testing)),NA,perc_positive_testing)) 
  
  
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
  
  
  tst_crossx1 <- left_join(tst_cross, test_type) %>%
    left_join(tst_cross30,by="ou_src_match") %>%
    left_join(tst_lastdate_newtests,by="ou_src_match") %>%
    #if new tests weren't reported, use the last date with a total reported:
    mutate(last_date_new_tests=if_else(is.na(last_date_new_tests),last_date,last_date_new_tests))
  
    #NEW addition
  #add percent positive
  tst_percentpos <- testinglong %>%
    filter(!(is.na(perc_positive_testing))) %>%
    arrange(iso3code,date) %>%
    group_by(ou_src_match) %>% 
    summarise(perc_positive_testing=last(perc_positive_testing)) %>%
    ungroup() 
  
  tst_crossx <- left_join(tst_crossx1,tst_percentpos) 
   
   
  #get standardized country and continent names
  geodf <- df_country %>%
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
