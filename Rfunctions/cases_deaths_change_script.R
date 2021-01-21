# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a RStudio Connect to create an API that is referenced by a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it returns the rate of change of cases and deaths, by country, based on several pre-defined time periods
# it requires two data frames called "ncov_data" which is the standardized case and death data set that can be generated using the "get_ncov_data.R" function in the directory
# and a data frame called "country_data" which is the standardized country information file that can be generated using the "get_country.R" function in the directory
# it will output two possible files: a cross-sectional file (requires "cross" for typex) or a longitudinal file (requires "series" for typex)
# Uncomment and run the code below to test the function
# rfunctions.dir<-"https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/Rfunctions/"
# fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
# fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
# ncov_data<-fun_ncov(rfunctions.dir)
# country_data<-fun_country()

# For the public version of the International views on the CDC Tracker, only cross sectional data and past 7, 14 and 30 days are produced. 
# Uncomment the referenced lines after 2) and 4) below to produce past 24 hours and series data

function(typex,ncov_data,country_data){
  
  # Script to create the following:
  # 1) View (Cases, Deaths)
  # 2) Metric (Counts, Rate, Cumulative)
  # 3) Period (1, 7, 14, 30)
  # 4) Source (JHU, WHO)
  

  # Getting country frame (get_country.R in the GitHub directory)
  ou_frame <- country_data %>% 
    dplyr::rename(country_code = iso3code) %>% 
    select(country, country_code) %>% 
    unique()
  
  # get base case and death data from JHU and WHO (get_ncov_data.R in the GitHub directory)
  # restructuring case and death dataset
  df <- ncov_data %>% 
    select(country_code, Date, 
           `Cumulative Cases`, `Cumulative Deaths`, 
           `Population 2018.x`,
           data_source) %>% 
    dplyr::rename(cases  = `Cumulative Cases`,
                  deaths = `Cumulative Deaths`,
                  pop    = `Population 2018.x`)
  
  
  # Setting up the filters for Power BI
  # 1) View (Cases, Deaths)
  indicat_vec <- c("cases", "deaths")
  # 2) Period (1, 7, 14, 30)
  # only need 7, 14, 30 for public views, limiting here for efficiency in RStudio Connect- uncomment below and delete the next line to include the past 24 hours
  # periodx_vec <- c(1, 7, 14, 30)
  periodx_vec <- c(7, 14, 30)
  # 3) Data source: JHU or WHO
  sourcex_vec <- c("JHU", "WHO") 
  # 4) Whether dataset is cross-sectional or time-series
  # only need cross for public views, limiting here for efficiency in RStudio Connect- uncomment below and delete the next line to include the time series
  # -James F revised this to also generate series data 1/21/2021
  datatyp_vec <- c("cross", "series")
  #datatyp_vec <- c("cross")
  
  change_ncov <- function(indicat , 
                         periodx , 
                         sourcex,
                         datatyp){
    
    dfx <- df %>% 
      filter(data_source %in% sourcex) %>% 
      gather(indicator, cumval, cases, deaths) %>% 
      filter(indicator %in% indicat) %>% 
      group_by(country_code) %>%
      arrange(Date) %>% 
      mutate(curr_val =  cumval - lag(cumval, periodx)) %>%
      mutate(prev_val =  lag(cumval, periodx) - lag(cumval, (periodx*2))) %>% 
      mutate(curr_date = Date) %>% 
      mutate(curr_datex = lag(Date, (periodx-1))) %>% 
      mutate(prev_date = lag(Date, periodx)) %>% 
      mutate(prev_datex = lag(Date, (periodx*2)-1)) %>% 
      ungroup() %>%
      mutate_at(vars(curr_val, prev_val), list(~replace_na(., 0))) %>% 
      mutate(change_val = curr_val - prev_val) %>%
      mutate(perc_change = if_else(prev_val>0, change_val/prev_val, 
        if_else(change_val>0, 99999.0, 0))) %>% 
      mutate(perc_changex = if_else(perc_change==99999.0, "Zero denominator",
        paste0(round(100*perc_change,1),"%"))) %>% 
      mutate(periodval = if_else(periodx==1, "24 hours",
                                         paste0(periodx, " days"))) %>%
      mutate(Indicator = case_when(
        indicator %in% c("deaths") ~ "Deaths",
        indicator %in% c("cases") ~  "Cases"
      )) %>% 
      mutate(curr_range = paste0(Indicator, " reported from ", (curr_datex), " to ", curr_date)) %>% 
      mutate(prev_range = paste0(Indicator, " reported from ", (prev_datex), " to ", prev_date)) %>% 
      select(Indicator, periodval, data_source,
             country_code, Date, pop, 
             change_val, perc_change, perc_changex, curr_val, prev_val, 
        prev_date, curr_date, prev_datex, curr_datex, curr_range, prev_range)
    
    xdfx <- dfx %>% filter(Date==max(df$Date))
    
    change_cuts <- c(-Inf, -.5, -.1, 0, .1, 1, Inf)
    
    xdfx$changecat <- cut(xdfx$perc_change, breaks = change_cuts, include.lowest=TRUE, 
labels = c(
  "decrease of more than 50%", 
  "decrease of 11-50%", 
  "decrease of 1-10%",
  "increase of 0-9%", 
  "increase of 10-99%", 
  "increase of 100%+"))
    
    xdfx$changecatx <- cut(xdfx$perc_change, breaks = change_cuts, include.lowest=TRUE, 
labels = c(
  "val1", 
  "val2", 
  "val3",
  "val4", 
  "val5", 
  "val6"))
    
    if (datatyp == "series"){
      return(dfx %>% mutate(dftype = datatyp))} else 
        if (datatyp == "cross"){
          return(xdfx %>% mutate(dftype = datatyp))}
  }
  
  
  # creating dataset with all permutations and combinations of variables
  veclist <- expand.grid(
    indicat = indicat_vec,
    periodx = periodx_vec,
    sourcex = sourcex_vec,
    datatyp = datatyp_vec, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = F)
  
  # running function on all combinations
  dchange <- purrr::pmap(veclist, change_ncov)
  
  # Binding the output dataframes together and cleaning names
  finaldfx <- dplyr::bind_rows(dchange) %>% 
    filter(periodval %in% c("14 days", "30 days", "7 days")) %>% 
    select(Indicator, periodval, data_source, 
           country_code, Date, pop, changecat, changecatx,
      change_val, perc_change, perc_changex, curr_val, prev_val,           
      prev_date, curr_date, prev_datex, curr_datex, curr_range, prev_range, dftype)
  

  finaldfxx <- left_join(finaldfx, ou_frame)
  
   if (typex == "series"){return(finaldfxx %>% filter(dftype %in% c("series")))} else 
     if (typex == "cross"){return(finaldfxx %>% filter(dftype %in% c("cross")))}

}

