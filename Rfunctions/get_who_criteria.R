# 
# Script to create binary variables for Mitigation re-opening for WHO criteria
# 
# 
# Created by Imran Mujawar (lrz5@cdc.gov) 
# Date: July 10, 2020
# Modified: July 22, 2020
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function(rfunctions.dir, df_ncov, df_testing_long, df_rt_ncov){

  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  
  # Loading the packages
  ldpkg(c("Hmisc", 
          "tidyverse",
          "openxlsx",
          "passport",
          "zoo"))
  
  # If NCOV base dataframe is missing as input, then call the script to generate it
  if (missing(df_ncov)) {
    # Function to get NCOV base data - cases and deaths
    fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
    df_ncov <- fun_ncov(rfunctions.dir)
  }
  
  # If testing long dataframe is missing as input, then call the script to generate it
  if (missing(df_testing_long)) {
    # Function to get COVID Testing
    fun_testing <- dget(paste0(rfunctions.dir, "covid_testing.R"))
    df_testing_long <- fun_testing("long", rfunctions.dir)
  }
  
  # If RT NCOV base dataframe is missing as input, then call the script to generate it
  if (missing(df_rt_ncov)) {
    # Function to get Rt NCOV - cases and deaths
    fun_rt_ncov <- dget(paste0(rfunctions.dir, "Rt_ncov.R"))
    df_rt_ncov <- fun_rt_ncov(rfunctions.dir)
  }

 dfx <- df_ncov %>% 
   mutate(cases_cum = if_else(is.na(`Cumulative Cases`), 0, `Cumulative Cases`)) %>% 
   mutate(deaths_cum = if_else(is.na(`Cumulative Deaths`), 0, `Cumulative Deaths`)) %>% 
   mutate_if(is.numeric, ~replace(., .<0, 0)) %>% 
   group_by(data_source, country_code) %>%
   arrange(Date) %>% 
   mutate(wkcase =  cases_cum - lag(cases_cum, 7)) %>% 
   mutate(prev_wkcase =  lag(cases_cum, 7)-lag(cases_cum, 14)) %>% 
   mutate(wkdeath =  deaths_cum - lag(deaths_cum, 7)) %>%
   ungroup() %>% 
   mutate_if(is.numeric, ~replace(., .<0, NA_real_)) %>%
   group_by(data_source, country_code) %>% 
   arrange(Date) %>%
   # Moving average of weekly cases and weekly deaths
   mutate(mavr_wk = rollmean(wkcase, k = 7, fill = NA, align = "right"),
          mort_wk = rollmean(wkdeath, k = 7, fill = NA, align = "right")) %>% 
   ungroup() %>% 
   mutate(mavr_wk_inci = if_else(`Population 2020` > 0, (mavr_wk/`Population 2020`)*100000, NA_real_)) %>% 
   # getting variable for growth and decline
   group_by(data_source, country_code) %>% 
   arrange(Date) %>%
   mutate(trajx = case_when(
     mavr_wk_inci>lag(mavr_wk_inci) ~ "growth", 
     mavr_wk_inci<lag(mavr_wk_inci) ~ "decline", 
     mavr_wk_inci==lag(mavr_wk_inci) ~ "plateau", 
     TRUE ~ "not estimated")) %>% 
   mutate(traj = case_when(
     mavr_wk_inci>lag(mavr_wk_inci) ~ "growth", 
     mavr_wk_inci<lag(mavr_wk_inci) ~ "decline",
     mavr_wk_inci==lag(mavr_wk_inci) ~ NA_character_, 
     TRUE ~ "not estimated")) %>%  
   mutate(mortraj=case_when(
     mort_wk>lag(mort_wk) ~ "growth", 
     mort_wk<lag(mort_wk) ~ "decline",
     mort_wk==lag(mort_wk) ~ "plateau",
     TRUE ~ "not estimated")) %>%
   ungroup() %>% 
   # Change plateaus flanked by growth or decline as growth or decline respectively
   arrange(data_source, country_code, Date) %>% 
   fill(traj, .direction = "down") %>% 
   # arrange(data_source, country_code, Date) %>%
   mutate(numdaysx = if_else(traj == lag(traj,1), 1, 0)) %>% 
   mutate(numdays_mort=if_else(mortraj==lag(mortraj,1),1,0)) %>%
   mutate(varx = 1) %>% 
   mutate(varx_mort = 1) %>% 
   # Getting growth infection points
   mutate(growth_inflect = if_else(numdaysx==1 & lead(numdaysx,1)==0 &
            traj %in% c("growth"), 1, 0)) 
  
 

# Setting up counter for getting days in each category 
for(i in c(1:length(dfx$varx))){   #day number times the number of days it's been on a trajectory,
  if(i==1){dfx$varx[i] = 1} else {  # plus 1 becasue the second day is listed as day 1
  dfx$varx[i] <- (dfx$varx[i-1]*dfx$numdaysx[i])+1 
 }} 
 for(i in c(1:length(dfx$varx_mort))){ #same thing for mortality 
   if(i==1){dfx$varx_mort[i] = 1} else {
     dfx$varx_mort[i] <- (dfx$varx_mort[i-1]*dfx$numdays_mort[i])+1 
   }}
 
df1 <- dfx %>% 
  # get last peak incidence value 
  mutate(lastpeak_inc = if_else(growth_inflect==1, mavr_wk_inci, NA_real_)) %>% 
  arrange(data_source, country_code, Date) %>% 
  fill(lastpeak_inc, .direction = "down") %>% 
  mutate(epi_cat = case_when(
                       growth_inflect==1                               ~"Peak",
                       traj %in% c("decline") & varx>=21 &  
                            mavr_wk_inci<=(0.5*lastpeak_inc)           ~"Decline (criteria met)",
                       trajx %in% c("growth")                          ~"Growth",
                       trajx %in% c("decline") & varx<21               ~"Decline (<3 wks)",
                       traj %in% c("decline") & varx>=21 &  
                            mavr_wk_inci>(0.5*lastpeak_inc)           ~"Decline (3+wks, criteria not met)",
                       trajx %in% c("plateau")                         ~"Plateau",  
                       TRUE                                            ~ "not estimated")) %>%
  mutate(death_cat = case_when(
                      mortraj %in% c("decline") & varx_mort>=21           ~"Decline (criteria met)",
                      mortraj %in% c("growth")                          ~"Growth",
                      mortraj %in% c("decline") & varx_mort<21               ~"Decline (<3 wks)",
                      mortraj %in% c("plateau")                         ~"Plateau",  
                      TRUE                                            ~ "not estimated")) 


#testing criteria: <= 5% positive for 2 weeks
names(df_testing_long)[which(colnames(df_testing_long)=="source")] <- "Source_testing"
dfx2 <- merge(df1,df_testing_long,by=c("ou_date_match"),all.x=T)
k=15
dfx2 <- dfx2 %>%
  #group_by(data_source,country_code) %>%
  arrange(Source_testing,data_source, country_code, Date) %>%
  mutate(test_5 =  perc_positive_testing < 5) %>% 
  mutate(test_5_3weeks = rollapplyr(test_5, k, function(x) all(x[k]==T & (x[k] == x[-k])), fill = NA)) %>% 
  mutate(who_criteria_testing=ifelse(test_5_3weeks==T, "Criteria Met","Criteria Not Met")) %>%
  ungroup()
##New addition
last_date <- dfx2 %>%
  filter(!is.na(who_criteria_testing)) %>%
  arrange(Source_testing,data_source, country_code, Date) %>%
  group_by(Source_testing,data_source, country_code) %>%
  summarise(who_criteria_testing_last=last(who_criteria_testing) ,
            date_criteria_testing_last=last(Date)) %>%
  ungroup()
dfx2 <- left_join(dfx2,last_date)

dfx3 <- merge(dfx2,df_rt_ncov,by=c("ou_date_src_match"),all.x=T)

k=15
dfx3 <- dfx3 %>%
  arrange(data_source.x, country_code, Date) %>%
  mutate(rt_1 = mean.mtf  < 1) %>% 
  mutate(rt_1_2weeks = rollapplyr(rt_1, k, function(x) all(x[k]==T & (x[k] == x[-k])), fill = NA)) %>% 
  mutate(who_criteria_Rt=ifelse(rt_1_2weeks==T, "Criteria Met","Criteria Not Met")) %>%
  ungroup()



#Decline in deaths for 3 weeks
k=22
dfx4 <- dfx3 %>%
  mutate(case_fatality = ifelse(cases_cum == 0, NA, deaths_cum/cases_cum)) %>%
  #group_by(data_source,country_code) %>%
  arrange(data_source.x, country_code, Date) %>%
  mutate(death_dec =  case_fatality < lag(case_fatality)) %>% 
  mutate(death_diff2 = rollapplyr(death_dec, k, function(x) all(x[k]==T & (x[k] == x[-k])), fill = NA)) %>% 
  mutate(who_criteria_death_decs=ifelse(death_diff2==T, "Criteria Met","Criteria Not Met")) %>%
  ungroup()

dfx4$who_criteria_cases <- "Criteria Not Met"
dfx4$who_criteria_cases[dfx4$epi_cat=="Decline (criteria met)"] <- "Criteria Met"
dfx4$who_criteria_death_decs2 <- "Criteria Not Met"
dfx4$who_criteria_death_decs2[dfx4$death_cat=="Decline (criteria met)"] <-"Criteria Met"

# namesdf <- c("Country","Date","WHO Region","Population 2020","Country Code","ou_date_src_match","ou_date_match.x",
#            "New Cases","Cumulative Cases","who_criteria_cases","New Deaths","Cumulative Deaths","who_criteria_death_decs2",
#            "wkcase","prev_wkcase","wkdeath","new_tests","total_tests","perc_positive_testing","who_criteria_testing",
#           "who_criteria_testing_last","date_criteria_testing_last", "mean.mtf","mean.ni","mean.li","who_criteria_Rt","case_fatality","who_criteria_death_decs",
#            "epi_cat","death_cat","Source_testing")

# Removing a lot of the columns in this dataset b/c it is too large, many columns are duplicates from other data tables
namesdf <- c("Country","Date","WHO Region","Country Code","ou_date_src_match","ou_date_match.x",
             "who_criteria_cases","who_criteria_death_decs2","perc_positive_testing","who_criteria_testing",
             "who_criteria_Rt","who_criteria_death_decs","Source_testing")
vars <- names(dfx4) %in% namesdf
finaldf <- dfx4[vars]

finaldf
}
 
 


