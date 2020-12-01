# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a RStudio Connect to create an API that is referenced by a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI:
# source("https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/Rfunctions/packages%20for%20Power%20BI.R")
# it returns a time series of the the trajectory classification of epi curves according to the CDC Case Surveillance Task Force methodology, by country
# it requires a data frame called "traj_out_series" which is the output data set that can be generated using the "trajectory_function_final_v2.R" function in the directory -> NOTE: THIS REQUIRES the package "data.table" to be installed to run
# see the bottom commented lines for the code to run to generate the two underlying data tables of the ITF Power BI Tracker views

#uncomment the code below to test the function
# rfunctions.dir<-"https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/Rfunctions/"
# traj_series <- dget(paste0(rfunctions.dir, "trajectory_function_final_v2.R"))
# fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
# traj_out_series<-traj_series("date",rfunctions.dir) #needs library(data.table)
# country_data <-fun_country()

function(traj_out_series,country_data){

  
#classification labels function for trajectory categories
  hbg.7labels <- c("High burden and growing", 
                    "High burden and not growing",
                    "Not high burden but growing", 
                    "Not high burden and not growing",
                    "<5 cases reported in the past 2 weeks"                )
  
  
# Creating HBG variable
fun_hgb7cat <- function(df){
  #  create categories for burden and growing categories
  df$ci2wk.2cat <- as.numeric(cut(as.numeric(df$ci.2wk), breaks = c(0, 100, Inf),
                                      include.lowest=TRUE), level=c("low","high")) # 1=low, 2=high
  df$slope.2cat <- as.numeric(cut(as.numeric(df$daily.ci.change), breaks = c(-Inf,0.1,Inf),
                                      include.lowest=TRUE), level=c("low","high")) # 1=low, 2=high

  df$hbg.7cat <- 7 
  
  df$hbg.7cat[df$ci2wk.2cat==2 & df$slope.2cat == 2] <-1
  df$hbg.7cat[df$ci2wk.2cat==2 & df$slope.2cat == 1] <-2
  df$hbg.7cat[df$ci2wk.2cat==1 & df$slope.2cat == 2] <-3
  df$hbg.7cat[df$ci2wk.2cat==1 & df$slope.2cat == 1] <-4
  df$hbg.7cat[df$hbg.cat %in% c("lessthan5"   )] <- 5
  df$hbg.7cat[df$hbg.cat %in% c("nonepast2wk" )] <- 5
  df$hbg.7cat[df$hbg.cat %in% c("none"        )] <- 5
  
  

  df$hbg.7catx <- "<5 cases in the past 2 weeks"
  
  df$hbg.7catx[df$hbg.7cat==1] <- hbg.7labels[1] 
  df$hbg.7catx[df$hbg.7cat==2] <- hbg.7labels[2] 
  df$hbg.7catx[df$hbg.7cat==3] <- hbg.7labels[3] 
  df$hbg.7catx[df$hbg.7cat==4] <- hbg.7labels[4] 
  df$hbg.7catx[df$hbg.7cat==5] <- hbg.7labels[5]
  # df$hbg.7catx[df$hbg.7cat==6] <- hbg.7labels[6]
  # df$hbg.7catx[df$hbg.7cat==7] <- hbg.7labels[7]

    return(df)
}

tseries <- fun_hgb7cat(traj_out_series)

# country data
ou_frame <- country_data %>% 
  dplyr::rename(country_code = iso3code) %>% 
  select(country, country_code) %>% 
  unique()

# Sovling issue with tooltips for greyed out entities on map
tseriesx <- left_join(ou_frame, tseries)

return(tseriesx)

}

# #plumber script
# tseriesx<-trajectory_output(traj_out_series,country_data)
# 
# #classification labels function for trajectory categories
# hbg.7labels <- c("High burden and growing", 
#                  "High burden and not growing",
#                  "Not high burden but growing", 
#                  "Not high burden and not growing",
#                  "<5 cases reported in the past 2 weeks")
# 
# # get longitudinal data (i.e. curve data)
# tseriesx.out <- tseriesx %>%
#   mutate(hbg.7catx = if_else(is.na(hbg.7catx), hbg.7labels[5], hbg.7catx)) %>% 
#   mutate(hbg.7cat = if_else(is.na(hbg.7cat), 5, hbg.7cat)) %>% 
#   mutate_if(is.numeric, list(~replace_na(., 0)))

# # get cross-sectional data (i.e. map data)
# tcross<-tseriesx %>% select(-ou_date_match, -ou_date_src_match) %>% filter(!is.na(date)) %>% filter(date %in% max(date))
# 
# 
