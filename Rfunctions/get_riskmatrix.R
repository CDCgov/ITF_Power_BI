function(typex, rfunctions.dir){

  # dir.root<- ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")),
  #                   paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/"),
  #                   ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/")),
  #                          paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/"),
  #                          "Directory does not exist"))
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

# folder where all R functions are housed
# rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")

# function to get the base JHU data with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data_vDASH.R"))
fun_rt   <- dget(paste0(rfunctions.dir, "Rt_ncov.R"))

# Load/install packages 
ldpkg(c("tidyverse"))


# Getting the datasets
ncov <- fun_ncov(rfunctions.dir)  # The cases and deaths
rt  <- fun_rt(rfunctions.dir)     # Rt values (7-day sliding window)

df <- left_join(ncov %>% select(-ou_date_match), 
                rt   %>% select(ou_date_src_match, mean.mtf))

df1 <- df %>% 
  mutate(cases_cum = if_else(is.na(`Cumulative Cases`), 0, `Cumulative Cases`)) %>% 
  group_by(data_source, country_code) %>%
  arrange(Date) %>% 
  mutate(wkcase =  cases_cum - lag(cases_cum, 7)) %>% 
  ungroup() %>% 
  # zero out negative values
  mutate_if(is.numeric, ~replace(., .<0, 0)) %>% 
  mutate(inci = if_else(`Population 2020`>0, 
                        ((wkcase/`Population 2020`)/7)*100000, 
                        NA_real_)) %>% 
  rename(Rt = mean.mtf) %>% 
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





