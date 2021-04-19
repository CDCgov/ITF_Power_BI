function(typex, rfunctions.dir, df_ncov, df_rt_ncov){

  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))

  # Load/install packages 
  ldpkg(c("tidyverse"))


  # If NCOV base dataframe is missing as input, then call the script to generate it
  if (missing(df_ncov)) {
    # Function to get NCOV base data - cases and deaths
    fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
    df_ncov <- fun_ncov(rfunctions.dir)
  }
  
  # If RT NCOV dataframe is missing as input, then call the script to generate it
  if (missing(df_rt_ncov)) {
    # Function to get RT NCOV values (7-day sliding window)
    fun_rt_ncov <- dget(paste0(rfunctions.dir, "Rt_ncov.R"))
    df_rt_ncov <- fun_rt_ncov(rfunctions.dir)
  }


  df <- left_join(df_ncov %>% select(-ou_date_match), 
                  df_rt_ncov   %>% select(ou_date_src_match, mean.mtf))
  
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
  
  if (typex == "date" ){
    return(df1)
  } else if (typex == "cross") {
    return(crossx)
  }
}





