function(typex, rfunctions.dir, df_ncov){

  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))

  # If NCOV base dataframe is missing as input, then call the script to generate it
  if (missing(df_ncov)) {
    # Function to get NCOV base data - cases and deaths
    fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
    df_ncov <- fun_ncov(rfunctions.dir)
  }
  
  # Load/install packages 
  ldpkg(c("tidyverse"))

  df <- df_ncov %>% select(-ou_date_match)
  
  df1 <- df %>% 
    mutate(cases_cum = if_else(is.na(`Cumulative Cases`), 0, `Cumulative Cases`)) %>% 
    mutate(deaths_cum = if_else(is.na(`Cumulative Deaths`), 0, `Cumulative Deaths`)) %>% 
    mutate_if(is.numeric, ~replace(., .<0, 0)) %>% 
    group_by(data_source, country_code) %>%
    arrange(Date) %>% 
    mutate(wkcase =  cases_cum - lag(cases_cum, 7)) %>% 
    mutate(prev_wkcase =  lag(cases_cum, 7)-lag(cases_cum, 14)) %>% 
    mutate(wkdeath =  deaths_cum - lag(deaths_cum, 7)) %>% 
    mutate(prev_wkdeath =  lag(deaths_cum, 7)-lag(deaths_cum, 14)) %>% 
    ungroup() %>% 
    mutate_if(is.numeric, ~replace(., .<0, NA_real_)) %>% 
    mutate(case_diff = wkcase-prev_wkcase) %>%
    mutate(death_diff = wkdeath-prev_wkdeath) %>% 
    mutate(wkcase_change = if_else(prev_wkcase>0,
                                   (case_diff)/prev_wkcase,
                                   NA_real_)) %>%
    mutate(wkdeath_change = if_else(prev_wkdeath>0,
                                    (death_diff)/prev_wkdeath,
                                    NA_real_)) %>%
    mutate(inci = if_else(`Population 2018.x`>0, 
                          ((wkcase/`Population 2018.x`)/7)*100000, 
                          NA_real_)) %>% 
    mutate(incideath = if_else(`Population 2018.x`>0, 
                               ((wkdeath/`Population 2018.x`)/7)*100000, 
                               NA_real_)) %>% 
    mutate(pct_chng =if_else( !(is.na(wkcase_change)), wkcase_change*100, NA_real_)) %>%
    mutate(pct_chngdeath =if_else( !(is.na(wkdeath_change)), wkdeath_change*100, NA_real_)) %>%
    mutate(Rt = 1) %>%
    mutate(ou_src_match = paste(country_code, data_source, sep="_"))
  
  crossx <- df1 %>% select(-ou_date_src_match) %>% 
    group_by(data_source) %>% 
    mutate(datex = max(Date)) %>% 
    ungroup() %>% 
    filter(Date == datex) %>% 
    select(-datex)

  if (typex == "date") {
    return(df1)
  } else if (typex == "cross") {
    return(crossx)
  }
}





