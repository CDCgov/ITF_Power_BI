# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(rfunctions.dir, df_ncov) {
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  # Loading the packages
  ldpkg(c("tidyverse",
          "zoo",
          "passport",
          "stringr",
          "SaviR"))

  # If NCOV base dataframe is missing as input, then call the script to generate it
  if (missing(df_ncov)) {
    # Function to get NCOV base data - cases and deaths
    # fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
    # df_ncov <- fun_ncov(rfunctions.dir)
    
    df_ncov<-get_covid_df() %>%
      #replace Other with JG to link to Onetable
      mutate(country_code=case_when(country=="Other"~"OT",TRUE~country_code),
             country=case_when(country %in% c("China","Hong Kong","Taiwan","Macau") & source=="JHU"~paste0(country,"-JHU"),
                               country %in% c("China") & source=="WHO"~paste0(country,"-WHO"),
                               TRUE~country),
             ) %>%  
      rename(iso2code=country_code,
         data_source=source) %>%
      left_join(onetable,by="iso2code")   %>%
      rename(country_code=id)

    
    
  }
  
  ncov_data <- df_ncov %>%
    # select(!`Country Code`) %>%
    # rename_all(tolower) %>%
    # rename_all(gsub, pattern=" ", replacement="_") %>%
    # mutate(new_cases = case_when(new_cases < 0 ~ 0, new_cases >= 0 ~ new_cases),
    #        new_deaths = case_when(new_deaths < 0 ~ 0, new_deaths >= 0 ~ new_deaths)) %>%
    # mutate(mort = 1000000 * new_deaths / population_2020,
    #        inc = 100000 * new_cases / population_2020,
    #        mort_cum = 1000000 * cumulative_deaths / population_2020,
    #        inc_cum = 100000 * cumulative_cases / population_2020) %>%
    mutate(mort = 1000000 * new_deaths / population,
           inc = 100000 * new_cases / population,
           mort_cum = 1000000 * cumulative_deaths / population,
           inc_cum = 100000 * cumulative_cases / population) %>%
    mutate(ou_date_match = paste(country_code, date, sep="_")) %>%
    #filter(population_2020>0) %>%
    filter(population>0) %>%
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

  overlay_list <- list("cases_deaths" = ncov_data, "stringency" = df.oxford)
  
  return(overlay_list)
}
