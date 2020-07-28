function(){
  # Creating basic functions to show top few rows of data
  View50 <- function(x){View(x[1:50,])}
  View100 <- function(x){View(x[1:100,])}
  
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
  ldpkg(c("maps", 
          "tidyverse",
          "countrycode",
          "passport",
          "wbstats",
          "rvest"
          ))
  
  # Take out all NAs in the dataset and replace with zero
  remove_nas <- function(df) { 
    df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Setting up folders for data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
  # Getting table with country and ISO alpha 3 code:
  iso_data <- maps::iso3166 
  iso_data$a3[iso_data$ISOname=="Kosovo"] <- "XKX"
  iso_data$a2[iso_data$ISOname=="Kosovo"] <- "XK"
  iso_data$a2[iso_data$ISOname=="Namibia"] <- "NA"
  
  
  iso_df <- iso_data %>% filter(!is.na(a2)) %>% 
    filter(a2 %ni% c("??")) %>% 
    select(a2, a3) %>% unique()
  
  
  # Adding WHO regions
  whoreg <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", stringsAsFactors=FALSE, encoding="UTF-8")  

  whoreg1 <- whoreg %>% select(Country, Country_code, WHO_region) %>% 
    rename(a2 = Country_code,
           who_region = WHO_region) %>%  
    select(Country, a2, who_region) %>% 
    unique() 

    
  iso_dataw <- left_join(whoreg1, iso_df)
  
  iso_dataw$a2[iso_dataw$who_region=="Other"] <- "OT"
  iso_dataw$a3[iso_dataw$who_region=="Other"] <- "OTH"
  iso_dataw$a2[iso_dataw$Country=="Namibia"] <- "NA"
  iso_dataw$a3[iso_dataw$Country=="Namibia"] <- "NAM"
  iso_dataw$Country[iso_dataw$Country=="Kosovo[1]"] <- "Kosovo"
  
  
  
  
## Adding population 
  # str(wb_cachelist, max.level = 1)
  # View(wb_cachelist$indicators)
  # View(wbsearch(c("economies")) )
  # pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2020) %>% 
  #   select(iso3c, value) %>% 
  #   rename(a3 = iso3c,
  #          pop_2018x = value)
  
  url <- "https://www.cia.gov/library/publications/the-world-factbook/fields/335rank.html"
  
  data <- url %>% read_html %>% html_nodes(xpath='//*[@id="rankOrder"]') %>% html_table()
  
  data <- data.frame(data)
  
  for (i in 1:nrow(data)) {
    split <- unlist(strsplit(data[i,2], " "))
    data$totalPop[i] <- split[1]
  }
  
  totalpop <- data[,c(2,3)] %>% 
    mutate(pop = as.numeric(str_replace_all(Population, ",", "")))
  
  pop_data <- totalpop %>% 
    mutate(a3 = case_when(
      Country %in% c("Eswatini") ~
        parse_country("Swaziland", 
                      to = "iso3c", how = c("regex", "google", "dstk"),
                      language = c("en")),
      Country %in% c("Kosovo") ~ "XKX",
      TRUE ~  parse_country(Country, to = "iso3c", how = c("regex", "google", "dstk"),
                            language = c("en")))) %>% 
    rename(pop_2020yr = pop) %>% 
    select(a3, pop_2020yr) %>% 
    group_by(a3) %>% 
    summarise_all(list(~sum(., na.rm=T))) %>% 
    ungroup()
    
    
  iso_wp <- left_join(iso_dataw, pop_data)
  
  
## Adding income categorizations
  # Work in progress !! :)
  
  df <- iso_wp %>% 
    dplyr::select(Country, a3, a2, who_region, pop_2020yr) %>% 
    dplyr::rename(country = Country,
                  iso3code = a3,
                  iso2code = a2) 

  base_frame <- df %>% 
    dplyr::select(country, iso3code, iso2code, who_region, pop_2020yr) %>% 
    unique()
  
  return(base_frame)
}
