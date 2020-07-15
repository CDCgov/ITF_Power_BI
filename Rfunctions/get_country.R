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
  
  #SETTING DIRECTORY FOR INTERNATIONAL TASK FORCE - if James, defaults to his own account, otherwise appends users' name to the path
  dir.root <- paste0("C:/Users/",
                     Sys.getenv("USERNAME"),
                     "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")
  

  rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  
  
  # # function to get the base JHU data with cases and deaths daily/cumulative 
  # fun_whoreg <- dget(paste0(rfunctions.dir, "who_regions.R"))
  # 
  # wreg <- fun_whoreg() %>% 
  #   mutate(a3 = countrycode(ou, origin = 'country.name', destination = 'iso3c')) %>% 
  #   select(a3, region)
  
  # Getting table with country and ISO alpha 3 code:
  iso_data <- maps::iso3166 
  iso_data$a3[iso_data$ISOname=="Kosovo"] <- "XKX"
  
  whodir <- paste0(dir.root, "Data/WHO Member States/")
  
  
  # Adding WHO regions
  whoreg <- read.csv(paste0(whodir, "who_regions_j.csv"))
  whoter <- read.csv(paste0(whodir, "who_region_terr.csv")) %>% 
    select(country, who_region_loc) %>% 
    rename(who_region = who_region_loc)
  
  names(whoreg) <- tolower(names(whoreg))
  names(whoter) <- tolower(names(whoter))
  
  whoregx <- bind_rows(whoreg, whoter)
  
  whoreg1 <- whoregx %>% 
    mutate(a3 = case_when(
      country %in% c("Eswatini") ~
        parse_country("Swaziland", 
              to = "iso3c", how = c("regex", "google", "dstk"),
              language = c("en")),
      country %in% c("Kosovo") ~ "XKX",
      TRUE ~  parse_country(country, to = "iso3c", how = c("regex", "google", "dstk"),
                                      language = c("en")))) %>% 
    filter(!is.na(a3)) %>% 
    select(a3, who_region) %>% 
    unique() 

    
  iso_dataw <- left_join(iso_data, whoreg1)
  
  iso_dataw$who_region[is.na(iso_dataw$who_region)] <- "Other"
  
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
    dplyr::select(ISOname, mapname, sovereignty, a3, a2, who_region, pop_2020yr) %>% 
    dplyr::rename(country = ISOname,
                  iso3code = a3,
                  iso2code = a2) 

  base_frame <- df %>% 
    dplyr::filter(country %ni% c("Paracel Islands", 
                                 "Clipperton Island"
    )) %>% 
    dplyr::filter(iso3code %ni% c("??", "???")) %>% 
    dplyr::select(country, iso3code, iso2code, who_region, pop_2020yr) %>% 
    unique()
  
  return(base_frame)
}