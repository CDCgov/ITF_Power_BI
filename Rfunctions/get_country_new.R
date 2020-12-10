# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it reads in exclusively public data sets and returns a  data frame with standardized country-level identifies by iso3code

function(){
  
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
   #       "wbstats",
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
  whoreg1$a2[whoreg1$Country=="Namibia"] <- "NA"
  
  iso_dataw <- full_join(iso_df,whoreg1)
  
  iso_dataw$a2[iso_dataw$who_region=="Other"] <- "OT"
  iso_dataw$a3[iso_dataw$who_region=="Other"] <- "OTH"
  iso_dataw$a2[iso_dataw$Country=="Namibia"] <- "NA"
  iso_dataw$a3[iso_dataw$Country=="Namibia"] <- "NAM"
  iso_dataw$Country[iso_dataw$Country=="Kosovo[1]"] <- "Kosovo"
  #Taiwan, Hong Kong, Macau, Western Sahara are broken out in the JHU data- HK and Macau are part of WPRO, Taiwan and Western Sahara are not covered
  iso_dataw$Country[iso_dataw$a3=="TWN"] <- "Taiwan"
  iso_dataw$Country[iso_dataw$a3=="HKG"] <- "Hong Kong"
  iso_dataw$Country[iso_dataw$a3=="MAC"] <- "Macau"
  iso_dataw$Country[iso_dataw$a3=="ESH"] <- "Western Sahara"
  iso_dataw$who_region[iso_dataw$a3 %in% c("HKG","MAC")] <- "WPRO"
  iso_dataw$who_region[iso_dataw$a3 %in% c("TWN","ESH")] <- "Other"
  
  iso_dataw<-filter(iso_dataw,!is.na(who_region))
  
  #adding continents
  continents<-read.csv("https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv",
                       stringsAsFactors = FALSE, encoding="UTF-8") %>%
    select(Continent_Name,Two_Letter_Country_Code,Three_Letter_Country_Code) %>%
    rename(a2=Two_Letter_Country_Code,
           a3=Three_Letter_Country_Code) %>%
    mutate(a2=if_else(a3=="NAM","NA",a2)) %>%
    #some countries have multiple continents- they are all in the WHO Euro region, so remove the rows where they have Asia as the continent
    filter(!(a3 %in% c("ARM","AZE","CYP","GEO","KAZ","RUS","TUR") & Continent_Name=="Asia"))
  
  
## Adding population 
  
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
                      to = "iso3c", how = c("regex", "google"),
                      language = c("en")),
      Country %in% c("Kosovo") ~ "XKX",
      TRUE ~  parse_country(Country, to = "iso3c", how = c("regex", "google"),
                            language = c("en")))) %>% 
    rename(pop_2020yr = pop) %>% 
    select(a3, pop_2020yr) %>% 
    group_by(a3) %>% 
    summarise_all(list(~sum(., na.rm=T))) %>% 
    ungroup()
    
    
  iso_wp <- left_join(left_join(iso_dataw, pop_data),continents) %>%
    mutate(Continent_Name=if_else(Country=="Kosovo","Europe",if_else(Country=="Other","Other",Continent_Name)))
  
  
## Adding income categorizations
  # Work in progress !! :)
  
  df <- iso_wp %>% 
    dplyr::select(Country, a3, a2, who_region, pop_2020yr, Continent_Name) %>% 
    dplyr::rename(country = Country,
                  iso3code = a3,
                  iso2code = a2)

  base_frame <- df %>% 
    dplyr::select(country, iso3code, iso2code, who_region, pop_2020yr, Continent_Name) %>% 
    unique()
  
  return(base_frame)
}
