# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
ldpkg(c("Hmisc", 
        "tidyverse",
        "openxlsx",
        "passport",
        "readxl"))

# Take out all NAs in the dataset and replace with zero
remove_nas <- function(df) { 
  df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ Setting up folders for data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dir.root <- paste0("C:/Users/",
                   Sys.getenv("USERNAME"),
                   "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")


rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")


fun_frame <- dget(paste0(rfunctions.dir, "get_country.R"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ Function to generate datasets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", as.is=TRUE, stringsAsFactors = FALSE, check.names=FALSE)
  
  #Convert to Long Data
  cases.long <- cases %>% 
    tidyr::gather("Date.Orig", "Cumulative Cases", c(5:length(names(cases))))

  deaths.long <- deaths %>% 
    tidyr::gather("Date.Orig", "Cumulative Deaths", c(5:length(names(cases))))

  
  #Clean up Date and Time
  cases.long$Date <- as.Date(as.character(cases.long$Date.Orig), format="%m/%d/%y")
  deaths.long$Date <- as.Date(as.character(deaths.long$Date.Orig), format="%m/%d/%y")
  
  #Combine Case and Death Data
  data.long <- merge(cases.long, deaths.long, by=c("Province/State","Country/Region","Lat","Long","Date"), all=TRUE)
  # Remove the redundant date column
  data.long[c("Date.Orig.x","Date.Orig.y")] <- NULL
  
  #Calculate Daily New Cases and New Deaths, Then Make 0 if negative)
  data.long$Country.Province <- paste0(data.long$`Country/Region`," - ", data.long$`Province/State`)
  data.long$`New Cases` <- ave(data.long$`Cumulative Cases`, factor(data.long$Country.Province), FUN=function(x) c(NA,diff(x)))
  data.long$`New Deaths` <- ave(data.long$`Cumulative Deaths`, factor(data.long$Country.Province), FUN=function(x) c(NA,diff(x)))
  data.long$`New Cases`[data.long$`New Cases` <0] <- 0
  data.long$`New Deaths`[data.long$`New Deaths` <0] <- 0
  
  
  #Aggregate data to the country level
  #Keep Taiwan, Hong Kong, and Macao separate from Mainland China
  data.long$Country <- data.long$`Country/Region`
  data.long$Country[data.long$`Province/State` %in% c("Hong Kong","Macau")] <- data.long$`Province/State`[data.long$`Province/State` %in% c("Hong Kong","Macau")]
  data.long$Country <- gsub("*","",data.long$Country, fixed=TRUE)
  data.countries <- aggregate(data.long[c("New Cases","New Deaths","Cumulative Cases","Cumulative Deaths")], by=list(Country=data.long$Country, Date=data.long$Date), sum)
  data.countries$Country.clean <- parse_country(data.countries$Country, to="en-iso")
  data.countries$Country.clean[is.na(data.countries$Country.clean)] <- data.countries$Country[is.na(data.countries$Country.clean)]
  data.countries$Country <- data.countries$Country.clean
  data.countries$Country[data.countries$Country %in% "Diamond Princess"] <- "International Conveyance - Japan"
  data.countries <- data.countries[c("Country","Date","New Cases","Cumulative Cases","New Deaths","Cumulative Deaths")]
  data.countries <- data.countries[order(data.countries$Country, data.countries$Date),]
  

df <- data.countries %>% 
  rename(country   = Country,
         date      = Date,
         cases_new = `New Cases`,
         cases_cum = `Cumulative Cases`,
         deaths_new = `New Deaths`,
         deaths_cum = `Cumulative Deaths`
           ) %>% 
  mutate(iso3code = case_when(
    country %in% c("Eswatini") ~
      parse_country("Swaziland", 
                    to = "iso3c", how = c("regex", "google", "dstk"),
                    language = c("en")),
    country %in% c("Kosovo") ~ "XKX",
    TRUE ~  parse_country(country, to = "iso3c", how = c("regex", "google", "dstk"),
                          language = c("en")))) %>% 
  filter(!is.na(iso3code)) %>% 
  filter(country %ni% c("International Conveyance - Japan"))

# Adding population from the World Bank and JHU
iso <- fun_frame() %>% select(iso3code, 
                              who_region, pop_2020yr) %>% 
  filter(!is.na(iso3code)) %>% 
  unique()

dfx <- left_join(df, iso)

  # Getting the basic final dataset
base_data <- dfx %>% 
    select(country, 
           date, 
           cases_new, 
           cases_cum,
           deaths_new,
           deaths_cum,
           who_region,
           pop_2020yr,
           iso3code) %>% 
    mutate(ou_date_match = paste(iso3code, date, sep="_")) 
    
  
base_data}
