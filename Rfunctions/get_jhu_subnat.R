# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(rfunctions.dir){

  # Creating the 'not in' function
  `%ni%` <- Negate(`%in%`) 
  
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  
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
  
  return(data.long)  
}
