# this code will run all of the required packages for the ITF Dashboard code
# It is necessary to run this on your local computer in the version of R you will
# use if you are setting up a Gateway to connect from the Power BI Service to your computer
# to update the dashboard


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
install.packages(c("Hmisc", 
       "tidyverse",
       "readr",
       "openxlsx",
       "passport",
       "readxl",
       "xlsx",
       "maps",
       "httr",
       "jsonlite",
       "EpiEstim", 
       "ggplot2",
       "countrycode",
       "gridExtra", 
       "ggpubr",
       "RColorBrewer",
       "zoo",
       "wbstats",
       "rvest",
       "xml2"))