# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(rfunctions.dir) {
  
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
          "dplyr",
          "openxlsx",
          "passport",
          "readxl",
          "zoo"))
  
  
 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Setting up list element for re-formatting~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  map_data_field <- setNames(c("Cumulative Doses Administered", "Cumulative Doses Administered", "Cumulative People Vaccinated (1 or more)", "Cumulative People Vaccinated (1 or more)",
                               "Cumulative People Vaccinated (Fully)", "Cumulative People Vaccinated (Fully)", "Daily Doses Administered (7-day average)", "Daily Doses Administered (7-day average)"),
                             c("total_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated", "people_vaccinated_per_hundred",
                               "people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "daily_vaccinations", "daily_vaccinations_per_million"))
  map_count_or_rate <- setNames(c("Count", "Rate", "Count", "Rate", "Count", "Rate", "Count", "Rate"),
                                c("total_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated", "people_vaccinated_per_hundred",
                                  "people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "daily_vaccinations", "daily_vaccinations_per_million"))
  map_data_suffix <- setNames(c(" ", "Per 100 people", " ", "Per 100 people", " ", "Per 100 people", " ", "Per 1M people"),
                              c("total_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated", "people_vaccinated_per_hundred",
                                "people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "daily_vaccinations", "daily_vaccinations_per_million"))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Function to generate datasets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  vax_raw <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv', as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  loc_raw <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv', as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  man_raw <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv', as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  
  # Drop all rows that do not have valid ISO 3 Code
  # This includes World data column, useful for displaying world data
  vax <- vax_raw %>%
    mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
    drop_na(iso_code) %>%
    filter(iso_code != "OWID_EUN" & iso_code != "")
  vax <- vax[, !(names(vax) %in% c("daily_vaccinations_raw"))]
  loc <- loc_raw %>%
    drop_na(iso_code) %>%
    filter(iso_code != "OWID_EUN" & iso_code != "") %>%
    rename(vaccine_manufacturer_list = vaccines)
  man <- man_raw %>%
    drop_na(location) %>%
    filter(location != "European Union" & location != "Wales" & location != "Scotland" & location != "")
  
  # Flag the last and first dates recorded for each country
  vax_by_country <- vax %>%
    arrange(desc(date)) %>%
    group_by(iso_code) %>%
    mutate(is_latest = row_number(iso_code) == 1) %>%
    mutate(is_first = row_number(iso_code) == n())
  
  map_location <- setNames(loc$location, loc$iso_code)
  all_dates <- seq.Date(from=as.Date(min(vax$date)), to=as.Date(max(vax$date)), by="day")
  
  # Change date to Date object
  # Fill in missing dates for the dataset
  vax_all <- vax_by_country %>%
    complete(iso_code, date = all_dates) %>%
    ungroup()
  
  vax_all$location[is.na(vax_all$location)] <- map_location[vax_all$iso_code[is.na(vax_all$location)]]
    
  
  # Split the vaccine manufacturer list and separate for each row
  loc$vaccine_manufacturer <- loc$vaccine_manufacturer_list
  loc <- loc %>%
    separate_rows(vaccine_manufacturer, sep = ",")
  loc$vaccine_manufacturer <- str_trim(loc$vaccine_manufacturer)
    
  vax_all.long <- vax_all %>% 
    gather("raw_field", "data_value", total_vaccinations:daily_vaccinations_per_million)
  vax_all.long$data_field <- map_data_field[vax_all.long$raw_field]
  vax_all.long$data_suffix <- map_data_suffix[vax_all.long$raw_field]
  vax_all.long$count_or_rate <- map_count_or_rate[vax_all.long$raw_field]

  count_vaccinated_filter <- function(df) { return(df$raw_field == "total_vaccinations" | df$raw_field == "people_vaccinated" | df$raw_field == "people_fully_vaccinated")}
  count_vaccinated_breaks <- c(0,1e3,1e4,1e5,1e6,1e7,Inf)
  count_vaccinated_ticks <- c(1e3,1e4,1e5,1e6,1e7,5e8)
  rate_vaccinated_filter <- function(df) { return(df$raw_field == "total_vaccinations_per_hundred" | df$raw_field == "people_vaccinated_per_hundred" | df$raw_field == "people_fully_vaccinated_per_hundred")}
  rate_vaccinated_breaks <- c(0,0.05,1,5,10,20,Inf)
  rate_vaccinated_ticks <- c(0.05,1,5,10,20,100)
  count_daily_filter <- function(df) { return(df$raw_field == "daily_vaccinations")}
  count_daily_breaks <- c(0,10,1e2,1e3,1e4,1e5,Inf)
  count_daily_ticks <- c(10,1e2,1e3,1e4,1e5,5e6)
  rate_daily_filter <- function(df) { return(df$raw_field == "daily_vaccinations_per_million")}
  rate_daily_breaks <- c(0,1,10,50,100,1e3,Inf)
  rate_daily_ticks <- c(1,10,50,100,1e3,5e4)
  
  vax_all.long$data_cat_num <- NA
  
  vax_all.long$data_cat_num[count_vaccinated_filter(vax_all.long)] <- cut(vax_all.long$data_value[count_vaccinated_filter(vax_all.long)], breaks = count_vaccinated_breaks, na.rm = TRUE)
  vax_all.long$data_cat_num[rate_vaccinated_filter(vax_all.long)] <- cut(vax_all.long$data_value[rate_vaccinated_filter(vax_all.long)], breaks = rate_vaccinated_breaks, na.rm = TRUE)
  vax_all.long$data_cat_num[count_daily_filter(vax_all.long)] <- cut(vax_all.long$data_value[count_daily_filter(vax_all.long)], breaks = count_daily_breaks, na.rm = TRUE)
  vax_all.long$data_cat_num[rate_daily_filter(vax_all.long)] <- cut(vax_all.long$data_value[rate_daily_filter(vax_all.long)], breaks = rate_daily_breaks, na.rm = TRUE)
  
  vax_all.long$data_cat <- NA
  vax_all.long$data_cat[!is.na(vax_all.long$data_cat_num)] <- paste("cat", vax_all.long$data_cat_num)[!is.na(vax_all.long$data_cat_num)]
  
  cats = expand.grid(cat_num = c(1:6),
                     raw_field = c("total_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated", "people_vaccinated_per_hundred",
                       "people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "daily_vaccinations", "daily_vaccinations_per_million")
                     )
  
  cats$data_field <- map_data_field[cats$raw_field]
  cats$data_suffix <- map_data_suffix[cats$raw_field]
  cats$count_or_rate <- map_count_or_rate[cats$raw_field]
  cats$tick_value <- 0
  cats$tick_value[count_vaccinated_filter(cats)] <- count_vaccinated_ticks[cats$cat_num[count_vaccinated_filter(cats)]]
  cats$tick_value[rate_vaccinated_filter(cats)] <- rate_vaccinated_ticks[cats$cat_num[rate_vaccinated_filter(cats)]]
  cats$tick_value[count_daily_filter(cats)] <- count_daily_ticks[cats$cat_num[count_daily_filter(cats)]]
  cats$tick_value[rate_daily_filter(cats)] <- rate_daily_ticks[cats$cat_num[rate_daily_filter(cats)]]
  
  
  
  
  vax_list <- list("all" = vax_all.long, "manufacturers" = loc, "rollout" = man, "categories" = cats)
  
  return(vax_list)
}
