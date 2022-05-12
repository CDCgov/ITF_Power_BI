# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function(rfunctions.dir) {
  
  # Creating the 'not in' function
  `%ni%` <- Negate(`%in%`) 
  
  
  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
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
                               "Cumulative People Vaccinated (Fully)", "Cumulative People Vaccinated (Fully)", "Daily Doses Administered (7-day average)", "Daily Doses Administered (7-day average)",
                               "Daily People Vaccinatd  (7-day average)", "Daily People Vaccinated (7-day average)", "Cumulative Booster Doses Administered", "Cumulative Booster Doses Administered"),
                             c("total_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated", "people_vaccinated_per_hundred",
                               "people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "daily_vaccinations", "daily_vaccinations_per_million",
                               "daily_people_vaccinaed","daily_people_vaccinated_per_hundred","total_boosters","total_boosters_per_hundred"))
  map_count_or_rate <- setNames(c("Count", "Rate", "Count", "Rate", "Count", "Rate", "Count", "Rate","Count", "Rate", "Count", "Rate"),
                                c("total_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated", "people_vaccinated_per_hundred",
                                  "people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "daily_vaccinations", "daily_vaccinations_per_million",
                                  "daily_people_vaccinaed","daily_people_vaccinated_per_hundred","total_boosters","total_boosters_per_hundred"))
  map_data_suffix <- setNames(c(" ", "Per 100 people", " ", "Per 100 people", " ", "Per 100 people", " ", "Per 1M people"," ", "Per 100 people", " ", "Per 100 people"),
                              c("total_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated", "people_vaccinated_per_hundred",
                                "people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "daily_vaccinations", "daily_vaccinations_per_million",
                                "daily_people_vaccinaed","daily_people_vaccinated_per_hundred","total_boosters","total_boosters_per_hundred"))
  
  map_age_data_field <- setNames(c("Cumulative People Vaccinated (1 or more)",
                               "Cumulative People Vaccinated (Fully)", 
                               "Cumulative People With Booster"),
                             c("people_vaccinated_per_hundred",
                               "people_fully_vaccinated_per_hundred", 
                               "people_with_booster_per_hundred"))
  map_age_count_or_rate <- setNames(c("Rate", "Rate", "Rate"),
                                c("people_vaccinated_per_hundred",
                                  "people_fully_vaccinated_per_hundred", 
                                  "people_with_booster_per_hundred"))
  map_age_data_suffix <- setNames(c("Per 100 people","Per 100 people","Per 100 people"),
                              c("people_vaccinated_per_hundred",
                                "people_fully_vaccinated_per_hundred", 
                                "people_with_booster_per_hundred"))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~ Function to generate datasets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  vax_raw <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv', as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  loc_raw <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv', as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  man_raw <- read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-manufacturer.csv', as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)
  vax_age_raw<-read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations-by-age-group.csv', as.is=TRUE, stringsAsFactors=FALSE, check.names=FALSE)

  # Drop all rows that do not have valid ISO 3 Code
  # This includes World data column, useful for displaying world data
  vax <- vax_raw %>%
    mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
    drop_na(iso_code) %>%
    mutate(iso_code = recode(iso_code, "OWID_KOS" = "XKX")) %>%
    filter(!str_detect(iso_code,"OWID"))
  vax <- vax[, !(names(vax) %in% c("daily_vaccinations_raw"))]
  loc <- loc_raw %>%
    drop_na(iso_code) %>%
    mutate(iso_code = recode(iso_code, "OWID_KOS" = "XKX")) %>%
    filter(!str_detect(iso_code,"OWID")) %>%
    rename(vaccine_manufacturer_list = vaccines)
  man <- man_raw %>%
    drop_na(location) %>%
    filter(location != "European Union" & location != "Wales" & location != "Scotland" & location != "")
  vaxages <- vax_age_raw %>%
    mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
    mutate(iso_code = parse_country(location,to="iso3c")) 
  
  # Flag the last and first dates recorded for each country AND indicator
  vax_by_country <- vax 
  # %>%
  #   arrange(desc(date)) %>%
  #   group_by(iso_code) %>%
  #   mutate(is_latest = row_number(iso_code) == 1) %>%
  #   mutate(is_first = row_number(iso_code) == n())
  
  
  map_location <- setNames(loc$location, loc$iso_code)
  
  
  all_dates <- seq.Date(from=as.Date(min(vax$date)), to=as.Date(max(vax$date)), by="day")
  
  # Change date to Date object
  # Fill in missing dates for the dataset
  vax_all <- vax_by_country %>%
    complete(iso_code, date = all_dates) %>%
    ungroup()
  
  vax_all$location[is.na(vax_all$location)] <- map_location[vax_all$iso_code[is.na(vax_all$location)]]
  
  all_age_dates <- seq.Date(from=as.Date(min(vaxages$date)), to=as.Date(max(vaxages$date)), by="day")
  
  vax_age_all.long<-vaxages %>%
    complete(iso_code, age_group,date = seq(min(date), max(date), by = "days")) %>%
    ungroup() %>%
    gather("raw_field", "Value", people_vaccinated_per_hundred:people_with_booster_per_hundred) %>%
    #flag latest for each variable 
    filter(!is.na(Value)) %>%
    group_by(iso_code,raw_field,age_group) %>%
    arrange(desc(date)) %>%
    mutate(is_latest = row_number(iso_code) == 1) %>%
    mutate(is_first = row_number(iso_code) == n()) 
  
  vax_age_all.long$location[is.na(vax_age_all.long$location)] <- map_location[vaxages$iso_code[is.na(vaxages$location)]]
  
  
  # Split the vaccine manufacturer list and separate for each row
  loc$vaccine_manufacturer <- loc$vaccine_manufacturer_list
  loc <- loc %>%
    separate_rows(vaccine_manufacturer, sep = ",")
  loc$vaccine_manufacturer <- str_trim(loc$vaccine_manufacturer)
  
  vax_all.long <- vax_all %>% 
    gather("raw_field", "data_value", total_vaccinations:daily_people_vaccinated_per_hundred) %>%
    #flag latest for each variable 
    filter(!is.na(data_value)) %>%
    group_by(iso_code,raw_field) %>%
    arrange(desc(date)) %>%
    mutate(is_latest = row_number(iso_code) == 1) %>%
    mutate(is_first = row_number(iso_code) == n())
  # %>%
  #     complete(iso_code,raw_field, date = all_dates) %>%
  #     ungroup()
  
  
  
  
  vax_all.long$data_field <- map_data_field[vax_all.long$raw_field]
  vax_all.long$data_suffix <- map_data_suffix[vax_all.long$raw_field]
  vax_all.long$count_or_rate <- map_count_or_rate[vax_all.long$raw_field]
  vax_all.long$ou_date_match <- paste(vax_all.long$iso_code, vax_all.long$date, sep="_")
  
  vax_age_all.long$data_field <- map_age_data_field[vax_age_all.long$raw_field]
  vax_age_all.long$data_suffix <- map_age_data_suffix[vax_age_all.long$raw_field]
  vax_age_all.long$count_or_rate <- map_age_count_or_rate[vax_age_all.long$raw_field]

  
  count_vaccinated_filter <- function(df) { return(df$raw_field == "total_vaccinations" | df$raw_field == "people_vaccinated" | df$raw_field == "people_fully_vaccinated" | df$raw_field =="total_boosters")}
  count_vaccinated_breaks <- c(0,1e3,1e4,1e5,1e6,1e7,Inf)
  count_vaccinated_ticks <- c(1e3,1e4,1e5,1e6,1e7,5e8)
  rate_vaccinated_filter <- function(df) { return(df$raw_field == "people_vaccinated_per_hundred" | df$raw_field == "people_fully_vaccinated_per_hundred"| df$raw_field == "total_boosters_per_hundred")| df$raw_field == "people_with_booster_per_hundred"}
  rate_vaccinated_breaks <- c(0,5,10,20,50,70,Inf)
  rate_vaccinated_ticks <- c(5,10,20,50,70,100)
  rate_doses_filter <- function(df) { return(df$raw_field == "total_vaccinations_per_hundred" | df$raw_field == "daily_people_vaccinated_per_hundred")}
  rate_doses_breaks <- c(0,5,10,20,50,70,Inf)
  rate_doses_ticks <- c(5,10,20,50,70,200)
  count_daily_filter <- function(df) { return(df$raw_field == "daily_vaccinations"| df$raw_field =="daily_people_vaccinated")}
  count_daily_breaks <- c(0,10,1e2,1e3,1e4,1e5,Inf)
  count_daily_ticks <- c(10,1e2,1e3,1e4,1e5,5e6)
  rate_daily_filter <- function(df) { return(df$raw_field == "daily_vaccinations_per_million")}
  rate_daily_breaks <- c(0,1,10,50,100,1e3,Inf)
  rate_daily_ticks <- c(1,10,50,100,1e3,10e4)
  
  vax_all.long$data_cat_num <- NA
  
  vax_all.long$data_cat_num[count_vaccinated_filter(vax_all.long)] <- cut(vax_all.long$data_value[count_vaccinated_filter(vax_all.long)], breaks = count_vaccinated_breaks, na.rm = TRUE)
  vax_all.long$data_cat_num[rate_vaccinated_filter(vax_all.long)] <- cut(vax_all.long$data_value[rate_vaccinated_filter(vax_all.long)], breaks = rate_vaccinated_breaks, na.rm = TRUE)
  vax_all.long$data_cat_num[rate_doses_filter(vax_all.long)] <- cut(vax_all.long$data_value[rate_doses_filter(vax_all.long)], breaks = rate_doses_breaks, na.rm = TRUE)
  vax_all.long$data_cat_num[count_daily_filter(vax_all.long)] <- cut(vax_all.long$data_value[count_daily_filter(vax_all.long)], breaks = count_daily_breaks, na.rm = TRUE)
  vax_all.long$data_cat_num[rate_daily_filter(vax_all.long)] <- cut(vax_all.long$data_value[rate_daily_filter(vax_all.long)], breaks = rate_daily_breaks, na.rm = TRUE)
  
  
  vax_age_all.long$data_cat_num <- NA
  vax_age_all.long$data_cat_num[rate_vaccinated_filter(vax_age_all.long)] <- cut(vax_age_all.long$Value[rate_vaccinated_filter(vax_age_all.long)], breaks = rate_vaccinated_breaks, na.rm = TRUE)
  
  
  vax_all.long$data_cat <- NA
  vax_all.long$data_cat[!is.na(vax_all.long$data_cat_num)] <- paste("cat", vax_all.long$data_cat_num)[!is.na(vax_all.long$data_cat_num)]
  
  vax_age_all.long$data_cat <- NA
  vax_age_all.long$data_cat[!is.na(vax_age_all.long$data_cat_num)] <- paste("cat", vax_age_all.long$data_cat_num)[!is.na(vax_age_all.long$data_cat_num)]
  
  
  cats = expand.grid(cat_num = c(1:6),
                     raw_field = c("total_vaccinations", "total_vaccinations_per_hundred", "people_vaccinated", "people_vaccinated_per_hundred",
                                   "people_fully_vaccinated", "people_fully_vaccinated_per_hundred", "daily_vaccinations", "daily_vaccinations_per_million","daily_people_vaccinated", "daily_people_vaccinated_per_hundred",
                                   "total_boosters","total_boosters_per_hundred"))
  
  cats$data_field <- map_data_field[cats$raw_field]
  cats$data_suffix <- map_data_suffix[cats$raw_field]
  cats$count_or_rate <- map_count_or_rate[cats$raw_field]
  cats$tick_value <- 0
  cats$tick_value[count_vaccinated_filter(cats)] <- count_vaccinated_ticks[cats$cat_num[count_vaccinated_filter(cats)]]
  cats$tick_value[rate_vaccinated_filter(cats)] <- rate_vaccinated_ticks[cats$cat_num[rate_vaccinated_filter(cats)]]
  cats$tick_value[rate_doses_filter(cats)] <- rate_doses_ticks[cats$cat_num[rate_doses_filter(cats)]]
  cats$tick_value[count_daily_filter(cats)] <- count_daily_ticks[cats$cat_num[count_daily_filter(cats)]]
  cats$tick_value[rate_daily_filter(cats)] <- rate_daily_ticks[cats$cat_num[rate_daily_filter(cats)]]
  
  
  #calculate daily count column for rollout
  man<-man %>%
    arrange(date) %>%
    group_by(location, vaccine) %>%
    mutate(daily_vaccinations=case_when(
      is.na(lag(total_vaccinations)) ~ total_vaccinations,
      TRUE ~ total_vaccinations - lag(total_vaccinations))) %>%
    ungroup() %>%
    mutate(iso3code=parse_country(location,to="iso3c"))
  
  
  
  vax_list <- list("all" = vax_all.long, "ages" = vax_age_all.long, "manufacturers" = loc, "rollout" = man, "categories" = cats)
  
  return(vax_list)
}