# ==================
# Pulling / Wrangling ADM2 data from
# DXY source
# Sean Browning
# partially functionalized and updated to include Baidu as a data source option (using code from Sean's repo)
# ==================================

# NOTE on source:
# Data are scraped from a website (DXY, https://3g.dxy.cn/newh5/view/pneumonia)
# See more information on data warehouse and scraping code:
# - https://github.com/BlankerL/DXY-COVID-19-Data/blob/master/README.en.md
# - https://github.com/BlankerL/DXY-COVID-19-Crawler/blob/master/README.en.md
# these more-or-less match up to official sources, but you should check:
# http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml
library(dplyr)
library(data.table)
library(tidyr)
# === Funcs ====================================
# NOTE: operates the same as lag()
# but faster, and keeps first number (where x - lag(x, 1) coerces to NA)
# Must ensure that all days are accounted for, or will be incorrect
inc_from_cum <- function(x, n=1L) {
  c(x[1], (x - data.table::shift(x, n=n, type = "lag"))[-1])
}

# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 

# === Globals ==========================================
out_folder <- file.path("itf_dashboard", "output")
# NOTE: We could also do the webscrape ourselves.

# Calculate the date based on UTC time
# (Where updates are pushed at 1800 hours)
current_UTC_time <- Sys.time()
attr(current_UTC_time, "tzone") <- "UTC"
data_date <- format(current_UTC_time, "%Y.%m.%d")

# Non-adm2 to keep
non_adm2 <- c("Hong Kong", "Taiwan", "Macau")
# Provincial cities that are not ADM2, but won't map to pop denoms
# if we don't keep at this level
prov_city <- c("Chongqing", "Beijing", "Tianjin", "Shanghai")



#function to choose between DXY or Baidu, parameters include any provinces that are separate in DXY, 
#and any cities that are considered provinces in either data sources (need to aggregate to calculate population rates, but if you just want case counts you can leave them in)
china_city_data_scrape<-function(source="DXY",non_adm2=c("Hong Kong", "Taiwan", "Macau"),
                                 prov_city=c("Chongqing", "Beijing", "Tianjin", "Shanghai")){
  
  
  if(source=="DXY"){
    # Create string
    china_data_source <- sprintf(
      "https://github.com/BlankerL/DXY-COVID-19-Data/releases/download/%s/DXYArea.csv",
      data_date
    )
  
    # === First pass cleaning, filtering ====================
    china_data_raw <- fread(china_data_source, encoding="UTF-8") %>%
      select(
        date = updateTime,
        country = countryEnglishName,
        province_en = provinceEnglishName,
        province_zh = provinceName,
        city_en = cityEnglishName,
        city_zh = cityName,
        province_confirmed = province_confirmedCount,
        province_suspected = province_suspectedCount,
        province_recovered = province_curedCount,
        province_deaths = province_deadCount,
        city_confirmed = city_confirmedCount,
        city_suspected = city_suspectedCount,
        city_recovered = city_curedCount,
        city_deaths = city_deadCount
      ) %>%
      filter(country == "China")
  
    # === Pulling ADM2 data ========================================
    china_adm2_data <- china_data_raw %>%
      # Pull over province HK, Macau, and Taiwan
      # They would get dropped in the filter below, otherwise
      mutate(
        city_en = if_else(province_en %in% non_adm2, province_en, city_en),
        city_zh = if_else(province_en %in% non_adm2, province_zh, city_zh),
        city_confirmed = if_else(city_en %in% non_adm2, province_confirmed, city_confirmed),
        city_recovered = if_else(city_en %in% non_adm2, province_recovered, city_recovered),
        city_suspected = if_else(city_en %in% non_adm2, province_suspected, city_suspected),
        city_deaths = if_else(city_en %in% non_adm2, province_deaths, city_deaths),
      ) %>%
      # Dropping ADM1 entries, we have JHU data for that
      filter(city_en != "", !province_en %in% prov_city) %>%
      select(-province_confirmed:-province_deaths) %>%
      mutate(date_floor = as.Date(date)) %>%
      # Take the latest intra-day update
      # So there's only 1 observation per day per city
      group_by(across(c(date_floor, country:city_zh))) %>%
      arrange(desc(date)) %>%
      slice(1) %>%
      ungroup() %>%
      # Make time stamp the last_update, and date an actual date
      select(
        date = date_floor,
        last_update = date,
        everything()
      ) %>%
      arrange(date, province_en, city_en)
    
    if(length(prov_city)==0){
      prov_adm2_data<-china_adm2_data[0,]
    }else{
      prov_adm2_data <- china_data_raw %>%
        filter(province_en %in% prov_city) %>%
        # Take "province" as "city" data, and do the same cleaning as above
        select(-city_en, -city_zh, -city_confirmed:-city_deaths) %>%
        distinct() %>%
        rename_with(~gsub("province_", "city_", .x)) %>%
        mutate(
          city_en = paste(city_en, "Municipality"),
          date_floor = as.Date(date),
          province_en = city_en,
          province_zh = city_zh
        ) %>%
        # Take the latest intra-day update
        # So there's only 1 observation per day per city
        group_by(across(c(date_floor, country:city_zh))) %>%
        arrange(desc(date)) %>%
        slice(1) %>%
        ungroup() %>%
        # Make time stamp the last_update, and date an actual date
        select(
          date = date_floor,
          last_update = date,
          everything()
        )
    }
  
  china_adm2_data_final <- china_adm2_data %>%
    bind_rows(prov_adm2_data) %>%
    arrange(date, province_en, city_en)
  
  china_adm2_data_final_new <- china_adm2_data_final %>%
    group_by(province_en, city_en) %>%
    # Ensure every date is explicitly included
    complete(date = seq(min(date), max(date), by = "days")) %>%
    # Sort from oldest observation, and fill down
    # (this works since it's still cumulative at this point)
    # so NA values that were created in last step are taken to be no change
    arrange(date) %>%
    fill(city_confirmed:city_deaths, .direction="down") %>%
    # Convert cumulative counts to new counts
    mutate(
      across(city_confirmed:city_deaths, inc_from_cum)
    ) %>%
    ungroup() %>%
    arrange(date, province_en, city_en)
  
  #end DXY chunk
  chn_list <- list("new" = china_adm2_data_final_new, "cumulative" = china_adm2_data_final)
  return(chn_list)
  
}else if(source=="Baidu"){

    sbgh<-"https://raw.githubusercontent.com/beansrowning/baidu_china_subnational/master/"

    source(paste0(sbgh,"R/funcs.R"))

    # === Read in previously extracted URLs ==============
    all_urls <- read_csv(paste0(sbgh,"data/page_lookup.csv"))
    # === Scrape all data ======================

    # Scrape
    all_city_data <- pblapply(all_urls[["url"]], debouce_scrape)

    # Parse into a data frame
    all_city_parsed <- pblapply(all_city_data, attempt_parse)

    # Make a list for all the cities that failed to parse
    failed <- vector("character")

    # Add english city name
    for (i in seq_along(all_city_parsed)) {
      if (is.null(all_city_parsed[[i]])) {
        failed <- c(failed, all_urls[["location"]][i])
        next
      }

      all_city_parsed[[i]] <- all_city_parsed[[i]] %>%
        mutate(area_en = all_urls[["location"]][i])
    }

    # Combine together
    all_city_data_df <- bind_rows(all_city_parsed) %>%
      left_join(
        distinct(all_urls, area_en = location, province),
        by = "area_en"
      ) %>%
      select(
        area_en, area_zh, province, date,
        new_asymptomatic = "\U65B0\U589E\U65E0\U75C7\U72B6",
        new_confirmed = "\U65B0\U589E\U672C\U571F"
      ) %>%
      mutate(
        date = as.Date(sprintf("%s.2022", date), "%m.%d.%Y")
      ) %>%
      arrange(date, province, area_en)

    # Failed table
    failed_table <- all_urls %>%
      filter(location %in% failed)
    
    
    if(length(prov_city)==0){
      prov_adm2_data<-all_city_data_df[0,]
    }else{
      prov_adm2_data <- all_city_data_df %>%
        filter(province %in% prov_city) %>%
        # Take "province" as "city" data
        mutate(
          area_en = paste(province, "Municipality"),
          area_zh=province) %>%
        group_by(area_en,province,area_zh,date) %>%
        summarise(new_asymptomatic=sum(new_asymptomatic,na.rm=TRUE),
                  new_confirmed=sum(new_confirmed,na.rm=TRUE)) %>%
        ungroup()
    }
    
    #append
    all_adm2_data_df<-all_city_data_df %>%
      filter(province %ni% prov_city) %>%
      bind_rows(prov_adm2_data)
    

    chn_list <- list("new" = all_city_data_df, "failed" = failed_table)
    return(chn_list)

}
   

}

chinadata_list<-china_city_data_scrape(source="DXY",non_adm2=c("Hong Kong", "Taiwan", "Macau"),
                                 prov_city=c("Chongqing", "Beijing", "Tianjin", "Shanghai"))
  
  
# === Write out to file ==========================
fwrite(chinadata_list$cumulative, file.path(out_folder, "china_adm2_covid_cumulative.csv"))
fwrite(chinadata_list$new, file.path(out_folder, "china_adm2_covid_new.csv"))
 