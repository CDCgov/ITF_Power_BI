# ==================
# Pulling / Wrangling ADM2 data from
# DXY source
# Sean Browning
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

# === Globals ==========================================
out_folder <- file.path("itf_dashboard", "output")
# NOTE: We could also do the webscrape ourselves.

# Calculate the date based on local HKG time
# (Where updates are pushed at 00 hours local time)
current_hkg_time <- Sys.time()
attr(current_hkg_time, "tzone") <- "Asia/Shanghai"
data_date <- format(current_hkg_time, "%Y.%m.%d")

# Create string
china_data_source <- sprintf(
  "https://github.com/BlankerL/DXY-COVID-19-Data/releases/download/%s/DXYArea.csv",
  data_date
)

# Non-adm2 to keep
non_adm2 <- c("Hong Kong", "Taiwan", "Macau")
# Provincial cities that are not ADM2, but won't map to pop denoms
# if we don't keep at this level
prov_city <- c("Chongqing", "Beijing", "Tianjin", "Shanghai")

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

# === Write out to file ==========================
fwrite(china_adm2_data_final, file.path(out_folder, "china_adm2_covid_cumulative.csv"))
fwrite(china_adm2_data_final_new, file.path(out_folder, "china_adm2_covid_new.csv"))
