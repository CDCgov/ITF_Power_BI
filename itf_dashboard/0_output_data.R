# Code that uses existing R functions to output CSVs for ITF Power BI Dashboard

library(data.table)

# Path to all local R functions
rfunctions.dir <- "Rfunctions/"

# Root for this project
root.dir <- "itf_dashboard/"

# Output directory to write data
output.dir <- paste0(root.dir, "output/")

#country data
# BUG: Data source is gone. This needs to be offloaded to SaviR
# where the metadata is present in onetable and updated automtically
# because it differs from that process, and thus all the routine reports.
# fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
# df_country <- fun_country(rfunctions.dir)
# write_csv(df_country,paste0(output.dir,"country_data.csv"),na="")

df_country <- fread(paste0(output.dir,"country_data.csv"))

#index country and date
fun_country_date <- dget(paste0(rfunctions.dir, "get_country_date.R"))
df_country_date <- fun_country_date(rfunctions.dir, df_country)
fwrite(df_country_date,paste0(output.dir,"index_data.csv"),na="")

# get the base jhu and who dataframes
fun_jhu <- dget(paste0(rfunctions.dir, "get_jhu_data.R"))
df_jhu <- fun_jhu(rfunctions.dir, df_country_date)
fun_who <- dget(paste0(rfunctions.dir, "get_who_data.R"))
df_who <- fun_who(rfunctions.dir, df_country_date)

# function to get the base data from JHU and WHO with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
df_ncov <- fun_ncov(rfunctions.dir, df_jhu, df_who)
fwrite(df_ncov,paste0(output.dir,"cases_deaths.csv"),na="")

df_gmob_raw <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", encoding="UTF-8")

# Getting google mobility dataset
fun_gmob <- dget(paste0(rfunctions.dir, "gmob.R"))
gmob <- fun_gmob(rfunctions.dir, df_country, df_gmob_raw)
fwrite(gmob,paste0(output.dir,"gmob.csv"),na="")


#vaccine data
fun_vax <- dget(paste0(rfunctions.dir, "get_vax_data.R"))
vax_dict <- fun_vax(rfunctions.dir)

fwrite(vax_dict$all %>% filter(count_or_rate=="Count"), paste0(output.dir,"vaccinations_all_counts.csv"), na="", row.names = FALSE)
fwrite(vax_dict$all %>% filter(count_or_rate=="Rate"), paste0(output.dir,"vaccinations_all_rates.csv"), na="", row.names = FALSE)
fwrite(vax_dict$manufacturers, paste0(output.dir, "vaccinations_manufacturers.csv"), na="", row.names=FALSE)
fwrite(vax_dict$rollout, paste0(output.dir, "vaccinations_rollout.csv"), na="", row.names=FALSE)
fwrite(vax_dict$categories, paste0(output.dir, "vaccinations_categories.csv"), na="", row.names=FALSE)

# overlay data
fun_overlay <- dget(paste0(rfunctions.dir, "get_country_overlays.R"))
overlay_dict <- fun_overlay(rfunctions.dir, df_ncov, df_gmob_raw)
fwrite(overlay_dict$cases_deaths, paste0(output.dir, "overlay_cases_deaths.csv"), na="", row.names=FALSE)
fwrite(overlay_dict$stringency, paste0(output.dir, "overlay_stringency.csv"), na="", row.names=FALSE)
fwrite(overlay_dict$mobility, paste0(output.dir, "overlay_mobility.csv"), na="", row.names=FALSE)