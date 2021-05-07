# Code that uses existing R functions to output CSVs for ITF Power BI Dashboard

library(readr)

# Path to all local R functions
rfunctions.dir <- "./Rfunctions/"

# Root for this project
root.dir <- "./itf_dashboard/"

# Output directory to write data
output.dir <- paste0(root.dir,"output/")

#country data
fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
df_country <- fun_country(rfunctions.dir)
write_csv(df_country,paste0(output.dir,"country_data.csv"),na="")

#index country and date
fun_country_date <- dget(paste0(rfunctions.dir, "get_country_date.R"))
df_country_date <- fun_country_date(rfunctions.dir, df_country)
write_csv(df_country_date,paste0(output.dir,"index_data.csv"),na="")

# get the base jhu and who dataframes
fun_jhu <- dget(paste0(rfunctions.dir, "get_jhu_data.R"))
df_jhu <- fun_jhu(rfunctions.dir, df_country_date)
fun_who <- dget(paste0(rfunctions.dir, "get_who_data.R"))
df_who <- fun_who(rfunctions.dir, df_country_date)

# get the base Rt jhu and Rt who dataframes
fun_rt_jhu <- dget(paste0(rfunctions.dir, "Rt_jhu.R"))
df_rt_jhu <- fun_rt_jhu(rfunctions.dir, df_jhu)
fun_rt_who <- dget(paste0(rfunctions.dir, "Rt_who.R"))
df_rt_who <- fun_rt_who(rfunctions.dir, df_who)

# function to get the base data from JHU and WHO with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
df_ncov <- fun_ncov(rfunctions.dir, df_jhu, df_who)
write_csv(df_ncov,paste0(output.dir,"cases_deaths.csv"),na="")

# Getting Rt dataset
fun_Rt <- dget(paste0(rfunctions.dir, "Rt_ncov.R"))
df_rt_ncov <- fun_Rt(rfunctions.dir, df_rt_jhu, df_rt_who)
write_csv(df_rt_ncov, paste0(output.dir,"Rt_data.csv"),na="")

#getting trajectory data
fun_traj <- dget(paste0(rfunctions.dir, "trajectory_function_final_newalgo.R"))
hotspot_data <- fun_traj("z", rfunctions.dir, df_ncov)
hotspot_map <- fun_traj("map", rfunctions.dir, df_ncov)

write_csv(hotspot_data,paste0(output.dir,"hotspot_data.csv"),na="")
write_csv(hotspot_map,paste0(output.dir,"hotspot_map.csv"),na="")

#testing data
fun_tst <- dget(paste0(rfunctions.dir, "covid_testing.R"))
testing_data <- fun_tst("long", rfunctions.dir, df_country)
testing_cross <- fun_tst("cross", rfunctions.dir, df_country)

write_csv(testing_data,paste0(output.dir,"testing_data.csv"),na="")
write_csv(testing_cross,paste0(output.dir,"testing_cross.csv"),na="")

df_gmob_raw <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", encoding="UTF-8")

# Getting google mobility dataset
fun_gmob <- dget(paste0(rfunctions.dir, "gmob.R"))
gmob <- fun_gmob(rfunctions.dir, df_country, df_gmob_raw)
write_csv(gmob,paste0(output.dir,"gmob.csv"),na="")

#risk matrix data
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix.R"))
riskmatrix <- fun_risk("date", rfunctions.dir, df_ncov, df_rt_ncov)
xriskmatrix <- fun_risk("cross", rfunctions.dir, df_ncov, df_rt_ncov)
write_csv(riskmatrix,paste0(output.dir,"riskmatrix.csv"),na="")
write_csv(xriskmatrix,paste0(output.dir,"xriskmatrix.csv"),na="")

#risk matrix_v2 data
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix_v2.R"))
riskmatrix_v2 <- fun_risk("date", rfunctions.dir, df_ncov)
xriskmatrix_v2 <- fun_risk("cross", rfunctions.dir, df_ncov)
write_csv(riskmatrix_v2,paste0(output.dir,"riskmatrix_v2.csv"),na="")
write_csv(xriskmatrix_v2,paste0(output.dir,"xriskmatrix_v2.csv"),na="")

#risk get_riskmatrix_v3_KM
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix_v3_KM.R"))
riskmatrix_v3 <- fun_risk("date", rfunctions.dir, df_ncov)
xriskmatrix_v3 <- fun_risk("cross", rfunctions.dir, df_ncov)
write_csv(riskmatrix_v3,paste0(output.dir,"riskmatrix_v3.csv"),na="")
write_csv(xriskmatrix_v3,paste0(output.dir,"xriskmatrix_v3.csv"),na="")

#who criteria data
fun_criteria <- dget(paste0(rfunctions.dir, "get_who_criteria.R"))
who_criteria <- fun_criteria(rfunctions.dir, df_ncov, testing_data, df_rt_ncov)
write.csv(who_criteria,paste0(output.dir,"who_criteria.csv"), na="")

#vaccine data
fun_vax <- dget(paste0(rfunctions.dir, "get_vax_data.R"))
vax_dict <- fun_vax(rfunctions.dir)
write.csv(vax_dict$all, paste0(output.dir, "vaccinations_all.csv"), na="", row.names=FALSE)
write.csv(vax_dict$manufacturers, paste0(output.dir, "vaccinations_manufacturers.csv"), na="", row.names=FALSE)
write.csv(vax_dict$rollout, paste0(output.dir, "vaccinations_rollout.csv"), na="", row.names=FALSE)
write.csv(vax_dict$categories, paste0(output.dir, "vaccinations_categories.csv"), na="", row.names=FALSE)

# overlay data
fun_overlay <- dget(paste0(rfunctions.dir, "get_country_overlays.R"))
overlay_dict <- fun_overlay(rfunctions.dir, df_ncov, df_gmob_raw)
write.csv(overlay_dict$cases_deaths, paste0(output.dir, "overlay_cases_deaths.csv"), na="", row.names=FALSE)
write.csv(overlay_dict$stringency, paste0(output.dir, "overlay_stringency.csv"), na="", row.names=FALSE)
write.csv(overlay_dict$mobility, paste0(output.dir, "overlay_mobility.csv"), na="", row.names=FALSE)
