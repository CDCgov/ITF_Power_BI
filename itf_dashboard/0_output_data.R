# Code that uses existing R functions to output CSVs for ITF Power BI Dashboard

library(readr)

# Path to all local R functions
rfunctions.dir <- "./Rfunctions/"

# Root for this project
dir.root <- "./covid_data_tracker/"

# Output directory to write data
out.dir <- paste0(dir.root,"output/")

    
rfunctions.dir <- paste0(dir.root,"PowerBI/R_scripts_testing/r_functions/")
rfunctions.dir2 <- "https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/Rfunctions/"

# Getting Rt dataset

fun_Rt <- dget(paste0(rfunctions.dir, "Rt_ncov.R"))
Rt_data <- fun_Rt()
write_csv(Rt_data,paste0(output.dir,"Rt_data.csv"),na="")

# function to get the base data with cases and deaths daily/cumulative 
fun_jhu <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
jhu_cases_deaths <- fun_jhu()
write_csv(jhu_cases_deaths,paste0(output.dir,"jhu_cases_deaths.csv"),na="")

#getting trajectory data
fun_traj <- dget(paste0(rfunctions.dir, "trajectory_function_final_newalgo.R"))
hotspot_data <- fun_traj("z")
hotspot_map <- fun_traj("map")

write_csv(hotspot_data,paste0(output.dir,"hotspot_data.csv"),na="")
write_csv(hotspot_map,paste0(output.dir,"hotspot_map.csv"),na="")

#testing data
fun_tst <- dget(paste0(rfunctions.dir, "covid_testing.R"))
testing_data <- fun_tst("long")
testing_cross <- fun_tst("cross")

write_csv(testing_data,paste0(output.dir,"testing_data.csv"),na="")
write_csv(testing_cross,paste0(output.dir,"testing_cross.csv"),na="")

#country data
fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
country_data <- fun_country()
write_csv(country_data,paste0(output.dir,"country_data.csv"),na="")

#index data
fun_frame <- dget(paste0(rfunctions.dir, "get_country_date.R"))
index_data <- fun_frame()
write_csv(index_data,paste0(output.dir,"index_data.csv"),na="")

# Getting google mobility dataset
fun_gmob <- dget(paste0(rfunctions.dir, "gmob.R"))
gmob <- fun_gmob()
write_csv(gmob,paste0(output.dir,"gmob.csv"),na="")


#risk matrix data
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix.R"))
riskmatrix <- fun_risk("date")
xriskmatrix <- fun_risk("cross")
write_csv(riskmatrix,paste0(output.dir,"riskmatrix.csv"),na="")
write_csv(xriskmatrix,paste0(output.dir,"xriskmatrix.csv"),na="")

#risk matrix_v2 data
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix_v2.R"))
riskmatrix_v2 <- fun_risk("date")
xriskmatrix_v2 <- fun_risk("cross")
write_csv(riskmatrix_v2,paste0(output.dir,"riskmatrix_v2.csv"),na="")
write_csv(xriskmatrix_v2,paste0(output.dir,"xriskmatrix_v2.csv"),na="")

#who criteria data
fun_criteria <- dget(paste0(rfunctions.dir, "get_who_criteria.R"))
who_criteria <- fun_criteria()
write.csv(who_criteria,paste0(output.dir,"who_criteria.csv"), na="")

#vaccine data
fun_vax <- dget(paste0(rfunctions.dir, "get_vax_data.R"))
vax_dict <- fun_vax()
write.csv(vax_dict$all, paste0(output.dir, "vaccinations_all.csv"), na="", row.names=FALSE)
write.csv(vax_dict$manufacturers, paste0(output.dir, "vaccinations_manufacturers.csv"), na="", row.names=FALSE)
write.csv(vax_dict$rollout, paste0(output.dir, "vaccinations_rollout.csv"), na="", row.names=FALSE)
write.csv(vax_dict$categories, paste0(output.dir, "vaccinations_categories.csv"), na="", row.names=FALSE)

