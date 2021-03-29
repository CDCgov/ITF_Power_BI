#Code to generate CSVs ITF Power BI CDC COVID Tracker Views


# Path to all local R functions
rfunctions.dir <- "./Rfunctions/"

# Root for this project
dir.root <- "./covid_data_tracker/"

# Output directory to write data
out.dir <- paste0(dir.root,"output/")


#read in packages needed to run code
source(paste0(rfunctions.dir,"packages_for_Power_BI.R"))


#read in datasets that are input data for subsequent functions
#cases and deaths
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
print("running country case death code")
ncov_data<-fun_ncov(rfunctions.dir)
#country metadata
fun_country <- dget(paste0(rfunctions.dir, "get_country.R"))
print("running country code")
country_data<-fun_country()

# #trajectory raw data
# traj_series <- dget(paste0(rfunctions.dir, "trajectory_function_final_v2.R"))
# print("running trajectory code")
# traj_out_series<-traj_series("date",rfunctions.dir) #needs library(data.table)


#start code to output tables for Tracker

#country date metadata
fun_country_date<-dget(paste0(rfunctions.dir,"get_country_date.R"))
print("running country date code")
country_date_long<-fun_country_date(rfunctions.dir)
write_csv(country_date_long,paste0(out.dir,"lookup_country_date.csv"),na="")

#cases & deaths
cases_deaths_script_cross <- dget(paste0(rfunctions.dir, "cases_deaths_script_cross.R"))
print("running case death cross code")
cross_dfx<-cases_deaths_script_cross(ncov_data)

cross_dfx_complete<-cross_dfx %>%
  #create empty rows for country/indicator/data source pairs without data in original source
  complete(country_code,nesting(data_source,Indicator,Metric,periodval,dftype,Date)) %>%
  #fill in values for population, maptitle, tabtitle according to the combinations of other variables values
  group_by(country_code,Indicator,Metric,periodval,dftype,Date) %>%
  fill(pop,.direction="downup") %>%
  fill(maptitle,.direction="downup") %>%
  fill(tabtitle,.direction="downup") %>%
  ungroup() %>%
  #fill in zero values for countries with no data in one source
  mutate(valuex=if_else(is.na(qvals),replace_na(valuex,0),valuex)) %>%
  mutate(qvals=if_else(is.na(qvals),"absent",qvals)) %>%
  #re-order names
  select(names(cross_dfx))

cross_cases_deaths <- cross_dfx_complete %>%
  mutate(periodvalx = case_when(
    periodval %in% c("24 hours") ~ "x1",
    periodval %in% c("7 days") ~ "x2",
    periodval %in% c("14 days") ~ "x3",
    periodval %in% c("30 days") ~ "x4",
    periodval %in% c("Cumulative") ~ "x5"
  ))
write_csv(cross_cases_deaths,paste0(out.dir,"cross_cases_deaths.csv"),na="")

cross_cases_deaths_table <- cross_dfx_complete %>%
  select(Indicator:dftype) %>%
  
  #Edit on 12/18
  # Pull out any country with missing country code (WHO disaggregated Bonaire, Sint Eustatius, and Saba)
  filter(!is.na(country_code)) %>%
  
  spread(Metric,  valuex) %>%
  mutate(tabtitlex = paste0(
           if_else(periodval=="Cumulative", "Cumulative ", ""),
           if_else(Indicator=="Cases", "Cases reported ", "Deaths reported "),
           if_else(periodval %in% c("Cumulative"), "", paste0("in the past ", periodval)
                   )))
write_csv(cross_cases_deaths_table,paste0(out.dir,"cross_cases_deaths_table.csv"),na="")

# #trajectory data -formatted
# trajectory_output<-dget(paste0(rfunctions.dir, "trajectory_output.R"))
# print("running trajectory output code")
# tseriesx<-trajectory_output(traj_out_series,country_data)
# 
# #classification labels function for trajectory categories
# hbg.7labels <- c("High burden and growing",
#                  "High burden and not growing",
#                  "Not high burden but growing",
#                  "Not high burden and not growing",
#                  "<5 cases reported in the past 2 weeks")
# 
# # get longitudinal data (i.e. curve data)
# traj_series <- tseriesx %>%
#   mutate(hbg.7catx = if_else(is.na(hbg.7catx), hbg.7labels[5], hbg.7catx)) %>%
#   mutate(hbg.7cat = if_else(is.na(hbg.7cat), 5, hbg.7cat)) %>%
#   mutate_if(is.numeric, list(~replace_na(., 0)))
# write_csv(traj_series,paste0(out.dir,"traj_series.csv"),na="")
# 
# 
# # get cross-sectional data (i.e. map data)
# traj_cross<-tseriesx %>% select(-ou_date_match, -ou_date_src_match) %>% filter(!is.na(date)) %>% filter(date %in% max(date))
#   
# traj_cross_complete<-traj_cross %>%
#   #create empty rows for country/indicator/data source pairs without data in original source
#   complete(country_code,nesting(data_source,datatype,date_text,date,rate_cut,slope_cut)) %>%
#   #fill in values for population, maptitle, tabtitle according to the combinations of other variables values
#   group_by(country_code,datatype,date_text,date,rate_cut,slope_cut) %>%
#   fill(country,.direction="downup") %>%
#   fill(pop,.direction="downup") %>%
#   fill(rate_cut,.direction="downup") %>%
#   fill(slope_cut,.direction="downup") %>%
#   ungroup() %>%
#   mutate(ou_cut_src_match=paste(country_code,slope_cut,rate_cut,data_source,sep="_")) %>%
#   mutate(hbg.7catx=replace_na(hbg.7catx,"<5 cases reported in the past 2 weeks")) %>% 
#   mutate(daily.ci.change = round(daily.ci.change, 2))
# 
# write_csv(traj_cross_complete,paste0(out.dir,"traj_cross.csv"),na="")


#case/death rate of change data
case_death_delta<-dget(paste0(rfunctions.dir, "cases_deaths_change_script.R"))
print("running case death delta code")
cross_delta_case_deaths<-case_death_delta("cross",ncov_data,country_data)


crossdelta_complete<-cross_delta_case_deaths %>%
  #create empty rows for country/indicator/data source pairs without data in original source
  complete(country_code,nesting(data_source,Indicator,periodval,dftype,Date)) %>%
  #fill in values for population, maptitle, tabtitle according to the combinations of other variables values
  group_by(country_code,Indicator,periodval,dftype,Date) %>%
  fill(country,.direction="downup") %>%
  fill(pop,.direction="downup") %>%
  fill(prev_date,.direction="downup") %>%
  fill(curr_date,.direction="downup") %>%
  fill(prev_datex,.direction="downup") %>%
  fill(curr_datex,.direction="downup") %>%
  fill(curr_range,.direction="downup") %>%
  fill(prev_range,.direction="downup") %>%
  ungroup() %>%
  #fill in zero values for countries with no data in one source
  mutate(curr_val=replace_na(curr_val,0),
         prev_val=replace_na(prev_val,0)) %>%
  
  #Edit changecat for sparse data, <10 cases in past XX days
  mutate(changecat = ifelse(curr_val<10, paste0("<10 ", Indicator, " in past ", periodval), as.character(changecat))) %>%
  mutate(changecatx = ifelse(curr_val<10, NA, changecatx)) %>%
  mutate(perc_change = ifelse(curr_val<10, NA, perc_change)) %>%
  mutate(perc_changex = ifelse(curr_val<10, "N/A", perc_changex)) %>%
  
  #Create trajectory categories
  mutate(incidence_7days = case_when(pop>0 ~ 100000 * curr_val / pop)) %>%
  mutate(perc_change_clean = case_when(perc_change > 5 ~ 5*100,
                                       perc_change <= 5 ~ perc_change*100)) %>%
  mutate(inc_cat = case_when(curr_val<10 ~ paste0("<10 ", Indicator, " in past ", periodval),
                             incidence_7days<10 ~ "Low Incidence",
                             incidence_7days>=10 & incidence_7days<50 ~ "Moderate Incidence",
                             incidence_7days>=50 & incidence_7days<100 ~ "Substantial Incidence",
                             incidence_7days>=100 ~ "High Incidence")) %>%
  mutate(perc_change_cat = case_when(curr_val<10 ~ "N/A",
                                     perc_change_clean <= -10 ~ "Decreasing",
                                     perc_change_clean > -10 & perc_change_clean < 0 ~ "Moderate Decreasing",
                                     perc_change_clean >= 0 & perc_change_clean < 10 ~ "Moderate Increasing",
                                     perc_change_clean >= 10 ~ "Increasing")) %>%
  mutate(inc_cat_simple = case_when(curr_val<10 ~ paste0("<10 ", Indicator, " in past ", periodval),
                                    inc_cat %in% c("Low Incidence","Moderate Incidence") ~ "Low/Moderate",
                                    inc_cat %in% c("Substantial Incidence","High Incidence") ~ "High/Substantial")) %>%
  mutate(perc_change_cat_simple = case_when(perc_change_cat == "N/A" ~ "N/A",
                                            perc_change_cat %in% c("Decreasing","Moderate Decreasing") ~ "Decreasing",
                                            perc_change_cat %in% c("Increasing","Moderate Increasing") ~ "Increasing")) %>%
  mutate(traj_cat_simple = case_when(curr_val<10 ~ paste0("<10 ", Indicator, " in past ", periodval),
                                     inc_cat_simple == "Low/Moderate" & perc_change_cat_simple == "Decreasing" ~ "Low/Moderate Incidence, Decreasing",
                                     inc_cat_simple == "Low/Moderate" & perc_change_cat_simple == "Increasing" ~ "Low/Moderate Incidence, Increasing",
                                     inc_cat_simple == "High/Substantial" & perc_change_cat_simple == "Decreasing" ~ "Substantial/High Incidence, Decreasing",
                                     inc_cat_simple == "High/Substantial" & perc_change_cat_simple == "Increasing" ~ "Substantial/High Incidence, Increasing")) %>%
  mutate(country_code_and_data_source = paste0(country_code, data_source, sep = " ")) %>%
  #re-order names
  select(names(cross_delta_case_deaths), perc_change_clean, incidence_7days, inc_cat, perc_change_cat, inc_cat_simple, perc_change_cat_simple, traj_cat_simple, country_code_and_data_source)


write_csv(crossdelta_complete,paste0(out.dir,"cross_delta_cases_deaths.csv"),na="")

series_delta_case_deaths <- case_death_delta("series",ncov_data,country_data) %>%
  filter(periodval=="7 days" &
           Indicator=="Cases") %>%

  #Create trajectory categories
  mutate(incidence_7days = case_when(pop>0 ~ 100000 * curr_val / pop)) %>%
  mutate(incidence_7days_daily = incidence_7days / 7) %>%
  mutate(perc_change_clean = case_when(perc_change > 5 ~ 5*100,
                                       perc_change <= 5 ~ perc_change*100)) %>%
  mutate(inc_cat = case_when(curr_val<10 ~ paste0("<10 ", Indicator, " in past ", periodval),
                             incidence_7days<10 ~ "Low Incidence",
                             incidence_7days>=10 & incidence_7days<50 ~ "Moderate Incidence",
                             incidence_7days>=50 & incidence_7days<100 ~ "Substantial Incidence",
                             incidence_7days>=100 ~ "High Incidence")) %>%
  mutate(perc_change_cat = case_when(curr_val<10 ~ "N/A",
                                     perc_change_clean <= -10 ~ "Decreasing",
                                     perc_change_clean > -10 & perc_change_clean < 0 ~ "Moderate Decreasing",
                                     perc_change_clean >= 0 & perc_change_clean < 10 ~ "Moderate Increasing",
                                     perc_change_clean >= 10 ~ "Increasing")) %>%
  mutate(inc_cat_simple = case_when(curr_val<10 ~ paste0("<10 ", Indicator, " in past ", periodval),
                                    inc_cat %in% c("Low Incidence","Moderate Incidence") ~ "Low/Moderate",
                                    inc_cat %in% c("Substantial Incidence","High Incidence") ~ "High/Substantial")) %>%
  mutate(perc_change_cat_simple = case_when(perc_change_cat == "N/A" ~ "N/A",
                                            perc_change_cat %in% c("Decreasing","Moderate Decreasing") ~ "Decreasing",
                                            perc_change_cat %in% c("Increasing","Moderate Increasing") ~ "Increasing")) %>%
  mutate(traj_cat_simple = case_when(curr_val<10 ~ paste0("<10 ", Indicator, " in past ", periodval),
                                     inc_cat_simple == "Low/Moderate" & perc_change_cat_simple == "Decreasing" ~ "Low/Moderate Incidence, Decreasing",
                                     inc_cat_simple == "Low/Moderate" & perc_change_cat_simple == "Increasing" ~ "Low/Moderate Incidence, Increasing",
                                     inc_cat_simple == "High/Substantial" & perc_change_cat_simple == "Decreasing" ~ "Substantial/High Incidence, Decreasing",
                                     inc_cat_simple == "High/Substantial" & perc_change_cat_simple == "Increasing" ~ "Substantial/High Incidence, Increasing")) %>%
  mutate(country_code_and_data_source = paste0(country_code, data_source, sep = " "))

write_csv(series_delta_case_deaths,paste0(out.dir,"series_delta_cases_deaths.csv"),na="")

