# Code for CDC Tracker Power BI file
# this code is called by R scripts that are run in a RStudio Connect to create an API that is referenced by a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it returns the rate of change of cases and deaths, by country, based on several pre-defined time periods
# it requires a data frame called "ncov_data" which is the standardized case and death data set that can be generated using the "get_ncov_data.R" function in the directory
# it will output a cross-sectional file of COVID cases and deaths, counts and rates, over 5 pre-specified time periods (1 day, 7 days, 14 days, 30 days and cumulative)
# it also creates categories for mapping, which are continuously updated
# see the bottom commented lines for the code to run to generate the two underlying data tables of the ITF Power BI Tracker views

# Uncomment and run the code below to test the function
# rfunctions.dir<-"https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/Rfunctions/"
# fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))
# ncov_data<-fun_ncov(rfunctions.dir)

function(ncov_data){
  
# Script to create the following:
# 1) View (Cases, Deaths)
# 2) Metric (Counts, Rate, Cumulative)
# 3) Period (1, 7, 14, 30)
# 4) Source (JHU, WHO)

# restructuring dataset
df <- ncov_data %>% 
  select(country_code, Date, 
         `Cumulative Cases`, `Cumulative Deaths`, 
         `Population 2018.x`,
         data_source) %>% 
  dplyr::rename(cases  = `Cumulative Cases`,
                deaths = `Cumulative Deaths`,
                pop    = `Population 2018.x`)


# Setting up the filters for Power BI
# 1) View (Cases, Deaths)
# 2) Metric (Counts, Rate, Cumulative)
# 3) Period (1, 7, 14, 30, or cumulative i.e. 999)
# 4) Source (JHU, WHO)
indicat_vec <- c("cases" ,  "deaths")
metricx_vec <- c("countsx", "ratesx")
periodx_vec <- c(1, 7, 14, 30, 999)
sourcex_vec <- c("JHU", "WHO") 


track_ncov_cross <- function(indicat , 
                       metricx , 
                       periodx , 
                       sourcex){
  
  dfx <- df %>% 
    filter(data_source %in% sourcex) %>% 
    gather(indicator, cumval, cases, deaths) %>% 
    filter(indicator %in% indicat) %>% 
    group_by(country_code) %>%
    arrange(Date) %>% 
    mutate(countsx =  case_when(periodx == 999 ~ cumval, 
                      TRUE ~        cumval - lag(cumval, periodx))) %>% 
    ungroup() %>% 
    mutate(countsx = if_else(countsx < 0, 0, countsx)) %>% 
    mutate(ratesx = if_else(pop > 0, round((countsx/pop)*100000,1), NA_real_)) %>% 
    mutate(pop_there = if_else(pop > 0, 1, 0)) %>% 
    gather(metrictype, valuex, countsx, ratesx) %>% 
    filter(metrictype %in% metricx) %>% 
    mutate(periodval = if_else(periodx==999, "Cumulative",
                         if_else(periodx==1, "24 hours",
                               paste0(periodx, " days")))) %>% 
    select(indicator, metrictype, periodval, data_source,
           country_code, Date, pop, 
           valuex) 
  
  xdfx <- dfx %>% filter(Date==max(df$Date)) 
  
q6  <- quantile(xdfx$valuex[xdfx$valuex>0], probs = seq(0, 1, 1/6), na.rm=T)     # Quintiles

# create category upper and lower ranges
cat1_range <- xdfx$valuex[ xdfx$valuex <  q6[2] & !is.na(xdfx$valuex)] 
cat2_range <- xdfx$valuex[ xdfx$valuex==q6[2] | (xdfx$valuex >= q6[2] &  xdfx$valuex < q6[3] & !is.na(xdfx$valuex))]
cat3_range <- xdfx$valuex[ xdfx$valuex==q6[3] | (xdfx$valuex >= q6[3] &  xdfx$valuex < q6[4] & !is.na(xdfx$valuex))]
cat4_range <- xdfx$valuex[ xdfx$valuex==q6[4] | (xdfx$valuex >= q6[4] &  xdfx$valuex < q6[5] & !is.na(xdfx$valuex))]
cat5_range <- xdfx$valuex[ xdfx$valuex==q6[5] | (xdfx$valuex >= q6[5] &  xdfx$valuex < q6[6] & !is.na(xdfx$valuex))]
cat6_range <- xdfx$valuex[ xdfx$valuex >= q6[6] & !is.na(xdfx$valuex)] 

# Creating legend upper and lower ranges
# cat1_up <- max(cat1_range, na.rm = T)
#   cat1_2diff <- min(cat2_range, na.rm = T)-cat1_up
# cat2_lw <- cat1_up + (1/(10^decimalplaces(cat1_2diff)))
# cat2_up <- max(cat2_range, na.rm = T)
#   cat2_3diff <- min(cat3_range, na.rm = T)-cat2_up
# cat3_lw <- cat2_up + (1/(10^decimalplaces(cat2_3diff)))
# cat3_up <- max(cat3_range, na.rm = T)
#   cat3_4diff <- min(cat4_range, na.rm = T)-cat3_up
# cat4_lw <- cat3_up + (1/(10^decimalplaces(cat3_4diff)))
# cat4_up <- max(cat4_range, na.rm = T)
#   cat4_5diff <- min(cat5_range, na.rm = T)-cat4_up
# cat5_lw <- cat4_up + (1/(10^decimalplaces(cat4_5diff)))

cat2_lw <- ifelse(is.finite(min(cat2_range, na.rm = T)), min(cat2_range, na.rm = T), 0)
cat3_lw <- ifelse(is.finite(min(cat3_range, na.rm = T)), min(cat3_range, na.rm = T), cat2_lw)
cat4_lw <- ifelse(is.finite(min(cat4_range, na.rm = T)), min(cat4_range, na.rm = T), cat3_lw)
cat5_lw <- ifelse(is.finite(min(cat5_range, na.rm = T)), min(cat5_range, na.rm = T), cat4_lw)
cat6_lw <- ifelse(is.finite(min(cat6_range, na.rm = T)), min(cat6_range, na.rm = T), cat5_lw)



legend_labs <- c(
  paste0("", format(cat2_lw, big.mark=",",scientific=FALSE)), 
  paste0("", format(cat3_lw, big.mark=",",scientific=FALSE)), 
  paste0("", format(cat4_lw, big.mark=",",scientific=FALSE)), 
  paste0("", format(cat5_lw, big.mark=",",scientific=FALSE)), 
  paste0("", format(cat6_lw, big.mark=",",scientific=FALSE)), 
  paste0("", cat6_lw, "+")) 



xdfx$qcats <- if_else(is.na(xdfx$valuex), "not estimated",
  if_else(xdfx$valuex %in% cat1_range, legend_labs[1],
  if_else(xdfx$valuex %in% cat2_range, legend_labs[2],
  if_else(xdfx$valuex %in% cat3_range, legend_labs[3],
  if_else(xdfx$valuex %in% cat4_range, legend_labs[4],
  if_else(xdfx$valuex %in% cat5_range, legend_labs[5], 
  if_else(xdfx$valuex %in% cat6_range, legend_labs[6], 
    "not estimated")))))))


xdfx$qvals <- if_else(is.na(xdfx$valuex), "absent",
  if_else(xdfx$valuex %in% cat1_range, "cat1",
  if_else(xdfx$valuex %in% cat2_range, "cat2",
  if_else(xdfx$valuex %in% cat3_range, "cat3",
  if_else(xdfx$valuex %in% cat4_range, "cat4",
  if_else(xdfx$valuex %in% cat5_range, "cat5", 
  if_else(xdfx$valuex %in% cat6_range, "cat6", 
    "absent")))))))


xdfx$dftype <- "cross"


fdfx <- xdfx %>% 
  # Changing names for categories for Power BI
mutate(Indicator = case_when(
  indicator %in% c("deaths") ~ "Deaths",
  indicator %in% c("cases") ~  "Cases"
)) %>% 
  mutate(Metric = case_when(
    metrictype %in% c("countsx") ~ "Counts",
    metrictype %in% c("ratesx")    ~  "Rate"
  )) %>% 
  select(Indicator, Metric, periodval, data_source, 
         country_code, Date, pop, valuex, dftype, qcats, qvals) %>% 
  mutate(maptitle = paste0(
           if_else(periodval=="Cumulative", "Global cumulative ", "Global "), 
           if_else(Indicator=="Cases", "cases of ", "deaths from "), 
           "COVID-19 reported ", 
           if_else(Metric=="Rate", "per 100,000 population ", ""),
           if_else(periodval %in% c("Cumulative"), "", paste0("in the past ", periodval)
                   ))) %>% 
  mutate(tabtitle = paste0(
           if_else(periodval=="Cumulative", "Cumulative ", ""), 
           if_else(Indicator=="Cases", "Cases reported ", "Deaths reported "), 
           if_else(Metric=="Rate", "per 100,000 population ", ""),
           if_else(periodval %in% c("Cumulative"), "", paste0("in the past ", periodval)
                   )))


return(fdfx)

}

# creating dataset with all permutations and combinations of variables
veclist <- expand.grid(
indicat = indicat_vec,
metricx = metricx_vec,
periodx = periodx_vec,
sourcex = sourcex_vec, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = F)

# running function on all combinations
trkdf <- purrr::pmap(veclist, track_ncov_cross)

cross_df <- dplyr::bind_rows(trkdf)
return(cross_df)
}

# The following code is run outside of this function to generate two data tables for visualization in Power BI
# cross_dfx<-cases_deaths_script_cross(ncov_data)
# 
# cross_dfx <- cross_df %>% 
#   mutate(periodvalx = case_when(
#     periodval %in% c("24 hours") ~ "x1",
#     periodval %in% c("7 days") ~ "x2",
#     periodval %in% c("14 days") ~ "x3",
#     periodval %in% c("30 days") ~ "x4",
#     periodval %in% c("Cumulative") ~ "x5"
#   )) 
# 
# cross_table <- cross_dfx %>%
#   select(Indicator:dftype) %>%
#   spread(Metric,  valuex) %>%
#   mutate(tabtitlex = paste0(
#            if_else(periodval=="Cumulative", "Cumulative ", ""),
#            if_else(Indicator=="Cases", "Cases reported ", "Deaths reported "),
#            if_else(periodval %in% c("Cumulative"), "", paste0("in the past ", periodval)
#                    )))


