# Hotspots for cumulative incidence or mortality
# Based on v4
# Original code by Kathryn Winglee
# Modified by Adam Vaughan & Michelle Schmitz / James Fuller / Imran Mujawar
# May 1, 2020 
# This version adopts the script for Power BI
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function(typex, rfunctions.dir){
  
final_hotspot  <- function(dfsource, typex){

  # Take out all NAs in the dataset and replace with zero
remove_nas <- function(df) { 
  df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}

# dir.root<- ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")),
#                   paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/"),
#                   ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/")),
#                          paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/"),
#                          "Directory does not exist"))

# Power BI script that pulls in all the analytic datasets in long format by Country and Date
# Folder path for all the Power BI scripts
# rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")

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
ldpkg(c("tidyverse",
        "passport",
        "readxl"))



# function to get the base JHU data with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data_vDASH.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ===== Beginning of Epi curve segmentation Function ===============
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Define functions for COUNTRY (not state or county) - presumably updated since hotspot analysis v9
getCategory <- function(catdf, 
                        rate.cut, 
                        slope.cut, 
                        hb.cut) {
  catdf = catdf[!is.na(catdf$deriv1),]
  deriv1 = catdf$deriv1
  catdf$category = "seven day"
  catdf$category[deriv1 >= 0 & deriv1 < slope.cut] = "plateau"
  catdf$category[deriv1 >= slope.cut] = "growth"
  catdf$category[deriv1 < 0] = "decline"
  
  # If CI is zero, there are no cases
  catdf$category <- ifelse(catdf$inc == 0, "none", catdf$category) 
  
  # Get a list of all the sequences of declines
  dec.col <- split(which(catdf$category == "decline"), 
                   cumsum(c(1, diff(which(catdf$category == "decline")) != 1)))
  
  # Change declines to plateau if the decline has occurred over 5 days or less
  if (length(dec.col) > 0) {
    for (j in 1:length(dec.col)) { # Loop through every decline period
      if (length(dec.col[[j]]) <= 5) {catdf$category[dec.col[[j]]] <- "plateau"}
      if (length(dec.col[[j]]) > 5) {catdf$category[dec.col[[j]][1:5]] <- "plateau"}
    }
  }
  
  ## Add incidence label for plateaus and growth
  catdf$category <- ifelse(catdf$ci.2wk <= rate.cut & 
                             !is.na(catdf$ci.2wk) & catdf$category == "plateau", 
                           "low incidence plateau",
                           ifelse(catdf$ci.2wk > rate.cut & 
                                    catdf$category == "plateau", 
                                  "elevated incidence plateau", catdf$category))
  catdf$category <- ifelse(catdf$ci.2wk <= rate.cut & !is.na(catdf$ci.2wk) & catdf$category == "growth", "low incidence growth",
                           ifelse(catdf$ci.2wk > rate.cut & 
                                    catdf$category == "growth", 
                                  "elevated incidence growth", catdf$category))
  
  # If it's in the first two weeks, use the cumulative incidence
  catdf$category <- ifelse(catdf$inc <= rate.cut & is.na(catdf$ci.2wk) & catdf$category == "plateau", 
                           "low incidence plateau",
                           ifelse(catdf$ci.2wk > rate.cut & catdf$category == "plateau", 
                                  "elevated incidence plateau", catdf$category))
  catdf$category <- ifelse(catdf$inc <= rate.cut & is.na(catdf$ci.2wk) & catdf$category == "growth", "low incidence growth",
                           ifelse(catdf$ci.2wk > rate.cut & catdf$category == "growth", "elevated incidence growth", catdf$category))
  
  
  ## If there are 5 or fewer cases in the past two weeks
  catdf$category <- ifelse(catdf$n.2wk <= 5 & !is.na(catdf$n.2wk) & catdf$n.2wk > 0, "lessthan5", catdf$category)
  
  ## If there are no cases in the past two weeks
  catdf$category <- ifelse(catdf$ci.2wk == 0 & catdf$inc > 0, "nonepast2wk", catdf$category) 
  catdf$category <- ifelse(catdf$ci.2wk == 0 & catdf$inc == 0, "none", catdf$category)
  
  #catdf$category[is.na(catdf$category)] <- "none"
  
  catdf$cat2 <- ifelse(catdf$ci.2wk > hb.cut & catdf$deriv1 >= slope.cut, "hbg", "not hbg")
  catdf$cat2 <- ifelse(catdf$n.2wk <= 5 & !is.na(catdf$n.2wk) & catdf$inc != 0, "lessthan5", catdf$cat2)
  catdf$cat2 <- ifelse(is.na(catdf$n.2wk) & catdf$inc != 0, "not hbg", catdf$cat2)
  
  catdf$cat2 <- ifelse(catdf$ci.2wk == 0 & catdf$inc > 0, "nonepast2wk", catdf$cat2)
  catdf$cat2 <- ifelse(catdf$ci.2wk == 0 & catdf$inc == 0, "none", catdf$cat2)
  # Growth in the first two weeks will be NA at this point. Recode to not hbg
  catdf$cat2[is.na(catdf$cat2)] <- "not hbg"
  
  return(catdf)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ End of GetCategory function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ Setting up folders for data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# #SETTING DIRECTORY FOR INTERNATIONAL TASK FORCE - if James, defaults to his own account, otherwise appends users' name to the path
# if(Sys.getenv("USERNAME")=="kux9") {
#   dir.root <- "C:/Users/kux9/OneDrive - CDC/COVID19/"
# } else dir.root <- paste0("C:/Users/",
#                           Sys.getenv("USERNAME"),
#                           "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")
# dir.root #check
# 
# 
# #Define Directories
# dir.data <- paste0(dir.root,"Data/")
# dir.who.regions <- paste0(dir.root,"Data/")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ Function to generate Hotspot file ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# base.data <- "Case"
# rate.cut = 10
# slope.cut = 0.1
# hb.cut = 100
# dsource = "JHU"

################################################
hotspot <- function(base.data,  # "Case" or "Mortality" 
                    rate.cut, 
                    slope.cut,
                    hb.cut, 
                    dsource){
  
  df.Countriesx <- fun_ncov(rfunctions.dir) # Pulling in case and death data
  df.Countries.Daily <- df.Countriesx %>% filter(data_source %in% dsource) 
  
  
  #consolidate the df.Countries.Daily dataset as some areas have multiple observations
  df.cases_fin <- df.Countries.Daily[
    c("country_code","Date",
      "WHO Region","Population 2018.x",
      "Cumulative Cases")] %>%   
    dplyr::group_by(country_code, Date,
                    `WHO Region`, `Population 2018.x`) %>% 
    dplyr::summarize(Cumulative_Cases = sum(`Cumulative Cases`, na.rm=T)) %>% 
    ungroup() %>% 
    filter(`Population 2018.x`>0 & !is.na(`Population 2018.x`)) %>% 
    spread(Date, Cumulative_Cases) %>% 
    mutate_if(is.numeric, ~replace(., .<0, 0)) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  
  #DEATHS
  df.deaths_fin <- df.Countries.Daily[
    c("country_code","Date",
      "WHO Region","Population 2018.x",
      "Cumulative Deaths")] %>%   
    dplyr::group_by(country_code, Date,
                    `WHO Region`, `Population 2018.x`) %>% 
    dplyr::summarize(Cumulative_Deaths = sum(`Cumulative Deaths`, na.rm=T)) %>% 
    ungroup() %>% 
    filter(`Population 2018.x`>0 & !is.na(`Population 2018.x`)) %>% 
    spread(Date, Cumulative_Deaths) %>% 
    mutate_if(is.numeric, ~replace(., .<0, 0)) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
### Setup data - use national data
  if (base.data == "Case") {
    ccount = df.cases_fin
    ccount$Type <- "CASES"}
  
  if (base.data == "Mortality") {
    ccount = df.deaths_fin
    ccount$Type <- "DEATHS"}

ccount$id <- 1:nrow(ccount)

  # should fix to subset to latest date!
  ccount <- ccount %>% 
    select(country_code:`Population 2018.x`,Type, id, everything()) %>% 
    # convert NAs to zero
      mutate_if(is.numeric, ~replace(., is.na(.), 0))


  ##convert to incidence
  cinc = ccount
  for(r in 1:nrow(ccount)) {
    cinc[r,-(1:5)] = 100000*ccount[r,-(1:5)]/(ccount$`Population 2018.x`[r]) #divide by population 2018
    #cinc[r,-(1:6)] = 100000*ccount[r,-(1:6)]/ccount$`Population 2018`[r]  
  }
  
#  colnames(cinc)[c(1:3)] <- c("name", "id", "pop")

# Remove contries with no cases ever 
  keep <- cinc$country_code[apply(ccount[,c(6:ncol(ccount))], 1, sum) != 0]
# View(keep)
#  keep <- cinc$Country[apply(cinc[4:ncol(cinc)], 1, function(x) max(x) >= 0.1)]

#  never.cases <- cinc[apply(ccount[,c(8:ncol(ccount))], 1, sum) == 0, c(1:4)]
#  cinc = cinc[cinc$id %in% keep,]
  cincx = cinc[cinc$country_code %in% keep,]
  cincx$id <- 1:nrow(cincx)
  cinc <- cincx %>% 
    select("country_code",         
           "WHO Region", 
           "Population 2018.x", 
           "Type", 
           id, everything())
  
  
# Rename columns (removed never.cases scenario)
colnames(ccount)[1:3] <- colnames(cinc)[1:3]

###### Common code for all geographic
##set up dates
strdate = as.Date(names(cinc)[6])
last.date = strdate + ncol(cinc) - 6
covid.dates = seq(from = strdate, to = last.date, by=1)

# Calculate the 2 week incidence
temp <- t(apply(cinc[,c(6:ncol(cinc))], 1, function(x) x-lag(x, n=14)))
ci.2wk <- cbind.data.frame(cinc[,1:5], temp)
remove(temp)

# Calculate the 2 week total cases
temp <- t(apply(ccount[,c(6:ncol(ccount))], 1, function(x) x-lag(x, n=14)))
n.2wkx <- cbind.data.frame(ccount[,1:5], temp)
remove(temp)
n.2wk <- n.2wkx %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# Since we can't calculate a 2 week incidence for the first 2 weeks, set it equal to the incidence
ci.2wk[,6:19] <- cinc[,6:19]

# calculate the 3-day mean
temp <- t(apply(cinc[,c(6:ncol(cinc))], 1, function(x) (x+lag(x, n=1)+lag(x, n=2))/3))
cum3x <- cbind.data.frame(cinc[,1:5], temp)
remove(temp)
cum3 <- cum3x %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# Calculate successive differences in CI
# This then transforms the data from cumulative incidence to daily incidence.
delta = cum3
for(c in 7:ncol(delta)) {
  delta[,c] = cum3[,c] - cum3[,c-1]
}

# Change all negative values to 0
delta[,6:ncol(delta)][delta[,6:ncol(delta)] < 0] <- 0

#####generate categories
#setwd("//cdc.gov/locker/NCIRD_nCoV_EpiTaskForce/DataTeam/Hot Spot/R analyses/results/")
# setwd(paste0(dir.root,"Epi Curves/Loess_Function_Derivatives/results"))
# getwd()


# Remove NAs in all datasets being used:
delta <- remove_nas(delta)
cum3 <- remove_nas(as.data.frame(cum3))
ci.2wk <- remove_nas(as.data.frame(ci.2wk))
n.2wk <- remove_nas(as.data.frame(n.2wk))

cat3 = NA
for(r in 1:nrow(delta)) {
  # print(paste(r, delta$Country[r], delta$id[r], sep="-"))
  # Using the 3 day average
  rin = as.numeric(cum3[cum3$id==delta$id[r], -c(1:5)])
  df = data.frame(name = delta$country_code[r],
                  id = delta$id[r],
                  dates = covid.dates,
                  pop = delta$`Population 2018`[r],
                  values = as.numeric(delta[r,-c(1:5)]),
                  inc = rin,
                  ci.2wk = as.numeric(ci.2wk[ci.2wk$id==delta$id[r],-(1:5)]),
                  n.2wk =  as.numeric(n.2wk[n.2wk$id==delta$id[r],-(1:5)]),
                  n.cum = as.numeric(ccount[ccount$id==delta$id[r],-(1:5)])
  ) 
  
  # Fit the spline
  spline_fit = smooth.spline(x=as.numeric(df$dates), y=df$values, spar=.5)
  df$spline = predict(spline_fit, as.numeric(df$dates))$y
  df$deriv1 = predict(spline_fit, as.numeric(df$dates), deriv=1)$y #first derivative
  df$deriv2 = predict(spline_fit, as.numeric(df$dates), deriv=2)$y #second derivative
  
  # Get the category
  df = getCategory(df, rate.cut, 
                   slope.cut, hb.cut)
  
  if(all(is.na(cat3))) {
    cat3 = df
  } else {
    cat3 = rbind(cat3, df)
  }
  
}

cat3 <- cat3[cat3$id != 0,]

############
# calculate the number of days in a downward trend
# Days count toward the downward trend if there was preceding growth or EIP and slope < 0 or LIP
# Any interruption of the downward trend resets the count

jurs = unique(cat3$id)
res = NA

for (j in jurs) {
  sub = cat3[cat3$id == j,]
  sub$numDaysDownward = NA
  
  ## Number of days in downward trajectory
  EI <- DT <- vector(length = nrow(sub))
  # Determine if each day is either elevated incidence or downward trajectory
  for (r in 1:nrow(sub)) {
    EI[r] <- ifelse(any(sub$category[1:r] %in% c("elevated incidence plateau", "elevated incidence growth")) &
                      sub$category[r] %in% c("elevated incidence plateau", "elevated incidence growth"),
                    TRUE,
                    FALSE)
    DT[r] <- ifelse(sub$category[r] != "none" & 
                      any(sub$category[1:r] %in% c("elevated incidence plateau", "elevated incidence growth")) &
                      (sub$deriv1[r] < 0 | sub$category[r] %in% c("low incidence plateau", "lessthan5")), 
                    TRUE, FALSE)
  }
  
  numDT <- rep(0, times = length(DT))
  numEI = 0
  for (r in 2:length(DT)) {
    if (DT[r]) {
      numDT[r] = numDT[r-1] + 1
      numEI = 0
    }
    else if (EI[r] & numDT[r-1] > 0) {
      numEI = numEI + 1
      if (numEI <= 5) {numDT[r] = numDT[r-1] + 1}
    }
    if (numEI == 5 & DT[r]) { numEI = 0 } # Reset the counter if there were exactly 5 EI and the next was DT
    # print(paste0(numDT[r], "-", numEI))
  }
  
  sub$numDaysDownward <- numDT
  
  # Rebound
  # Rebound is defined as not being in a downward trajectory following at last 14 days of being in a 
  # downward trajectory. Upnot14 is a rebound without the 14 day requirement
  sub$rebound <- NA
  for(r in 5:nrow(sub)) {
    sub$rebound[r] <- ifelse(sub$category[r] %in% c("none", "lessthan5","nonepast2wk"), sub$category[r],
                             ifelse(all(is.na(sub$numDaysDownward[1:r])), "no", 
                                    ifelse(max(sub$numDaysDownward[1:r], na.rm=TRUE) >= 14 & 
                                             sub$numDaysDownward[r] == 0, "rebound", 
                                           ifelse(max(sub$numDaysDownward[1:r], na.rm=TRUE) >= 1 & sub$numDaysDownward[r] == 0, 
                                                  "Upnot14", "no"))))
  }
  
  if(all(is.na(res))) {
    res = sub
  } else {
    res = rbind(res, sub)
  }
}

cat3 = res

###########
# Add the areas that don't have any reported cases back to the dataset
# Expand to add a row for every date
# never.cases[,"name"] <- as.character(never.cases[,"name"])
# never.cases[,"id"] <- as.character(never.cases[,"id"])
# never.cases$start <- as.Date(min(cat3$dates), format="%Y-%m-%d")
# never.cases$end <- as.Date(max(cat3$dates), format="%Y-%m-%d")
# # Ignore warnings
# never.cases <- never.cases %>% group_by(name, id, pop) %>%
#   do(data.frame(name = .$name, id=.$id, 
#                 dates=seq(.$start, .$end, by=1)))
# 
# never.cases <- never.cases[,c("name", "id", "dates", "pop")]
# 
# never.cases$category <- "none"
# never.cases$cat2 <- "none"
# never.cases$rebound <- "none"
# never.cases[,c("values", "inc", "ci.2wk", "n.2wk", "n.cum", "numDaysDownward")] <- 0
# never.cases[,c("spline", "deriv1")] <- NA
# 
# # Join the data with the never cases  
# cat3 <- rbind.data.frame(cat3, never.cases)

#####
# Calculate the percent of cases in the past 2 weeks
cat3$pct.n.2wk <- ifelse(cat3$n.cum > 0, 100*cat3$n.2wk/cat3$n.cum, 0)

# rename columns so they make sense
colnames(cat3) <- c("name", 
                    "id", 
                    "dates", 
                    "pop", 
                    "daily.inc", 
                    "cum.inc", 
                    "ci.2wk", 
                    "n.2wk", 
                    "n.cum", 
                    "spline", 
                    "daily.ci.change", 
                    "deriv2",
                    "epi.curve.cat", 
                    "hbg.cat", 
                    "numDaysDownward", 
                    "rebound", 
                    "pct.n.2wk")

if (base.data == "Mortality") { 
  colnames(cat3)[colnames(cat3) == "inc"] <- "mort"
  colnames(cat3)[colnames(cat3) == "ci.2wk"] <- "mort.2wk"
}

ds <- cat3 %>% 
  mutate(rate_cut = rate.cut,
         slope_cut = slope.cut,
         hb_cut = hb.cut,
         datatype = base.data) %>% 
  mutate(date_text = as.character(format(dates, "%Y-%m-%d")))


ds$epi.curve.cat[is.na(ds$epi.curve.cat)] <- "none" # These NAs happen in 3 counties after merging with the shapefile
ds$epi.curve.cat <- as.character(ds$epi.curve.cat)


# For the map, rebound is combined with the epi curve status
ds$epi.curve.map.cat <- ifelse(ds$rebound %in% c("rebound"), "rebound", ds$epi.curve.cat)

ds$epi.curve.map.cat[is.na(ds$epi.curve.map.cat)] <- "none"

ds$epi.curve.cat <- factor(ds$epi.curve.cat, levels = c("low incidence growth", 
                                                        "elevated incidence growth", 
                                                        "elevated incidence plateau", 
                                                        "decline", 
                                                        "low incidence plateau", 
                                                        "lessthan5", 
                                                        "nonepast2wk", 
                                                        "none"))

ds$epi.curve.map.cat <- factor(ds$epi.curve.map.cat, levels = c("low incidence growth", 
                                                                "elevated incidence growth", 
                                                                "elevated incidence plateau", 
                                                                "decline", 
                                                                "low incidence plateau", 
                                                                "rebound", 
                                                                "lessthan5", 
                                                                "nonepast2wk", 
                                                                "none"))

ds$growth.cat.labels <- Hmisc::capitalize(as.character(ds$epi.curve.cat))
ds$growth.cat.labels[ds$growth.cat.labels == "Decline"] <- "Sustained decline"
ds$growth.cat.labels[ds$growth.cat.labels == "None"] <- "No reported cases"
ds$growth.cat.labels[ds$growth.cat.labels == "Lessthan5"] <- "1-5 cases in the past two weeks"
ds$growth.cat.labels[ds$growth.cat.labels == "Nonepast2wk"] <- "0 cases in the past two weeks"

ds$growth.map.cat.labels <- Hmisc::capitalize(as.character(ds$epi.curve.map.cat))
ds$growth.map.cat.labels[ds$growth.map.cat.labels == "Decline"] <- "Sustained decline"
ds$growth.map.cat.labels[ds$growth.map.cat.labels == "None"] <- "No reported cases"
ds$growth.map.cat.labels[ds$growth.map.cat.labels == "Lessthan5"] <- "1-5 cases in the past two weeks"
ds$growth.map.cat.labels[ds$growth.map.cat.labels == "Nonepast2wk"] <- "0 cases in the past two weeks"

finaldf <- ds

return(finaldf)
}


alldata <- bind_rows(purrr::map2(.x = sort(rep(c(1,5,10,15),4)), 
                     .y = c(rep(c(.01,.05,.1,.5),4)), .f = ~hotspot("Case", .x, .y, 100, dsource=dfsource)))


allthedata <- alldata %>%
  mutate(data_source = dfsource) %>% 
  mutate(ou_date_match = paste(name, dates, sep="_")) %>% 
  #Creating variable that is not date dependent
  mutate(ou_cut_match = paste(name, slope_cut, rate_cut, sep="_")) %>% 
  rename(country_code = name) %>% 
  rename(date = dates) %>% 
  mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) %>% 
  mutate(ou_cut_src_match = paste(ou_cut_match, data_source, sep="_")) %>% 
  select(-ou_cut_match)
  
alldf <- allthedata

gisdata <- alldf %>% select(-ou_date_match, -ou_date_src_match) %>% 
  filter(date %in% max(date))

if (typex == "date"){
return(alldf)} else if (typex == "map"){
return(gisdata)}
  }
  
whodf <- final_hotspot("WHO", typex) 
jhudf <- final_hotspot("JHU", typex) 

trajdf <- bind_rows(whodf, jhudf) 

return(trajdf)
}

