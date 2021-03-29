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
  
final_hotspot  <- function(typex, dfsource){

  # Take out all NAs in the dataset and replace with zero
remove_nas <- function(df) { 
  df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}

# dir.root<- ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")),
#                   paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/"),
#                   ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/")),
#                          paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/"),
#                          "Directory does not exist"))

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

# Power BI script that pulls in all the analytic datasets in long format by Country and Date
# Folder path for all the Power BI scripts
# rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")


# function to get the base JHU data with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data_vDASH.R"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ===== Beginning of Epi curve segmentation Function ===============
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Define functions for COUNTRY (not state or county) - presumably updated since hotspot analysis v9
getCategory <- function(catdf, 
                        base.data,
                        slope.cut,
                        rate.cut,
                        hb.cut,
                        ti) {
  # Called by 'getNumberDaysDeclineBaselineMod'

  #catdf = catdf[!is.na(catdf$deriv1),]
  deriv1 = catdf$deriv1
  catdf$category = "seven day"
  catdf$category[deriv1 >= 0 & deriv1 < slope.cut] = "plateau"
  catdf$category[deriv1 >= slope.cut] = "growth"
  catdf$category[deriv1 < 0] = "decline"
  
  # If CI is zero, there are no cases
  catdf$category <- ifelse(catdf$inc == 0, "none", catdf$category) 
  
  # Correct so that there's not a plateau between growing and declining
  # for(r in 2:(nrow(catdf)-1)) {
  #   if(catdf$category[r-1] == "growth" & catdf$category[r]  == "plateau" & catdf$category[r+1]  == "decline") {
  #     catdf$category[r] = "growth"
  #   }
  #   if(catdf$category[r-1] == "decline" & catdf$category[r]  == "plateau" & catdf$category[r+1]  == "growth") {
  #     catdf$category[r] = "decline"
  #   }
  # }
  
  # The first 5 days of a decline should be a plateau
  ##sustained decline only after 5 days
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
  
  ##add incidence label for plateaus and growth
  catdf$category <- ifelse(catdf$ci.2wk <= rate.cut & !is.na(catdf$ci.2wk) & catdf$category == "plateau", 
                           "low incidence plateau",
                           ifelse(catdf$ci.2wk > rate.cut & catdf$category == "plateau", 
                                  "elevated incidence plateau", catdf$category))
  catdf$category <- ifelse(catdf$ci.2wk <= rate.cut & !is.na(catdf$ci.2wk) & catdf$category == "growth", "low incidence growth",
                           ifelse(catdf$ci.2wk > rate.cut & catdf$category == "growth", "elevated incidence growth", catdf$category))
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
  
  catdf[ti,c("category", "cat2")]
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ End of GetCategory function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ getNumberDaysDeclineFromCumulativeCountBaselineMod ~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hotspot <- function(base.data,
                      slope.cut,
                      rate.cut,
                      hb.cut,
                    dsource) {
  
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

  ##set up dates
  strdate = as.Date(names(ccount)[6])
  last.date = strdate + ncol(ccount) - 6
  covid.dates = seq(from = strdate, to = last.date, by=1)
  
  #####generate categories
  
  cat3 = NA
  for(r in ccount$id) {
    
    df = data.frame(name = ccount$country_code[ccount$id==r],
                    id   =  r,
                    dates = covid.dates,
                    pop = ccount$`Population 2018.x`[ccount$id==r],
                    n.cum = as.numeric(ccount[r,-(1:5)]), stringsAsFactors = FALSE)
    
    df$n.2wk <- df$n.cum-lag(df$n.cum,n=14)
    df$n.2wk[1:14] <- df$n.cum[1:14]
    
    df$inc.raw <- 100000 * df$n.cum / df$pop
    df$inc <- stats::filter(df$inc.raw, rep(1 / 3, 3), sides = 1)
    df$inc[1:3] <- df$inc.raw[1:3]
    df$inc <- as.numeric(df$inc)
    
    df$ci.2wk <- df$inc.raw - lag(df$inc.raw, n=14)
    df$ci.2wk[1:14] <- df$inc.raw[1:14]
    
    df$daily.inc.raw <- 100000 * c(0, diff(df$n.cum)) / df$pop
    df$daily.inc.raw[which(df$daily.inc.raw < 0)] <- 0
    
    df$values <- c(0, diff(df$inc))
    df$values[1:3] <- df$daily.inc.raw[1:3]
    
    ### Negative daily incidence values need to be set to zero -JG
    df$values[df$values < 0] <- 0
    
    df <- df[,c("name", "id", "dates", "pop", "values", "inc", "ci.2wk", "n.2wk", "n.cum")]
    df$spline <- 0
    df$deriv1 <- 0
    
    df$category <- "none"
    df$cat2 <- "none"
    df$numDaysDownward <- 0
    df$rebound <- "no"
    
    # #if a date is in the first 14 days, the algorithm does not calculate a trajectory category
    # df[1:14,]$category<-"first 14 days"
    # df[1:14,]$cat2<-"1st14"
    
    ## Number of days in downward trajectory
    EI <- DT <- vector(length = nrow(df))
    
    for (ti in 15:nrow(df)) {
      if (sum(df$inc[1:ti], na.rm = TRUE) > 0) {
        # Fit the spline
        spline_fit = smooth.spline(x=as.numeric(df$dates[1:ti]), y=df$values[1:ti], spar=.5)
        df$spline[1:ti] = predict(spline_fit, as.numeric(df$dates[1:ti]))$y
        df$deriv1[1:ti] = predict(spline_fit, as.numeric(df$dates[1:ti]), deriv=1)$y #first derivative
        
        tdf <- df[1:ti,]
        # Get the category
        df[ti,c("category", "cat2")] = getCategory(tdf, base.data, 
                                                   slope.cut=slope.cut, rate.cut=rate.cut, ti)[ti,]
      }
      
      
      ###
      # calculate the number of days in a downward trend
      # Days count toward the downward trend if there was preceding growth or EIP and slope < 0 or LIP
      # Any interruption of the downward trend resets the count
      
      # Determine if each day is either elevated incidence or downward trajectory
      
      EI[ti] <- ifelse(any(df$category[1:ti] %in% c("elevated incidence plateau", "elevated incidence growth")) &
                         df$category[ti] %in% c("elevated incidence plateau", "elevated incidence growth", "low incidence growth"), TRUE, FALSE)
      DT[ti] <- ifelse(df$category[ti] != "none" & 
                         any(df$category[1:ti] %in% c("elevated incidence plateau", "elevated incidence growth")) &
                         (all(df$deriv1[(ti-5):ti] < 0) | df$category[ti] %in% c("low incidence plateau", "lessthan5", "nonepast2wk")), 
                       TRUE, FALSE)
      
      
      numDT <- rep(0, times=ti)
      numEI = 0
      for (rr in 2:ti) {
        if (DT[rr]) {
          numDT[rr] = numDT[rr-1] + 1
          numEI = 0
        }
        else if (EI[rr] & numDT[rr-1] > 0) {
          numEI = numEI + 1
          if (numEI <= 5) {numDT[rr] = numDT[rr-1] + 1}
        }
        if (numEI == 5 & DT[rr]) { numEI = 0 } # Reset the counter if there were exactly 5 EI and the next was DT
        #print(paste0(numDT[r], "-", numEI))
      }
      
      df$numDaysDownward[ti] <- numDT[ti]
      
      # Rebound
      # Rebound is defined as not being in a downward trajectory following at last 14 days of being in a 
      # downward trajectory. Upnot14 is a rebound without the 14 day requirement
      
      df$rebound[ti] <- ifelse(df$category[ti] %in% c("none", "lessthan5","nonepast2wk"), df$category[ti],
                               ifelse(all(is.na(df$numDaysDownward[1:ti])), "no", 
                                      ifelse(max(df$numDaysDownward[1:ti], na.rm=TRUE) >= 14 & 
                                               df$numDaysDownward[ti] == 0, "rebound", 
                                             ifelse(max(df$numDaysDownward[1:ti], na.rm=TRUE) >= 1 & df$numDaysDownward[ti] == 0, 
                                                    "Upnot14", "no"))))
      
    }
    
    if(all(is.na(cat3))) {
      cat3 = df
    } else {
      cat3 = rbind(cat3, df)
    }
  }
  
  # Calculate the percent of cases in the past 2 weeks
  cat3$pct.n.2wk <- ifelse(cat3$n.cum > 0, 100*cat3$n.2wk/cat3$n.cum, 0)
  
  # rename columns so they make sense
  colnames(cat3) <- c("name", "id", "dates", "pop", "daily.inc", "cum.inc", "ci.2wk", "n.2wk", "n.cum", "spline", 
                      "daily.ci.change", "epi.curve.cat", "hbg.cat", "numDaysDownward", "rebound", 
                      "pct.n.2wk")
  
  if (base.data == "Mortality") { 
    colnames(cat3)[colnames(cat3) == "inc"] <- "mort"
    colnames(cat3)[colnames(cat3) == "ci.2wk"] <- "mort.2wk"
  }


# base.data <- "Case"
# rate.cut = 10
# slope.cut = 0.1
# hb.cut = 100
# dsource = "JHU"

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

# alldata <- bind_rows(purrr::map2(.x = c(rep(c(.01,.05,.1,.5),4)), 
#                      .y = sort(rep(c(1,5,10,15),4)), .f = ~hotspot("Case", .x, .y, 100, dsource=dfsource)))

alldata <- hotspot("Case", 0.1, 10, 100, dsource=dfsource)


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
  
whodf <- final_hotspot(typex, "WHO") 
jhudf <- final_hotspot(typex, "JHU") 

trajdf <- bind_rows(whodf, jhudf) 

return(trajdf)
}

