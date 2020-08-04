# Code for CDC Tracker Power BI file
# this code is called by an R script run in a Power Query that feeds into a Power BI Report
# all packages must already be installed on the version of R that is called by Power BI
# it will output two possible files: a cross-sectional file (requires "cross" for typex) or a longitudinal file (requires "long" for typex)
# it requires an object called "rfunctions.dir" which is the directory where the other code, "get_ncov_data.R" and "traj_sub_functions.R", is stored
# This code calls applies an algorithm to identify the trajectory status of epidemic curves for cumulative incidence or mortality 
# This algorithm was developed by the CDC Case Based Surveillance Task Force and will potentially be updated in the future
# Last update to the trajectory algorithm: 7/23/2020

# These are the two key functions of the trajectory code- hotspot() calls getCategory()
# ****FOR THOSE WHO WOULD LIKE TO USE THE ALGORITHM ON THEIR OWN DATA****
# hotspot() can take ANY data source as long as it is formatted as WIDE data frame,
# set up with THREE FIRST COLUMNS: column names:
#1. "country_code" (can be a state or municipality)
#2. "WHO Region" can be blank, but needs to be in the first 6 columns
#3. `Population 2018.x` (can be any year) 
#4. Separate columns with the CUMULATIVE case or death counts for each DAY, with the column names being the date


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



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ hotspot function (called getNumberDaysDeclineFromCumulativeCountBaselineMod in CBS TF code) ~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hotspot <- function(input.data,
                    base.data,
                    slope.cut,
                    rate.cut,
                    hb.cut,
                    dsource) {
  
  ccount<-input.data
  
  ### Setup data - use national data
  if (base.data == "Case") {
    ccount$Type <- "CASES"
  }
  
  if (base.data == "Mortality") {
    ccount$Type <- "DEATHS"
  }
  
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
  
  ds <- cat3 %>% 
    mutate(rate_cut = rate.cut,
           slope_cut = slope.cut,
           hb_cut = hb.cut,
           datatype = base.data) %>% 
    mutate(date_text = as.character(format(dates, "%Y-%m-%d")))
  
  
  ds$epi.curve.cat[is.na(ds$epi.curve.cat)] <- "none" # These NAs happen in 3 counties after merging with the shapefile
  ds$epi.curve.cat <- as.character(ds$epi.curve.cat)
  
  #if a date is in the first 14 days, the algorithm does not calculate a trajectory category
  ds[1:14,]$epi.curve.cat<-"first 14 days"
  
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
                                                          "none",
                                                          "first 14 days"))
  
  ds$epi.curve.map.cat <- factor(ds$epi.curve.map.cat, levels = c("first 14 days",
                                                                  "low incidence growth", 
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
  ds$growth.cat.labels[ds$growth.cat.labels == "First 14 days"] <- "First 14 days of data"
  
  ds$growth.map.cat.labels <- Hmisc::capitalize(as.character(ds$epi.curve.map.cat))
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "Decline"] <- "Sustained decline"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "None"] <- "No reported cases"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "Lessthan5"] <- "1-5 cases in the past two weeks"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "Nonepast2wk"] <- "0 cases in the past two weeks"
  ds$growth.map.cat.labels[ds$growth.map.cat.labels == "First 14 days"] <- "First 14 days of data"
  
  finaldf <- ds
  
  return(finaldf)
}