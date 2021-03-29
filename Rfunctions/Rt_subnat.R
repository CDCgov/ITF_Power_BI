function(rfunctions.dir){
  
  #SETTING DIRECTORY FOR INTERNATIONAL TASK FORCE
  # dir.root<- ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")),
  #                   paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/"),
  #                   ifelse(dir.exists(paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/")),
  #                          paste0("C:/Users/",Sys.getenv("USERNAME"),"/CDC/ITF-COVID19 International Task Force - DataViz/Data and Analysis/"),
  #                          "Directory does not exist"))
  
  # folder where all R functions are housed
  # rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  
  
  # Function to load
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
  
  # Creating the 'not in' function
  `%ni%` <- Negate(`%in%`) 
  

  # function to get the base JHU data with cases and deaths daily/cumulative 
  jhu_subnat <- dget(paste0(rfunctions.dir, "get_jhu_subnat.R"))
  
  
  # Load/install packages 
  ldpkg(c("EpiEstim",
          "Hmisc", 
          "tidyverse",
          "openxlsx",
          "passport",
          "readxl"))

data.longx <- jhu_subnat(rfunctions.dir)

############## epi_curve function 7 day sliding window####################################
epi_curve<-function(country,ctyname,index2)
{
  #run EpiEstim
  index2<-8:nrow(country)
  T0 <- nrow(country)
  t_start <- seq(2, T0-6) # starting at 2 as conditional on the past observations
  t_end <- t_start + 6
  res_biweekly_ni <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 4.7,
                                  std_si = 2.9))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_ni)<-c("mean.ni","lower.ni","upper.ni")
  
  #####################################################################
  
  res_biweekly_li <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 7.5,
                                  std_si = 3.4))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_li)<-c("mean.li","lower.li","upper.li")
  
  ######################################################################
  
  res_biweekly_mtf <- estimate_R(country[,c("date","I")],
                                 method="parametric_si",
                                 config = make_config(list(
                                    t_start = t_start,
                                    t_end = t_end,
                                   mean_si = 5.61,
                                   std_si = 4.53))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_mtf)<-c("mean.mtf","lower.mtf","upper.mtf")
  
  ###########################################################################
  
  cbind(country=ctyname,date=country$date[index2],ncase=country$I[index2],country_actual=country$Country_Region[index2],province_state=country$Province_State[index2],ccase=country$ccase[index2],res_biweekly_ni,res_biweekly_li,res_biweekly_mtf)
}
#**********************************************#
d <- data.longx
d$Date <- format(as.Date(d$Date), "%m/%d/%Y")

index_date<-which(d$Date=="01/22/2020")
d<-d[-index_date,]

# index_country<-which(d$`Province/State`=="")
# d<-d[-index_country,]
d$Lat <- NULL
d$Long <- NULL
d$Country <- NULL

names(d)<-c("Province_State","Country_Region","date","ccase","cdeath","country","I","ndeath")
#countries <- unique(d$country)

dsplit<-split(d[,c("Province_State","Country_Region","date","ccase","cdeath","country","I","ndeath")],d$country)
countries <- names(dsplit)
N<-length(countries)

index2<-8:nrow(dsplit[[1]])

epi_output_7<-lapply(1:N,function(i,x,y,z) epi_curve(x[[i]],y[i],z),x=dsplit,y=countries,z=index2)
names(epi_output_7)<-countries

subnational_7<-do.call("rbind",epi_output_7)
row.names(subnational_7)<-NULL



#### epi_curve function - 14 day sliding window ############################## #####
epi_curve_14<-function(country,ctyname,index2)
{
  #run EpiEstim
  T0 <- nrow(country)
  t_start <- seq(2, T0-13) # starting at 2 as conditional on the past observations
  t_end <- t_start + 13
  res_biweekly_ni <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 4.7,
                                  std_si = 2.9))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_ni)<-c("mean.ni","lower.ni","upper.ni")
  
  #####################################################################
  
  res_biweekly_li <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 7.5,
                                  std_si = 3.4))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_li)<-c("mean.li","lower.li","upper.li")
  
  ######################################################################
  
  res_biweekly_mtf <- estimate_R(country[,c("date","I")],
                                 method="parametric_si",
                                 config = make_config(list(
                                   t_start = t_start,
                                   t_end = t_end,
                                   mean_si = 5.61,
                                   std_si = 4.53))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_mtf)<-c("mean.mtf","lower.mtf","upper.mtf")
  
  ###########################################################################
  
  cbind(country=ctyname,date=country$date[index2],ncase=country$I[index2],country_actual=country$Country_Region[index2], province_state=country$Province_State[index2],ccase=country$ccase[index2],res_biweekly_ni,res_biweekly_li,res_biweekly_mtf)
}

d <- data.longx
d$Date <- format(as.Date(d$Date), "%m/%d/%Y")

index_date<-which(d$Date=="01/22/2020")
d<-d[-index_date,]
# 
# index_country<-which(d$`Province/State`=="")
# d<-d[-index_country,]
d$Lat <- NULL
d$Long <- NULL
d$Country <- NULL

names(d)<-c("Province_State","Country_Region","date","ccase","cdeath","country","I","ndeath")
#countries <- unique(d$country)

dsplit<-split(d[,c("Province_State","Country_Region","date","ccase","cdeath","country","I","ndeath")],d$country)
countries <- names(dsplit)
N<-length(countries)

index2<-15:nrow(dsplit[[1]])

epi_output_14<-lapply(1:N,function(i,x,y,z) epi_curve_14(x[[i]],y[i],z),x=dsplit,y=countries,z=index2)
names(epi_output_14)<-countries

subnational_14<-do.call("rbind",epi_output_14)
row.names(subnational_14)<-NULL

subnat_7 <- subnational_7 %>% mutate(windowx = "7-day")
subnat_14 <- subnational_14 %>% mutate(windowx = "14-day")

finaldf <- bind_rows(subnat_14, subnat_7)

combined_output <- finaldf %>% 
  filter(!is.na(province_state)) %>% 
  filter(province_state %ni% c("")) %>% 
  mutate(Rt_baseline = 1)
  # mutate(ncasex = if_else(is.na(ncase), 0, ncase)) %>% 
  # group_by(country, windowx) %>%
  # mutate(cum_cases = cumsum(ncase)) %>% 
  # ungroup() 

# combined_output[which(combined_output$cum_cases<12), c(4:12)] <- NA
return(combined_output)
}
