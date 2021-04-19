function(rfunctions.dir, df_jhu){


  # Pulling in the load package function R file
  # Load function to install list of packages
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))


  # Load/install packages 
  ldpkg(c("EpiEstim", 
          "ggplot2",
          "readxl"))
  
  # If JHU dataframe is missing as input, then call the script to generate it
  if (missing(df_jhu)) {
    # Function to get JHU data
    fun_jhu <- dget(paste0(rfunctions.dir, "get_jhu_data.R"))
    df_jhu <- fun_jhu(rfunctions.dir)
  }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Core EpiEstim function ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### epi_curve function #####
epi_curve_14<-function(country,ctyname)
{
  #run EpiEstim
  index2<-15:nrow(country)
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
                                   mean_si = 6.14,
                                   std_si  = 3.96))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_mtf)<-c("mean.mtf","lower.mtf","upper.mtf")
  
  ###########################################################################
  
  cbind(country=ctyname,date=country$date[index2],ncase=country$I[index2],res_biweekly_ni,res_biweekly_li,res_biweekly_mtf)
}
#**********************************************#

epi_curve_7<-function(country,ctyname)
{
  #run EpiEstim
  index2<-8:nrow(country)
  T0 <- nrow(country)
  t_start <- seq(2, T0-6) # starting at 2 as conditional on the past observations
  t_end <- t_start + 6
  res_weekly_ni <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 4.7,
                                  std_si = 2.9))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_weekly_ni)<-c("mean.ni","lower.ni","upper.ni")
  
  #####################################################################
  
  res_weekly_li <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 7.5,
                                  std_si = 3.4))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_weekly_li)<-c("mean.li","lower.li","upper.li")
  
  ######################################################################
  
  res_weekly_mtf <- estimate_R(country[,c("date","I")],
                                 method="parametric_si",
                                 config = make_config(list(
                                   t_start = t_start,
                                   t_end = t_end,
                                   mean_si = 6.14,
                                   std_si =  3.96))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_weekly_mtf)<-c("mean.mtf","lower.mtf","upper.mtf")
  
  ###########################################################################
  
# View(  
  cbind(country=ctyname,date=country$date[index2],ncase=country$I[index2],res_weekly_ni,res_weekly_li,res_weekly_mtf)
        # )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Overall function to generate Rt dataset ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
row.names(df_jhu)<-NULL
dx <- df_jhu
######country data
#d <- read_excel(paste0(dir.data, "JHU Data by Country - Long.xlsx"))
index<-which(dx$date==min(dx$date))
dx<-dx[-index,]

d <- dx %>% select(c(1:6))
names(d)<-c("country","date","I","ccase","ndeath","cdeath")
d$date <- as.Date(d$date,"%m/%d/%Y")
countries <- unique(d$country)

dsplit<-split(d[,c("country","date","I")],d$country)
N<-length(countries)

# epi_output<-lapply(1:N,function(i,x,y,z) epi_curve(x[[i]],y[i]),x=dsplit,y=countries)

epi_output_7  <- purrr::map2(.x = dsplit, .y=countries, .f = ~epi_curve_7(.x, .y)) 
epi_output_14 <- purrr::map2(.x = dsplit, .y=countries, .f = ~epi_curve_14(.x, .y))

epi_7 <- dplyr::bind_rows(epi_output_7) %>% mutate(windowx = "7-day")
epi_14 <- dplyr::bind_rows(epi_output_14) %>% mutate(windowx = "14-day")

combodf <- dplyr::bind_rows(epi_7, epi_14) 

row.names(combodf)<-NULL

# Getting cumulative cases to filter out values less than 12
combined_output <- combodf %>% 
  mutate(ncasex = if_else(is.na(ncase), 0, ncase)) %>% 
  group_by(country, windowx) %>%
  mutate(cum_cases = cumsum(ncase)) %>% 
  ungroup() 

combined_output[which(combined_output$cum_cases<12), c(4:12)] <- NA

# Creating the match variable so datasets can be joined in Power BI 
# using Country, Date ---> combines country code and date
names(df_jhu) <- tolower(names(df_jhu))
base_frame <- df_jhu %>% 
  select(ou_date_match, country, date, iso3code) %>% 
  unique()

finaldf <- left_join(combined_output, base_frame) %>% 
  mutate(Rt_baseline = 1)

finaldf 
}





