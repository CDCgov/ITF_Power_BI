#collate hospitalization data from Euro CDC, OWID and other sources for ITF internal dashboard

hospdata_combine<-function(){
    #read in data
  owid_data<-fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/hospitalizations/covid-hospitalizations.csv") %>%
    select(-entity) %>%
    mutate(source="OWID")
  ecdc_data<-fread("https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv") %>%
    mutate(iso_code=passport::parse_country(country,to = "iso3c")) %>%
    select(-source,-url,-year_week,-country) %>%
    mutate(source="ECDC")
  #add and mutate other sources so they look like ecdc (i.e. long by date/indicator)
  
  ecdc_indicators<-unique(ecdc_data$indicator)
  owid_indicators<-unique(owid_data$indicator)
  all_dates <- seq.Date(from=as.Date(min(owid_data$date,ecdc_data$date)), to=as.Date(max(ecdc_data$date,owid_data$date)), by="day")
  
  #append and transform
  hospdata<-bind_rows(ecdc_data,owid_data) 
  hosp_list <- list("hospdata" = hospdata, "ecdc_indicators" = ecdc_indicators, "owid_indicators" = owid_indicators)
  
  return(hosp_list)
}

 cross_hosp<-function(hospdata){
  
  hospdata_recent<-hospdata %>%
    group_by(iso_code,indicator,source) %>%
    filter(!is.na(value)) %>%
    arrange(date) %>%
    slice_max(order_by = date, n = 1) %>%
    ungroup()
  return(hospdata_recent)
 }
 
  long_hosp_fill<-function(hospdata,ecdc_indicators,owid_indicators){
  
    
  hospdata_long<-hospdata  %>%
  tidyr::complete(iso_code,indicator,source,date) %>%
  group_by(iso_code,indicator,source) %>%
  arrange(date) %>%
  mutate(carry_fwd_value=zoo::na.locf(value,na.rm=F,maxgap=14)) %>%
  ungroup() %>%
  #remove unoverlapping indicators
  filter((source=="ECDC" & indicator %in% ecdc_indicators) | (source=="OWID" & indicator %in% owid_indicators) )
  return(hospdata_long)
}