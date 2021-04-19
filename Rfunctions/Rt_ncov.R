function(rfunctions.dir, df_rt_jhu, df_rt_who){
  
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  
  # If RT JHU dataframe is missing as input, then call the script to generate it
  if (missing(df_rt_jhu)) {
    # Function to get RT JHU data
    fun_rt_jhu <- dget(paste0(rfunctions.dir, "Rt_jhu.R"))
    df_rt_jhu <- fun_rt_jhu(rfunctions.dir)
  }
  
  # If RT WHO dataframe is missing as input, then call the script to generate it
  if (missing(df_rt_who)) {
    # Function to get WHO data
    fun_rt_who <- dget(paste0(rfunctions.dir, "Rt_who.R"))
    df_rt_who <- fun_rt_who(rfunctions.dir)
  }

  
  # loading packages
  ldpkg(c("tidyverse"))
  
  
  jdf <- df_rt_jhu
  jdf$data_source = "JHU"
  
  wdf <- df_rt_who
  wdf$data_source = "WHO"
  
  df <- rbind(jdf, wdf) %>% 
    mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) 
  
  return(df)
}