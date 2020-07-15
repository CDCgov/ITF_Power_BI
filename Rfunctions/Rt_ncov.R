function(){

  dir.root <- paste0("C:/Users/",
                     Sys.getenv("USERNAME"),
                     "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")
  
  
  rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")
  
  ldpkg <- dget(paste0(rfunctions.dir, "ldpkg.R"))
  jhu <- dget(paste0(rfunctions.dir, "Rt_jhu.R"))
  who <- dget(paste0(rfunctions.dir, "Rt_who.R"))

  
  # loading packages
  ldpkg(c("tidyverse"))
  
  
  jdf <- jhu() 
  jdf$data_source = "JHU"
  
  wdf <- who() 
  wdf$data_source = "WHO"
  
  df <- rbind(jdf, wdf) %>% 
    mutate(ou_date_src_match = paste(ou_date_match, data_source, sep="_")) 
  
  return(df)
  }