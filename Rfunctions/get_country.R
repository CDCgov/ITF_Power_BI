#' Code for CDC Tracker Power BI file
#' This function reads in exclusively public data
#' @param functions.dir The directory where the other code resides
#' @return A dataframe of country metadata with standardized country-level identifiers by iso3code

function(functions.dir){

  # HACK: Revert commit when fixed. See CDCGov/ITF_Power_BI#3
  base_frame <- readr::read_csv("https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/itf_dashboard/output/country_data.csv")
  
  return(base_frame)
}
