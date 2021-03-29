# R functions used in dashboard
The code in this repository assumes a file structure where all code is stored in the same folder, and most require that filepath to be passed as a parameter called "rfunctions.dir." To learn more about using R in Power BI, please consult this resource: https://docs.microsoft.com/en-us/power-bi/connect-data/desktop-r-scripts. 
These functions can also be run outside of Power BI to obtain the underlying data sets to do additional analyses with, or to create CSVs which can be read into Power BI.

An example piece of code that can be run in R or called from the Power BI Power Query to run the code is as follows:

### Example R script that pulls in the case and death data sets (a data frame called "ncov_cases_deaths")
rfunctions.dir <- {THE FILE PATH WHERE ALL THE CODE IS SAVED LOCALLY}

fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))

ncov_cases_deaths <- fun_ncov(rfunctions.dir)

### Example R script that pulls in the longitudinal "hotspot trajectory" data set (a data frame called "hotspot_data")
rfunctions.dir <- {THE FILE PATH WHERE ALL THE CODE IS SAVED LOCALLY}

fun_traj <- dget(paste0(rfunctions.dir, "trajectory_function_final - call separate.R"))

hotspot_data <- fun_traj("date", rfunctions.dir)

### Example R script that pulls in the cross-sectional "Risk matrix" data set (a data frame called "xriskmatrix")

rfunctions.dir <- {THE FILE PATH WHERE ALL THE CODE IS SAVED LOCALLY}

fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix_v2.R"))

hotspot_data <- fun_traj("crpss", rfunctions.dir)
