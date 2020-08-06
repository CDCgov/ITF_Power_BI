# Sample script that runs all ITF Tracker code that gets data sets

rfunctions.dir<-"./Rfunctions"

# get base case and death data from JHU and WHO
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R")) 
cases_deaths <- fun_ncov(rfunctions.dir) 

# get longitudinal hotspot trajectory data (for creating curve graphics)
fun_traj <- dget(paste0(rfunctions.dir, "trajectory_function_final - call separate.R")) 
hotspot_data <- fun_traj("date", rfunctions.dir) 

#get cross-sectional hotspot trajectory data (for mapping)
hotspot_map <- fun_traj("map", rfunctions.dir) 

#get longitudinal risk matrix data (for creating curve graphics)
fun_risk <- dget(paste0(rfunctions.dir, "get_riskmatrix_v2.R")) 
riskmatrix_data <- fun_risk("date", rfunctions.dir)

#get cross-sectional risk matrix data (for mapping)
xriskmatrix_data <- fun_risk("cross", rfunctions.dir)

#get longitudinal testing data (for creating curves)
fun_tst<-dget(paste0(rfunctions.dir,"covid_testing.R"))
testing_data<-fun_tst("long")

#get cross-sectional testing data (for mapping)
testing_cross<-fun_tst("cross")





