# R functions used in dashboard
The code in this repository assumes a file structure where all code is stored in the same folder, and most require that filepath to be passed as a parameter called "rfunctions.dir." To learn more about using R in Power BI, please consult this resource: https://docs.microsoft.com/en-us/power-bi/connect-data/desktop-r-scripts. An example piece of code that can be called from the Power BI Power Query to run the code is as follows:

### Example Power BI script that pulls in the case and death data sets 
rfunctions.dir <- {THE FILE PATH WHERE ALL THE CODE IS SAVED LOCALLY}

fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))

ncov_cases_deaths <- fun_ncov(rfunctions.dir)

