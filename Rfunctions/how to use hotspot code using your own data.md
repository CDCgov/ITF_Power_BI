# Background
The "Hotspot Trajectory" is an algorithm that was created by the CDC COVID-19 Response Case Based Surveillance Task Force to classify the status of an epidemic curve 
based on the two-week incidence rate standardized to per 100,000 population, and the slope of a spline curve fit to the average daily incidence.

Once the following code is run, the function "hotspot" can be used to calculate the trajectory, as long as your input data is formatted correctly:
source("https://raw.githubusercontent.com/CDCgov/ITF_Power_BI/master/Rfunctions/traj_sub_functions.R")

# Setting up the data
The code has been adapted to be run using any data, as long as it is formatted in the following way:

Each row represents a day for single geographic unit, with two columns for population and the the cumulative case or death count for that day

The first two columns have the headings "id", and "date". The id column should be a unique identfier for your geographic unit of interest, and can represent different
geographic levels (don't have to be country and region). The third column, "pop" should be a numeric value representing the latest available estimate of the population for the 
geographic unit of that row. The fourth column should represent the cumulative case count up to that date.

# Input parameters
Once the code "traj_sub_functions.R" has been run, the function hotspot() can be called. There are five parameters that can be changed, including the input data frame name:

First parameter-   data.source:   This should be a data frame formatted according to the specifications listed above
Second parameter-  base.data:     This should be either "case" or "Mortality" depending on whether the data you are analyzing represent cases or deaths
Third parameter-   slope.cut:     the cut-point that determines whether a spline curve is in "growth" vs "plateau"/"decline." The Case Based Surveillance Task Force
                                  and ITF Dashboard both use 0.1 for this parameter.
Fourth parameter-  rate.cut:      The cut point that determines whether the two-week indicence is considered "elevated" vs "low". The Case Based Surveillance Task Force
                                  and the ITFDashboard both use 10 for this parameter.
Fifth parameter-   hb.cut:        The cut point that determines whether a locality is in "high burden growth" status (along with the slope cut. The Case Based Surveillance 
                                  Task Force and the ITF both use 100 for this parameter.
Sixth parameter-   n:             The number of days to use to calculate the average daily incidence for input into the curve fit (used to be 3, currently ITF uses 7)
Seventh parameter- spar.param:     The smoothing parameter for the smooth.spline function (currently ITF uses 0.6)

# How to run the function
Once your data set is formatted, an example of a way to apply the hotspot trajectory is to run the following code:

new.data.frame<- hotspot(formatted.data.frame, "case", 0.1, 10, 100,7,0.6)

The new.data.frame will contain the trajectory categorizations in the "growth.map.cat.labels" column

# Making maps
The file "hotspot_colors.csv" contains the hex codes for the different categories that the CDC has used in standard visualizations

