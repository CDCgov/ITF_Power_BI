# Background
The "Hotspot Trajectory" is an algorithm that was created by the CDC COVID-19 Response Case Based Surveillance Task Force to classify the status of an epidemic curve 
based on the two-week incidence rate standardized to per 100,000 population, and the slope of a spline curve fit to the three-day average daily incidence.

# Setting up the data
The code has been adapted to be run using any data, as long as it is formatted in the following way:
Each row represents a single geographic unit, with the columns being the cumulative case or death count for each day
The first three columns have the headings "country_code", "WHO Region", and "`Population 2018.x`". The first two should be character variables, and can represent different
geographic levels (don't have to be country and region). The third column should be a numeric value representing the latest available estimate of the population for the 
geographic unit of that row.
The remaining columns should represent consecutive days of observed cases. The column headers should be the dates. Even if a day has 0 observed cases, a column still 
must be included with a "0" for the algorithm to apply.

# Input parameters
Once the code "traj_sub_functions.R" has been run, the function hotspot() can be called. There are five parameters that can be changed, including the input data frame name:

First parameter-   data.source:   This should be a data frame formatted according to the specifications listed above
Second parameter-  base.data:     This should be either "Case" or "Mortality" depending on whether the data you are analyzing represent cases or deaths
Third parameter-   slope.cut:     the cut-point that determines whether a spline curve is in "growth" vs "plateau"/"decline." The Case Based Surveillance Task Force
                                  and ITF Dashboard both use 0.1 for this parameter.
Fourth parameter-  rate.cut:      The cut point that determines whether the two-week indicence is considered "elevated" vs "low". The Case Based Surveillance Task Force
                                  and the ITFDashboard both use 10 for this parameter.
Fifth parameter-   hb.cut:        The cut point that determines whether a locality is in "high burden growth" status (along with the slope cut. The Case Based Surveillance 
                                  Task Force and the ITF both use 100 for this parameter.

# How to run the function
Once your data set is formatted, an example of a way to apply the hotspot trajectory is to run the following code:
new.data.frame<- hotspot(formatted.data.frame, "Case", 0.1, 10, 100)

The new.data.frame will contain the trajectory categorizations in the "growth.map.cat.labels" column

# Making maps
The file "hotspot_colors.csv" contains the hex codes for the different categories that the CDC has used in standard visualizations

