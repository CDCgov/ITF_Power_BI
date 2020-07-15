# 
# Script to create binary variables for Mitigation re-opening
# 
# 
# 
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Functions used in code ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# function(typex){

  # Take out all NAs in the dataset and replace with zero
remove_nas <- function(df) { 
  df %>% mutate_if(is.numeric, ~replace(., is.na(.), 0))}

dir.root <- paste0("C:/Users/",
                   Sys.getenv("USERNAME"),
                   "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")

# Pulling in the load package function R file
# Load function to install list of packages
ldpkg <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

# Creating the 'not in' function
`%ni%` <- Negate(`%in%`) 

# Loading the packages
ldpkg(c(
        "Hmisc", 
        "tidyverse",
        "openxlsx",
        "passport",
        "readxl",
        "grid",
        "gridExtra", 
        "ggpubr",
        "RColorBrewer"
))

# Power BI script that pulls in all the analytic datasets in long format by Country and Date
# Folder path for all the Power BI scripts
rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")


# function to get the base JHU data with cases and deaths daily/cumulative 
fun_ncov <- dget(paste0(rfunctions.dir, "get_ncov_data.R"))

fun_frame <- dget(paste0(rfunctions.dir, "get_country_date.R"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~ Function to generate datasets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df.Countriesx <- fun_ncov() # Pulling in case and death data
df.Countries.Daily <- df.Countriesx %>% filter(data_source %in% dsource) 


#consolidate the df.Countries.Daily dataset as some areas have multiple observations
df.cases_fin <- df.Countries.Daily[
  c("country_code","Date",
    "WHO Region","Population 2018.x",
    "Cumulative Cases")] %>% 
  rename(Cumulative_Cases = `Cumulative Cases`) %>%  
  spread(Date, Cumulative_Cases) 
# head(df.cases_fin)

#DEATHS
df.deaths_fin <- df.Countries.Daily[
  c("country_code","Date",
    "WHO Region","Population 2018.x",
    "Cumulative Deaths")] %>% 
  rename(Cumulative_Deaths = `Cumulative Deaths`) %>%  
  spread(Date, Cumulative_Deaths) 

# head(df.deaths_fin)
# setdiff(names(df.cases_fin), names(df.deaths_fin))
df.cases_fin <<- df.cases_fin
df.deaths_fin <<- df.deaths_fin
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ===== Beginning of Epi curve segmentation Function ===============
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ccount = df.cases_fin
    ccount$Type <- "CASES"
    
    ccount$id <- 1:nrow(ccount)
    
    # should fix to subset to latest date!
    ccount <- ccount %>% 
      select(country_code:`Population 2018.x`,Type, id, everything()) %>% 
      # convert NAs to zero
      mutate_if(is.numeric, ~replace(., is.na(.), 0))
    
    
    ##convert to incidence
    cinc = ccount
    for(r in 1:nrow(ccount)) {
      cinc[r,-(1:5)] = 100000*ccount[r,-(1:5)]/(ccount$`Population 2018.x`[r]) #divide by population 2018
      #cinc[r,-(1:6)] = 100000*ccount[r,-(1:6)]/ccount$`Population 2018`[r]  
    }
    
    #  colnames(cinc)[c(1:3)] <- c("name", "id", "pop")
    
    # View(keep)
    #  keep <- cinc$Country[apply(cinc[4:ncol(cinc)], 1, function(x) max(x) >= 0.1)]
    
    #  never.cases <- cinc[apply(ccount[,c(8:ncol(ccount))], 1, sum) == 0, c(1:4)]
    #  cinc = cinc[cinc$id %in% keep,]
    cinc$id <- 1:nrow(cinc)
    cinc <- cinc %>% 
      select("country_code",         
             "WHO Region", 
             "Population 2018.x", 
             "Type", 
             id, everything())
    
    
    # Rename columns (removed never.cases scenario)
    colnames(ccount)[1:3] <- colnames(cinc)[1:3]
    
    ###### Common code for all geographic
    ##set up dates
    last.date = as.Date("2020-01-22") + ncol(cinc) - 6
    covid.dates = seq(from = as.Date("2020-01-22"), to = last.date, by=1)
 
who_cat_fun <- function(days){       
    # # Calculate the week incidence
    temp <- t(apply(cinc[,c(6:ncol(cinc))], 1, function(x) x-lag(x, n=days)))
    ci.wkx <- cbind.data.frame(cinc[,1:5], temp)
    remove(temp)
    ci.wks <- ci.wkx %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) 
  
    
    # Calculate the week total cases
    temp <- t(apply(ccount[,c(6:ncol(ccount))], 1, function(x) x-lag(x, n=days)))
    n.wkx <- cbind.data.frame(ccount[,1:5], temp)
    remove(temp)
    n.wks <- n.wkx %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0))
    
    # Since we can't calculate a 2 week incidence for the first 2 weeks, set it equal to the incidence
    ci.wks[,6:12] <- cinc[,6:12]
    
    nolock <- c(
      "Bulgaria",
      "Denmark",
      "Estonia",
      "Finland",
      "Iceland",
      "Latvia",
      "Lithuania",
      "Macedonia",
      "Montenegro",
      "Netherlands",
      "Norway",
      "Sweden",
      "Switzerland")
    
# read in mitigation relaxation dates
    mitx <- read.csv(paste0(dir.root,
    "Epi Curves/Epi Curves for Every Country/","mit_relax.csv")) %>% 
      filter(country %ni% c("Cyprus", "Andorra")) %>% 
      mutate(lockdown = if_else(country %in% nolock, "No", "Yes")) %>% 
      mutate(Country = case_when(
        country %in% c("Macedonia")      ~     "North Macedonia",
        country %in% c("United Kingdom") ~    "The United Kingdom" ,
        country %in% c("Kyrgyz Republic") ~    "Kyrgyzstan" ,
        country %in% c("Slovak Republic") ~    "Slovakia" ,
        TRUE ~ country
      )) %>% 
      mutate(lifted = as.Date(lifted, format = "%m/%d/%Y"))
    
    # # country list 
    # mitvec <- unique(mitx$Country)
    # citvec  <- unique(ci.wk$Country)
    # 
    # setdiff(mitvec, citvec)

ci.wk <- left_join(ci.wks, mitx) %>% filter(!is.na(country))   
ci.wk$id <- 1:nrow(ci.wk)

n.wk  <- left_join(n.wks, mitx) %>% filter(!is.na(country))    
n.wk$id <- 1:nrow(n.wk)

ci.wk <- ci.wk %>% 
  select("Country"           ,
         "WHO Region"       ,
         "Population 2018.x" ,
         "Type"             ,
         "id",
         "country"          ,
         "lifted"            ,
         "lockdown",
         "type"             ,
         "notes", everything())

n.wk <- n.wk %>% 
  select("Country"           ,
         "WHO Region"       ,
         "Population 2018.x" ,
         "Type"             ,
         "id",
         "country"          ,
         "lifted"            ,
         "lockdown",
         "type"             ,
         "notes", everything())

    case_cut_3wk <- function(r){
      
      df = data.frame(name = n.wk[r,]$Country,
                      id = ci.wk$id[r],
                      dates = covid.dates,
                      pop = ci.wk$`Population 2018.x`[r],
                      values = as.numeric(ci.wk[r,-c(1:10)]),
                      cases = as.numeric(n.wk[r,-c(1:10)])      ) 
    
      
      # Fit the spline
      spline_fit = smooth.spline(x=as.numeric(df$dates), y=df$values, spar=.5)
      df$spline = predict(spline_fit, as.numeric(df$dates))$y
      df$deriv1 = predict(spline_fit, as.numeric(df$dates), deriv=1)$y #first derivative
      df$deriv2 = predict(spline_fit, as.numeric(df$dates), deriv=2)$y #second derivative
      

  dff <- df %>% 
    mutate(cat = if_else(deriv1 > 0, "growth", "no growth")) 
  
  dfx <- dff %>% filter(dates <= ci.wk$lifted[r])

  #Where peak is detected in latest growth
  peak_date <- ci.wk$lifted[r] - 21

  who_case_cat <- if_else(dfx[dfx$dates==peak_date,]$values*0.50 >= 
                              dfx[dfx$dates==ci.wk$lifted[r],]$values, "Met criteria",
                            "Not met")

dfinal <- df %>% 
  mutate(whocat = who_case_cat,
         datelifted = ci.wk$lifted[r],
         axisx = dates - datelifted,
         peakdate = peak_date,
         lockdown = ci.wk[r,]$lockdown)


return(dfinal) }
    
    
    
    case_cut_pk <- function(r){
      
      df = data.frame(name = n.wk[r,]$Country,
                      id = ci.wk$id[r],
                      dates = covid.dates,
                      pop = ci.wk$`Population 2018.x`[r],
                      values = as.numeric(ci.wk[r,-c(1:10)]),
                      cases = as.numeric(n.wk[r,-c(1:10)])      ) 
      
      
      # Fit the spline
      spline_fit = smooth.spline(x=as.numeric(df$dates), y=df$values, spar=.5)
      df$spline = predict(spline_fit, as.numeric(df$dates))$y
      df$deriv1 = predict(spline_fit, as.numeric(df$dates), deriv=1)$y #first derivative
      df$deriv2 = predict(spline_fit, as.numeric(df$dates), deriv=2)$y #second derivative
      
      
      dff <- df %>% 
        mutate(cat = if_else(deriv1 > 0, "growth", "no growth")) 
      
      dfx <- dff %>% filter(dates <= ci.wk$lifted[r])
      
      # Identify latest peak
      # Get a list of all the sequences of declines
      growths <- split(which(dfx$cat == "growth"), 
                       cumsum(c(1, diff(which(dfx$cat == "growth")) != 1)))
      
      who_case_cat <- "no peaks detected"
      
      if(length(growths)>0){    
        
        latest_g <- growths[[max(length(growths))]]  
        
        #Where peak is detected in latest growth
        peak_date <- dfx[which(dfx$values == max(dfx[latest_g,]$values)),]$dates[1]
        
        who_case_cat <- if_else(dfx[dfx$dates==peak_date,]$values*0.50 >= 
                                  dfx[dfx$dates==ci.wk$lifted[r],]$values, "Met criteria",
                                "Not met") }
      
      dfinal <- df %>% 
        mutate(whocat = who_case_cat,
               datelifted = ci.wk$lifted[r],
               axisx = dates - datelifted,
               peakdate = peak_date,
               peakval  = dfx[dfx$dates==peak_date,]$values,
               lockdown = ci.wk[r,]$lockdown)
      
      
      return(dfinal) }
    
  
case_cat_3wk <- dplyr::bind_rows(purrr::map(.x=ci.wk$id, .f=~case_cut_3wk(.x))) %>% 
  mutate(cum_period = days) %>% mutate(methodx = "fixed")

case_cat_pk <- dplyr::bind_rows(purrr::map(.x=ci.wk$id, .f=~case_cut_pk(.x))) %>% 
  mutate(cum_period = days) %>% mutate(methodx = "last_peak")


case_cat <- bind_rows(case_cat_3wk, case_cat_pk)

return(case_cat)
}

d7 <- who_cat_fun(7)
d1 <- who_cat_fun(1)

write.csv(d1, "who_cat_mitigation.csv", na="", row.names = F)

mapdf <- d1 %>% 
  filter(dates==datelifted) %>% filter(methodx %in% c("last_peak")) %>% 
  select(name, whocat, lockdown, values, cases, cum_period, methodx) 

write.csv(mapdf, "WHO_MAP_who_cat_mitigation.csv", na="", row.names = F)

d1x <- d1 %>% filter(methodx %in% c("last_peak")) %>% 
  filter(axisx >= -70) %>% rename(Country = name)

d1x$Country[d1x$Country=="The former Yugoslav Republic of Macedonia"] <- "Macedonia"

ous_met <- d1x %>% filter(whocat %in% c("Met criteria")) %>% 
  select(Country, peakval) %>% unique() %>% arrange(desc(peakval)) %>% 
  mutate(peakrank = row_number()) %>% filter(peakrank <=5)

ous_not <- d1x %>% filter(whocat %in% c("Not met")) %>% 
  select(Country, peakval) %>% unique() %>% arrange(desc(peakval)) %>% 
  mutate(peakrank = row_number()) %>% filter(peakrank <=4 | Country %in% c("Poland"))

ous_meto <- d1x %>% filter(whocat %in% c("Met criteria")) %>% 
  select(Country, peakval) %>% unique() %>% arrange(desc(peakval)) %>% 
  mutate(peakrank = row_number()) %>% filter(peakrank >5)

ous_noto <- d1x %>% filter(whocat %in% c("Not met")) %>% 
  select(Country, peakval) %>% unique() %>% arrange(desc(peakval)) %>% 
  mutate(peakrank = row_number()) %>% filter(peakrank >4 & 
                                               Country %ni% c("Poland"))

ous_met$Country
ous_not$Country
ous_meto$Country
ous_noto$Country
  

icpicol <- read.csv("https://raw.githubusercontent.com/ICPI/DIV/master/Color_Palettes/ICPI_Color_Palette.csv") %>% 
  filter(palette %ni% c("Coast of Bohemia")) %>% 
  mutate(hex = if_else(color %in% c("grullo"), "red", hex))

d2x <- d1x %>% 
  mutate(countryx = if_else(Country %in% c(ous_met$Country, ous_not$Country), Country, " Other countries")) %>% 
  mutate(leg_met = if_else(Country %in% c(ous_met$Country, ous_not$Country), Country, 
                           paste0("Other countries:\n", 
                                  paste0(ous_meto$Country, collapse = ",\n")
                                         ))) %>% 
  mutate(leg_not = if_else(Country %in% c(ous_met$Country, ous_not$Country), Country, 
                           paste0("Other countries:\n", 
                                  paste0(ous_noto$Country, collapse = ",\n")
                           )))



myColors <- c("grey", icpicol$hex[1:length(unique(d2x$countryx))-1])
names(myColors) <- unique(d2x$countryx)
colScale <- scale_colour_manual(name = "countryx", values = myColors)

onlyplot <- ggplot() +
  geom_line(data=d2x %>% filter(Country %ni% c(ous_met$Country, ous_not$Country)), 
            aes(x=axisx, y=spline, group=Country, color=countryx), size=1, alpha=0.5) +
  geom_line(data=d2x %>% filter(Country %in% c(ous_met$Country, ous_not$Country)), 
            aes(x=axisx, y=spline, group=Country, color=countryx), size=1.25, alpha=0.9) +
  geom_vline(xintercept = 0, linetype="dashed", color="darkgrey")+
  ylim(0,30)+
  theme_classic()+
  colScale+
  facet_grid(facets=vars(whocat), scales="fixed")+
  theme(legend.position = "none")+
  ylab("Cases per 100,000 (spline)")+
  xlab("Time since date of lifting measures")
  # ggtitle("Met criteria")

# Getting plots
# ~~~~~~~~~~~~~~~~~~~ Legends ~~~~~~~~~~~~~~~~~~~ 
p1x <- get_legend(ggplot() +
                    geom_line(data=d2x %>% filter(whocat %in% c("Met criteria")) , 
                              aes(x=axisx, y=spline, group=Country, color=countryx), size=1.25, alpha=0.9) +
                    theme_classic()+
                    colScale+
  theme(legend.position = "right")+
  ggtitle("Met criteria"))


p2x <- get_legend(ggplot() +
                    geom_line(data=d2x %>% filter(whocat %in% c("Not met")) %>% 
                                filter(Country %in% c(ous_met$Country, ous_not$Country)), 
                              aes(x=axisx, y=spline, group=Country, color=countryx), size=1.25, alpha=0.9) +
                    theme_classic()+
                    colScale+
                    theme(legend.position = "right")+
                    ggtitle("Met criteria"))

plistx <- list(as_ggplot(p1x), as_ggplot(p2x))

# plot as grid in 1 columns
legendplot <- cowplot::plot_grid(plotlist = plistx, ncol = 2)


p1xo <- get_legend(ggplot() +
                    geom_line(data=d2x %>% filter(Country %in% ous_meto$Country) , 
                              aes(x=axisx, y=spline, group=Country, color=leg_met), size=1.25, alpha=0.9) +
                    theme_classic()+
                    scale_colour_manual(name = "leg_met", values = "grey")+
                   theme(legend.position = "right")+
                    ggtitle("Met criteria"))


p2xo <- get_legend(ggplot() +
                     geom_line(data=d2x %>% filter(Country %in% ous_noto$Country) , 
                               aes(x=axisx, y=spline, group=Country, color=leg_not), size=1.25, alpha=0.9) +
                     theme_classic()+
                     scale_colour_manual(name = "leg_not", values = "grey")+
                     theme(legend.position = "right")+
                     ggtitle("Not met"))


plistxo <- list(as_ggplot(p1xo), as_ggplot(p2xo))


legendploto <- cowplot::plot_grid(plotlist = plistxo, ncol = 2)

ff <- paste0(dir.root,
       "Epi Curves/Epi Curves for Every Country/")

png(paste0(ff, "WHO_comboplot2.png"), width=9, height=7, units="in", res=600)
onlyplot
dev.off()


png(paste0(ff,"WHO_legends2.png"), width=9, height=3, units="in", res=600)
legendplot
dev.off()

png(paste0(ff,"WHO_legends_other2.png"), width=9, height=6, units="in", res=600)
legendploto
dev.off()


png("met.png", width=9, height=6, units="in", res=600)
p1
dev.off()

png("notmet.png", width=9, height=6, units="in", res=600)
p2
dev.off()


# Pulling in the Trajectory data
rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts/r_functions/")


# function to get the base JHU data with cases and deaths daily/cumulative 
fun_traj <- dget(paste0(rfunctions.dir, "trajectory_function.R"))

fun_index <- dget(paste0(rfunctions.dir, "get_country_date.R"))

framedf <- fun_index() %>% select(country, country_code, alpha2) %>% 
  unique() %>% rename(name = country) %>% 
  mutate(name = case_when(
    name %in% c("Czech Republic") ~ "Czechia",
    name %in% c("the former Yugoslav Republic of Macedonia") ~ "The former Yugoslav Republic of Macedonia",
    TRUE ~ name
  ))
  



hspot <- fun_traj("date")

h <- hspot %>% filter(rate_cut==10 & slope_cut==0.1) %>% 
  select(ou_date_match, growth.map.cat.labels)

#Test match
chex <- left_join(d1x, framedf) %>% filter(is.na(country_code)) %>% 
  select(name, country_code) %>% unique()
                  
d2 <- left_join(d1x, framedf) %>% 
  mutate(ou_date_match = paste(country_code, dates, sep="_")) 


# Joining the hotspot data
dhot <- left_join(d2, h)

mapdf_hot <- dhot %>% 
  filter(dates==datelifted) %>% filter(methodx %in% c("last_peak")) %>% 
  select(name, whocat, lockdown, growth.map.cat.labels, values, cases, cum_period, methodx) 


write.csv(mapdf_hot, "hotspotMAP_who_cat_mitigation.csv", na="", row.names = F)

hplot <- ggplot() +
  geom_line(data=dhot %>% filter(axisx >= -70),
            aes(x=axisx, y=spline, group=name, color=whocat), size=1) +
  geom_vline(xintercept = 0, linetype="dashed", color="darkgrey")+
  facet_grid(facets=vars(lockdown),
             scales="free_y")+
  theme_classic()+
  theme(legend.position = "right")+
  ylab("Cases per 100,000 (spline)")+
  xlab("Time since date of lifting of measures")+
  ggtitle(d1x$methodx[1])

