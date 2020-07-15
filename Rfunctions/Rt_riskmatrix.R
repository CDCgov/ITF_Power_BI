function(){

#SETTING DIRECTORY FOR INTERNATIONAL TASK FORCE - if James, defaults to his own account, otherwise appends users' name to the path
if(Sys.getenv("USERNAME")=="kux9") {
  dir.root <- "C:/Users/kux9/OneDrive - CDC/COVID19/"
} else dir.root <- paste0("C:/Users/",
                          Sys.getenv("USERNAME"),
                          "/CDC/International Task Force-COVID19 - DataViz/Data and Analysis/")

# Function to load
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

# folder where all R functions are housed
rfunctions.dir <- paste0(dir.root, "PowerBI/R_scripts_testing/r_functions/")

# function to get the base JHU data with cases and deaths daily/cumulative 
who_data <- dget(paste0(rfunctions.dir, "get_who_data.R"))


# Load/install packages 
ldpkg(c("EpiEstim", 
        "ggplot2",
        "readxl"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Core EpiEstim function ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### epi_curve function #####
epi_curve_14<-function(country,ctyname)
{
  #run EpiEstim
  index2<-15:nrow(country)
  T0 <- nrow(country)
  t_start <- seq(2, T0-13) # starting at 2 as conditional on the past observations
  t_end <- t_start + 13
  res_biweekly_ni <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 4.7,
                                  std_si = 2.9))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_ni)<-c("mean.ni","lower.ni","upper.ni")
  
  #####################################################################
  
  res_biweekly_li <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 7.5,
                                  std_si = 3.4))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_li)<-c("mean.li","lower.li","upper.li")
  
  ######################################################################
  
  res_biweekly_mtf <- estimate_R(country[,c("date","I")],
                                 method="parametric_si",
                                 config = make_config(list(
                                   t_start = t_start,
                                   t_end = t_end,
                                   mean_si = 6.14,
                                   std_si  = 3.96))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_biweekly_mtf)<-c("mean.mtf","lower.mtf","upper.mtf")
  
  ###########################################################################
  
  cbind(country=ctyname,date=country$date[index2],ncase=country$I[index2],res_biweekly_ni,res_biweekly_li,res_biweekly_mtf)
}
#**********************************************#

epi_curve_7<-function(country,ctyname)
{
  #run EpiEstim
  index2<-8:nrow(country)
  T0 <- nrow(country)
  t_start <- seq(2, T0-6) # starting at 2 as conditional on the past observations
  t_end <- t_start + 6
  res_weekly_ni <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 4.7,
                                  std_si = 2.9))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_weekly_ni)<-c("mean.ni","lower.ni","upper.ni")
  
  #####################################################################
  
  res_weekly_li <- estimate_R(country[,c("date","I")],
                                method="parametric_si",
                                config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 7.5,
                                  std_si = 3.4))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_weekly_li)<-c("mean.li","lower.li","upper.li")
  
  ######################################################################
  
  res_weekly_mtf <- estimate_R(country[,c("date","I")],
                                 method="parametric_si",
                                 config = make_config(list(
                                   t_start = t_start,
                                   t_end = t_end,
                                   mean_si = 6.14,
                                   std_si =  3.96))
  )$R[,c("Mean(R)","Quantile.0.025(R)","Quantile.0.975(R)")]
  names(res_weekly_mtf)<-c("mean.mtf","lower.mtf","upper.mtf")
  
  ###########################################################################
  
# View(  
  cbind(country=ctyname,date=country$date[index2],ncase=country$I[index2],res_weekly_ni,res_weekly_li,res_weekly_mtf)
        # )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ============= Overall function to generate Rt dataset ~~~~~~~===============
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
base_data <- who_data()
row.names(base_data)<-NULL

dx <- base_data
######country data
#d <- read_excel(paste0(dir.data, "JHU Data by Country - Long.xlsx"))
index<-which(dx$Date==min(dx$Date))
dx<-dx[-index,]

d <- dx %>% select(c(1:6)) %>% 
  mutate_if(is.numeric, ~replace(., .<0, 0))


names(d)<-c("country","date","I","ccase","ndeath","cdeath")
d$date <- as.Date(d$date,"%m/%d/%Y")
countries <- unique(d$country)

dsplit<-split(d[,c("country","date","I")],d$country)
N<-length(countries)

# epi_output<-lapply(1:N,function(i,x,y,z) epi_curve(x[[i]],y[i]),x=dsplit,y=countries)

epi_output_7  <- purrr::map2(.x = dsplit, .y=countries, .f = ~epi_curve_7(.x, .y)) 
epi_output_14 <- purrr::map2(.x = dsplit, .y=countries, .f = ~epi_curve_14(.x, .y))


epi_7 <- dplyr::bind_rows(epi_output_7) %>% mutate(windowx = "7-day")
epi_14 <- dplyr::bind_rows(epi_output_14) %>% mutate(windowx = "14-day")


popdf <- base_data %>% select(country_code, Country, 
                              "Population 2018.x",
                              `WHO Region`) %>% unique() %>% 
  rename(country = Country)

row.names(epi_14)<-NULL


epi_14x <- left_join(epi_14, popdf) %>% 
  filter(!is.na(`Population 2018.x`))

countrylist <- unique(epi_14x$country)


fun_rx <- function(i){
  
  df <- epi_14x %>% filter(country %in% i) %>% 
    select(country, date, ncase, mean.mtf, 
           country_code,  
           "Population 2018.x",
           `WHO Region`) %>% 
    rename(pop = "Population 2018.x",
           WHO_region = `WHO Region`)
  
  last.date = max(df$date)
  two.week  = last.date - 14  

  two.wk.date = seq(from = two.week+1, to = last.date, by=1)
  
  df2 <- df %>% filter(date %in% two.wk.date)
  
  cases <- sum(df2$ncase, na.rm=T)
  inci  <- ((cases/unique(df$pop)[1])/14)*100000
  rt    <- df2[df2$date==last.date,]$mean.mtf
  

fdf <- data.frame(
  country = unique(df$country)[1],
  country_code = unique(df$country_code)[1],
  WHO_region = unique(df$WHO_region)[1],
  Rt = rt,
  Incidence = inci,
  two_wk_cases = cases
)

return(fdf)

  }


rmat  <- dplyr::bind_rows(
  purrr::map(.x = countrylist, .f = ~fun_rx(.x)) )

write.csv(rmat, "Risk_Matrix_df_WHO.csv", na="", row.names = F)

library(plotly)



ggplot(rmat, aes(x=Rt, y=Incidence, size = two_wk_cases)) +
  geom_point(alpha=0.7)


p <- rmat %>% filter(Rt<3 & two_wk_cases>100) %>% 
  arrange(desc(two_wk_cases)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=Rt, y=Incidence, text=country,
             size=two_wk_cases, color=WHO_region)) +
  geom_point(alpha=0.5) 

ggplotly(p)

icpicol <- read.csv("https://raw.githubusercontent.com/ICPI/DIV/master/Color_Palettes/ICPI_Color_Palette.csv") 


myColors <- icpicol$hex[1:length(unique(rmat$WHO_region))]
names(myColors) <- unique(rmat$WHO_region)
colScale <- scale_colour_manual(name = "WHO_region", values = myColors)


rtop <- rmat %>%
  select(WHO_region, country_code, two_wk_cases) %>% 
  group_by(WHO_region) %>% 
  arrange(desc(two_wk_cases)) %>% 
  mutate(ranx = row_number()) %>% 
  ungroup() %>% 
  filter(ranx==1 | country_code %in% c("USA"))

rtop$country_code

rmatx <- rmat %>% filter(Rt<3 & two_wk_cases>100) %>% 
  mutate(blab = if_else(country_code %in% rtop$country_code, country_code, ""))

library(scales)
library(plotly)

p <- rmatx %>% 
  ggplot(aes(x=Rt, y=Incidence, label=blab,
             size=two_wk_cases, color=WHO_region)) +
  geom_point(alpha=0.5) +
  geom_text(color="black", size=4)+
  colScale+
  geom_vline(xintercept = 1, linetype="dashed", color="darkgrey")+
  theme_classic()+
  scale_size(range = c(2, 16), name="Cases over last 2 weeks", 
             labels=comma)+
  guides(color=guide_legend(override.aes=list(size=8))) 

ggplotly(p)

png("risk.png", width=12, height=6, units="in", res=600)
p
dev.off()



ous_met$Country
ous_not$Country
ous_meto$Country
ous_noto$Country


icpicol <- read.csv("https://raw.githubusercontent.com/ICPI/DIV/master/Color_Palettes/ICPI_Color_Palette.csv") %>% 
  filter(palette %ni% c("Coast of Bohemia")) %>% 
  mutate(hex = if_else(color %in% c("grullo"), "red", hex))

rmaty <- rmat %>% filter(WHO_region %in% c("EURO")) %>% 
  mutate(countryx = if_else(country %in% c(ous_met$Country, ous_not$Country), country, "Other countries")) %>%   mutate(labl = if_else(countryx %in% c("Other countries"), "", country_code))

  

myColors <- c("grey", icpicol$hex[1:length(unique(rmaty$countryx))-1])
names(myColors) <- unique(rmaty$countryx)
colScale <- scale_colour_manual(name = "countryx", values = myColors)

p <- rmaty %>% 
  ggplot(aes(x=Rt, y=Incidence, label=labl,
             size=two_wk_cases, color=countryx)) +
  geom_point(alpha=0.7) +
  geom_text(color="black", size=3.5)+
  colScale+
  geom_vline(xintercept = 1, linetype="dashed", color="darkgrey")+
  theme_classic()+
  scale_size(range = c(1, 20), name="Cases over last 2 weeks", 
             labels=comma)+
  guides(color=guide_legend(override.aes=list(size=8))) +
  facet_grid(facets=vars(whocat), scales="fixed")+
  

ggplotly(p)

png("risk.png", width=12, height=6, units="in", res=600)
p
dev.off()

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


