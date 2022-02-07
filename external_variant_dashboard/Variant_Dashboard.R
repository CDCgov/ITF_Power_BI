#Set working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/OneDrive - CDC/ITF_Power_BI"))  

#Code to generate CSVs ITF Power BI CDC COVID Tracker Views - Variant Map


# Path to all R functions on GitHub
rfunctions.dir <- "./Rfunctions/"

# Directory for teams folder (where WHO variant data is placed)
dir.teams <- paste0("C:/Users/", Sys.getenv("USERNAME"),
                   "/CDC/ITF-COVID19-SAVI - Documents/")

# root for this dashboard
dir.root <- "./external_variant_dashboard/"

#output directory to write csvs
out.dir <- paste0(dir.root,"output/")


#read in packages needed to run code
source(paste0(rfunctions.dir,"packages_for_Power_BI.R"))


#Read in Raw Variant Data
library(readxl)
#d.variant.raw <- readxl::read_xlsx(paste0(dir.root,"/Routine Reports/3. Variant Maps/Inputs/20211222_Variant_Tracker_Shared_with_Partners.xlsx"), sheet = 1)
variant_path <- paste0(dir.teams,"/Routine Reports/3. Variant Maps/Inputs/")
variant_file_latest <- list.files(path=variant_path,pattern="Variant")
d.variant.raw <- readxl::read_excel(paste0(variant_path, variant_file_latest),
                               sheet = 1)

#IF NO ISO CODE: Read in variants data and dictionary - join based on country to get iso code in excel sheet
#d.isocode.raw <- read_xlsx(paste0(dir.input, "20210511_WHO_SARS_COV_2_Variants_Tracker_(shared_with_partners).xlsx"), sheet="dictionary")
#d.variant.raw <- dplyr::full_join(d.variant.raw, d.isocode.raw, by = "Country")

#Get Full List of Countries
fun_get_country <- dget(paste0(rfunctions.dir,"get_country.R"))
get_country <- fun_get_country() %>%
  rename(country_name=country,
         country_code=iso3code) %>%
  select(country_name, country_code)

# Saba (iso3code = XCA) reported in WHO data separately from ISO3code BES (Bonaire, Sint Eustatius and Saba)
saba <- data.frame("country_name" = "Saba", "country_code" = "XCA")

get_country <- rbind(get_country, saba)


#Create Final Dataset
df.variants <- d.variant.raw %>%
  rename(country_name = Country,
         country_code = ISO) %>%
  mutate(country_name=gsub(pattern="[1]",replacement="",country_name, fixed=TRUE)) %>%
  select(Variants, country_code, Verification) %>%
  
  #Custom changes to country_code
  mutate(country_code = if_else(country_code=="XAA", "BES", country_code))

#Identify mismatched countries 
#This will list any countries in the variant dataset that do not match based on country code
unique(df.variants$country_code[!(df.variants$country_code %in% get_country$country_code)])

# 01 June 2021 - WHO modified variants reporting and naming convention 
# (https://www.who.int/en/activities/tracking-SARS-CoV-2-variants/)

df.variants.b.1.351 <- df.variants %>%
  filter(Variants=="501Y.V2 (B.1.351)") %>%
  right_join(get_country, by="country_code") %>%
  mutate(Variants = "501Y.V2 (B.1.351)")

df.variants.p.1 <- df.variants %>%
  filter(Variants=="P.1 (B.1.1.28)") %>%
  right_join(get_country, by="country_code") %>%
  mutate(Variants = "P.1 (B.1.1.28)")

df.variants.b.1.1.7 <- df.variants %>%
  filter(Variants=="VOC 202012/01 (B.1.1.7)") %>%
  right_join(get_country, by="country_code") %>%
  mutate(Variants = "VOC 202012/01 (B.1.1.7)")

df.variants.b.1.617.2 <- df.variants %>%
  filter(Variants=="B.1.617.2") %>%
  right_join(get_country, by="country_code") %>%
  mutate(Variants = "B.1.617.2")

# Double check that "B.1.1.529" is how Omicron appears in WHO dataset; adjust as needed
df.variants.b.1.1.529 <- df.variants %>%
  filter(Variants=="B.1.1.529") %>%
  right_join(get_country, by="country_code") %>%
  mutate(Variants = "B.1.1.529")

df.variants.full <- 
  df.variants.b.1.351 %>%
  bind_rows(df.variants.b.1.1.7) %>%
  bind_rows(df.variants.p.1) %>%
  bind_rows(df.variants.b.1.617.2) %>%
  bind_rows(df.variants.b.1.1.529) %>%
  #Re-code Verification Categories
  mutate(Verification = replace_na(Verification, "Not reported")) %>%
  mutate(Verification = case_when(Verification=="Verified" ~ "Verified",
                                  Verification=="Official" ~ "Verified",
                                  Verification=="Not verified" ~ "Unverified",
                                  Verification=="Not reported" ~ "Not reported")) %>%
  
  #Recode Variant Names
  mutate(Variants = case_when(Variants=="501Y.V2 (B.1.351)" ~ "Beta (B.1.351)",
                              Variants=="P.1 (B.1.1.28)" ~ "Gamma (P.1)",
                              Variants=="VOC 202012/01 (B.1.1.7)" ~ "Alpha (B.1.1.7)",
                              Variants=="VOC 202012/01 (B.1.1.7)" ~ "Alpha (B.1.1.7)",
                              Variants=="B.1.617.2" ~ "Delta (B.1.617.2)",
                              Variants=="B.1.1.529" ~ "Omicron (B.1.1.529)")) %>%
  
  #Enter Data Update Date
  #mutate(data_updated = Sys.Date()) %>%
  #mutate(data_updated_text = paste0("Data updated on ", as.character(data_updated)))
  #Enter Date of Data File
  mutate(variant_file_date = as.Date(str_match(variant_file_latest, pattern = "\\d{8}"), format = "%Y%m%d")) %>%
  mutate(variant_date = paste0("Data as of ", as.character(variant_file_date - 1)))

#export CSV
write.csv(df.variants.full, paste0(out.dir, "variants.csv"), fileEncoding="UTF-8", row.names=FALSE, na="")


  
