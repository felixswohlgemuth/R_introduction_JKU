library(tidyverse)
library(readxl)
cws_data <- read_excel("_raw/CWS-data-2020.xlsx")


# optional variable preparation
cws_data <- cws_data %>% 
  mutate(fem_employment = 100 * (flabfo / tlabfo))

# install.package("stargazer")
library(stargazer)
cws_data_90  <- cws_data %>% 
  filter(year == 1990) %>% 
  select(family_pub, socx_pub, fempar, rtcrseat, leftseat, ud, fem_employment, ptemp_f) %>% 
  as.data.frame() # select erstellt ein tibble, aber stargazer möchte ein Dataframe#<< 

stargazer(cws_data_90, # Dataframe                 #<<
          type = "html", # Dateityp           #<<
          out= "descriptives_90.html") # Speicherort #<