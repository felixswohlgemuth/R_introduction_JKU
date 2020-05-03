### title: Uebung 7 ####
### topic: Daten aufbereiten
### author: Felix Wohlgemuth
### date: 29.04.2020


### preliminaries  ####
library(tidyverse) # Load the 'Tidyverse' including dplyr

### load dataset ####
socx_data <- read_csv("data/SOCX_AGG_20042020191205895.csv") # import dataframe

### clean dataset ###
socx_data <- socx_data %>% # remove rows with OECD mean and overwrite dataframe
  filter(COUNTRY != "OECD")

### analysis of public expenditure on formal childcare
## check for missings in dataframe
childcare_na <- socx_data %>%   # save output in new dataframe
  select(COUNTRY, YEAR, family_service_childcare_pct_gdp) %>% # reduce dataframe to variable of interest + country & year
  filter(is.na(family_service_childcare_pct_gdp)) %>% # keep only rows with missing values in variable of interest
  print() # print dataframe - all country-years with missing values

## limit analysis to 2000 - 2015 due to NAs
socx_data <- socx_data %>% # overwrite dataframe
  filter(YEAR <= 2015) # keep all observations between 2000 & 2015

### task 1 ####
## Wählen Sie eine Variable Ihrer Wahl.
## Filtern Sie den Datensatz, so dass die Variable nur fuer ein Jahr Ihrer Wahl ausgegeben wird.
## expenditure for childcare in 2007

## Berechnen Sie ein passendes Lagemass und ein passendes Streuungsmass fuer dieses Jahr.
## min(), max(), mean(), sd()

childcare_07_descr <- socx_data %>% # save summarise output in new dataframe
  filter(YEAR == 2007) %>%  # keep only observations from 2007
  summarise(min_care = min(family_service_childcare_pct_gdp), # define descriptives and variable of interest 
            max_care = max(family_service_childcare_pct_gdp),
            mean_care = mean(family_service_childcare_pct_gdp),
            sd_care = sd(family_service_childcare_pct_gdp)) %>% 
  print() # print output dataframe

### task 2 ####
## Verwenden Sie `group_by` und einen Zeitraum Ihrer Wahl
# 2000 - 2007
## Berechnen Sie fuer den Zeitraum ein Lagemass und ein Streuungsmas fuer jedes Land getrennt.

childcare_00_07_descr <- socx_data %>%  # save summarise output in new dataframe
  filter(YEAR >= 2000 & YEAR <= 2007) %>%   # keep observations between 2000 & 2007
  group_by(COUNTRY) %>%   # define for which groups (each country) summarise() is applied to separately
  summarise(min_care = min(family_service_childcare_pct_gdp), # define descriptives and variable of interest
            max_care = max(family_service_childcare_pct_gdp),
            mean_care = mean(family_service_childcare_pct_gdp),
            sd_care = sd(family_service_childcare_pct_gdp)) %>% 
  print(n = 5)  # print descriptives for top 5 countries - dataset is arranged alphabetically