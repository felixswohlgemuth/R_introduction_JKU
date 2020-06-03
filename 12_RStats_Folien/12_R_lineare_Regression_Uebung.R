### title: Uebung 12 ####
### topic: (multiple) lineare Regression
### author: Felix Wohlgemuth
### date: 05.06.2020

### preliminaries  ####
library(tidyverse)
library(readxl)

### load dataset ####
cws_data <- read_excel("_raw/CWS-data-2020.xlsx")

### linear regression model ####
# DV: family_pub : Public expenditure on family benefits, as percentage of GDP
# IV1: socx_pub : Total public social expenditure, as percentage of GDP
# IV2: fempar : share of seats in parliament held by women
# IV3: rtcrseat : Share of seats in parliament won by parties classified as right Christian
# IV4: leftseat : Share of seats in parliament won by parties classified as left
# IV5: ud : Union density, defined as net union membership as a percentage of employed wage and salary earners
# IV6: fem_employment : Share of female labour force (flabfo), as percentage of total labour force (tlabfo)
# IV7: ptemp_f : Part-time employment for females, all ages, as a percentage of employment

### create IV6 fem_employment variable
cws_data <- cws_data %>% 
  mutate(fem_employment = 100 * (flabfo / tlabfo))

### linear regression model ####
# OPTIONAL: create 3 regression models (1990, 2000, 2010) to compare the change in effects
lm_90 <- cws_data %>% 
  filter(year == 1990) %>% 
  lm(family_pub ~ socx_pub +  fempar + rtcrseat + leftseat + ud + fem_employment + ptemp_f, data = . )
lm_00 <- cws_data %>% 
  filter(year == 2000) %>% 
  lm(family_pub ~ socx_pub +  fempar + rtcrseat + leftseat + ud + fem_employment + ptemp_f, data = . )
lm_10 <- cws_data %>% 
  filter(year == 2010) %>% 
  lm(family_pub ~ socx_pub +  fempar + rtcrseat + leftseat + ud + fem_employment + ptemp_f, data = . )
# (data = . is necessary because lm() does not recognise %>%. To use filter and %>% we need to specify the data with . this works in all functions that don't recognise %>% )


### Results
# linear regression model for 1990
summary(lm_90)
# The expenditure for family policy increases by 0.0358 percentage points when the general social expenditures increases by 1 percentage point, keeping all other variables in the model constant.
# Increasing the union density rate by 1 percentage point increases the family policy expenditure by 0.0188 percentage points
# Both effects are statistically significant on the 5 percentage level.
# The adjusted R^2 of 0.769 is rather high. lm_90 explains the data better than the null model.

summary(lm_00)
# Non of the variables have a statically significant effect on the height of public family policy expenditures.

summary(lm_10)
# Keeping all other variables constant:
# increasing the union density rate by 1 percentage point increases the public expenditure on family policy by 0.0217 percentage points. The effect is statistically significant on the 5 percentage level.
# increasing the female employment rate by 1 percentage point increases the public expenditure on family policy by 0.0340 percentage points. The effect is statistically significant on the 5 percentage level.
# increasing the part-time employment rate by 1 percentage point increases the public expenditure on family policy by 0.0295 percentage points.  The effect is statistically significant on the 5 percentage level.
# The adjusted R^2 of 0.3538 states that the model is better than the null model, but in comparison to the lm_90 with a lower goodness of fit. 

# Results comparing time points
# Only the effect of the union density rate remained statistically significant in the 90 and 2010 models. It seems that current data is better explained by socio-economic factors. However the lower model fit indicates that there might be other variables that explain the country differences in family policy expenditures better. 
