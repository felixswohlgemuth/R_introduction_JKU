library(tidyverse)
socx_data <- read_csv("_raw/SOCX_AGG_20042020191205895.csv")

socx_data <- socx_data %>% 
  mutate(`Type of Expenditure` = recode(`Type of Expenditure`, "Total" = "total", "Cash benefits" = "cash", "Benefits in kind" = "service")) %>%
  mutate(Branch = recode(Branch, "Old age" = "age", "Family" = "family")) %>%
  mutate(`Type of Programme` = recode(`Type of Programme`, "Total" = "total",
                              "Old age - Early retirement pension" = "early_retirement",
                              "Old age - Residential care / Home-help services" = "care",
                              "Old age - Pension" = "pension",
                              "Old age - Other cash benefits" = "other_cash",
                              "Old age - Other benefits in kind" = "other_services",
                               "Family - Family allowances" = "allowances",
                               "Family - Maternity and parental leave" = "leave",
                               "Family - Other cash benefits" = "other_cash", 
                               "Family - Early childhood education and care (ECEC)" = "childcare",
                               "Family - Home help / Accomodation" = "accomodation", 
                               "Family - Other benefits in kind" = "other_services")) %>%
  mutate(UNIT = recode(UNIT, "PCT_GDP" = "pct_gdp")) %>%
  filter(Branch == "family") %>%
  mutate(varnames = paste(Branch, `Type of Expenditure`, `Type of Programme`, UNIT, sep = "_")) %>%
  select(COUNTRY, YEAR, varnames, Value) %>%
  spread(varnames, Value)
write_csv(socx_data, "data/SOCX_AGG_20042020191205895.csv")

