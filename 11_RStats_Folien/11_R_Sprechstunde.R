### R Sprechstunde 28.05.20

## Start: 11:00

# Vorgehen:
# Wir sammeln heute Fragen am Anfang der Sprechstunde
# 1. Im Chat schreiben ob Sie heute eine Frage haben
# 2. Ich rufe Sie auf

Von Wendy Barrantes an alle:  11:20 AM
rm(list = ls())
library(readr)
wvs_short_w6 <- readRDS("data/wvs_short_w6.rds")
View(wvs_short_w6)
library(tidyverse)
library(naniar)
wvs_short_w6 <- wvs_short_w6 %>%
  select(V2, V3, V240, V242, V248, V4, V57, women_index, V239, V229)
# V2=Land, V3=interviewer number, V240=Sex, V242=Age, V248=Highest educational level attained, V4=Important in life: Family, V57=marital status, V239=Scale of income, V229=Employment status
View(wvs_short_w6)
install.packages("GGally")
library(GGally)
wvs_short_w6 <- wvs_short_w6 %>%
  mutate(V240 = as.character(V240)) %>% 
  mutate(V248 = as.ordered(V248)
View(wvs_short_w6)

wvs_short_w6 %>% 
  filter(V2 %in% c("DEU", "ARM")) %>% 
  ggpairs( columns = c("V3", "V240", "V248", "V4", "V239"))

# ggpairs(wvs_short_w6, columns = c("V2", "V3", "V240", "V248", "V4", "V239")) - He de revisar el comando porque genera error :/
