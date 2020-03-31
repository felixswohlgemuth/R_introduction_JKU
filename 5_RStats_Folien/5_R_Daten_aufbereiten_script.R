# 5 R Daten aufbereiten
# multilinksdataset 
# Multilinks (2011). Multilinks Database on Intergenerational Policy Indicators. Version 2.0, 
# Multilinks Project and Wissenschaftszentrum Berlin für Sozialforschung (WZB), http://multilinks-database.wzb.eu [accessed: 30.03.20].

library(haven)
library(tidyverse)

multilinks <- read_sav("_raw/ml_db_version_2-1.sav")
write_csv(multilinks, "_raw/ml_db_version_2-1.csv")
rm(multilinks)


multilinks <- read.csv("_raw/ml_db_version_2-1.csv")

# eurostat [educ_uoe_enra10] Pupils aged between 4 years old and the starting age of compulsory education, by sex - as % of the population of the corresponding age group
# http://ec.europa.eu/eurostat/product?code=educ_uoe_enra10&language=en&mode=view

# .csv Daten laden mit read_csv aus dem tidyverse::readr Paket
ece_org <- read_csv("_raw/educ_uoe_enra10_1_Data.csv")

# Spalten auswählen mit select()

ece <- select(ece_org, TIME, GEO, Value)
ece <- select(ece_org, -SEX, -UNIT)

# %>% pipes 

ece <- ece %>% 
  select(TIME, GEO, Value)

# Zeilen auswählen mit filter()
## Welche Länder sind im Datensatz?
unique(ece$GEO)

## Datensatz ohne die zwei European Union Beobachtungen
ece <- filter(ece, 
                  GEO != "European Union - 27 countries (from 2020)" & 
                    GEO != "European Union - 28 countries (2013-2020)")
ece <- ece %>%
  filter(! GEO %in% c("European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"))

# Hautptvariable Betreuungsquote umbenennen und als numerische Variable speichern

ece <- ece %>%
  rename(ece_o4y = Value) %>%
  mutate(ece_o4y = as.numeric(ece_o4y)) %>% # : wurde automatisch als NA gesetzt
  drop_na(ece_o4y) %>% # Länder löschen die für ein Jahr keine Daten haben
  mutate(GEO = recode(GEO, "Germany (until 1990 former territory of the FRG)" = "Germany"))

# Nach Ländernamen und Jahr sortieren
ece <- ece %>%
  arrange(GEO, TIME)

# Jahres range pro Land berechnen 
# für welche Jahre haben wie Daten pro Jahr?
sum_ece <- ece %>% 
  group_by(GEO) %>%
  summarise(min_year = min(TIME), 
            max_year = max(TIME), 
            range_year = (max(TIME) - min(TIME)), 
            sum = sum(TIME), 
            n_observations = n()) %>%
  arrange(n_observations) %>%
  print()

# nur Länder die 10 Beobachtungen von 2008 bis 2017 haben
ece <- ece %>%
  group_by(GEO) %>%
  mutate(n_observations = n()) %>%
  filter(n_observations == 10) %>%
  select(-n_observations)


# Übung OECD SOCX Daten aufbereiten
OECD_SOCX_family <- read_csv("_raw/OECD_SOCX_family.csv")
