### title: Uebung 13 ####
### topic: Kommunikation von Ergebnissen 1
### author: Felix Wohlgemuth
### date: 19.06.2020

### preliminaries  ####
library(tidyverse)
library(readxl)
library(stargazer)

# Verwendung CWS 2020 Daten ####
cws_data <- read_excel("_raw/CWS-data-2020.xlsx")

# Auswahl Variablen ####
# DV: family_pub : Public expenditure on family benefits, as percentage of GDP
# IV1: socx_pub : Total public social expenditure, as percentage of GDP
# IV2: fempar : share of seats in parliament held by women
# IV3: rtcrseat : Share of seats in parliament won by parties classified as right Christian
# IV4: leftseat : Share of seats in parliament won by parties classified as left
# IV5: ud : Union density, defined as net union membership as a percentage of employed wage and salary earners
# IV6: fem_employment : Share of female labour force (flabfo), as percentage of total labour force (tlabfo)
# IV7: ptemp_f : Part-time employment for females, all ages, as a percentage of employment

# Erstellen Sie eine Tabelle mit deskriptiven Statistikmaßen und kopieren Sie diese in ein Worddokument
cws_data <- cws_data %>% 
  mutate(fem_employment = 100 * (flabfo / tlabfo))

cws_data_90  <- cws_data %>% 
  filter(year == 1990) %>% 
  select(family_pub, socx_pub, fempar, rtcrseat, leftseat, ud, fem_employment, ptemp_f) %>% 
  as.data.frame()

stargazer(cws_data_90,                
          digits = 2,           
          decimal.mark = ",", 
          title = "Deskriptive Statistik",
          covariate.labels = c("öffentliche Ausgaben für Familienpolitik <i>(% BIP)</i>",
                               "öffentliche Sozialausgaben insgesamt <i>(% BIP)</i>",
                               "Anteil weibliche Parlamentsabgeordnete",
                               "Anteil Abgeordnete rechter christlicher Parteien",
                               "Anteil Abgeordnete linker Parteien",
                               "Anteil Gewerkschaftsmitglieder <i>(% Angestellten)</i>",
                               "Anteil Frauen an Erwerbstätigen",
                               "Anteil Teilzeibeschäftigen an weiblichen Erbwerbstätigen"),
          type = "html",          
          out= "output/descriptives_90.html")

# Erstellen Sie ein Regressionmodell mit `lm()`.

lm_90 <- cws_data %>% 
  filter(year == 1990) %>% 
  lm(family_pub ~ socx_pub +  fempar + rtcrseat + leftseat + ud + fem_employment + ptemp_f, data = . )
lm_00 <- cws_data %>% 
  filter(year == 2000) %>% 
  lm(family_pub ~ socx_pub +  fempar + rtcrseat + leftseat + ud + fem_employment + ptemp_f, data = . )
lm_10 <- cws_data %>% 
  filter(year == 2010) %>% 
  lm(family_pub ~ socx_pub +  fempar + rtcrseat + leftseat + ud + fem_employment + ptemp_f, data = . )

# Erstellen Sie für das Modell eine Regressiontabelle und kopieren Sie diese in das Worddokument.
stargazer(lm_90, lm_00, lm_10,
          column.labels = c("Jahr = 1990", "Jahr = 2000", "Jahr = 2010"),
          dep.var.labels = c("öffentliche Ausgaben für Familienpolitik <i>(% BIP)</i>"),
          covariate.labels = c("öffentliche Sozialausgaben insgesamt <i>(% BIP)</i>",
                               "Anteil weibliche Parlamentsabgeordnete",
                               "Anteil Abgeordnete rechter christlicher Parteien",
                               "Anteil Abgeordnete linker Parteien",
                               "Anteil Gewerkschaftsmitglieder <i>(% Angestellten)</i>",
                               "Anteil Frauen an Erwerbstätigen",
                               "Anteil Teilzeibeschäftigen an weiblichen Erbwerbstätigen",
                               "Konstante"),
          no.space = TRUE,
          type = "html",
          out = "output/lm_table_2.html")
