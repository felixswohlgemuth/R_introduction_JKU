### title: Uebung 11 ####
### topic: Visuelle Darstellung mehrerer Variablen
### author: Felix Wohlgemuth
### date: 15.05.2020

### preliminaries  ####
library(tidyverse)
library(readxl)
library(GGally)

### load dataset ####
cws_data <- read_excel("_raw/CWS-data-2020.xlsx")

### task 1: Auswahl 6 Variablen ####
cws_data_short <- select(cws_data, id, year, # country names, year
                         family_pub, # Public expenditure on family benefits, as a percentage of GDP.
                         lisrpr_child, # Relative post-fisc poverty rate for children (aged 17 and younger)
                         tfr, # Total fertility rate, births per woman.
                         leftcab, # Share of seats in parliament held by leftist parties in the most recent government 
                         rtcab, # Share of seats in parliament held by (secular) right parties in the most recent government
                         fempar) # Share of seats in parliament held by women after the most recent election

### task 2: Visualisierung Zusammenhang 6 Variablen ####
ggpairs(cws_data_short, columns = c("family_pub", "lisrpr_child", "tfr", "leftcab", "rtcab" , "fempar"))

# Auswahl Variablen: family_pub & fempar 

### task 3: Visualisierung Zusammenhang family_pub & lisrpr_child

ggplot(cws_data_short, aes(y = lisrpr_child, x = family_pub)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between family policy & poverty among children",
       x = "public expenditure family benefits (% GDP)",
       y = "poverty rate children (post transfer)")

cws_data_short %>% 
  filter(year == 2004) %>% 
  ggplot(aes(y = lisrpr_child, x = family_pub)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between family policy & poverty among children",
       subtitle = "year = 2004",
       x = "public expenditure family benefits (% GDP)",
       y = "poverty rate children (post transfer)")

# Beide Plots zeigen, dass Laender mit hoeheren Ausgaben für Familienpolitik eine niedrigere Kinder-Armutsquote (nach Sozialtransfer) haben. 
# Die lineare Regressionsgerade beschreibt den Zusammenhang gut, jedoch kann aus den Plots kein kausaler Zusammenhang abgeleitet werden.
# Dazu benoetigt es eine theoretische Begruendung.
