### title: R Sprechstunde # 9 ####
### topic: Einzelvariablen visualisieren
### author: Felix Wohlgemuth
### date: 07.05.2020

# Inhalt: ####
# 1. manuell die End- und Anfangspunkte der x- oder y-Achse festlegen
# 2. Umgang mit YEAR auf der y-Achse: Darstellung von nur vollen Jahreszahlen
# 2a. Umwandlung YEAR zu Faktor-Variable
# 2b. Skala der y-Achse definieren, so dass nur ganze Jahreszahlen angezeigt werden
# 3. Antwortitems umbennen
# 4. Legende mit freigewählten Text zu Plot hinzufügen
# 5. boxplot & y-Achse


# Vorbereitung ####
## Pakte und Daten laden
library(tidyverse)
library(naniar)

## wvs data
wvs_data <- readRDS("_raw/wvs_short.rds")
## Index erstellen
wvs_data <- wvs_data %>% 
  # answer "neither" transformed to NA
  replace_with_na(replace = list(C001 = 3)) %>% 
  # standardise each variable
  mutate(C001_trans = (((C001 - 1) / 1) - 1) * -1 ) %>% 
  mutate(D057_trans = (((D057 - 1) / 3) - 1) * -1 ) %>% 
  mutate(D063_B_trans = (D063_B - 1) / 2) %>%
  # create women_index variable
  mutate(women_index = (C001_trans + D057_trans + D063_B_trans) / 3) %>% 
  select(-C001_trans, -D057_trans, -D063_B_trans)

## SOCX Data
socx_data <- read_csv("_raw/SOCX_AGG_20042020191205895.csv")
## relative Ausgaben por Jahr berechnen
socx_data <- socx_data %>%
  mutate(cash_prop = (family_cash_total_pct_gdp / family_total_total_pct_gdp) * 100) %>%
  mutate(services_prop = (family_service_total_pct_gdp / family_total_total_pct_gdp) * 100)

# 1. manuell die End- und Anfangspunkte der x- oder y-Achse festlegen ####
## Vergleich der relativen Ausgaben für cash_prop für vier Länder
socx_data %>% 
  filter(COUNTRY %in% c("AUT", "AUS", "DEU", "ITA")) %>% 
  ggplot(aes(x = YEAR, y = cash_prop, colour = COUNTRY)) +
  geom_line()

## Da keiner der Werte unter circa 50% fällt wird dieser Bereich nicht angezeigt
## mit xlim() und ylim() kann der Start- und Endpunkt der Skalen festgelegt werden
socx_data %>% 
  filter(COUNTRY %in% c("AUT", "AUS", "DEU", "ITA")) %>% 
  ggplot(aes(x = YEAR, y = cash_prop, colour = COUNTRY)) +
  geom_line() +
  xlim(2000, 2015) +
  ylim(0, 100)

## Mehr Infos: https://ggplot2.tidyverse.org/reference/lims.html 

# 2. Umgang mit YEAR auf der y-Achse: Darstellung von nur vollen Jahreszahlen ####
## ggplot wählt passende Werte für die Achsenbeschriftung
## Da YEAR sind als numerische Variable definiert ist,
## kann das dazu führen, dass ggplot einfach 2001.5 anzeigt, obwohl es inhaltlich kein Sinn ergibt.
## Wir haben keine Beobachtungen für Halbjahre im Datensatz.

socx_data %>% 
  filter(YEAR >= 2001 & YEAR < 2004) %>% 
  filter(COUNTRY %in% c("AUT", "AUS", "DEU", "ITA")) %>% 
  ggplot(aes(x = YEAR, y = cash_prop, colour = COUNTRY)) +
  geom_point() +
  geom_line()

# 2a. Umwandlung YEAR zu Faktor-Variable #####
## Eine Möglichkeit das Problem zu lösen, ist YEAR in eine Faktor-Variable umzuwandeln.
## YEAR ist eine ordinal-skalierte Variable deshalb nutzen wir as.ordered() und nicht as.factor().
socx_data <- socx_data %>% 
  mutate(YEAR_fac = as.ordered(YEAR))

socx_data %>% 
  filter(YEAR_fac %in% c(2001, 2002, 2003)) %>% 
  filter(COUNTRY %in% c("AUT", "AUS", "DEU", "ITA")) %>% 
  ggplot(aes(x = YEAR_fac, y = cash_prop, colour = COUNTRY)) +
  geom_point() 

## leider funktionieren einige geom_functions nicht mit Faktor-Variablen zB geom_line()

socx_data %>% 
  filter(YEAR_fac %in% c(2001, 2002, 2003)) %>% 
  filter(COUNTRY %in% c("AUT", "AUS", "DEU", "ITA")) %>% 
  ggplot(aes(x = YEAR_fac, y = cash_prop, colour = COUNTRY)) +
  geom_point() +
  geom_line()

# 2b. Skala der y-Achse definieren, so dass nur ganze Jahreszahlen angezeigt werden ####
## Wir können auch einfach die angezeigten Werte auf den Achsen festlegen
## dazu müssen wir YEAR nicht umwandeln
## das funktioniert mit scale_x_continous(), scale_y_continous() aber auch scale_x_discrete, scale_y_discrete
## Folgende Optionen können verändert werden:


socx_data %>% 
  filter(YEAR %in% c(2001, 2002, 2003)) %>% 
  filter(COUNTRY %in% c("AUT", "AUS", "DEU", "ITA")) %>% 
  ggplot(aes(x = YEAR, y = cash_prop, colour = COUNTRY)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(2001, 2002, 2003))














## Übungsaufgabe 9, Roland Fürst
library(tidyverse)
library(naniar)
wvs_short <- readRDS("_raw/wvs_short.rds")

wvs_short_scale <- wvs_short %>%
  group_by(S003) %>%
  replace_with_na(replace = list(C001 = 3)) %>%
  mutate(C001 = scale(C001)) %>%
  mutate(D057 = scale(D057)) %>%
  mutate(D063_B = scale(D063_B)) %>%
  ungroup() %>%
  mutate(moddern_woman_index = D057 - D063_B) %>%
  mutate(herdindex = D063_B - D057 - C001)

plot1 <- wvs_short_scale %>%
  filter(S003 == "DEU") %>%
  ggplot(aes( x= herdindex)) +
  geom_boxplot ()
print(plot1)

plot2 <- wvs_short_scale %>%
  ggplot(aes( x= S003, y= moddern_woman_index)) +
  geom_boxplot() +
  facet_grid( rows =vars(X001)) +
  labs(x= "Männer = 1, Frauen = 2")
print (plot2)


wvs_data %>%
  filter(S003 == "DEU") %>% 
  ggplot(aes(x = X003, y = S003)) +
  geom_boxplot()
