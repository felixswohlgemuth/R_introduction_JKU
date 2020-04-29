### title: Uebung 8 ####
### topic: Daten aufbereiten
### author: Felix Wohlgemuth
### date: 01.05.2020


### preliminaries  ####
library(tidyverse) # Load the 'Tidyverse' including ggplot2

### load dataset ####
socx_data <- read_csv(file.choose()) # import dataframe with file.choose()

### clean dataset ###
socx_data <- socx_data %>%
  filter(COUNTRY != "OECD") %>% # remove rows with OECD mean
  filter(YEAR <= 2015) # keep all observations between 2000 & 2015


### task 1 ####
# Erstellen Sie ein scatterplot mit einer Variable Ihrer Wahl auf der y-Achse und den Jahren auf der x-Achse
# Ausgaben fuer formale Kinderbetreuung

ggplot(data = socx_data, aes(x = YEAR, y = family_service_childcare_pct_gdp)) + # specify variables in ggplot()
  geom_point()

ggplot(data = socx_data) + 
  geom_point(aes(x = YEAR, y = family_service_childcare_pct_gdp)) # or specify variables in geom_function()

# Ausgaben fuer Kinderbetreuung im Zeitraum 2000 - 2010 

socx_data %>% 
  filter(YEAR >= 2000 & YEAR <= 2010) %>% # filter years for following functions
  ggplot() + # dataset specified with %>%  in line before
  geom_point(aes(x = YEAR, y = family_service_childcare_pct_gdp))

# X-Achse zeigt Halbjahre fuer die wir keine Daten haben - geschieht da YEAR als numerische Variable definiert ist
# mit scale_x_continuous werden Anfang und Ende der Skala und die angezeigten Schritte angezeigt

socx_data %>% 
  filter(YEAR >= 2000 & YEAR <= 2010) %>% # filter years for following functions
  ggplot() + # dataset specified with %>%  in line before
  geom_point(aes(x = YEAR, y = family_service_childcare_pct_gdp)) +
  scale_x_continuous(limits = c(2000, 2010), breaks = c(2000, 2005, 2010)) # limits = start & end; breaks = shown years on scale


### task 2 ####
# Waehlen Sie 4 Laender aus und erweitern Sie den scatterplot mit einer Gruppierung der Laender per shape = 
# Oesterreich, USA, Finnland, Litauen
socx_data %>% 
  filter(COUNTRY %in% c("AUT", "USA", "FIN", "LTU" )) %>% 
  filter(YEAR >= 2000 & YEAR <= 2010) %>%
  ggplot() +
  geom_point(aes(x = YEAR, y = family_service_childcare_pct_gdp, shape = COUNTRY)) + # shape = in aes()
  scale_x_continuous(limits = c(2000, 2010), breaks = c(2000, 2005, 2010)) # limits = start & end; breaks = shown years on scale


### task 3 ####
# Probieren Sie eine andere geom_function vom cheatsheet aus (Beispiele finden Sie auf: https://www.r-graph-gallery.com/)
# Boxlot f?r alle L?nder und gesamten Zeitraum 2000 - 2015
socx_data %>% 
  ggplot(aes(x = COUNTRY, y = family_service_childcare_pct_gdp)) +
  geom_boxplot()

# Laendernamen sind nicht lesbar mit theme(axis.text.x = element_text()) kann der Text der x-Achse bearbeitet werden
# angle = mit welchen Winkel der Text angezeigt wird und vjust = oder hjust = dass der Text in der Mitte des Striches steht
# mehr Infos unter https://www.gl-li.com/2017/08/18/place-text-at-right-location/

socx_data %>% 
  ggplot(aes(x = COUNTRY, y = family_service_childcare_pct_gdp)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) # angle = 90 Rotation des Textes um 90? und vjust = 0.5 Text nach links verschieben

