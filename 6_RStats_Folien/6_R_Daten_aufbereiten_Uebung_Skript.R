### title: Uebung 6 ####
### topic: Daten aufbereiten
### author: Felix Wohlgemuth
### date: 22.04.2020


### Preliminaries  ####
library(tidyverse) # Easily Install and Load the 'Tidyverse' 



### task 1  ####
# Laden Sie von moodle den OECD Datensatz "SOCX_AGG_31032020142101957.csv" herunter.
# Bearbeiten Sie den Datensatz, so dass Sie ein tidy dataset bekommen.

# load dataset
socx_data <- read_csv("_raw/SOCX_AGG_31032020142101957.csv") # import dataframe

# clean data
socx_data <- socx_data %>%             # select dataframe and replace dataframe
  select(COUNTRY, YEAR, Value) %>%     # select essential variables
  rename(fampol_exp_total_pct = Value) # rename Value Variable based in information in other variables



### task 2 ####
# Berechnen Sie die durchschnittlichen Ausgaben fuer Familienpolitik in oesterreich im Zeitraum 2000 - 2015

# average public expenditure on family policy for Austria (2000 - 2015)
socx_data %>%                                                       # select dataframe
  filter(COUNTRY == "AUS") %>%                                      # select only Austrian data
  summarise(mean_fampol_exp_total_pct = mean(fampol_exp_total_pct)) # specify mean()



### task 3 ####
# Welche fuenf Laender haben im Jahr 2010 am meisten fuer Familienpolitik ausgegeben (in % des BIPs)?
# Speichern Sie die Ergebnisse als # Kommentar im Skript.

socx_data %>%                         # select dataframe
  filter(YEAR == 2010) %>%            # select onle 2010 values
  arrange(-fampol_exp_total_pct) %>%  # arrange data ascending by family policy expenditure in 2010
  head( n = 5)                        # print first 5 values