### title: Uebung 10 ####
### topic: Beschreibende Statistik: mehrere Variablen
### author: Felix Wohlgemuth
### date: 15.05.2020


### preliminaries  ####
library(tidyverse)
library(naniar)

### load dataset ####
wvs_data <- readRDS("data/wvs_short.rds")

# create women_index
wvs_data <- wvs_data %>% 
  # answer "neither" transformed to NA
  replace_with_na(replace = list(C001 = 3)) %>% 
  # 0-1 scale of each variable
  mutate(C001_trans = (((C001 - 1) / 1) - 1) * -1 ) %>% 
  mutate(D057_trans = (((D057 - 1) / 3) - 1) * -1 ) %>% 
  mutate(D063_B_trans = (D063_B - 1) / 2) %>%
  # create women_index variable
  mutate(women_index = (C001_trans + D057_trans + D063_B_trans) / 3) %>% 
  select(-C001_trans, -D057_trans, -D063_B_trans)

# Variablen: D063_B "Job best way for women to be independent" &
# C001 "Job scarce: men more right to a job than women"

### task 1 ####
# erstellen Sie eine Kreuztabelle mit absoluten Häufigkeiten
t_1 <- table(wvs_data$D063_B, wvs_data$C001)
t_1

### task 2 ####
# erstellen Sie eine Kreuztabelle mit relativen Häufigkeiten
prop.table(t_1) # proportional zur Gesamtsumme
prop.table(t_1, 1) # proportional zur Reihenvariable
prop.table(t_1, 2) # proportional zur Spaltenvariable

### task 3 ### 
# Testen Sie die Korrelation zweier Variablen Ihrer Wahl
# Variablen: X003 "age" & women_index
cor(wvs_data$X003, wvs_data$women_index, use = "complete.obs")
# Alter und women_index hängen nur sehr schwach zusammen