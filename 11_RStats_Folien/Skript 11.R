# Preliminaries
library(tidyverse)
library(naniar)
# install.packages("countrycode")
library(countrycode)
wvs_w6_data <- readRDS("_raw/F00007762-WV6_Data_R_v20180912.rds")

# reduce dataframe
wvs_w6_data <- wvs_w6_data %>% 
  select(V2, V3, V258, S019, V240,V242, V57, V58,
         V248, V239, V229, V235, Y001, Y002, Y003,
         V4, V45, V102, V54, V50, V48, V47) %>% 
  replace_with_na_at(.vars = variable.names(wvs_w6_data)[ variable.names(wvs_w6_data) != "Y003"],
    condition = ~.x <= -1) %>% 
  replace_with_na(replace = list(Y003 = -5)) %>%
  mutate(V2 = countrycode(V2, "wvs", "iso3c")) 

# create NEW women_index
wvs_w6_data <- wvs_w6_data %>% 
  mutate(women_index = (
    ((V45 - 1) / 2) +
    ((V54 - 1) / 3) +
    ((((V48 - 1) / 2) -1 ) * -1) +
    ((V50 - 1) / 3) + 
    ((V47 - 1) / 2)) / 5)

wvs_w6_data[c(5, 7:22)] <- lapply(wvs_w6_data[c(5, 7:22)] , function(x) as.ordered(x))
wvs_w6_data$V2 <- as.factor(wvs_w6_data$V2)

saveRDS(wvs_w6_data, "data/wvs_short_w6.rds")
write_csv(wvs_w6_data, "data/wvs_short_w6.csv")

rm(list = ls())

wvs_w6_data <- readRDS("data/wvs_short_w6.rds")

# Kovariation
## zwischen zwei kategorialen Variablen
## Visualisierung von table()
wvs_w6_data %>% 
  drop_na(V54, V45) %>% 
  ggplot() +
  geom_count(aes(x = V54, y = V45))

#Heatmap

wvs_w6_data %>% 
  drop_na(V54, V45) %>% 
  count(V54, V45) %>% 
  ggplot() +
  geom_tile(aes(x = V54, y = V45, fill = n))


## zwischen einer kategorialen und einer kontinuierlichen Variable

## Boxplot 
## Verteilung der Werte der kontinuierlichen Variable je Gruppe der kategorialen Variable

wvs_w6_data %>% 
  drop_na(Y003, women_index) %>%
  ggplot() +
  geom_boxplot(aes(x = Y003, y = women_index))

## zwei kontinuerlichen Variablen
## Scatterplot: Muster der Punkte - Zusammenhang erkenne

# data: CWS-data 
# https://www.lisdatacenter.org/news-and-events/comparative-welfare-states-dataset-2020/

library(readxl)
cws_data <- read_excel("_raw/CWS-data-2020.xlsx")

## Kovarianz zwischen öffentliche Ausgaben für Familienpolitik und Anteil von linken Parteien im Parlament
cor(cws_data$family_pub, cws_data$leftseat, use = "complete.obs")

cws_data %>% 
  filter(year == 2000) %>% 
  ggplot(aes(x = family_pub, y = leftseat)) +
  geom_point() +
  geom_smooth(method = "lm")

## Kovarianz zwischen öffentliche Ausgaben für Familienpolitik und Anteil von Frauen im Parlament
cor(cws_data$family_pub, cws_data$fempar, use = "complete.obs")

cws_data %>% 
  filter(year == 2000) %>% 
  ggplot(aes(x = family_pub, y = fempar)) +
  geom_point() +
  geom_smooth() 

cws_data %>% 
  ggplot(aes(x = family_pub, y = fempar)) +
  geom_point(aes(colour = id)) +
  geom_smooth(method = "lm")

## Vielzahl von Datenpunkten
cws_data <- cws_data %>% 
  mutate(year_10s = recode(year, "60s" = 1960 : 1969,
                                 "70s" = 1970 : 1979,
                                 "80s" = 1980 : 1989,
                                 "90s" = 1990 : 1999,
                                 "00s" = 2000 : 2009,
                                 "10s" = 2010 : 2018))

cws_data <- cws_data %>% 
  mutate(year_10s = cut(year, breaks = c(1960, 1969, 1979, 1989, 1999, 2009, 2018),
                        labels = c("60s", "70s", "80s", "90s", "00s", "10s"),
                        include.lowest = TRUE))
         

cws_data %>% 
  filter(year_10s %in% c("80s", "90s", "00s", "10s")) %>% 
  ggplot(aes(x = family_pub, y = fempar)) +
  geom_point(aes(colour = id)) +
  geom_smooth(method = "lm") +
  facet_grid( 
    rows = vars(year_10s))
  

cws_data %>% 
  ggplot(aes(x = family_pub, y = fempar)) +
  geom_bin2d()

# install.packages("hexbin")
library(hexbin)
cws_data %>% 
  ggplot(aes(x = family_pub, y = fempar)) +
  geom_hex()

## ggpairs
## schneller Überblick über den Zusammenhang zwischen einer Reihe von Variablen
# install.package("GGally")
library(GGally)
cws_data %>% 
  mutate(fed = as.ordered(fed)) %>% 
  ggpairs(columns = c("family_pub", "fed", "tfr", "leftseat", "rtseat" , "fempar"))
ggsave("figures/ggpairs.png", width = 9.5, height = 7, dpi = 150)