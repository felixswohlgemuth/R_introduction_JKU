### title: Uebung 9 ####
### topic: Visualisierung Einzelvariablen
### author: Felix Wohlgemuth
### date: 08.05.2020


### preliminaries  ####
library(tidyverse) # Load the 'Tidyverse' including ggplot2

### load dataset ####
wvs_data <- readRDS(file.choose()) # import dataframe with file.choose()

### task 1 ####
# Erstellen Sie ein Boxplot für eine Variable Ihrer Wahl, entweder für Deutschland oder Schweden.
wvs_data %>%
  filter(S003 == "DEU") %>% # filter DEU
  ggplot(aes(y = D057)) + # define y-axis variable & data
  geom_boxplot() + # set boxplot as geom_function
  labs(title = "Eine Hausfrau zu sein ist genauso erfüllend wie eine bezahlte Arbeit",
       subtitle = "1 = Stimme voll und ganz zu <-> 4 = Stimme überhaupt nicht zu\nDeutschland Welle 6 2013",
       y = "")  # set labels


# Visualisierung speichern
ggsave("boxplot_hausfrau_de.png", path = "figures", height = 6.5, width = 6.5)

# Macht ein Boxplot für eine ordinal skalierte Variable Sinn?
# Wir sehen, dass das 50%- und das 75%-Quartil gleich 3 "stimme nicht zu" ist
# und dass das 25%-Quartil gleich "stimme zu" ist
# Ein Säulendiagramm mit der Anzahl der Ausprägungen pro Item mach mehr Sinn

wvs_data %>%
  filter(S003 == "DEU") %>% # filter DEU
  ggplot(aes(x = D057)) + # define x-axis variable & data
  geom_bar() + # set barplot as geom_function
  labs(title = "Eine Hausfrau zu sein ist genauso erfüllend wie eine bezahlte Arbeit",
       subtitle = "1 = Stimme voll und ganz zu <-> 4 = Stimme überhaupt nicht zu\nDeutschland Welle 6 2013",
       y = "Anzahl", x= "")  # set labels

# Visualisierung speichern
ggsave("bar_hausfrau_de.png", path = "figures", height = 6.5, width = 6.5)


### task 2 ####
# Erstellen Sie Boxplots für Schweden und Deutschland und Männer und Frauen in einer Grafik mit `facet_grid()`.

# Für die bessere Übersichtlichkeit benennen wir die Ausprägung von X001 in Mann und Frau um
wvs_data <- wvs_data %>% 
  mutate(X001 = as.character(X001)) %>% 
  mutate(X001 = recode(X001, "1" = "Mann", "2" = "Frau"))

wvs_data %>%
  ggplot(aes(y = D057)) + # define y-axis variable & data
  geom_boxplot() + # set boxplot  as geom_function
  facet_grid( 
    cols = vars(S003), 
    rows = vars(X001)) + # set facet variables
  labs(title = "Eine Hausfrau zu sein ist genauso erfüllend wie eine bezahlte Arbeit",
       subtitle = "1 = Stimme voll und ganz zu <-> 4 = Stimme überhaupt nicht zu\nDeutschland & Schweden Welle 6 2013, getrennt nach Geschlecht",
       y = "")  # set labels

# Visualisierung speichern
ggsave("boxplot_hausfrau_de_se_gender.png", path = "figures", height = 6.5, width = 6.5)

# Auch hier zeigt sich dass ein Boxplot für die ordinal skalierte Variablen nicht geeignet ist
# Die gleiche Darstellung mit Säulendiagrammen

wvs_data %>%
  ggplot(aes(x = D057)) + # define x-axis variable & data
  geom_bar() + # set boxplot  as geom_function
  facet_grid( 
    cols = vars(S003), 
    rows = vars(X001)) + # set facet variables
  labs(title = "Eine Hausfrau zu sein ist genauso erfüllend wie eine bezahlte Arbeit",
       subtitle = "1 = Stimme voll und ganz zu <-> 4 = Stimme überhaupt nicht zu\nDeutschland 2013 & Schweden 2011 Welle 6, getrennt nach Geschlecht",
       y = "Anzahl", x = "")  # set labels

# Visualisierung speichern
ggsave("bar_hausfrau_de_se_gender.png", path = "figures", height = 6.5, width = 6.5)


