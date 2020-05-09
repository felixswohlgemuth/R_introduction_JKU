library(tidyverse)
socx_data <- read_csv(file.choose())


plot3 <- SOCX_AGG_20042020191205895 %>% 
  drop_na(family_service_total_pct_gdp) %>%
  filter(COUNTRY %in% c(shape = "AUT", "BEL", "ESP", "HUN")) %>%
  ggplot(aes(x = YEAR, y = family_service_total_pct_gdp, group = COUNTRY)) + geom_point(aes(shape = COUNTRY, colour = COUNTRY)) + geom_line(aes(colour = COUNTRY))

plot6 <- SOCX_AGG_20042020191205895 %>%
  filter(COUNTRY %in% c("AUT", "SWE", "GRC", "OECD", "BEL")) %>%
  filter(YEAR >= 2005 & YEAR <= 2014) %>%
  ggplot(aes(x = YEAR, y = family_cash_leave_pct_gdp, colour = COUNTRY)) +
  geom_col()

socx_data %>%
  filter(COUNTRY %in% c("AUT", "DEU")) %>%
  ggplot() + geom_area(mapping = aes(x=YEAR, y=family_service_childcare_pct_gdp, color = COUNTRY))

socx_data %>%
  filter(COUNTRY %in% c("AUT", "DNK")) %>%
  filter(YEAR >= 2000 & YEAR <= 2015) %>%
  drop_na(family_cash_total_pct_gdp, family_total_total_pct_gdp, family_service_total_pct_gdp) %>%
  mutate(Propartionale_Barleistungen = family_cash_total_pct_gdp / family_total_total_pct_gdp) %>%
  mutate(Propartionale_Serviceleistungen = family_service_total_pct_gdp / family_total_total_pct_gdp) %>%
  ggplot()+
  geom_density(aes(x = Propartionale_Barleistungen), colour = "green", fill = "green", alpha = 0.15)+
  geom_density(aes(x = Propartionale_Serviceleistungen), colour = "blue", fill = "blue", alpha = 0.15)
labs(x = "green = Propartionale_Barleistungen" | "blue = Propartionale_Serviceleistungen" )

socx_data %>% drop_na(family_total_total_pct_gdp) %>%
  filter(COUNTRY %in% c("AUT", "GBR", "ITA", "SWE")) %>%
  ggplot(aes(x= YEAR, y= family_total_total_pct_gdp)) +
  geom_point(aes(shape= COUNTRY)) +
  geom_line(aes(color= COUNTRY))

socx_data %>%
  filter(COUNTRY %in% c("AUT")) %>%
  drop_na(family_cash_allowances_pct_gdp) %>%
  ggplot(aes(x = YEAR, y = family_cash_allowances_pct_gdp)) +
  geom_col()

socx_data %>%
  filter(COUNTRY %in% c("AUT", "DEU", "ITA", "NOR")) %>%
  drop_na(family_cash_allowances_pct_gdp) %>%
  ggplot(aes(x = YEAR, y = family_cash_allowances_pct_gdp)) +
  geom_point(aes(shape = COUNTRY)) +
  geom_line(aes(colour = COUNTRY))

socx_data %>%
  filter(COUNTRY %in% c("AUT","USA","JPN","FRA")) %>%
  filter(YEAR >= 2000 & YEAR <= 2010) %>%
  ggplot(aes(x = YEAR, y = family_service_childcare_pct_gdp, colour = COUNTRY)) +
  geom_point(shape=21) +
  geom_line(aes(colour=COUNTRY))

daten_austria <- select(socx_data,COUNTRY,YEAR, family_service_childcare_pct_gdp)
AUT <- daten_austria %>%
  filter(COUNTRY == "AUT") %>%
  filter(YEAR >= 2000 & YEAR <= 2010)
view(AUT)

lolliplot<-ggplot(AUT, aes(x=family_service_childcare_pct_gdp, y=YEAR)) +
  geom_segment( aes(x=family_service_childcare_pct_gdp, xend=family_service_childcare_pct_gdp, y=YEAR, yend=YEAR)) +
  geom_point( size=5, color="red", fill=alpha("green", 0.3), alpha=0.7, shape=21, stroke=2) 

socx_data %>%
  filter(COUNTRY %in% c("AUT", "SWE", "USA", "JPN")) %>%
  ggplot(aes(x = YEAR, y = family_total_total_pct_gdp, colour = COUNTRY)) +
  geom_smooth()

plot1 <- ggplot(daten = "socx_data") +
  geom_point(mapping = aes(x = YEAR, y = family_cash_allowances_pct_gdp))

socx_data %>%
  drop_na(family_cash_allowances_pct_gdp) %>%
  ggplot() +
  geom_point(mapping = aes(x = YEAR, y = family_total_total_pct_gdp, shape = COUNTRY))

socx_data %>%
  drop_na(family_service_total_pct_gdp) %>%  
  ggplot() + geom_bin2d(mapping = aes(x = YEAR, y = family_service_total_pct_gdp, colour = COUNTRY))
+   
  geom_point(mapping = aes(x = YEAR, y = family_service_total_pct_gdp)) + 
  geom_line(aes(x = YEAR, y = family_service_total_pct_gdp,colour = COUNTRY)) 

socx_data%>%
  filter(COUNTRY %in% c("AUT", "BEL", "AUS", "FIN"))%>%
  filter(YEAR >=2010 & YEAR <= 2015)%>%
  ggplot(aes(x=YEAR, y=Value, colour=COUNTRY, shape=COUNTRY))+geom_point()+geom_line()


library(forcats)
socx_data %>%
  filter (COUNTRY %in% c("AUT", "BEL", "AUS", "FIN"))%>%
  filter(YEAR >= 2010 & YEAR <= 2015) %>%
  mutate(name = fct_reorder(COUNTRY, desc(family_service_total_pct_gdp))) %>%
  ggplot( aes(x=family_service_total_pct_gdp, y=COUNTRY)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

socx_data %>%
  filter(COUNTRY %in% c("AUT", "OECD", "BEL", "ESP", "FIN")) %>%
  drop_na(family_cash_leave_pct_gdp) %>%
  ggplot(aes(x = YEAR, y = family_cash_leave_pct_gdp, shape = COUNTRY)) +
  geom_point() +
  geom_line(aes(color=COUNTRY))

socx_data %>%
  filter(COUNTRY %in% c("AUT")) %>%
  filter(YEAR >= 2000 & YEAR <= 2008) %>%
  ggplot(aes(x= family_cash_leave_pct_gdp, y=YEAR, colour = COUNTRY)) +
  geom_col()

socx_data <- socx_data %>%
  filter(COUNTRY != "OECD") %>%
  filter(YEAR >= 2000 & YEAR <= 2015) %>%
  mutate(cash_prop = (family_cash_total_pct_gdp / family_total_total_pct_gdp) * 100) %>%
  mutate(services_prop = (family_service_total_pct_gdp / family_total_total_pct_gdp) * 100)

socx_data %>%
  ggplot() +
  geom_density(aes(x = cash_prop), colour = "red", fill = "red", alpha = 0.1) +
  geom_density(aes(x = services_prop), colour = "blue", fill = "blue", alpha = 0.1) +
  labs(x = "blue = services_prop  |  red = cash_prop")

socx_data %>%
  filter(COUNTRY %in% c("AUS", "BEL", "DNK", "ESP")) %>%
  ggplot(aes(x = YEAR, y = family_cash_total_pct_gdp, shape = COUNTRY )) +
  geom_point() +
  geom_line()

socx_data %>%
  filter(COUNTRY %in% c("AUS", "BEL", "OECD", "DEU")) %>%
  filter(YEAR == 2015) %>%
  drop_na(family_cash_total_pct_gdp) %>%
  ggplot() +
  geom_col(aes(x = family_cash_total_pct_gdp, y = COUNTRY), colour = "blue",  alpha = 5, fill = "blue", size = 3) +
  geom_col(aes(x =  family_service_total_pct_gdp, y = COUNTRY), colour = "blue", alpha = 5, fill = "red", size = 3) +
  labs(x = "blue = family_cash_total_pct_gdp | red = family_service_total_pct_gdp")

socx_data %>%
  select("COUNTRY", "YEAR", "family_cash_total_pct_gdp", "family_service_total_pct_gdp") %>%
  filter(YEAR == 2015) %>%
  filter(COUNTRY %in% c("AUS", "BEL", "OECD", "DEU"))


socx_data %>%
  filter(COUNTRY %in% c("AUT", "GBR", "HUN", "OECD", "FRA")) %>%
  filter(YEAR >= 2000 & YEAR <= 2015) %>%
  ggplot(aes(x = YEAR, y = family_cash_total_pct_gdp, shape = COUNTRY)) +
  geom_point() +
  geom_line()

data_usa <- socx_data %>% 
  select(COUNTRY, YEAR, family_total_total_pct_gdp) %>% 
  filter(COUNTRY %in% c("AUT", "USA", "CZE", "AUS"))

ggplot(data = data_usa) +  
  geom_point(mapping = aes(x = YEAR, y = family_total_total_pct_gdp, shape=COUNTRY))+
  geom_smooth(mapping = aes(x = YEAR, y = family_total_total_pct_gdp, color=COUNTRY))

socx_data %>%
  filter(COUNTRY %in% c("AUT", "CAN", "DEU", "GRC")) %>%
  ggplot(aes(x = COUNTRY, y = family_total_total_pct_gdp)) +
  geom_violin()

socx_data %>%
  filter(COUNTRY %in% c("AUT", "CAN", "DEU", "GRC")) %>%
  ggplot(aes(x = YEAR, y = family_total_total_pct_gdp, colour = COUNTRY)) +
  geom_violin()

socx_data %>%
  filter(COUNTRY %in% c("AUT", "BEL", "DEU", "OECD")) %>%
  filter(YEAR >= 2000 & YEAR <= 2015) %>%
  ggplot(aes(x = family_total_total_pct_gdp)) +
  geom_histogram(binwidth = 0.8, position = "dodge")

socx_data %>%
  drop_na(family_total_total_pct_gdp) %>%
  filter(COUNTRY %in% c("AUT"))%>%
  ggplot(aes(x = YEAR,y = family_total_total_pct_gdp)) +
  geom_bar(stat="identity")

socx_data %>%
  drop_na(family_total_total_pct_gdp) %>%
  filter(COUNTRY %in% c("AUT","DNK","CZE","FRA"))%>%
  ggplot(aes(x = YEAR,y = family_total_total_pct_gdp,shape = COUNTRY)) +
  geom_point() +
  geom_line()

(socx_data) %>%
  filter(COUNTRY %in% c("DNK", "FIN", "ISL", "SWE")) %>%
  ggplot(aes(x = YEAR, y = family_cash_leave_pct_gdp, colour = COUNTRY)) +
  geom_area()
