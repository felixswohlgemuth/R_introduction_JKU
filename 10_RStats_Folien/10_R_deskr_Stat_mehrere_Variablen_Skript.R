wvs_data <- readRDS("_raw/wvs_short.RDS")

## two way table
table(wvs_data$X047, wvs_data$D057)

# three way table
table(wvs_data$X047, wvs_data$D057, wvs_data$S003)

# proportional two way table
# by total occurance
prop.table(table(wvs_data$X047, wvs_data$D057))
# by row total
prop.table(table(wvs_data$X047, wvs_data$D057), 1)
# by column total
prop.table(table(wvs_data$X047, wvs_data$D057), 2)

# chi^2 for two variable
summary(table(wvs_data$X047, wvs_data$D057))

## correlation
library(tidyverse)

cor(wvs_data$X047, wvs_data$D057, wvs_data$D063_B)

