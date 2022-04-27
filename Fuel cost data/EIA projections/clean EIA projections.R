setwd("~/Documents/GitHub/heatpump-CBA/Fuel cost data/EIA projections")

library(dplyr)
library(stringr)
library(tidyr)

df <- read.csv("Table_3._Energy_Prices_by_Sector_and_Source.csv", skip = 4)

df <- df[c(1:which(str_detect(df$X,"Commercial"))[1]),]

df <- filter(df, !is.na(X2021))
df <- select(df, -X2020, -X, -api.key, - units)

df$full.name <- str_remove_all(df$full.name, "Energy Prices: Residential: ")

df <- as.data.frame(t(df))
colnames(df) <- df[1,]
df$year <- row.names(df)
df <- df[-1,]
df$year <- as.numeric(str_remove(df$year, "X"))
df <- filter(df, !is.na(year))

write.csv(df, "Energy_Prices_Residential_projections.csv", row.names = F)




