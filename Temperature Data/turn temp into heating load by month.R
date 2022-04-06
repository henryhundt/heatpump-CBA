setwd("~/Documents/GitHub/heatpump-CBA/Temperature Data")

library(dplyr)
library(stringr)

df <- read.csv("2020 15 year temp normals in WI by 11 HDD zones.csv")

df$HDH <- 65-df$HLY.TEMP.NORMAL
df <- mutate(df, HDH = ifelse(HDH < 0, 0, HDH))

df <- group_by(df, month, zone)

df <- summarise(df, HDH = sum(HDH))

totals <- group_by(df, zone)
totals <- summarise(totals, HDH = sum(HDH))

df$proportion <- 0
for(i in df$zone){
  df[df$zone == i,"proportion"] <- df[df$zone == i,"HDH"]/as.double(totals[totals$zone == i,"HDH"])
}

df <- arrange(df, zone)

write.csv(df, "HDD Proportions by Month by Climate Zone Using 2020 15 Year Normals.csv", row.names = F)



