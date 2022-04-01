setwd("~/Documents/GitHub/heatpump-CBA/NREL data/heating and cooling load")

library(dplyr)
library(stringr)
library(lubridate)

files <- dir()[which(str_detect(dir(), "single-family"))]

final <- NA
units <- 0
for(i in 1:length(files)){
  temp <- read.csv(files[i])
  units <- unique(temp$units_represented)
  county <- unique(temp$in.county)
  temp <- select(temp, timestamp, out.electricity.cooling.energy_consumption, out.electricity.fans_cooling.energy_consumption)
  temp$cooling_kWh <- apply(temp[,c(2:3)], 1, function(row){sum(row)})
  temp$month <- month(temp$timestamp)
  temp <- group_by(temp, month)
  temp <- summarise(temp, cooling_kWh = sum(cooling_kWh))
  temp$cooling_kWh <- temp$cooling_kWh/units
  colnames(temp)[2] <- county
  if(i == 1) final <- temp else final <- left_join(final, temp, by = "month")
  print(i)
}

#sum(apply(final[,c(2:73)], 2, function(col){sum(col)}))/units

counties <- colnames(final)[c(2:73)]
counties <- str_remove(counties, "G550")
counties <- paste0("55", str_remove(counties, "0$"))
colnames(final)[c(2:73)] <- counties


write.csv(final, "cooling kWh by county.csv", row.names = F)
