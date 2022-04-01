setwd("~/Documents/GitHub/heatpump-CBA/NREL data/typical meteorological year weather")

library(dplyr)
library(stringr)
library(lubridate)

files <- dir()[which(str_detect(dir(), "tmy"))]

final <- NA
for(i in 1:length(files)){
  temp <- read.csv(files[i])
  temp$temperature <- temp$Dry.Bulb.Temperature...C.*9/5+32
  county <- str_extract(files[i], "5.*0")
  county <- str_remove(county, "G550")
  temp$county <- paste0("55", str_remove(county, "0$"))
  temp$date_time <- ymd_hms(temp$date_time)
  temp$month <- month(temp$date_time)
  temp$day <- day(temp$date_time)
  temp$hour <- hour(temp$date_time)
  temp <- select(temp, month, day, hour, temperature, county)
  final <- rbind(final, temp)
}

final <- na.omit(final)

write.csv(final, "temperature data by county.csv", row.names = F)
