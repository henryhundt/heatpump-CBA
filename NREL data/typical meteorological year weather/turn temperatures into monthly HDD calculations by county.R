setwd("~/Documents/GitHub/heatpump-CBA/NREL data/typical meteorological year weather")

library(dplyr)
library(stringr)
library(lubridate)

files <- dir()[which(str_detect(dir(), ".csv"))]

final <- NA
for(i in 1:length(files)){
  temp <- read.csv(files[i])
  temp$temp_fahrenheit <- temp$Dry.Bulb.Temperature...C.*9/5+32
  county <- str_extract(files[i], "5.*0")
  temp$date_time <- ymd_hms(temp$date_time)
  temp$month <- month(temp$date_time)
  temp$HDH <- 65 - temp$temp_fahrenheit 
  temp <- mutate(temp, HDH = ifelse(HDH < 0, 0, HDH))
  temp <- group_by(temp, month)
  temp <- summarise(temp, HDD = sum(HDH))  
  temp$HDD <- temp$HDD/24  
  colnames(temp)[2] <- county
  if(i == 1) final <- temp else final <- left_join(final, temp, by = "month")
}

counties <- colnames(final)[c(2:73)]
counties <- str_remove(counties, "G550")
counties <- paste0("55", str_remove(counties, "0$"))
colnames(final)[c(2:73)] <- counties


write.csv(final, "temperature bins by county.csv", row.names = F)
