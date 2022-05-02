library(dplyr)
library(stringr)
library(lubridate)

setwd("~/Documents/GitHub/heatpump-CBA/NREL data/heating and cooling load")

load_files <- dir()[which(str_detect(dir(), "g55"))]

setwd("~/Documents/GitHub/heatpump-CBA/NREL data/typical meteorological year weather")

temperature_files <- dir()[which(str_detect(dir(), "G55"))]


setwd("~/Documents/GitHub/heatpump-CBA/NREL data")

final <- NA
units <- 0
for(i in 1:length(load_files)){
  path <- paste0("./heating and cooling load/",load_files[i])
  temp <- read.csv(path)
  units <- unique(temp$units_represented)
  county <- unique(temp$in.county)
  FIPS <- str_extract(county, "5.*0")
  FIPS <- str_remove(FIPS, "550")
  FIPS <- paste0("55", str_remove(FIPS, "0$"))
  temp <- select(temp, timestamp, out.electricity.heating.energy_consumption,
                 out.electricity.heating_supplement.energy_consumption,
                 out.fuel_oil.heating.energy_consumption,
                 out.natural_gas.heating.energy_consumption,
                 out.propane.heating.energy_consumption,
                 out.wood.heating.energy_consumption, 
                 out.electricity.cooling.energy_consumption, 
                 out.electricity.fans_cooling.energy_consumption)
  ## aggregate values by hour
  temp$hour <- hour(temp$timestamp)
  temp$day <- day(temp$timestamp)
  temp$month <- month(temp$timestamp)
  temp <- group_by(temp, hour, day, month)
  temp <- summarise(temp, out.electricity.heating.energy_consumption = sum(out.electricity.heating.energy_consumption),
                    out.electricity.heating_supplement.energy_consumption = sum(out.electricity.heating_supplement.energy_consumption),
                    out.fuel_oil.heating.energy_consumption = sum(out.fuel_oil.heating.energy_consumption),
                    out.natural_gas.heating.energy_consumption = sum(out.natural_gas.heating.energy_consumption),
                    out.propane.heating.energy_consumption = sum(out.propane.heating.energy_consumption),
                    out.wood.heating.energy_consumption = sum(out.wood.heating.energy_consumption), 
                    out.electricity.cooling.energy_consumption = sum(out.electricity.cooling.energy_consumption), 
                    out.electricity.fans_cooling.energy_consumption = sum(out.electricity.fans_cooling.energy_consumption))
  temp$FIPS <- FIPS
  path <- paste0("./typical meteorological year weather/",temperature_files[i])
  temperature <- read.csv(path)
  temperature$temperature <- temperature$Dry.Bulb.Temperature...C.*9/5+32
  temperature$hour <- hour(temperature$date_time)
  temperature$day <- day(temperature$date_time)
  temperature$month <- month(temperature$date_time)
  temperature <- select(temperature, temperature, hour, day, month)
  temp <- left_join(temp, temperature, by = c("hour", "day", "month"))
  if(i == 1) final <- temp else final <- rbind(final, temp)
  print(i)
}

write.csv(final, "non-aggregated space heating energy use for all counties.csv", row.names = F)
