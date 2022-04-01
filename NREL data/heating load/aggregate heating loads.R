setwd("~/Documents/GitHub/heatpump-CBA/NREL data/heating load")

library(dplyr)
library(stringr)
library(lubridate)

files <- dir()[which(str_detect(dir(), ".csv"))]

final <- NA
units <- 0
for(i in 1:length(files)){
  temp <- read.csv(files[i])
  units <- unique(temp$units_represented)
  county <- unique(temp$in.county)
  temp <- select(temp, timestamp, out.electricity.heating.energy_consumption,
                 out.electricity.heating_supplement.energy_consumption,
                 out.fuel_oil.heating.energy_consumption,
                 out.natural_gas.heating.energy_consumption,
                 out.propane.heating.energy_consumption,
                 out.wood.heating.energy_consumption)
  kwh_to_mmBTU <- 3412/10^6 #https://www.eia.gov/energyexplained/units-and-calculators/energy-conversion-calculators.php
  ##convert all kwh to mmBTU
  temp[,c(2:7)] <- lapply(temp[,c(2:7)], function(col){col*kwh_to_mmBTU})
  ##convert all non-electricity fuels to final energy using assumed efficiencies
  ##we assume the efficiecny of all electricity is 100%
  temp$out.fuel_oil.heating.energy_consumption <- temp$out.fuel_oil.heating.energy_consumption*.9
  temp$out.natural_gas.heating.energy_consumption <- temp$out.natural_gas.heating.energy_consumption*.95
  temp$out.propane.heating.energy_consumption <- temp$out.propane.heating.energy_consumption*.90
  temp$out.wood.heating.energy_consumption <- temp$out.wood.heating.energy_consumption*.69
  ##take sum of mmBTU by row
  temp$heating_load <- apply(temp[,c(2:7)], 1, function(row){sum(row)})
  temp$month <- month(temp$timestamp)
  temp <- group_by(temp, month)
  temp <- summarise(temp, heating_load = sum(heating_load))
  temp$heating_load <- temp$heating_load/units
  colnames(temp)[2] <- county
  if(i == 1) final <- temp else final <- left_join(final, temp, by = "month")
  print(i)
}

#sum(apply(final[,c(2:73)], 2, function(col){sum(col)}))/units

counties <- colnames(final)[c(2:73)]
counties <- str_remove(counties, "G550")
counties <- paste0("55", str_remove(counties, "0$"))
colnames(final)[c(2:73)] <- counties


write.csv(final, "heating load by county.csv", row.names = F)
