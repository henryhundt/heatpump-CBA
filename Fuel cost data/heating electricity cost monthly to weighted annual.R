setwd("~/Desktop/ENV 810/Data")

library(readxl)
library(dplyr)
library(lubridate)
#library(stringr)

temp <- read.csv("./Temp/HDD Proportions by Month by Climate Zone Using 2020 15 Year Normals.csv")

elec_cost <- read_xlsx("./Fuel cost data/electricity cost, 2-21 to 1-22, no coops.xlsx")

elec_cost$`Bill Date` <- ymd(elec_cost$`Bill Date`)
elec_cost$month <- month(elec_cost$`Bill Date`)

utility <- "Algoma Utility Commission"

elec_cost <- filter(elec_cost, `Utility Name` == utility)

climate_zone <- "5"

temp <- filter(temp, zone == climate_zone)

elec_cost <- left_join(elec_cost, temp, by = "month")

electricity_heating_cost <- sum(elec_cost$`per kWh total`*elec_cost$proportion)
