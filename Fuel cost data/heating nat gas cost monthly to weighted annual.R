setwd("~/Desktop/ENV 810/Data")

library(readxl)
library(dplyr)
library(lubridate)
#library(stringr)

temp <- read.csv("./Temp/HDD Proportions by Month by Climate Zone Using 2020 15 Year Normals.csv")
ng_cost <- read.csv("./Fuel cost data/nat. gas cost, 6-19 to 6-21, no coops.csv")

ng_cost$price <- ng_cost$Revenue/ng_cost$Therms
ng_cost <- group_by(ng_cost, Month, Utility.Code)
ng_cost <- summarise(ng_cost, price = mean(price))

utility <- "3270"

ng_cost <- filter(ng_cost, Utility.Code == utility)

climate_zone <- "10"

temp <- filter(temp, zone == climate_zone)

colnames(ng_cost)[1] <- "month"
ng_cost <- left_join(ng_cost, temp, by = "month")

sum(ng_cost$price*ng_cost$proportion)
