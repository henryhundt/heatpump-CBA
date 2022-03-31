setwd("~/Documents/GitHub/heatpump-CBA")
library(readxl)
library(dplyr)
library(lubridate)

elec_cost <- read.csv("./Fuel cost data/electricity data.csv")

final <- data.frame()
for(i in unique(elec_cost$Utility.ID)){
  temp <- filter(elec_cost, Utility.ID == i)
  rows <- nrow(temp)
  if(rows > 12){
    remove <- rows - 12
    temp <- temp[-c(1:remove),]
  }
  final <- rbind(final, temp)
}

final$Bill.Date <- mdy(final$Bill.Date)
final$Month <- month(final$Bill.Date)
colnames(final)[4] <- "Rate"
final <- select(final, Utility.ID, Utility.Name, Month, Rate)


coops <- read.csv("./Fuel cost data/coop standard rates.csv")


final <- rbind(final, coops)



write.csv(final, "final electricity data.csv")
