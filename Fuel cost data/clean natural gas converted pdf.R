setwd("~/Desktop/ENV 810/Data/Fuel cost data")

library(readxl)
library(dplyr)
library(stringr)

final <- data.frame()

for(i in seq(from = 4, to = 28, by = 4)){
  df <- read_xlsx("AF2_May2020_June2021 (1)-converted.xlsx", sheet = i, col_names = F)
  place <- which(df[,1] == "Residential Space Heat") + 1
  if(length(place) > 0){df <- df[c(1, place:(place + 13)),]
  final <- rbind(final, df)}
}

final <- final[,c(1:7)]

colnames(final) <- c("Year", "Month", "Utility.Code", "Customer.Class", "Revenue", "Therms","Customers")
final$Revenue <- final$Revenue*1000
final$Therms <- final$Therms*1000

utilities <- filter(final, str_detect(Year, "="))
utilities$utility <- str_remove_all(utilities$Year,"[0-9]+ = ")
utilities$Utility.Code <- str_remove_all(utilities$Year," = .+")
utilities <- select(utilities, utility, Utility.Code)
utilities$Utility.Code <- as.double(utilities$Utility.Code)

final <- filter(final, !str_detect(Year, "="))

final <- left_join(final, utilities, by = "Utility.Code")

temp <- data.frame()

for(i in seq(from = 4, to = 28, by = 4)){
  df <- read_xlsx("AF2_June2019-July2020-converted.xlsx", sheet = i, col_names = F)
  place <- which(df[,1] == "Residential Space Heat") + 1
  if(length(place) > 0){df <- df[c(1, place:(place + 13)),]
  temp <- rbind(temp, df)}
}

temp <- temp[,c(1:7)]

colnames(temp) <- c("Year", "Month", "Utility.Code", "Customer.Class", "Revenue", "Therms","Customers")
temp$Revenue <- temp$Revenue*1000
temp$Therms <- temp$Therms*1000

utilities <- filter(temp, str_detect(Year, "="))
utilities$utility <- str_remove_all(utilities$Year,"[0-9]+ = ")
utilities$Utility.Code <- str_remove_all(utilities$Year," = .+")
utilities <- select(utilities, utility, Utility.Code)
utilities$Utility.Code <- as.double(utilities$Utility.Code)

temp <- filter(temp, !str_detect(Year, "="))

temp <- left_join(temp, utilities, by = "Utility.Code")

temp <- filter(temp, !(Year == 2020 & Month > 4))

final <- rbind(temp, final)

final <- arrange(final, Month)
final <- arrange(final, Year)
final <- arrange(final, Utility.Code)

write.csv(final, "nat. gas cost, 6-19 to 6-21, no coops.csv", row.names = F)




