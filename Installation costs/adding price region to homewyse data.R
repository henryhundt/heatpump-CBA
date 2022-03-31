library(dplyr)

setwd("~/Documents/GitHub/heatpump-CBA/Installation costs")

files <- dir()[str_detect(dir(), "cost")]

for(i in 1:length(files)){
  temp <- read.csv(files[i])
  temp <- mutate(temp, price_region =ifelse(zipcode < 54301, 1, ifelse(zipcode < 54601, 2, ifelse(zipcode < 54901, 3, 2))))
  write.csv(temp, file = files[i], row.names = F)
}

