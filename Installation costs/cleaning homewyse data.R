setwd("~/Documents/GitHub/heatpump-CBA/Installation costs")

files <- dir()[str_detect(dir(), ".csv")]

for(i in 2:length(files)){
  temp <- read.csv(files[i])
  for(j in 2:9){
    temp[,j] <- as.numeric(str_remove_all(temp[,j], "[$,]"))
  }
  write.csv(temp, file = files[i], row.names = F)
}

