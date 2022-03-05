setwd("~/Documents/GitHub/heatpump-CBA/Installation costs")

files <- dir()[str_detect(dir(), ".csv")]

for(i in files){
  temp <- read.csv(files[i])
  for(j in 2:9){
    temp[,j] <- as.numeric(str_remove_all(temp[,j], "[$,]"))
  }
  write.csv(i, row.names = F)
}

