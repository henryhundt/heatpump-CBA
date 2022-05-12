## 1. open terminal, 
## 2. open docker
## 2a. if you want to look at the liveview on docker, the password is "secret"
## 3a. (if not done already) run the following: docker pull selenium/standalone-chrome:4.1.2-20220217
## 3. run the following: docker run -d -p 4445:4444 selenium/standalone-chrome:4.1.2-20220217
## 4. run the following code

setwd("~/Desktop/ENV 810/Data/scraping")

library(RSelenium)  
library(stringr)
library(dplyr)

heatpumps <- na.omit(data.frame(brand = NA, AHRI = NA, Ducting_Configuration = NA,
                                EER = NA, SEER = NA, HSPF = NA,
                                temp1 = NA, output1 = NA, COP1 = NA,
                                temp2 = NA, output2 = NA, COP2 = NA,
                                temp3 = NA, output3 = NA, COP3 = NA,
                                temp4 = NA, output4 = NA, COP4 = NA,
                                temp5 = NA, output5 = NA, COP5 = NA, URL = NA))
                    
URLs <- c(57039:58000)

blah <- filter(heatpumps, !str_detect(brand, "75b"))
heatpumps <- blah
badURLs <- list()

heatpumps <- read.csv("temp.csv")





## start Selenium session
rD <- rsDriver(port = 4445L, browser = "chrome")
remDr <- rD$client

for(i in 1:length(URLs)){
  number <- URLs[i]
  url <- paste0("https://ashp.neep.org/#!/product/",number)
  remDr$navigate(url)
  Sys.sleep(1)
  brand <- remDr$findElement(using = "css", value = ".product_title")$getElementText()
  if(brand != "" & brand != "delete") {
    heatpumps[31813 + i, "URL"] <- url
    heatpumps[31813 + i, "brand"] <- brand
    heatpumps[31813 + i, "AHRI"] <- remDr$findElement(using = "css", value = "tr:nth-child(4) .ng-binding")$getElementText()
    heatpumps[31813 + i, "Ducting_Configuration"] <- remDr$findElement(using = "css", value = "tr:nth-child(3) td:nth-child(2)")$getElementText()
    heatpumps[31813 + i, "EER"] <- remDr$findElement(using = "css", value = "tr:nth-child(6) .ng-binding")$getElementText()
    heatpumps[31813 + i, "SEER"] <- remDr$findElement(using = "css", value = "tr:nth-child(11) .ng-binding")$getElementText()
    heatpumps[31813 + i, "HSPF"] <- remDr$findElement(using = "css", value = "tr:nth-child(12) .ng-binding")$getElementText()
    heatpumps[31813 + i, "temp1"] <- remDr$findElement(using = "css", value = "thead+ .ng-scope .ng-binding+ .ng-binding:nth-child(2)")$getElementText()
    heatpumps[31813 + i, "output1"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(2) td:nth-child(6)")$getElementText()
    heatpumps[31813 + i, "COP1"] <- remDr$findElement(using = "css", value = "tr:nth-child(3) td:nth-child(3)")$getElementText()
    heatpumps[31813 + i, "temp2"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(3) .ng-binding+ .ng-binding:nth-child(2)")$getElementText()
    heatpumps[31813 + i, "output2"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(3) td:nth-child(6)")$getElementText()
    heatpumps[31813 + i, "COP2"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(3) tr:nth-child(3) td:nth-child(3)")$getElementText()
    heatpumps[31813 + i, "temp3"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(4) .ng-binding+ .ng-binding:nth-child(2)")$getElementText()
    heatpumps[31813 + i, "output3"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(4) td:nth-child(6)")$getElementText()
    heatpumps[31813 + i, "COP3"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(4) tr:nth-child(3) td:nth-child(3)")$getElementText()
    heatpumps[31813 + i, "temp4"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(5) .ng-binding+ .ng-binding:nth-child(2)")$getElementText()
    heatpumps[31813 + i, "output4"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(5) td:nth-child(6)")$getElementText()
    heatpumps[31813 + i, "COP4"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(5) tr:nth-child(3) td:nth-child(3)")$getElementText()
    heatpumps[31813 + i, "temp5"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(6) .ng-binding+ .ng-binding:nth-child(2)")$getElementText()
    heatpumps[31813 + i, "output5"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(6) td:nth-child(6)")$getElementText()
    heatpumps[31813 + i, "COP5"] <- remDr$findElement(using = "css", value = ".ng-scope:nth-child(6) tr:nth-child(3) td:nth-child(3)")$getElementText()
    print(number)
  } else {
    print(url)
    len <- length(badURLs)
    badURLs[len+1] <- url
  }
  rm(brand)
  rm(url)
  if(i%%50 == 0){ write.csv(heatpumps, "temp.csv", row.names = F)}
  if(i%%1000 == 0){
    remDr$close()
    rD <- rsDriver(port = 4445L, browser = "chrome")
    remDr <- rD$client
  }
}

remDr$close()
