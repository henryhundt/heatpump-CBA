## 1. open terminal, 
## 2. open docker
## 2a. if you want to look at the liveview on docker, the password is "secret" (somewhat helpful in this case)
## 3a. (if not done already) run the following: docker pull selenium/standalone-chrome:4.1.2-20220217
## 3. run the following: docker run -d -p 4445:4444 selenium/standalone-chrome:4.1.2-20220217
## 4. run the following code

#### to adjust the CSS id's used in the code below, I recommend using the SelectorGadget
#### chrome extension (potentially also available elsewhere).

library(RSelenium)  
library(stringr)
library(dplyr)

setwd("~/Desktop/ENV 810/Data/scraping")
## all wisconsin zipcodes, according to https://www.zipcodestogo.com/Wisconsin/
wisconsin_zip <- read.csv("wisconsin zip.csv")
wisconsin_zip <- wisconsin_zip[,1]

## dataset structure
costs <- data.frame(zipcode = 0, totalcost_low = 0, totalcost_high = 0, systemcost_low = 0,
                    systemcost_high = 0, laborcost_low = 0, laborcost_high = 0,
                    jobsupplycost_low = 0, jobsupplycost_high = 0)
i <- 1

## start Selenium session
rD <- rsDriver(port = 4445L, browser = "chrome")
remDr <- rD$client

## navigate to homewyse - it seems to help to go to the homepage first to minimize
## crashing but that might just be me making something up. 
remDr$navigate("https://www.homewyse.com") 
remDr$navigate("https://www.homewyse.com/costs/cost_of_oil_furnaces.html")

## this moves the results on the page from the low efficiency unit to the higher
## efficiency unit (exact details depends on the page)
a1 <- remDr$findElement(using = "css", value = "#a1")
a1$clickElement()
dd <- remDr$findElement(using = "css", value = "#dd")
dd$clickElement()
dd$sendKeysToElement(list("0", key = "enter"))
b1 <- remDr$findElement(using = "css", value = "#b1")
b1$clickElement()
b1$clickElement()
dd$clickElement()
dd$sendKeysToElement(list("1", key = "enter"))
a1$clickElement()

## the way homewyse is set up, there are only 3 unique costs within the state
## of wisconsin. Each occur at the below 3 zip codes and the rest of the zip codes
## all have the same cost as one of those 3, with a relatively simple pattern 
## used in the if else statement below
key_zips <- c(53001, 54301, 54601)

## scrape the webpage for the zipcodes given
for(i in 1:length(key_zips)){
  zip <- key_zips[i]
  costs[i,]$zipcode <- zip
  ## the unicode is for backspace
  remDr$findElement(using = "css", value = '#postcode5')$sendKeysToElement(list("\u0008", "\u0008", "\u0008", "\u0008", "\u0008", as.character(zip)))
  remDr$findElement(using = "css", value = "#button1")$click()
  ## this is just to make sure that the page updates before the text is taken.
  ## 5 seconds is excessive, but since we're only scraping 3 zipcodes its fine.
  Sys.sleep(5)
  costs[i,]$totalcost_low <- remDr$findElement(using = "css", value = "#z12")$getElementText()
  costs[i,]$totalcost_high <- remDr$findElement(using = "css", value = "#z13")$getElementText()
  costs[i,]$systemcost_low <- remDr$findElement(using = "css", value = "#v12")$getElementText()
  costs[i,]$systemcost_high <- remDr$findElement(using = "css", value = "#v13")$getElementText()
  costs[i,]$laborcost_low <- remDr$findElement(using = "css", value = "#u12")$getElementText()
  costs[i,]$laborcost_high <- remDr$findElement(using = "css", value = "#u13")$getElementText()
  costs[i,]$jobsupplycost_low <- remDr$findElement(using = "css", value = "#v22")$getElementText()
  costs[i,]$jobsupplycost_high <- remDr$findElement(using = "css", value = "#v23")$getElementText()
  print(zip)
}

## this serious of code takes the data from the scraping and applies it to the
## rest of the zipcodes in wisconsin.
test <- as.data.frame(apply(costs,2,as.character))
final <- test
for(i in wisconsin_zip){
  temp <- test
  if(i < 54301) {temp <- temp[1,]} else if(i < 54601) {temp <- temp[2,]} else if(i < 54901){temp <- temp[3,]} else {temp <- temp[2,]}
  temp$zipcode <- i
  final <- rbind(final, temp)
}
final <- final[-c(1:3),]
## this should be changed each time manually 
final$size <- "70K"
final$efficiency <- "85%+"

write.csv(final, "cost to install oil furnace - 70K BTU 85%+ efficiency.csv", row.names = F)

## this code can be used to quickly change which kind of tech is being looked at
## in terms of size and efficiency
old <- new
old$clickElement()
old$clickElement()
dd <- remDr$findElement(using = "css", value = "#dd")
dd$sendKeysToElement(list("0", key = "enter"))
new <- remDr$findElement(using = "css", value = "#b3")
new$clickElement()
new$clickElement()
dd$clickElement()
dd$sendKeysToElement(list("1", key = "enter"))
a1$clickElement()

remDr$close()

