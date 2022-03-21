library(dplyr)
library(stringr)

setwd("~/Downloads")

zip <- read.csv("final_wi_zip_fips_county.csv")

tracts <- read.csv("tracts_trim_0.csv")
tracts <- filter(tracts, STATE == "WI")

tracts$county_fips <- as.integer(str_extract(tracts$FIPS, "[0-9]{5}"))

zip <- select(zip, zip, county_fips)

blah <- left_join(tracts, zip, by = "county_fips")

blah <- mutate(blah, zip_54301 = ifelse(zip < 54301, T, F))
blah <- mutate(blah, zip_54601 = ifelse((zip < 54601 & zip >= 54301) | zip >= 54901, T, F))
blah <- mutate(blah, zip_54901 = ifelse(zip < 54901 & zip >= 54601, T, F))

final <- NA
for(i in unique(blah$FIPS)){
  temp <- filter(blah, FIPS == i)
  zip_54301 <- sum(temp$zip_54301)
  zip_54601 <- sum(temp$zip_54601)
  zip_54901 <- sum(temp$zip_54901)
  if(zip_54301 > zip_54601 & zip_54301 > zip_54901){
    temp <- filter(temp, zip_54301)
    temp <- temp[1,]
  } else if(zip_54601 > zip_54301 & zip_54601 > zip_54901){
    temp <- filter(temp, zip_54601)
    temp <- temp[1,]
  } else {
    temp <- filter(temp, zip_54901)
    temp <- temp[1,]
  }
  final <- rbind(temp, final)
}

final <- final[-1529,]

write.csv(final, "combined census tracts and zipcodes.csv")

final2 <- final[!duplicated(final$COUNTY),]

write.csv(final2, "combined counties and zipcodes.csv")