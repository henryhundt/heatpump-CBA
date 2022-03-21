library(dplyr)
library(stringr)

setwd("~/Documents/GitHub/heatpump-CBA/creating geographic data set")


## a csv of zip codes and their "primary" counties according to 
## https://simplemaps.com/data/us-zips
zip <- read.csv("final_wi_zip_fips_county.csv")

## a csv of census tracts downloaded from arcGIS online's Live Atlas
## https://uw-mad.maps.arcgis.com/home/item.html?id=db3f9c8728dd44e4ad455e0c27a85eea
tracts <- read.csv("tracts_trim_0.csv")
tracts <- filter(tracts, STATE == "WI")

## find the county fips code for each census tract (first 5 digits of the tract's FIPS)
tracts$county_fips <- as.integer(str_extract(tracts$FIPS, "[0-9]{5}"))

## remove superfluous columns from the zip code dataframe
zip <- select(zip, zip, county_fips)

## combine the zips to the census tracts
blah <- left_join(tracts, zip, by = "county_fips")


## Now, taking advantage of the fact that homewyse only has 3 different price "regions"
## within Wisconsin, the rest of the code finds the majority "price region" for each county
## and then just applies that price region, using a representative zip code to all census
## tracts within a county.
## So, for example, Adams county has five zip codes in it. Four of these fall into the price region
## for all zip codes below 54301. One of them falls into the between 54601 and 54901. We just assume
## that the price for anyone in the county will correspond to the former price region, since that's the
## price estimated by homewyse for the majority of zip codes associated with Adams County. The ultimate
## dataset is at the census tract, or county, level and each tract/county has a single zip code associated
## with it that can then be turned into a set of installation costs in the monte carlo code.
##
## Note that this is less accurate than determining which zip code a majority of each census tract actually falls into.
##
## The code will need to be changed if we want to work at the zip code level in the tool or map. 


blah <- mutate(blah, zip_54301 = ifelse(zip < 54301, T, F))
blah <- mutate(blah, zip_54601 = ifelse((zip < 54601 & zip >= 54301) | zip >= 54901, T, F))
blah <- mutate(blah, zip_54901 = ifelse(zip < 54901 & zip >= 54601, T, F))

final <- NA
## for each census tract, find out which price region a majority of the associated 
## zip codes fall into. Then take one of those zip codes and add it to the final
## dataframe so that that census tract is associatd with that price region. 
for(i in unique(blah$FIPS)){
  temp <- filter(blah, FIPS == i)
  zip_54301 <- sum(temp$zip_54301)
  zip_54601 <- sum(temp$zip_54601)
  zip_54901 <- sum(temp$zip_54901)
  if(zip_54301 >= zip_54601 & zip_54301 >= zip_54901){
    temp <- filter(temp, zip_54301)
    temp <- temp[1,]
  } else if(zip_54601 >= zip_54301 & zip_54601 >= zip_54901){
    temp <- filter(temp, zip_54601)
    temp <- temp[1,]
  } else {
    temp <- filter(temp, zip_54901)
    temp <- temp[1,]
  }
  final <- rbind(temp, final)
}

final <- final[-1529,]

##  a dataframe at the census tract level
write.csv(final, "combined census tracts and zipcodes.csv")

final2 <- final[!duplicated(final$COUNTY),]
## a dataframe at the county level
write.csv(final2, "combined counties and zipcodes.csv")