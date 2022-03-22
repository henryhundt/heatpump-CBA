library(dplyr)
library(stringr)

setwd("~/Documents/GitHub/heatpump-CBA/creating geographic data set")

df <- read.csv("climate_zone_price_regions_and_all_utilities_0.csv")

#df <- filter(df, Area.in.Square.Miles > 1)

#df <- select(df, OBJECTID, Area.in.Square.Miles, Zone, price.region, FID_WI_MUNI_UTILITY_BOUNDARIES_, FID_WI_IOU_UTILITY_BOUNDARIES_W, FID_COOP_UTILITY_BOUNDARIES_COO,  FID_WI_NATURAL_GAS_UTILITY_BOUN, PSC_ID, PSC_ID.1,  PSC_ID.2, Util_ID)
