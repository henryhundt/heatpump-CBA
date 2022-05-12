library(dplyr)

setwd("~/Documents/GitHub/heatpump-CBA")

df <- read.csv("final results.csv")

locations <- read.csv("result locations.csv")

df <- select(df, contains("load"), contains("cool"))

df <- cbind(locations, df)

census <- read.csv("county fuel type data.csv")
census$FIPS <- as.integer(census$FIPS)
census <- census[,c(1,56:ncol(census))]

df <- left_join(df, census, by = "FIPS")

df$perc_households_NG <- as.integer(df$Total..Utility.Gas)/as.integer(df$Total)
df$perc_households_P <- as.integer(df$Total..Bottled.Tank.or.Lp.Gas)/as.integer(df$Total)
df$perc_households_HO <- as.integer(df$Total..Fuel.Oil.Kerosene.Etc.)/as.integer(df$Total)
df$perc_households_ElecRes <- as.integer(df$Total..Electricity)/as.integer(df$Total)
df$perc_households_wood <- as.integer(df$Total..Wood)/as.integer(df$Total)

df$perc_positiveNB_cool <- df$perc_households_HO*df$perc_positive_NB_HO_cool + df$perc_households_NG*df$perc_positive_NB_NG_cool + 
  df$perc_households_P*df$perc_positive_NB_P_cool + df$perc_households_ElecRes*df$perc_positive_NB_ElecRes_cool + 
  df$perc_households_wood*df$perc_positive_NB_wood_cool

df$perc_positivePrivate_cool <- df$perc_households_HO*df$perc_positive_private_HO_cool + df$perc_households_NG*df$perc_positive_private_NG_cool + 
  df$perc_households_P*df$perc_positive_private_P_cool + df$perc_households_ElecRes*df$perc_positive_private_ElecRes_cool + 
  df$perc_households_wood*df$perc_positive_private_wood_cool

df$perc_positiveNB_nocool <- df$perc_households_HO*df$perc_positive_NB_HO_nocool + df$perc_households_NG*df$perc_positive_NB_NG_nocool + 
  df$perc_households_P*df$perc_positive_NB_P_nocool + df$perc_households_ElecRes*df$perc_positive_NB_ElecRes_nocool + 
  df$perc_households_wood*df$perc_positive_NB_wood_nocool

df$perc_positivePrivate_nocool <- df$perc_households_HO*df$perc_positive_private_HO_nocool + df$perc_households_NG*df$perc_positive_private_NG_nocool + 
  df$perc_households_P*df$perc_positive_private_P_nocool + df$perc_households_ElecRes*df$perc_positive_private_ElecRes_nocool + 
  df$perc_households_wood*df$perc_positive_private_wood_nocool

df <- select(df, contains("perc_"), contains("mean_"), NAME, FIPS, x, y)

write.csv(df, "final results for map.csv", row.names = F)



