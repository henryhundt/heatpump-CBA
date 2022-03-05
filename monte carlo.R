##### the code is structured as follows ####
# 1. read in key libraries
# 2. create a set of functions that provide important calculations in the model
# 3. set constant variables and scenario variables
# 4. start the monte carlo simulation and add other key variables that vary by simulation
# 5. start the year-loop from year 1 and update any variables that change on an annual basis
# 6. convert variables to equivalent units
# 7. calculate final annual costs for each of the single-fuel and dual-fuel systems
# 8. use the annual costs to find the net-present cost of each system
# 9. present some summaries of the net-present costs and the net-present value
#    of dual-fuel systems over just relying on their backup systems     
############################################

#set working directory
rm(list = ls())
setwd("~/Documents/GitHub/heatpump-CBA")

# read in necessary libraries
### if not downloaded on your computer, first run the install.packages 
### commands that have been commented out
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("purrr")
#install.packages("readxl")
#install.packages("lubridate")
library(stringr)
library(dplyr)
library(purrr)
library(readxl)
library(lubridate)

################### set functions ########################################
############### functions to be used for calculations #####################
## calculate proportion of a year's heating that comes in each temperature bin
heatingbin_adjust <- function(indoor_design_temperature, outdoor_design_temperature,
                              temperature_bin_hours){
  ## reduce the temperature bin hours dataset to just the temperature bins that 
  ## we will consider for our calculations
  temperature_bin_hours <- filter(temperature_bin_hours, HLY.TEMP.NORMAL < indoor_design_temperature)
  
  ## calculates the distance between outdoor design temp and when design 
  ## heating load reaches 0 (aka indoor design temperature), also could be thought 
  ##of as the distance of the x-axis in a design heating load graph
  temperature_distance <- indoor_design_temperature - outdoor_design_temperature
  
  ## We now assume that the design heating load (measured in btu/hour) falls
  ## linearly with temperature until it reaches 0 at the indoor temperature. 
  ## Knowing the hours in the year in different temperature bins and the final
  ## average annual heating load of a house in WI, we can back out a calculation
  ## of the proportion of the annual heating load that comes in each temperature
  ## bin
  temperature_bin_hours$load_proportion <- 0
  
  ## we model the design heating load as a linear model y = mx + b, where y is the
  ## design heating load, b is the design heating load in btu/hr at the design 
  ## heating temperature and y = 0 @ the indoor design temperature.
  ## We can find the design heating load at each temperature given in terms of
  ## the design heating load variable "b" and then the sum of all of those values should
  ## equal the annual heating load of the house. 
  
  # we proceed by solving the equation: 
  #"annual_heating_load_in_a_temp_bin = sum of design_heating_load_in_each_temp_hour
  # where design_heating_load_in_each_temp_hour = y = mx+b
  
  ## One key to this equation is that at the indoor design temp, the design heating load = 0, 
  ## therefore, 0 = m(temperature_distance)+b and we find that m = -b/temperature_distance.
  ## plugging this back into the design_heating_load_in_each_temp_hour equation
  ## and we get y = -bx/temperature_distance + b. 
  ## Therefore, the result that we find in the "load_proportion" variable is simply the constant on b
  ## in the above equation, i.e. (temperature_distance - x)/temperature_distance

  temperature_bin_hours$distance <- temperature_bin_hours$HLY.TEMP.NORMAL - outdoor_design_temperature
  temperature_bin_hours$load_proportion <- (temperature_distance-temperature_bin_hours$distance)/temperature_distance
  
  return(temperature_bin_hours)
}

## calculate the proportion of heating that is covered by the backup system
## given a switchover temperature
backupheatingload <- function(heating_bin_hours, switchover_temp){
  backup <- filter(heating_bin_hours, HLY.TEMP.NORMAL < switchover_temp)
  total_prop <- sum(heating_bin_hours$load_proportion)
  backup_prop <- sum(backup$load_proportion)
  return(heating_load*backup_prop/total_prop)
}

# ## this function's purpose is just to create a "temperature_bin_hours" data frame
# ## that can be passed to the annual_COP function below to calculate average 
# ## cooling COP. 
# cooling_load_proportion <- function(indoor_design_temperature, 
#                                     temperature_bin_hours){
#   
#   #remove temperature bins that don't require cooling
#   low <- which(str_detect(temperature_bin_hours$Temperature.Range, 
#                           paste0(indoor_design_temperature, " to ")))
#   temperature_bin_hours <- temperature_bin_hours[c(1:low),]
#   
#   # calculate distance between highest temperature and indoor design temp
#   temperature_distance = low*5
#   
#   # now calculate the COP weighted by amount of cooling that needs to be
#   # provided in each temperature bin hour
#   temperature_bin_hours$load_proportion <- 0
#   temperature_bin_hours[1, "load_proportion"] <- 
#     temperature_bin_hours[1, "Weighted.Average"]
#   # from above, the "5" comes from the size of the temperature bin
#   # the two comes from the fact that it's an average of two numbers
#   proportion = 1
#   denominator <- temperature_distance/5*2
#   for(i in (2):low){
#     temperature_bin_hours[i, "load_proportion"] <- 
#       temperature_bin_hours[i, "Weighted.Average"] * (1-proportion/denominator)
#     proportion <- proportion + 2
#   }
#   
#   # we flip the order of the cooling bin hours dataframe so that it fits
#   # the code of the COP function better
#   temperature_bin_hours <- map_df(temperature_bin_hours, rev)
#   
#   return(temperature_bin_hours)
# }

# calculate the average COP of the ASHP for cooling and for heating 
# for heating, it uses COP values at 5 and 65 degrees Fahrenheit
# for cooling, it uses COP values at 
annual_COP <- function(COP_low, COP_high, temperature_bin_hours, 
                       switchover_temp, heating_yes){
  ## for heating, remove temperature bin data covered by backup heating
  if(heating_yes){
    if(switchover_temp >= 65) return(1)
    temperature_bin_hours <- filter(temperature_bin_hours, HLY.TEMP.NORMAL > switchover_temp)
  }
  
  #calculate the slope of the COP at which COP changes with temperature
  ## we assume that COP changes linearly (while fundamentally untrue, we believe 
  ## this is a reasonable assumption at the temperatures we're looking at)
  ###42 is the distance between 47 and 5, the two temperatures at which we get heating COPs
  ###13 is the distance between 95 and 82, the temperatures at which we get cooling COPs
  if(heating_yes) {COP_slope <- abs(COP_high-COP_low)/42
  }else{COP_slope <- abs(COP_high - COP_low)/13}
  # create some variables to calculate COP for the different temperature bins
  temperature_bin_hours$COP <- 0
  ## set COP_high to be the COP at 65 degrees
  if(heating_yes){
    COP_high <- COP_high + COP_slope*(65-47)
    temperature_bin_hours$COP <- COP_high - COP_slope*(65-temperature_bin_hours$HLY.TEMP.NORMAL)
  } else{
    COP_high <- COP_high + COP_slope*(82-65)
    TKTK
  }
  
  # working off of the strategy used to calculate backup heating load, we now
  # come up with weights for the different COP values. For example, if the COP
  # at 35 degrees is 3.4, that should be weighted by the amount of heating
  # the ASHP needs to provide during that temperature bin. We've already calculated
  # this value in the backup heating function and it is the constant that is multiplied
  # with b to find the design heating temp y.
  total_load <- sum(temperature_bin_hours$load_proportion)
  temperature_bin_hours$COP.Weight <- temperature_bin_hours$load_proportion/total_load
  return(sum(temperature_bin_hours$COP.Weight*temperature_bin_hours$COP))
}

methane_leak <- function(input, methane_leakage_rate, energy_rate, density){
  # convert the fuel/electricity input from mmBTUs to volume
  ## for electricity it's volume of fuel needed to create the electricity
  input_volume <- input/energy_rate
  # convert from volume to kilograms
  input_weight <- input_volume*density
  # use the leakage rate, which is as a proportion of produced fuel, 
  # to implied kg of methane
  methane_leaked <- input_weight*methane_leakage_rate
  # return the CO2e of the leaked methane
  return(methane_leaked*GWP_CH4)
}

############### end of functions ##########################################

#################### set key variables #####################
##### constants #####
#100-year global warming potential
#"Gases are converted to CO2e by multiplying by their global warming potential (GWP)"
GWP_CO2 <- 1 
GWP_CH4 <- 28
GWP_N2O <- 265

#### monte carlo variables
set.seed(1111) 
n_trials <- 1000
years_of_analysis <- 15 ### 2021-2035

#### set scenario variables ####
### comment out the ones that you don't want to use
##discount rate 
#discount_rate <- 0.07
discount_rate <- 0.02
#discount_rate <- 0.0

## determine whether just the heat pumps heating should be considered or
## also it's cooling. If cooling is included, then the baseline scenario will 
## include the cost of installing a new AC and the fuel cost of covering
## the cooling load. 
#cooling <- T # yes cooling
cooling <- F # no cooling

## determine carbon intensity of the grid
#### does so by specifying the year when the grid will have 0 emissions
#### the "aggressive decarbonization" option is meant to simulate a grid that becomes
#### 80% cheaper by 2035 and assumes it does so linearly and doesn't actually reflect
#### a belief that the grid will reach 0 carbon emissions by 2039. This comes from 
#### the fact that some utilities are planning to remove all coal by 2035. The 
#### grid may move much more slowly to 0 after those first 16 years and not actually 
#### reach 0 by 2039.
decarb_year <- 2050 #BAU
#decarb_year <- 2039 #aggressive decarbonization

## two testing scenarios, for now:
county <- "Dane"
zip <- 53703
elec_utility <- "3270"
gas_utility <- "3270"
# county <- "Oneida"
# zip <- 54501
# elec_utility <- "6690"
# gas_utility <- "6690"

##### read in necessary files ######################################
#### read in and set up price projections for later
### values from EIA's projected costs of fuel/electricity over time. 
projections_base <- read.csv("Energy_Prices_Residential_projections.csv", skip = 4)
#changes order so first row is year 1 (i.e. 2021)
projections_base <- arrange(projections_base, Year)
## convert to growth rates instead of absolute numbers
## keep the projections_base data since we will use it for areas for which we don't
## have spatial data 
base <- projections_base[1,]
fuel_cost_growth_rates_base <- as.data.frame(t(apply(projections_base, 1, function(rowval) unlist(rowval / base))))

#### read in list of ASHP COPs
ASHP_COPs <- read.csv("ASHP random sample.csv")
ASHP_COPs <- select(ASHP_COPs, starts_with("cop"))

## climate zones
climate_zone <- read.csv("./Temperature Data/HDD zones by county.csv")
climate_zone <- filter(climate_zone, County == county)
climate_zone <- climate_zone$Zone

## Annual temperatures
## source: https://www.ncei.noaa.gov/access/search/data-search/normals-hourly-2006-2020
temperature_bin_hours <- read.csv("./Temperature Data/2020 15 year temp normals in WI by 11 HDD zones.csv")
temperature_bin_hours <- filter(temperature_bin_hours, zone == climate_zone)
temperature_bin_hours <- select(temperature_bin_hours, HLY.TEMP.NORMAL)
temperature_bin_hours <- arrange(temperature_bin_hours, HLY.TEMP.NORMAL)

## indoor design temperature
#source: https://focusonenergy.com/sites/default/files/bpdeemedsavingsmanuav10_evaluationreport.pdf
indoor_design_temperature <- 65 #degrees Fahrenheit 

## outdoor design temperature csv
#source: https://casetext.com/regulation/wisconsin-administrative-code/agency-department-of-safety-and-professional-services/safety-buildings-and-environment/commercial-building-code/chapter-sps-363-energy-conservation/subchapter-ii-changes-additions-or-omissions-to-the-international-energy-conservation-code-iecc/section-sps-3630302-exterior-design-conditions
outdoor_design_temperature <- read.csv("./Temperature Data/Outdoor design temps by county.csv")
outdoor_design_temperature <- filter(outdoor_design_temperature, County == county)
outdoor_design_temperature <- outdoor_design_temperature$Outdoor.Design.Temp

###efficiency variables - note that efficiency is a unitless value
# this can be thought of as the average efficiency of whichever furnace across
# users in Wisconsin
naturalgas_furnace_efficiency <- .95 
heatingoil_furnace_efficiency <- .9
propane_furnace_efficiency <- .9

### year 1 fuel prices
weights <- read.csv("./Temperature Data/HDD Proportions by Month by Climate Zone Using 2020 15 Year Normals.csv")
weights <- filter(weights, zone == climate_zone)

## electricity
elec_cost <- read.csv("./Fuel cost data/final electricity data.csv")
elec_cost <- filter(elec_cost, Utility.ID == elec_utility)
elec_cost$Bill.Date <- mdy(elec_cost$Bill.Date)
elec_cost$month <- month(elec_cost$Bill.Date)
elec_cost <- left_join(elec_cost, weights, by = "month")
electricity_heating_cost <- sum(elec_cost$Total.Charge..per.kWh.*elec_cost$proportion)

### natural gas
natgas_cost <- read.csv("nat. gas cost, monthly averages.csv")
natgas_cost <- filter(natgas_cost, Utility.Code == gas_utility)
colnames(natgas_cost)[1] <- "month"
natgas_cost <- left_join(natgas_cost, weights, by = "month")
ng_heating_cost <- sum(natgas_cost$price*natgas_cost$proportion)

### installation sizing and efficiency 
### TKTKTK
size <- 2
efficiency <- 16

### keeps track of monte carlo results
track_trials <- data.frame(n = c(1:n_trials), NG = 0, HO = 0, P = 0, ASHP_NG = 0,
                           ASHP_HO = 0, ASHP_P = 0)
# track_years_total <- na.omit(data.frame(n = NA, NG = NA, HO = NA, P = NA, ASHP_NG = NA,
#                                         ASHP_HO = NA, ASHP_P = NA, trial = NA))
# track_emissions_and_private <- data.frame(n = c(1:n_trials), NG_install = NA,
#                                           NG_emissions = NA, NG_private = NA,
#                                           HO_install = NA, HO_emissions = NA,
#                                           HO_private = NA, P_install = NA,
#                                           P_emissions = NA, P_private = NA,
#                                           ASHP_NG_install = NA, ASHP_NG_emissions = NA,
#                                           ASHP_NG_private = NA,ASHP_HO_install = NA,
#                                           ASHP_HO_emissions = NA, ASHP_HO_private = NA,
#                                           ASHP_P_install = NA, ASHP_P_emissions = NA,
#                                           ASHP_NG_private = NA)
for(i in 1:n_trials){
  #base cost variables 
  ## randomly choose what energy projections to run based on one of 10 different
  ## scenarios that EIA provides, which determines the price of all fuels/
  ## electricity over time for a given trial. (all prices are in 2020 $s/MMBTU)
  scenario <- floor(runif(1, min = 1, max = 11))
  columns <- c(scenario + 1, scenario + 11, scenario + 21, scenario + 31)
  projections <- projections_base[,columns]
  fuel_cost_growth_rates <- fuel_cost_growth_rates_base[,columns]
  
  ## heating variables
  #### calculate the proportion of heating load that the backup heating system 
  #### covers based on the switchover temperature
  ## Switch over temperature (above the switchover, the ASHP heats, below, the backup heats)
  ### Switch over temperature is determined by the relative prices of electricity and each
  ### respective fuel at year 1. 
  ### we start with a COP value that is then translated into a temperature later with the use
  ### of the COP function
  ASHP_NG_switchover_COP <- electricity_heating_cost*
    (naturalgas_furnace_efficiency/
       ng_heating_cost)
  ASHP_HO_switchover_COP <- electricity_heating_cost*
    (heatingoil_furnace_efficiency/
       projections[1,which(str_detect(colnames(projections), "Distillate.Fuel.Oil"))])
  ASHP_P_switchover_COP <- electricity_heating_cost*
    (propane_furnace_efficiency/
       projections[1,which(str_detect(colnames(projections), "Propane"))])
  
  #### generate heating COP values to translate switchover COP to a temperature
  ### COP values were found by taking a random sample of 10 ASHPs from the NEEP
  ### ASHP list with an SEER of 16 and a max heating capacity at 5 degrees of 
  ### between 20000 and 40000 btu/hr. The below numbers are one standard deviation
  ### above and below the mean value found at both temperatures.
  ASHP_num <- floor(runif(1, min = 1, max = (nrow(ASHP_COPs) + 1)))
  heat_COP_low <- ASHP_COPs[ASHP_num,"cop.at.5"]
  heat_COP_high <- ASHP_COPs[ASHP_num,"cop.at.47"]
  ### translate the COP to a temperature value: switchover_COP = COP_low + COP_slope*x, find x
  #the 42 comes from the difference between 47 degrees and 5 degrees, the two
  #temperatures at which we get COP values
  COP_slope <- (heat_COP_high - heat_COP_low)/42
  #the five comes from the fact that heat_COP_low is measured at 5 degrees
  ASHP_NG_switchover <- ((ASHP_NG_switchover_COP - heat_COP_low)/COP_slope) + 5
  ASHP_HO_switchover <- ((ASHP_HO_switchover_COP - heat_COP_low)/COP_slope) + 5
  ASHP_P_switchover <- ((ASHP_P_switchover_COP - heat_COP_low)/COP_slope) + 5
  
  ## heating load uses TRM's current estimate
  heating_load <- 64.3 #total heating load of houses in WI in mmBTU/year 
  
  ## proportion of heating load covered by backup - different for each fuel 
  ## because of different switchovers first adjust the temperature_heating_bin 
  ## data to account for heating load at each temperature bin
  heating_bin_hours <- heatingbin_adjust(indoor_design_temperature, 
                                         outdoor_design_temperature, temperature_bin_hours)
  #next determine the proportion of heating provided by backup for each fuel
  ASHP_NG_backup_heating <- backupheatingload(heating_bin_hours, ASHP_NG_switchover)
  ASHP_HO_backup_heating <- backupheatingload(heating_bin_hours, ASHP_HO_switchover)
  ASHP_P_backup_heating <- backupheatingload(heating_bin_hours, ASHP_P_switchover)
  
  ## and by ASHP for each fuel
  ASHP_NG_heating <- heating_load - ASHP_NG_backup_heating
  ASHP_HO_heating <- heating_load - ASHP_HO_backup_heating
  ASHP_P_heating <- heating_load - ASHP_P_backup_heating
  
  #installment cost
  ### all values are the mean value of Homewyse estimates of the cost of installing 
  ### each respective piece of technology in the 12 most populous Wisconsin counties.
  ### the average costs were compared to an average in smaller counties and in all 
  ### instances they were found to be similar for the ASHPs and the ACs, the Homewyse 
  ### "fair cost guide" was used, and the 2 ton 16 SEER option was selected for both, 
  ### with other options left to their default values.
  ### The costs are split between nonlabor costs (materials) and labor costs since 
  ### we assume that labor costs will covary.
  ### Note that, following the lead of the RMI's "The Economics of Electrifying
  ### Buildings" report (https://rmi.org/insight/the-economics-of-electrifying-buildings/)
  ### we set the costs of the natural gas furnaces and propane furnaces to be
  ### equal to each other.
  ### Also, ASHPs and ACs are sized at 2 tons (24K Btus/hour) and the efficiency
  ### is assumed to be an SEER of 16, while the furnaces which are required to 
  ### meet load at much lower temperatures, are sized at 70K Btus/hour and are 
  ### assumed to be efficient (homewyse essentially allows for an efficient or
  ### inefficient option for the furnaces).
  ### sources are as follows:
  # ASHP: https://www.homewyse.com/costs/cost_of_heat_pump_systems.html
  # NG: https://www.homewyse.com/costs/cost_of_energy_efficient_gas_furnaces.html
  # HO: https://www.homewyse.com/costs/cost_of_oil_furnaces.html
  # P: https://www.homewyse.com/costs/cost_of_energy_efficient_gas_furnaces.html
  # AC: https://www.homewyse.com/costs/cost_of_central_air_conditioning_systems.html
  
  ASHP_installment_file <- read.csv(paste0("./Installation costs/cost to install heat pump - ",size," ton ", efficiency, " SEER.csv"))
  ASHP_installment_file <- filter(ASHP_installment_file, zipcode == zip)
  AC_installment_file <- read.csv(paste0("./Installation costs/cost to install AC - ",size," ton ", efficiency, " SEER.csv"))
  AC_installment_file <- filter(AC_installment_file, zipcode == zip)
  NG_P_installment_file <- read.csv(paste0("./Installation costs/cost to install NG furnace - 70K BTU 92%+ efficiency.csv"))
  NG_P_installment_file <- filter(NG_P_installment_file, zipcode == zip)
  HO_installment_file <- read.csv(paste0("./Installation costs/cost to install oil furnace - 70K BTU 85%+ efficiency.csv"))
  HO_installment_file <- filter(HO_installment_file, zipcode == zip)
  
  # nonlabor cost of installing an ASHP in dollars per unit 
  ASHP_nonlabor_installment_cost <- runif(1, min = ASHP_installment_file$systemcost_low, max = ASHP_installment_file$systemcost_high) 
  # cost of installing a natural gas furnace in dollars per unit 
  naturalgas_furnace_nonlabor_installment_cost <- runif(1, min = NG_P_installment_file$systemcost_low, max = NG_P_installment_file$systemcost_high) 
  # cost of installing a heating oil furnace in dollars per unit 
  heatingoil_furnace_nonlabor_installment_cost <- runif(1, min = HO_installment_file$systemcost_low, max = HO_installment_file$systemcost_high) 
  # cost of installing a propane furnace in dollars per unit 
  propane_furnace_nonlabor_installment_cost <- naturalgas_furnace_nonlabor_installment_cost
  # cost of installing air conditioning in dollars per unit
  AC_nonlabor_installment_cost <- 0
  if(cooling){AC_nonlabor_installment_cost <- runif(1, min = AC_installment_file$systemcost_low, max = AC_installment_file$systemcost_high)} 
  ## labor costs - we assume these to covary perfectly
  labor_costs <- runif(1,0,1)
  # take the low labor cost and add the difference between high and low times our
  # random value for how expensive labor is in the area that the heating tech is
  # being installed in (labor_costs, above)
  ASHP_labor_installment_cost <- ASHP_installment_file$laborcost_low + labor_costs*(ASHP_installment_file$laborcost_high - ASHP_installment_file$laborcost_low) #low = 1289, high = 1860
  naturalgas_furnace_labor_installment_cost <- NG_P_installment_file$laborcost_low + labor_costs*(NG_P_installment_file$laborcost_high - NG_P_installment_file$laborcost_low) #low = 923, high = 1341
  heatingoil_furnace_labor_installment_cost <- HO_installment_file$laborcost_low + labor_costs*(HO_installment_file$laborcost_high - HO_installment_file$laborcost_low) #low = 636, high = 810
  propane_furnace_labor_installment_cost <- naturalgas_furnace_labor_installment_cost #low = 923, high = 1341 
  AC_labor_installment_cost <- 0
  #if(cooling){AC_labor_installment_cost <- 2364 + labor_costs*867} #low = 2364, high = 3232
  ## after receiving feedback from PSC that questioned the Homewyse values, we chose
  ## to allow AC installment labor costs to equal ASHP installment costs.
  if(cooling){AC_labor_installment_cost <- ASHP_labor_installment_cost} #low = 1289, high = 1860
  ### combine costs
  ASHP_installment_cost <- ASHP_nonlabor_installment_cost + ASHP_labor_installment_cost
  naturalgas_furnace_installment_cost <- naturalgas_furnace_nonlabor_installment_cost + 
    naturalgas_furnace_labor_installment_cost
  heatingoil_furnace_installment_cost <- heatingoil_furnace_nonlabor_installment_cost + 
    heatingoil_furnace_labor_installment_cost
  propane_furnace_installment_cost <- propane_furnace_nonlabor_installment_cost + 
    propane_furnace_labor_installment_cost
  AC_installment_cost <- AC_nonlabor_installment_cost + AC_labor_installment_cost
  
  # First simulate the efficiency of the "average" air source heat pump in the
  # scenario.  
  # calculate the COP by weighting the COP within each temperature bin by
  # how often the ASHP has to run while in that bin and how often that bin
  # occurs
  ASHP_NG_heating_ASHP_COP <- annual_COP(heat_COP_low, heat_COP_high, 
                                    heating_bin_hours, ASHP_NG_switchover, TRUE)
  ASHP_HO_heating_ASHP_COP <- annual_COP(heat_COP_low, heat_COP_high, 
                                    heating_bin_hours, ASHP_HO_switchover, TRUE)
  ASHP_P_heating_ASHP_COP <- annual_COP(heat_COP_low, heat_COP_high, 
                                    heating_bin_hours, ASHP_P_switchover, TRUE)
  
  #### calculate input for all heating types + cooling 
  ## cooling load is in kWh and is an average for the state
  cooling_input <- 1011
  # the rest are all in mmBTUs 
  ASHP_NG_input <- ASHP_NG_heating/ASHP_NG_heating_ASHP_COP
  ASHP_HO_input <- ASHP_HO_heating/ASHP_HO_heating_ASHP_COP
  ASHP_P_input <- ASHP_P_heating/ASHP_P_heating_ASHP_COP
  backup_NG_input <- ASHP_NG_backup_heating/naturalgas_furnace_efficiency
  backup_HO_input <- ASHP_HO_backup_heating/heatingoil_furnace_efficiency
  backup_P_input <- ASHP_P_backup_heating/propane_furnace_efficiency
  full_NG_input <- heating_load/naturalgas_furnace_efficiency
  full_HO_input <- heating_load/heatingoil_furnace_efficiency
  full_P_input <- heating_load/propane_furnace_efficiency
  
  ###other variables
  social_cost_of_CO2 <- runif(1, min = 14, max = 51)/1000 #dollars per kg of CO2
  ###emissions variables (fuel data from https://www.epa.gov/sites/default/files/2021-04/documents/emission-factors_apr2021.pdf)
  ### WI grid specific electricity grid data is from https://www.epa.gov/sites/default/files/2021-02/documents/egrid2019_summary_tables.pdf
  #CO2
  ## CO2 associated with electricity being used for heating in pounds of CO2/MWh
  electricity_CO2 <- 1225.4 
  ## CO2 from burning natural gas in a furnace in kg of CO2 per mmBTU
  naturalgas_CO2 <- 53.06 
  ## CO2 from burning heating oil (Distillate Fuel Oil No. 2) in a furnace in kg of CO2 per mmBTU
  heatingoil_CO2 <- 73.96 
  ## CO2 from burning propane in a furnace in kg of CO2 per mmBTU
  propane_CO2 <- 62.87 
  
  #CH4
  # CH4 associated with electricity being used for heating in lb of CH4/MWh
  electricity_CH4 <- 0.113
  # CH4 from burning natural gas in a furnace in kg of CH4 per mmBTU
  naturalgas_CH4 <- 1/1000 
  # CH4 from burning heating oil (Distillate Fuel Oil No. 2) in a furnace in kg of CH4 per mmBTU
  heatingoil_CH4 <- 3/1000 
  # CH4 from burning propane in a furnace in kg of CH4 per mmBTU
  propane_CH4 <- 3/1000 
  
  #N2O
  # N2O associated with electricity being used for heating in pounds of N2O/MWh
  electricity_N2O <- 0.016 
  # N2O from burning natural gas in a furnace in kg of N2O per mmBTU
  naturalgas_N2O <- 0.1/1000 
  # N2O from burning heating oil (Distillate Fuel Oil No. 2) in a furnace in kg of N2O per mmBTU
  heatingoil_N2O <- 0.6/1000 
  # N2O from burning propane in a furnace in kg of N2O per mmBTU
  propane_N2O <- 0.6/1000 
  
  ### decarbonization rate of the grid
  #2019 is the year from which our grid emissions values come from
  decarb <- 1/(decarb_year - 2019) 
  
  ##### we assume that methane leakage per production rates will remain constant
  ##### over time. We also assume that all fuel burned whether for heating or
  ##### electricity production is from the U.S. and not imported. 
  ## account for methane leakage in the natural gas and oil production cycle
  # estimated amount of produced methane that leaks during the natural gas production cycle
  # source: https://www-science-org.ezproxy.library.wisc.edu/doi/10.1126/science.aar7204
  # the paper provides 95% confidence interval of 2 to 2.7 with a mean of 2.3. They don't
  # give any information on the distribution used and it is clearly not symmetric, but
  # our best estimation is a normal distribution. The code below essentially splits the
  # difference and finds the standard deviation of a normal distribution with a mean of
  # 2.3 and a low of 1.95 and high of 2.65. 
  sd <- (abs((2 - 2.3)/1.96) + abs((2.7 - 2.3)/1.96))/2
  NG_and_Petroleum_methane_leakage_rate <- rnorm(1, 2.3, sd)/100
  
  ## energy to volume
  mmBTU_per_cubic_ft_NG <- 0.001026 #source: https://www.epa.gov/sites/default/files/2021-04/documents/emission-factors_apr2021.pdf
  mmBTU_per_gallon_HO <- 0.138 #source: https://www.epa.gov/sites/default/files/2021-04/documents/emission-factors_apr2021.pdf
  mmBTU_per_gallon_P <- 0.091 #source: https://www.epa.gov/sites/default/files/2021-04/documents/emission-factors_apr2021.pdf
  ## volume to weight - if not specified, source: https://cngcenter.com/wp-content/uploads/2013/09/UnitsAndConversions.pdf
  NG_density <- 0.717 #kg/m^3
  m3_to_ft3 <- 35.3 #ft^3/m^3 
  cubic_ft_NG_to_kg <- NG_density/m3_to_ft3 #kg/ft^3
  HO_density <- 0.87 #kg/L source: https://henrycounty.in.gov/DocumentCenter/View/318/Fuel-Oil-Number-2-PDF
  liter_to_gallon <- 0.26 #gal/L
  gallon_HO_to_kg <- HO_density/liter_to_gallon
  P_density <- 0.495 #kg/L @ 25 degrees Celcius, source: https://www.engineeringtoolbox.com/specific-gravity-liquids-d_336.html
  gallon_P_to_kg <- P_density/liter_to_gallon #kg/gallon, source: https://henrycounty.in.gov/DocumentCenter/View/318/Fuel-Oil-Number-2-PDF
  
  full_NG_methane_leak_CO2e <- methane_leak(full_NG_input, NG_and_Petroleum_methane_leakage_rate, 
                                            mmBTU_per_cubic_ft_NG, cubic_ft_NG_to_kg)
  full_HO_methane_leak_CO2e <- methane_leak(full_HO_input, NG_and_Petroleum_methane_leakage_rate,
                                            mmBTU_per_gallon_HO, gallon_HO_to_kg)
  full_P_methane_leak_CO2e <- methane_leak(full_P_input, NG_and_Petroleum_methane_leakage_rate, 
                                           mmBTU_per_gallon_P, gallon_P_to_kg)
  backup_NG_methane_leak_CO2e <- methane_leak(backup_NG_input, NG_and_Petroleum_methane_leakage_rate, 
                                              mmBTU_per_cubic_ft_NG, cubic_ft_NG_to_kg)
  backup_HO_methane_leak_CO2e <- methane_leak(backup_HO_input, NG_and_Petroleum_methane_leakage_rate, 
                                              mmBTU_per_gallon_HO, gallon_HO_to_kg)
  backup_P_methane_leak_CO2e <- methane_leak(backup_P_input, NG_and_Petroleum_methane_leakage_rate, 
                                             mmBTU_per_gallon_P, gallon_P_to_kg)
  
  ### account for life-cycle emissions with regard to electricity production
  # source: https://www.epa.gov/sites/default/files/2021-02/documents/egrid2019_summary_tables.pdf
  # static (we assume these don't change over the course of the simulation)
  proportion_electricity_from_oil <- 0.002
  proportion_electricity_from_NG <- 0.328
  proportion_electricity_from_nuclear <- 0.162
  proportion_electricity_from_hydro <- 0.043
  proportion_electricity_from_biomass <- 0.022
  # we assume that all improvements to the grid during the simulation comes 
  # from the removal of coal from the grid and that it will be replaced with an
  # equal proportion of solar and wind
  proportion_electricity_from_coal <- 0.413
  proportion_electricity_from_wind <- 0.03
  proportion_electricity_from_solar <- 0.001
  
  # the above figures are from 2019, so we update them to 2020 numbers so that
  # they can be used in the iterative year for-loop below.
  # The 1.25 (1/.8) reflects the fact that coal is removed from the grid at a rate 
  # faster than the grid reaches 0 emissions (e.g. in our aggressive case, all coal 
  # is removed by 2035, but the grid still has 20% of it's original emissions)
  drop_in_coal <- proportion_electricity_from_coal*(decarb*1.25)
  proportion_electricity_from_coal <- proportion_electricity_from_coal - drop_in_coal
  proportion_electricity_from_wind <- proportion_electricity_from_wind + drop_in_coal/2
  proportion_electricity_from_solar <- proportion_electricity_from_solar + drop_in_coal/2
  
  # lifecycle emission rates - not including combustion emissions, where applicable,
  # since those are accounted for elsewhere
  # source: https://www.nrel.gov/docs/fy21osti/80580.pdf
  # all numbers are in kg CO2e/kWh
  biomass_lifecycle <- 52/1000
  photovoltaic_lifecycle <- 43/1000
  hydro_lifecycle <- 21/1000
  wind_lifecycle <- 13/1000
  nuclear_lifecycle <- 13/1000
  NG_lifecycle <- (0.8+71+0.02)/1000
  #the literature lacks necessary details to come up with a non-combustion 
  #life-cycle figure for oil, but the literature often applies the same value to
  #oil production emissions as to NG production emissions, so we do the same here
  #with NG
  oil_lifecycle <- NG_lifecycle 
  coal_lifecycle <- (5 + 10 + 5)/1000 # we assume that <5 is equal to 5
    
  ## create dataset to track the 15 year simulation for this trial
  track_years <- data.frame(n = c(0:years_of_analysis), NG = 0, HO = 0, P = 0, 
                            ASHP_NG = 0, ASHP_HO = 0, ASHP_P = 0, NG_private = 0,
                            NG_emissions = 0, HO_private = 0, HO_emissions = 0,
                            P_private = 0, P_emissions = 0, ASHP_NG_private = 0,
                            ASHP_NG_emissions = 0, ASHP_HO_private = 0,
                            ASHP_HO_emissions = 0, ASHP_P_private = 0,
                            ASHP_P_emissions = 0)
  #### input year 0 costs (i.e. installation costs)
  track_years[track_years$n == 0,"NG"] <- naturalgas_furnace_installment_cost
  track_years[track_years$n == 0,"HO"] <- heatingoil_furnace_installment_cost
  track_years[track_years$n == 0,"P"] <- propane_furnace_installment_cost
  
  ### if an ASHP is being installed we add the cost of an ASHP
  ## otherwise we add the cost of air conditioning (but the cost = 0 if we are
  ## not considering cooling i.e. cooling == FALSE)
  track_years[track_years$n == 0,"ASHP_NG"] <- track_years[track_years$n == 0,"NG"] + 
    ASHP_installment_cost
  track_years[track_years$n == 0,"ASHP_HO"] <- track_years[track_years$n == 0,"HO"] + 
    ASHP_installment_cost
  track_years[track_years$n == 0,"ASHP_P"] <- track_years[track_years$n == 0,"P"] + 
    ASHP_installment_cost
  track_years[track_years$n == 0,"NG"] <- track_years[track_years$n == 0,"NG"] + 
    AC_installment_cost
  track_years[track_years$n == 0,"HO"] <- track_years[track_years$n == 0,"HO"] + 
    AC_installment_cost
  track_years[track_years$n == 0,"P"] <- track_years[track_years$n == 0,"P"] + 
    AC_installment_cost
  
  #### start simulation of annual costs ####
  for(j in 1:years_of_analysis){
    ###cost variables
    #fuel cost in 2020 $/mmBTU from EIA projections for year j
    electricity_price <- electricity_heating_cost*fuel_cost_growth_rates[j, which(str_detect(colnames(fuel_cost_growth_rates), "Electricity"))]
    naturalgas_price <- ng_heating_cost*fuel_cost_growth_rates[j, which(str_detect(colnames(fuel_cost_growth_rates), "Gas"))] 
    heatingoil_price <- projections[j, which(str_detect(colnames(projections), "Oil"))] 
    propane_price <- projections[j, which(str_detect(colnames(projections), "Propane"))] 
    
    ### emissions variables
    # accounts for the fact that the grid gets cleaner every year at a linear rate
    electricity_CO2_use <- electricity_CO2*(1-decarb*(j+1))
    electricity_CH4_use <- electricity_CH4*(1-decarb*(j+1))
    electricity_N2O_use <- electricity_N2O*(1-decarb*(j+1))
    
    ### lifecycle calculations
    proportion_electricity_from_coal <- proportion_electricity_from_coal - drop_in_coal
    proportion_electricity_from_wind <- proportion_electricity_from_wind + drop_in_coal/2
    proportion_electricity_from_solar <- proportion_electricity_from_solar + drop_in_coal/2
    
    ### lifecycle emissions for electricity (not including combustion below)
    # kg of CO2e per kWh of electricity
    electricity_lifecycle <- biomass_lifecycle*proportion_electricity_from_biomass + 
      photovoltaic_lifecycle*proportion_electricity_from_solar +
      hydro_lifecycle*proportion_electricity_from_hydro + 
      wind_lifecycle*proportion_electricity_from_wind +
      nuclear_lifecycle*proportion_electricity_from_nuclear +
      NG_lifecycle*proportion_electricity_from_NG +
      oil_lifecycle*proportion_electricity_from_oil +
      coal_lifecycle*proportion_electricity_from_coal
    # convert to kg of CO2e per mmBTU to add to the combustion value below
    mmBTU_per_kWh <- 3412/1000000 #source: https://www.eia.gov/energyexplained/units-and-calculators/
    electricity_lifecycle <- electricity_lifecycle*mmBTU_per_kWh
    
    ################# convert variables to standard units ###################
    ##carbon dioxide equivalents of emissions in kg per mmBTU
    #electricity 
    lb_to_kg <- .45359237 #source: https://www.metric-conversions.org/weight/pounds-to-kilograms.htm
    electricity_CO2e <- (electricity_CO2_use*GWP_CO2 + electricity_CH4_use*GWP_CH4 +
                          electricity_N2O_use*GWP_N2O)*lb_to_kg/(mmBTU_per_kWh*1000) +
                           electricity_lifecycle

    #nat gas
    ## multiply each emission by the emissions global warming potential and add to get CO2 equivalents
    naturalgas_CO2e <- naturalgas_CO2*GWP_CO2 + naturalgas_CH4*GWP_CH4 + naturalgas_N2O*GWP_N2O
    
    #heating oil
    heatingoil_CO2e <- heatingoil_CO2*GWP_CO2 + heatingoil_CH4*GWP_CH4 + heatingoil_N2O*GWP_N2O
    
    #propane
    propane_CO2e <- propane_CO2*GWP_CO2 + propane_CH4*GWP_CH4 + propane_N2O*GWP_N2O
    ############### end of conversions ########################################
    
    ################# Calculations section ###############
    ####Calculate heating costs for all heating types
    cooling_fuel_cost <- cooling_input*mmBTU_per_kWh*electricity_price
    ASHP_NG_fuel_cost <- ASHP_NG_input*electricity_price + cooling_fuel_cost
    ASHP_HO_fuel_cost <- ASHP_HO_input*electricity_price + cooling_fuel_cost
    ASHP_P_fuel_cost <- ASHP_P_input*electricity_price + cooling_fuel_cost
    backup_naturalgas_fuel_cost <-  backup_NG_input*naturalgas_price
    backup_heatingoil_fuel_cost <- backup_HO_input*heatingoil_price
    backup_propane_fuel_cost <- backup_P_input*propane_price
    full_naturalgas_fuel_cost <- full_NG_input*naturalgas_price
    full_heatingoil_fuel_cost <- full_HO_input*heatingoil_price
    full_propane_fuel_cost <- full_P_input*propane_price
    
    if(cooling){
      full_naturalgas_fuel_cost <- full_naturalgas_fuel_cost + cooling_fuel_cost
      full_heatingoil_fuel_cost <- full_heatingoil_fuel_cost + cooling_fuel_cost
      full_propane_fuel_cost <- full_propane_fuel_cost + cooling_fuel_cost
    }
    
    # all electric emissions account for line loss, the % of electricity lost
    # from production to end-source. Since producers account for line-loss in their
    # production output (e.g. if they expect a demand of 1 kWh they know to produce
    # 1 * (1 + lineloss) kWh of electricity) the end user who demanded that electricity
    # is responsible for the line loss as well.
    line_loss <- 0.05
    #### Calculate emissions for all heating types and monetize with social carbon costs
    cooling_emissions_cost <- (cooling_input*mmBTU_per_kWh*electricity_CO2e)*
      (1+line_loss)*social_cost_of_CO2
    ASHP_NG_emissions_cost <- (ASHP_NG_input*electricity_CO2e)*(1+line_loss)*
      social_cost_of_CO2 + cooling_emissions_cost
    ASHP_HO_emissions_cost <- (ASHP_HO_input*electricity_CO2e)*(1+line_loss)*
      social_cost_of_CO2 + cooling_emissions_cost
    ASHP_P_emissions_cost <- (ASHP_P_input*electricity_CO2e)*(1+line_loss)*
      social_cost_of_CO2 + cooling_emissions_cost
    backup_naturalgas_emissions_cost <- (backup_NG_input*naturalgas_CO2e + 
                                           backup_NG_methane_leak_CO2e)*social_cost_of_CO2
    backup_heatingoil_emissions_cost <- (backup_HO_input*heatingoil_CO2e + 
                                           backup_HO_methane_leak_CO2e)*social_cost_of_CO2
    backup_propane_emissions_cost <- (backup_P_input*propane_CO2e + 
                                        backup_P_methane_leak_CO2e)*social_cost_of_CO2
    full_natgas_emissions_cost <- (full_NG_input*naturalgas_CO2e + 
                                     full_NG_methane_leak_CO2e)*social_cost_of_CO2
    full_heatingoil_emissions_cost <- (full_HO_input*heatingoil_CO2e + 
                                         full_HO_methane_leak_CO2e)*social_cost_of_CO2
    full_propane_emissions_cost <- (full_P_input*propane_CO2e + 
                                      full_P_methane_leak_CO2e)*social_cost_of_CO2
    
    if(cooling){
      full_natgas_emissions_cost <- full_natgas_emissions_cost + 
        cooling_emissions_cost
      full_heatingoil_emissions_cost <- full_heatingoil_emissions_cost + 
        cooling_emissions_cost
      full_propane_emissions_cost <- full_propane_emissions_cost + 
        cooling_emissions_cost
    }
    
    ################### End of "calculation section" ################
    
    ######################## NPV ####################################
    ## calculate this years costs
    NG <- full_natgas_emissions_cost + full_naturalgas_fuel_cost
    HO <- full_heatingoil_emissions_cost + full_heatingoil_fuel_cost
    P <- full_propane_emissions_cost + full_propane_fuel_cost
    ASHP_NG <- ASHP_NG_fuel_cost + ASHP_NG_emissions_cost + 
      backup_naturalgas_emissions_cost + backup_naturalgas_fuel_cost
    ASHP_HO <- ASHP_HO_fuel_cost + ASHP_HO_emissions_cost + 
      backup_heatingoil_emissions_cost + backup_heatingoil_fuel_cost
    ASHP_P <- ASHP_P_fuel_cost + ASHP_P_emissions_cost + 
      backup_propane_emissions_cost + backup_propane_fuel_cost
    
    track_years[track_years$n == j, "NG_private"] <-
      full_naturalgas_fuel_cost/(1+discount_rate)^j
    track_years[track_years$n == j, "HO_private"] <-
      full_heatingoil_fuel_cost/(1+discount_rate)^j
    track_years[track_years$n == j, "P_private"] <-
      full_propane_fuel_cost/(1+discount_rate)^j
    track_years[track_years$n == j, "NG_emissions"] <-
      full_natgas_emissions_cost/(1+discount_rate)^j
    track_years[track_years$n == j, "HO_emissions"] <-
      full_heatingoil_emissions_cost/(1+discount_rate)^j
    track_years[track_years$n == j, "P_emissions"] <-
      full_propane_emissions_cost/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_NG_private"] <-
      (ASHP_NG_fuel_cost + backup_naturalgas_fuel_cost)/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_HO_private"] <-
      (ASHP_HO_fuel_cost + backup_heatingoil_fuel_cost)/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_P_private"] <-
      (ASHP_P_fuel_cost + backup_propane_fuel_cost)/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_NG_emissions"] <-
      (ASHP_NG_emissions_cost + backup_naturalgas_emissions_cost)/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_HO_emissions"] <-
      (ASHP_HO_emissions_cost + backup_heatingoil_emissions_cost)/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_P_emissions"] <-
      (ASHP_P_emissions_cost + backup_propane_emissions_cost)/(1+discount_rate)^j
    ## discount the annual costs to derive the NPV
    track_years[track_years$n == j, "NG"] <- NG/(1+discount_rate)^j
    track_years[track_years$n == j, "HO"] <- HO/(1+discount_rate)^j
    track_years[track_years$n == j, "P"] <- P/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_NG"] <- ASHP_NG/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_HO"] <- ASHP_HO/(1+discount_rate)^j
    track_years[track_years$n == j, "ASHP_P"] <- ASHP_P/(1+discount_rate)^j
    ######################### end of "NPV" section ####################
  }
  ## at the end of the 15 years, we then take the sum of the annual NPVs to find
  ## the total NPV
  track_trials[i, "NG"] <- sum(track_years$NG)
  track_trials[i, "HO"] <- sum(track_years$HO)
  track_trials[i, "P"] <- sum(track_years$P)
  track_trials[i, "ASHP_NG"] <- sum(track_years$ASHP_NG)
  track_trials[i, "ASHP_HO"] <- sum(track_years$ASHP_HO)
  track_trials[i, "ASHP_P"] <- sum(track_years$ASHP_P)
  # keep track of individual years to allow for an average annual net benfits 
  # calculation
  # track_years$trial <- i
  # track_years_total <- rbind(track_years_total, track_years)
  # keep track of installation, emissions, and private costs, separately to
  # create a graph later on for each ASHP scenario
  # track_emissions_and_private[i, "NG_install"] <- track_years[1,"NG"]
  # track_emissions_and_private[i, "HO_install"] <- track_years[1,"HO"]
  # track_emissions_and_private[i, "P_install"] <- track_years[1,"P"]
  # track_emissions_and_private[i, "ASHP_NG_install"] <- track_years[1,"ASHP_NG"]
  # track_emissions_and_private[i, "ASHP_HO_install"] <- track_years[1,"ASHP_HO"]
  # track_emissions_and_private[i, "ASHP_P_install"] <- track_years[1,"ASHP_P"]
  # track_emissions_and_private[i, "NG_private"] <- sum(track_years$NG_private)
  # track_emissions_and_private[i, "HO_private"] <- sum(track_years$HO_private)
  # track_emissions_and_private[i, "P_private"] <- sum(track_years$P_private)
  # track_emissions_and_private[i, "ASHP_NG_private"] <-
  #   sum(track_years$ASHP_NG_private)
  # track_emissions_and_private[i, "ASHP_HO_private"] <-
  #   sum(track_years$ASHP_HO_private)
  # track_emissions_and_private[i, "ASHP_P_private"] <-
  #   sum(track_years$ASHP_P_private)
  # track_emissions_and_private[i, "NG_emissions"] <- sum(track_years$NG_emissions)
  # track_emissions_and_private[i, "HO_emissions"] <- sum(track_years$HO_emissions)
  # track_emissions_and_private[i, "P_emissions"] <- sum(track_years$P_emissions)
  # track_emissions_and_private[i, "ASHP_NG_emissions"] <-
  #   sum(track_years$ASHP_NG_emissions)
  # track_emissions_and_private[i, "ASHP_HO_emissions"] <-
  #   sum(track_years$ASHP_HO_emissions)
  # track_emissions_and_private[i, "ASHP_P_emissions"] <-
  #   sum(track_years$ASHP_P_emissions)
  print(i)
}

## results
#install.packages("ggplot2")
# library(ggplot2)
# ggplot(track_trials, aes(NG)) + geom_histogram()
# ggplot(track_trials, aes(HO)) + geom_histogram()
# ggplot(track_trials, aes(P)) + geom_histogram()
# ggplot(track_trials, aes(ASHP_NG)) + geom_histogram()
# ggplot(track_trials, aes(ASHP_HO)) + geom_histogram()
# ggplot(track_trials, aes(ASHP_P)) + geom_histogram()

# ## average costs of full systems
# mean(track_trials$NG)
# mean(track_trials$HO)
# mean(track_trials$P)
# mean(track_trials$ASHP_NG)
# mean(track_trials$ASHP_HO)
# mean(track_trials$ASHP_P)

## differences between systems per trial
track_trials$NG_dif <- track_trials$NG - track_trials$ASHP_NG
track_trials$HO_dif <- track_trials$HO - track_trials$ASHP_HO
track_trials$P_dif <- track_trials$P - track_trials$ASHP_P

# means of differences between systems
mean_NGdif <- mean(track_trials$NG_dif)
mean_HOdif <- mean(track_trials$HO_dif)
mean_Pdif <- mean(track_trials$P_dif)
mean_NGdif
mean_HOdif
mean_Pdif

## proportion positive differences between systems
mean(track_trials$NG_dif > 0)
mean(track_trials$HO_dif > 0)
mean(track_trials$P_dif > 0)

# graphs of differences between systems
# ggplot(track_trials, aes(NG_dif)) + geom_histogram() + 
#   xlab("Net Benefits") + geom_vline(xintercept = mean_NGdif, color = "red")
# ggplot(track_trials, aes(HO_dif)) + geom_histogram() + 
#   xlab("Net Benefits") + geom_vline(xintercept = mean_HOdif, color = "red")
# ggplot(track_trials, aes(P_dif)) + geom_histogram() + 
#   xlab("Net Benefits") + geom_vline(xintercept = mean_Pdif, color = "red")

# ## differences for a graph that shows benefits vs. costs annually
# track_years_total$NG_dif <- track_years_total$NG - track_years_total$ASHP_NG
# track_years_total$HO_dif <- track_years_total$HO - track_years_total$ASHP_HO
# track_years_total$P_dif <- track_years_total$P - track_years_total$ASHP_P
# track_years_total <- group_by(track_years_total, n)
# track_years_P <- summarise(track_years_total, P_dif = mean(P_dif))
# track_years_HO <- summarise(track_years_total, HO_dif = mean(HO_dif))
# track_years_NG <- summarise(track_years_total, NG_dif = mean(NG_dif))
# 
# group.colors <- c(positive = "darkgreen", negative = "red")
# track_years_P <- mutate(track_years_P, group = ifelse(P_dif >= 0, "positive", "negative"))
# ggplot(track_years_P, aes(y = P_dif, x = n, fill = group)) + geom_bar(stat = "identity") +
#   xlab("Year") + ylab("Net Benefits") + theme(legend.position = "none") +
#   scale_fill_manual(values=group.colors)
# 
# track_years_HO <- mutate(track_years_HO, group = ifelse(HO_dif >= 0, "positive", "negative"))
# ggplot(track_years_HO, aes(y = HO_dif, x = n, fill = group)) + geom_bar(stat = "identity") +
#   xlab("Year") + ylab("Net Benefits") + theme(legend.position = "none") +
#   scale_fill_manual(values=group.colors)
# 
# track_years_NG <- mutate(track_years_NG, group = ifelse(NG_dif >= 0, "positive", "negative"))
# ggplot(track_years_NG, aes(y = NG_dif, x = n, fill = group)) + geom_bar(stat = "identity") +
#   xlab("Year") + ylab("Net Benefits") + theme(legend.position = "none") +
#   scale_fill_manual(values=group.colors)
# 
# # differences for a graph that groups net benefits into 3 categories and provides a total
# track_emissions_and_private$NG_install_dif <- 
#   track_emissions_and_private$NG_install - track_emissions_and_private$ASHP_NG_install
# track_emissions_and_private$NG_private_dif <- 
#   track_emissions_and_private$NG_private - track_emissions_and_private$ASHP_NG_private
# track_emissions_and_private$NG_emissions_dif <- 
#   track_emissions_and_private$NG_emissions - track_emissions_and_private$ASHP_NG_emissions
# track_emissions_and_private$HO_install_dif <- 
#   track_emissions_and_private$HO_install - track_emissions_and_private$ASHP_HO_install
# track_emissions_and_private$HO_private_dif <- 
#   track_emissions_and_private$HO_private - track_emissions_and_private$ASHP_HO_private
# track_emissions_and_private$HO_emissions_dif <- 
#   track_emissions_and_private$HO_emissions - track_emissions_and_private$ASHP_HO_emissions
# track_emissions_and_private$P_install_dif <- 
#   track_emissions_and_private$P_install - track_emissions_and_private$ASHP_P_install
# track_emissions_and_private$P_private_dif <- 
#   track_emissions_and_private$P_private - track_emissions_and_private$ASHP_P_private
# track_emissions_and_private$P_emissions_dif <- 
#   track_emissions_and_private$P_emissions - track_emissions_and_private$ASHP_P_emissions
# 
# summary_graph_NG <- data.frame(Value = 0, Category = c("Installation", "Fuel", "Emissions", "Total"))
# summary_graph_NG[1,"Value"] <- mean(track_emissions_and_private$NG_install_dif)
# summary_graph_NG[2,"Value"] <- mean(track_emissions_and_private$NG_private_dif)
# summary_graph_NG[3,"Value"] <- mean(track_emissions_and_private$NG_emissions_dif)
# summary_graph_NG[4,"Value"] <- mean_NGdif
# summary_graph_NG
# summary_graph_NG <- mutate(summary_graph_NG, group = ifelse(Value >= 0, "positive", "negative"))
# summary_graph_NG$Category <- factor(summary_graph_NG$Category,
#                                    levels = c("Installation", "Fuel", "Emissions", "Total"))
# ggplot(summary_graph_NG, aes(y = Value, x = Category, fill = group)) +
#   geom_bar(stat = "identity") + theme(legend.position = "none") +
#   scale_fill_manual(values=group.colors) +
#   xlab("Cost Category") + ylab("Net Benefits")
# 
# summary_graph_HO <- data.frame(Value = 0, Category = c("Installation", "Fuel", "Emissions", "Total"))
# summary_graph_HO[1,"Value"] <- mean(track_emissions_and_private$HO_install_dif)
# summary_graph_HO[2,"Value"] <- mean(track_emissions_and_private$HO_private_dif)
# summary_graph_HO[3,"Value"] <- mean(track_emissions_and_private$HO_emissions_dif)
# summary_graph_HO[4,"Value"] <- mean_HOdif
# summary_graph_HO
# summary_graph_HO <- mutate(summary_graph_HO, group = ifelse(Value >= 0, "positive", "negative"))
# summary_graph_HO$Category <- factor(summary_graph_HO$Category,
#                                    levels = c("Installation", "Fuel", "Emissions", "Total"))
# ggplot(summary_graph_HO, aes(y = Value, x = Category, fill = group)) +
#   geom_bar(stat = "identity") + theme(legend.position = "none") +
#   scale_fill_manual(values=group.colors) +
#   xlab("Cost Category") + ylab("Net Benefits")
# 
# summary_graph_P <- data.frame(Value = 0, Category = c("Installation", "Fuel", "Emissions", "Total"))
# summary_graph_P[1,"Value"] <- mean(track_emissions_and_private$P_install_dif)
# summary_graph_P[2,"Value"] <- mean(track_emissions_and_private$P_private_dif)
# summary_graph_P[3,"Value"] <- mean(track_emissions_and_private$P_emissions_dif)
# summary_graph_P[4,"Value"] <- mean_Pdif
# summary_graph_P
# summary_graph_P <- mutate(summary_graph_P, group = ifelse(Value >= 0, "positive", "negative"))
# summary_graph_P$Category <- factor(summary_graph_P$Category,
#                                 levels = c("Installation", "Fuel", "Emissions", "Total"))
# ggplot(summary_graph_P, aes(y = Value, x = Category, fill = group)) +
#   geom_bar(stat = "identity") + theme(legend.position = "none") +
#   scale_fill_manual(values=group.colors) +
#   xlab("Cost Category") + ylab("Net Benefits")

  