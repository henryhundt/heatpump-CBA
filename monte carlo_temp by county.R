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
  temperature_bin_hours <- filter(temperature_bin_hours, temperature < indoor_design_temperature)
  
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

  temperature_bin_hours$distance <- temperature_bin_hours$temperature - outdoor_design_temperature
  temperature_bin_hours$load_proportion <- (temperature_distance-temperature_bin_hours$distance)/temperature_distance
  
  ## heating technology is generally not installed to function at below the outdoor design temp
  ## so when such temperatures do occur, the technology can at best run at 100% capacity. As such
  ## we set their proportion of the design heating load to 1
  temperature_bin_hours <- mutate(temperature_bin_hours, load_proportion = 
                                    ifelse(load_proportion > 1, 1, load_proportion))
  
  return(temperature_bin_hours)
}

## calculate the proportion of heating that is covered by the backup system
## given a switchover temperature
backupheatingload <- function(heating_bin_hours, switchover_temp, heating_load){
  backup <- filter(heating_bin_hours, temperature < switchover_temp)
  total_prop <- sum(heating_bin_hours$load_proportion)
  backup_prop <- sum(backup$load_proportion)
  return(heating_load*backup_prop/total_prop)
}

# calculate the average COP of the ASHP for cooling and for heating 
# for heating, it uses COP values at 5 and 65 degrees Fahrenheit
# for cooling, it uses COP values at 
annual_COP <- function(COP_low, COP_high, temperature_bin_hours, 
                       switchover_temp, heating_yes){
  ## for heating, remove temperature bin data covered by backup heating
  if(heating_yes){
    if(switchover_temp >= 65) return(1)
    temperature_bin_hours <- filter(temperature_bin_hours, temperature > switchover_temp)
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
    temperature_bin_hours$COP <- COP_high - COP_slope*(65-temperature_bin_hours$temperature)
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

installment_costs <- function(df, nonmaterial_costs) {
  material_cost <-  runif(1, min = df$systemcost_low, max = df$systemcost_high) 
  nonmaterial_low <- df$laborcost_low + df$jobsupplycost_low
  nonmaterial_high <- df$laborcost_high + df$jobsupplycost_high
  nonmaterial_cost <- nonmaterial_low + nonmaterial_costs*nonmaterial_high
  return(material_cost + nonmaterial_cost)
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
years_of_analysis <- 15
start_year <- 2022

#### set scenario variables ####
### comment out the ones that you don't want to use
##discount rate 
#discount_rate <- 0.07
discount_rate <- 0.02
#discount_rate <- 0.0

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

##### read in necessary files ######################################
#### read in the geographic dataframe:
## counties
geo <- read.csv("./creating geographic data set/county_installprice_all_elec.csv")

#### read in and set up price projections for later
### values from EIA's projected costs of fuel/electricity over time.
### source: https://www.eia.gov/outlooks/aeo/data/browser/#/?id=3-AEO2022&sourcekey=0
projections_base <- read.csv("./Fuel cost data/EIA projections/Energy_Prices_Residential_projections.csv")
#changes order so first row is year 1 (i.e. 2021)
projections_base <- arrange(projections_base, year)
start <- which(projections_base$year == start_year)
end <- start + years_of_analysis - 1
projections_base <- projections_base[c(start:end),]
## convert to growth rates instead of absolute numbers
## keep the projections_base data since we will use it for areas for which we don't
## have spatial data 
base <- projections_base[1,]
fuel_cost_growth_rates_base <- as.data.frame(t(apply(projections_base, 1, function(rowval) unlist(rowval / base))))

#### read in list of ASHP COPs
TKTKTK #update this? with the scraped data?
ASHP_COPs <- read.csv("ASHP random sample.csv")
ASHP_COPs <- select(ASHP_COPs, starts_with("cop"))

## temperature and load data
## source: https://www.ncei.noaa.gov/access/search/data-search/normals-hourly-2006-2020
temperature_bin_hours_base <- read.csv("./NREL Data/typical meteorological year weather/temperature data by county.csv")

## indoor design temperature
#source: https://focusonenergy.com/sites/default/files/bpdeemedsavingsmanuav10_evaluationreport.pdf
indoor_design_temperature <- 65 #degrees Fahrenheit 

## outdoor design temperature csv
#source: https://casetext.com/regulation/wisconsin-administrative-code/agency-department-of-safety-and-professional-services/safety-buildings-and-environment/commercial-building-code/chapter-sps-363-energy-conservation/subchapter-ii-changes-additions-or-omissions-to-the-international-energy-conservation-code-iecc/section-sps-3630302-exterior-design-conditions
outdoor_design_temperature_base <- read.csv("./Temperature Data/Outdoor design temps by county.csv")

## electricity
elec_cost_base <- read.csv("./final electricity data.csv")

# heating load
heating_load_base <- read.csv("./NREL data/heating and cooling load/heating load by county.csv")

# cooling kWh
cooling_kWh_base <- read.csv("./NREL data/heating and cooling load/cooling kWh by county.csv")

## not to keep
### installation sizing and efficiency 
TKTKTK
size <- 2
efficiency <- 16

#installment cost
### sources are as follows:
# ASHP: https://www.homewyse.com/costs/cost_of_heat_pump_systems.html
# NG: https://www.homewyse.com/costs/cost_of_energy_efficient_gas_furnaces.html
# HO: https://www.homewyse.com/costs/cost_of_oil_furnaces.html
# P: https://www.homewyse.com/costs/cost_of_energy_efficient_gas_furnaces.html
# ElecRes: https://www.homewyse.com/costs/cost_of_electric_baseboard_heaters.html

ASHP_installment_file_base <- read.csv(paste0("./Installation costs/cost to install heat pump - ",size," ton ", efficiency, " SEER.csv"))
## after receiving feedback from PSC that questioned the Homewyse values, we chose
## to allow AC installment nonmaterial costs to equal ASHP installment costs.
AC_installment_file_base <- read.csv(paste0("./Installation costs/cost to install AC - ",size," ton ", efficiency, " SEER.csv"))
AC_installment_file_base[,c(6:9)] <- ASHP_installment_file_base[,c(6:9)]
NG_P_installment_file_base <- read.csv(paste0("./Installation costs/cost to install NG furnace - 70K BTU 92%+ efficiency.csv"))
HO_installment_file_base <- read.csv(paste0("./Installation costs/cost to install oil furnace - 70K BTU 85%+ efficiency.csv"))
ElecRes_installment_file_base <- read.csv("./Installation costs/cost to install 2 electric resistance baseboards.csv")

###efficiency variables - note that efficiency is a unitless value
# this can be thought of as the average efficiency of whichever furnace across
# users in Wisconsin
## source is the "typical" 2020 efficiency for the respective technologies from
## here: https://www.eia.gov/analysis/studies/buildings/equipcosts/pdf/appendix-a.pdf
## electric resistance efficiency comes from the text after the table
naturalgas_furnace_efficiency <- .92
heatingoil_furnace_efficiency <- .83
propane_furnace_efficiency <- .92
ElecRes_efficiency <- .98
wood_pellet_efficiency <- .77
wood_cord_efficiency <- .78

## wood prices - source: https://fyi.extension.wisc.edu/energy/files/2018/07/wood_heating_appliances_for_homes_and_businesses-1.pdf
## Also, EIA prices are given in 2021 $/mmBTU and we assume that wood prices will
## rise with inflation but have no information to lead them to change otherwise.
## therefore, we leave them to be the same throughout since having all prices at 2021 $/mmBTU
## means we don't need to account for inflation
wood_cord_cost <- 225 #2013 $/cord of wood
wood_cord_cost <- wood_cord_cost*1.16 #adjusted for inflation from 2013 to 2021: https://www.officialdata.org/us/inflation/2013?endYear=2021&amount=1
wood_cord_cost <- wood_cord_cost/22 #2021 $/mmBTU
wood_pellet_cost <- 190 #2013 $/ton of wood pellets
wood_pellet_cost <- wood_pellet_cost*1.16 #adjusting for inflation like above
wood_pellet_cost <- wood_pellet_cost/15.4 #2021 $/mmBTU

## wood heating installment costs come from eia. We assume they are the typical
## total installment costs in 2020 from the following source:
## url: https://www.eia.gov/analysis/studies/buildings/equipcosts/pdf/appendix-a.pdf
wood_cord_installment_cost <- 7600
wood_pellet_installment_cost <- 4700

###emissions variables (fuel data from https://www.epa.gov/sites/default/files/2021-04/documents/emission-factors_apr2021.pdf)
### WI grid specific electricity grid data is from https://www.epa.gov/system/files/documents/2022-01/egrid2020_summary_tables.pdf
#CO2
## CO2 associated with electricity being used for heating in pounds of CO2/MWh
electricity_CO2 <- 1184.9
## CO2 from burning natural gas in a furnace in kg of CO2 per mmBTU
naturalgas_CO2 <- 53.06 
## CO2 from burning heating oil (Distillate Fuel Oil No. 2) in a furnace in kg of CO2 per mmBTU
heatingoil_CO2 <- 73.96 
## CO2 from burning propane in a furnace in kg of CO2 per mmBTU
propane_CO2 <- 62.87 
## CO2 from burning wood (Wood and Wood Residuals)
wood_CO2 <- 93.80

#CH4
# CH4 associated with electricity being used for heating in lb of CH4/MWh
electricity_CH4 <- 0.106
# CH4 from burning natural gas in a furnace in kg of CH4 per mmBTU
naturalgas_CH4 <- 1/1000
# CH4 from burning heating oil (Distillate Fuel Oil No. 2) in a furnace in kg of CH4 per mmBTU
heatingoil_CH4 <- 3/1000
# CH4 from burning propane in a furnace in kg of CH4 per mmBTU
propane_CH4 <- 3/1000
## CH4 from burning wood
wood_CH4 <- 7.2/1000

#N2O
# N2O associated with electricity being used for heating in pounds of N2O/MWh
electricity_N2O <- 0.015
# N2O from burning natural gas in a furnace in kg of N2O per mmBTU
naturalgas_N2O <- 0.1/1000
# N2O from burning heating oil (Distillate Fuel Oil No. 2) in a furnace in kg of N2O per mmBTU
heatingoil_N2O <- 0.6/1000
# N2O from burning propane in a furnace in kg of N2O per mmBTU
propane_N2O <- 0.6/1000
## N2O from burning wood
wood_N2O <- 3.6/1000

### decarbonization rate of the grid
#2020 is the year from which our grid emissions values come from
decarb <- 1/(decarb_year - 2020) 

## energy to volume constants
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

### account for life-cycle emissions with regard to electricity production
# source: https://www.epa.gov/sites/default/files/2021-02/documents/egrid2019_summary_tables.pdf
# static (we assume these don't change over the course of the simulation)
proportion_electricity_from_oil <- 0.002
proportion_electricity_from_NG <- 0.358
proportion_electricity_from_nuclear <- 0.159
proportion_electricity_from_hydro <- 0.045
proportion_electricity_from_biomass <- 0.019
# we assume that all improvements to the grid during the simulation comes
# from the removal of coal from the grid and that it will be replaced with an
# equal proportion of solar and wind
proportion_electricity_from_coal <- 0.387
proportion_electricity_from_wind <- 0.029
proportion_electricity_from_solar <- 0.001

# the above figures are from 2020, so we update them to 2021 numbers so that
# they can be used in the iterative year for-loop below.
# The 1.25 (1/.8) reflects the fact that coal is removed from the grid at a rate 
# faster than the grid reaches 0 emissions (e.g. in our aggressive case, all coal 
# is removed by 2035, but the grid still has 20% of it's original emissions)
drop_in_coal <- proportion_electricity_from_coal*(decarb*1.25)
## every year update the amount of energy that is now in renewables
## we assume that all improvements in the grid come from switching coal to
## renewables. This just updates the values to 2021
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

### clean the spatial dataset and determine which electric utility's data to use
geo <- filter(geo, Area.in.Square.Miles > 0.5)
geo <- filter(geo, !is.na(FIPS))
geo <- filter(geo, !(FID_COOP_UTILITY_BOUNDARIES_COO == -1 & FID_WI_IOU_UTILITY_BOUNDARIES_W == -1 & FID_WI_MUNI_UTILITY_BOUNDARIES_ == -1))
## we assume that the hierarchy of electricity usage is municipality >> coop >> IOU
geo <- mutate(geo, elec_utility = ifelse(FID_WI_MUNI_UTILITY_BOUNDARIES_ != -1, PSC_ID, 
                                         ifelse(FID_COOP_UTILITY_BOUNDARIES_COO != -1, 
                                                PSC_ID.1, PSC_ID.2)))

for(k in 1:nrow(geo)) {
  county <- geo[k,]$FIPS
  price_region <- geo[k,]$price_region
  elec_utility <- geo[k,]$elec_utility
  
  #temperature bins
  temperature_bin_hours <- filter(temperature_bin_hours_base, FIPS == county)
  temperature_bin_hours <- select(temperature_bin_hours, temperature)
  temperature_bin_hours <- arrange(temperature_bin_hours, temperature)

  #outdoor design temperature
  outdoor_design_temperature <- filter(outdoor_design_temperature_base, county_fips == county)
  outdoor_design_temperature <- outdoor_design_temperature$Outdoor.Design.Temp
  
  # heating load
  heating_load <- select(heating_load_base, contains(as.character(county)))
  total_heating <- sum(heating_load)
  heating_load$weight <- heating_load[,1]/total_heating
  
  # cooling kWh
  cooling_kWh <- select(cooling_kWh_base, contains(as.character(county)))
  cooling_kWh <- sum(cooling_kWh)
  
  # electricity cost
  elec_cost <- filter(elec_cost_base, Utility.ID == elec_utility)
  #convert from $/kWh to $/mmBTU
  elec_cost$Rate <- elec_cost$Rate/3412*10^6
  
  ## Find the annual heating electricity cost by weighting each months electricity
  ## price by the proportion of the year's heating load
  heating_electricity_cost <- sum(elec_cost$Rate*heating_load$weight)
  heating_load <- sum(heating_load[,1])
  
  ### keeps track of monte carlo results
  track_trials <- data.frame(n = c(1:n_trials), NG = 0, HO = 0, P = 0, ASHP_NG = 0,
                           ASHP_HO = 0, ASHP_P = 0)

    for(i in 1:n_trials){
    #base cost variables 
    ## randomly choose what energy projections to run based on one of 10 different
    ## scenarios that EIA provides, which determines the price of all fuels/
    ## electricity over time for a given trial. (all prices are in 2020 $s/MMBTU)
    scenario <- floor(runif(1, min = 1, max = 10))
    columns <- c(scenario, scenario + 10, scenario + 20, scenario + 30)
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
    ASHP_NG_switchover_COP <- heating_electricity_cost*
      (naturalgas_furnace_efficiency/
         projections[1,which(str_detect(colnames(projections), "Natural.Gas"))])
    ASHP_HO_switchover_COP <- heating_electricity_cost*
      (heatingoil_furnace_efficiency/
         projections[1,which(str_detect(colnames(projections), "Distillate.Fuel.Oil"))])
    ASHP_P_switchover_COP <- heating_electricity_cost*
      (propane_furnace_efficiency/
         projections[1,which(str_detect(colnames(projections), "Propane"))])
    ASHP_ElecRes_switchover_COP <- heating_electricity_cost*
      (ElecRes_efficiency/
         heating_electricity_cost)
    ASHP_WoodCord_switchover_COP <- heating_electricity_cost*
      (wood_cord_efficiency/
         wood_cord_cost)
    ASHP_WoodPellet_switchover_COP <- heating_electricity_cost*
      (wood_pellet_efficiency/
         wood_pellet_cost)
    
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
    ASHP_NG_switchover <- round(((ASHP_NG_switchover_COP - heat_COP_low)/COP_slope) + 5,1)
    ASHP_HO_switchover <- round(((ASHP_HO_switchover_COP - heat_COP_low)/COP_slope) + 5,1)
    ASHP_P_switchover <- round(((ASHP_P_switchover_COP - heat_COP_low)/COP_slope) + 5,1)
    ASHP_ElecRes_switchover <- round(((ASHP_ElecRes_switchover_COP - heat_COP_low)/COP_slope) + 5,1)
    ASHP_WoodCord_switchover <- round(((ASHP_WoodCord_switchover_COP - heat_COP_low)/COP_slope) + 5,1)
    ASHP_WoodPellet_switchover <- round(((ASHP_WoodPellet_switchover_COP - heat_COP_low)/COP_slope) + 5,1)
    
    ## proportion of heating load covered by backup - different for each fuel 
    ## because of different switchovers first adjust the temperature_heating_bin 
    ## data to account for heating load at each temperature bin
    heating_bin_hours <- heatingbin_adjust(indoor_design_temperature, 
                                           outdoor_design_temperature, temperature_bin_hours)
    #next determine the proportion of heating provided by backup for each fuel
    ASHP_NG_backup_heating <- backupheatingload(heating_bin_hours, ASHP_NG_switchover, heating_load)
    ASHP_HO_backup_heating <- backupheatingload(heating_bin_hours, ASHP_HO_switchover, heating_load)
    ASHP_P_backup_heating <- backupheatingload(heating_bin_hours, ASHP_P_switchover, heating_load)
    ASHP_ElecRes_backup_heating <- backupheatingload(heating_bin_hours, ASHP_ElecRes_switchover, heating_load)
    ASHP_WoodCord_backup_heating <- backupheatingload(heating_bin_hours, ASHP_WoodCord_switchover, heating_load)
    ASHP_WoodPellet_backup_heating <- backupheatingload(heating_bin_hours, ASHP_WoodPellet_switchover, heating_load)
    
    ## and by ASHP for each fuel
    ASHP_NG_heating <- heating_load - ASHP_NG_backup_heating
    ASHP_HO_heating <- heating_load - ASHP_HO_backup_heating
    ASHP_P_heating <- heating_load - ASHP_P_backup_heating
    ASHP_ElecRes_heating <- heating_load - ASHP_ElecRes_backup_heating
    ASHP_WoodCord_heating <- heating_load - ASHP_WoodCord_backup_heating
    ASHP_WoodPellet_heating <- heating_load - ASHP_WoodPellet_backup_heating
    
    ## installment costs
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
    ASHP_installment_file <- filter(ASHP_installment_file_base, price_region == price_region)[1,]
    AC_installment_file <- filter(AC_installment_file_base, price_region == price_region)[1,]
    NG_P_installment_file <- filter(NG_P_installment_file_base, price_region == price_region)[1,]
    HO_installment_file <- filter(HO_installment_file_base, price_region == price_region)[1,]
    ElecRes_installment_file <- filter(ElecRes_installment_file_base, price_region == price_region)[1,]
    
    # nonlabor cost of installing an ASHP in dollars per unit 
    ## we assume nonmaterial costs to covary perfectly
    nonmaterial_costs <- runif(1,0,1)
    ASHP_installment_cost <- installment_costs(ASHP_installment_file, nonmaterial_costs)
    naturalgas_furnace_installment_cost <- installment_costs(NG_P_installment_file, nonmaterial_costs)
    heatingoil_furnace_installment_cost <- installment_costs(ASHP_installment_file, nonmaterial_costs)
    propane_furnace_installment_cost <- naturalgas_furnace_installment_cost
    ElecRes_installment_cost <- installment_costs(ElecRes_installment_file, nonmaterial_costs)
    AC_installment_cost <- installment_costs(AC_installment_file, nonmaterial_costs)
    
    # First simulate the efficiency of the "average" air source heat pump in the
    # scenario.  
    # calculate the COP by weighting the COP within each temperature bin by
    # how often the ASHP has to run while in that bin and how often that bin
    # occurs
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
    ASHP_ElecRes_heating_ASHP_COP <- annual_COP(heat_COP_low, heat_COP_high,
                                                heating_bin_hours, ASHP_ElecRes_switchover, TRUE)
    ASHP_WoodCord_heating_ASHP_COP <- annual_COP(heat_COP_low, heat_COP_high,
                                                 heating_bin_hours, ASHP_WoodCord_switchover, TRUE)
    ASHP_WoodPellet_heating_ASHP_COP <- annual_COP(heat_COP_low, heat_COP_high,
                                                   heating_bin_hours, ASHP_WoodPellet_switchover, TRUE)
    
    #### calculate input for all heating types + cooling 
    # the rest are all in mmBTUs 
    ASHP_NG_input <- ASHP_NG_heating/ASHP_NG_heating_ASHP_COP
    ASHP_HO_input <- ASHP_HO_heating/ASHP_HO_heating_ASHP_COP
    ASHP_P_input <- ASHP_P_heating/ASHP_P_heating_ASHP_COP
    ASHP_ElecRes_input <- ASHP_ElecRes_heating/ASHP_ElecRes_heating_ASHP_COP
    ASHP_WoodCord_input <- ASHP_WoodCord_heating/ASHP_WoodCord_heating_ASHP_COP
    ASHP_WoodPellet_input <- ASHP_WoodPellet_heating/ASHP_WoodPellet_heating_ASHP_COP
    backup_NG_input <- ASHP_NG_backup_heating/naturalgas_furnace_efficiency
    backup_HO_input <- ASHP_HO_backup_heating/heatingoil_furnace_efficiency
    backup_P_input <- ASHP_P_backup_heating/propane_furnace_efficiency
    backup_ElecRes_input <- ASHP_ElecRes_backup_heating/ElecRes_efficiency
    backup_WoodCord_input <- ASHP_WoodCord_backup_heating/wood_cord_efficiency
    backup_WoodPellet_input <- ASHP_WoodPellet_backup_heating/wood_pellet_efficiency
    full_NG_input <- heating_load/naturalgas_furnace_efficiency
    full_HO_input <- heating_load/heatingoil_furnace_efficiency
    full_P_input <- heating_load/propane_furnace_efficiency
    full_ElecRes_input <- heating_load/ElecRes_efficiency
    full_WoodCord_input <- heating_load/wood_cord_efficiency
    full_WoodPellet_input <- heating_load/wood_pellet_efficiency
    
    ###other variables
    social_cost_of_CO2 <- runif(1, min = 14, max = 51)/1000 #dollars per kg of CO2
    
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
      
    ## create dataset to track the 15 year simulation for this trial
    track_years <- track_years <- data.frame(n = c(0:years_of_analysis))
    #### input year 0 costs (i.e. installation costs)
    track_years[track_years$n == 0,"NG"] <- naturalgas_furnace_installment_cost
    track_years[track_years$n == 0,"HO"] <- heatingoil_furnace_installment_cost
    track_years[track_years$n == 0,"P"] <- propane_furnace_installment_cost
    track_years[track_years$n == 0, "ElecRes"] <- ElecRes_installment_cost
    track_years[track_years$n == 0, "WoodCord"] <- wood_cord_installment_cost
    track_years[track_years$n == 0, "WoodPellet"] <- wood_pellet_installment_cost
    track_years[track_years$n == 0, "AC"] <- AC_installment_cost
    track_years[track_years$n == 0,"ASHP_NG"] <- naturalgas_furnace_installment_cost +
      ASHP_installment_cost
    track_years[track_years$n == 0,"ASHP_HO"] <- heatingoil_furnace_installment_cost +
      ASHP_installment_cost
    track_years[track_years$n == 0,"ASHP_P"] <- propane_furnace_installment_cost +
      ASHP_installment_cost
    track_years[track_years$n == 0,"ASHP_ElecRes"] <- ElecRes_installment_cost +
      ASHP_installment_cost
    track_years[track_years$n == 0,"ASHP_WoodCord"] <- wood_cord_installment_cost +
      ASHP_installment_cost
    track_years[track_years$n == 0,"ASHP_WoodPellet"] <- wood_pellet_installment_cost +
      ASHP_installment_cost
  
    #### start simulation of annual costs ####
    for(j in 1:years_of_analysis){
      ###cost variables
      #fuel cost in 2021 $/mmBTU from EIA projections for year j
      heating_electricity_price <- heating_electricity_cost*fuel_cost_growth_rates[j, which(str_detect(colnames(fuel_cost_growth_rates), "Electricity"))]
      naturalgas_price <- projections[j, which(str_detect(colnames(fuel_cost_growth_rates), "Gas"))] 
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
      
      #wood cords
      wood_CO2e <- wood_CO2*GWP_CO2 + wood_CH4*GWP_CH4 + wood_N2O*GWP_N2O
      ############### end of conversions ########################################
      
      ################# Calculations section ###############
      ####Calculate heating costs for all heating types
      ASHP_NG_fuel_cost <- ASHP_NG_input*heating_electricity_price
      ASHP_HO_fuel_cost <- ASHP_HO_input*heating_electricity_price
      ASHP_P_fuel_cost <- ASHP_P_input*heating_electricity_price
      ASHP_ElecRes_fuel_cost <- ASHP_ElecRes_input*heating_electricity_price
      ASHP_WoodCord_fuel_cost <- ASHP_WoodCord_input*wood_cord_cost
      ASHP_WoodPellet_fuel_cost <- ASHP_WoodPellet_input*wood_pellet_cost
      backup_naturalgas_fuel_cost <-  backup_NG_input*naturalgas_price
      backup_heatingoil_fuel_cost <- backup_HO_input*heatingoil_price
      backup_propane_fuel_cost <- backup_P_input*propane_price
      backup_ElecRes_fuel_cost <- backup_ElecRes_input*heating_electricity_price
      backup_WoodCord_fuel_cost <- backup_WoodCord_input*wood_cord_cost
      backup_WoodPellet_fuel_cost <- backup_WoodPellet_input*wood_pellet_cost
      full_naturalgas_fuel_cost <- full_NG_input*naturalgas_price
      full_heatingoil_fuel_cost <- full_HO_input*heatingoil_price
      full_propane_fuel_cost <- full_P_input*propane_price
      full_ElecRes_fuel_cost <- full_ElecRes_input*heating_electricity_price
      full_WoodCord_fuel_cost <- full_WoodCord_input*wood_cord_cost
      full_WoodPellet_fuel_cost <- full_WoodPellet_input*wood_pellet_cost
      
      # all electric emissions account for line loss, the % of electricity lost
      # from production to end-source. Since producers account for line-loss in their
      # production output (e.g. if they expect a demand of 1 kWh they know to produce
      # 1 * (1 + lineloss) kWh of electricity) the end user who demanded that electricity
      # is responsible for the line loss as well.
      line_loss <- 0.05
      #### Calculate emissions for all heating types and monetize with social carbon costs
      cooling_emissions_cost <- (cooling_kWh*mmBTU_per_kWh*electricity_CO2e)*
        (1+line_loss)*social_cost_of_CO2
      ASHP_NG_emissions_cost <- (ASHP_NG_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      ASHP_HO_emissions_cost <- (ASHP_HO_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      ASHP_P_emissions_cost <- (ASHP_P_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      ASHP_ElecRes_emissions_cost <- (ASHP_ElecRes_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      ASHP_WoodCord_emissions_cost <- (ASHP_WoodCord_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      ASHP_WoodPellet_emissions_cost <- (ASHP_WoodPellet_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      
      backup_naturalgas_emissions_cost <- (backup_NG_input*naturalgas_CO2e +
                                             backup_NG_methane_leak_CO2e)*social_cost_of_CO2
      backup_heatingoil_emissions_cost <- (backup_HO_input*heatingoil_CO2e +
                                             backup_HO_methane_leak_CO2e)*social_cost_of_CO2
      backup_propane_emissions_cost <- (backup_P_input*propane_CO2e +
                                          backup_P_methane_leak_CO2e)*social_cost_of_CO2
      backup_ElecRes_emissions_cost <- (backup_ElecRes_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      backup_WoodCord_emissions_cost <- (backup_WoodCord_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      backup_WoodPellet_emissions_cost <- (backup_WoodPellet_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      
      full_natgas_emissions_cost <- (full_NG_input*naturalgas_CO2e +
                                       full_NG_methane_leak_CO2e)*social_cost_of_CO2
      full_heatingoil_emissions_cost <- (full_HO_input*heatingoil_CO2e +
                                           full_HO_methane_leak_CO2e)*social_cost_of_CO2
      full_propane_emissions_cost <- (full_P_input*propane_CO2e +
                                        full_P_methane_leak_CO2e)*social_cost_of_CO2
      full_ElecRes_emissions_cost <- (full_ElecRes_input*electricity_CO2e)*(1+line_loss)*
        social_cost_of_CO2
      full_WoodCord_emissions_cost <- (full_WoodCord_input*wood_CO2e)*social_cost_of_CO2
      full_WoodPellet_emissions_cost <- (full_WoodPellet_input*wood_CO2e)*social_cost_of_CO2
      
      ################### End of "calculation section" ################
      
      ######################## NPV ####################################
      ## calculate this years costs
      NG <- full_natgas_emissions_cost + full_naturalgas_fuel_cost
      HO <- full_heatingoil_emissions_cost + full_heatingoil_fuel_cost
      P <- full_propane_emissions_cost + full_propane_fuel_cost
      ElecRes <- full_ElecRes_emissions_cost + full_ElecRes_fuel_cost
      WoodCord <- full_WoodCord_emissions_cost + full_WoodCord_fuel_cost
      WoodPellet <- full_WoodPellet_emissions_cost + full_WoodPellet_fuel_cost
      AC <- cooling_emissions_cost
      
      ASHP_NG <- ASHP_NG_fuel_cost + ASHP_NG_emissions_cost +
        backup_naturalgas_emissions_cost + backup_naturalgas_fuel_cost + cooling_emissions_cost
      ASHP_HO <- ASHP_HO_fuel_cost + ASHP_HO_emissions_cost +
        backup_heatingoil_emissions_cost + backup_heatingoil_fuel_cost + cooling_emissions_cost
      ASHP_P <- ASHP_P_fuel_cost + ASHP_P_emissions_cost +
        backup_propane_emissions_cost + backup_propane_fuel_cost + cooling_emissions_cost
      ASHP_ElecRes <- ASHP_ElecRes_fuel_cost + ASHP_ElecRes_emissions_cost +
        backup_ElecRes_emissions_cost + backup_ElecRes_fuel_cost + cooling_emissions_cost
      ASHP_WoodCord <- ASHP_WoodCord_fuel_cost + ASHP_WoodCord_emissions_cost +
        backup_WoodCord_emissions_cost + backup_WoodCord_fuel_cost + cooling_emissions_cost
      ASHP_WoodPellet <- ASHP_WoodPellet_fuel_cost + ASHP_WoodPellet_emissions_cost +
        backup_WoodPellet_emissions_cost + backup_WoodPellet_fuel_cost + cooling_emissions_cost
      
      ## discount the annual costs to derive the NPV
      track_years[track_years$n == j, "NG"] <- NG/(1+discount_rate)^j
      track_years[track_years$n == j, "HO"] <- HO/(1+discount_rate)^j
      track_years[track_years$n == j, "P"] <- P/(1+discount_rate)^j
      track_years[track_years$n == j, "ElecRes"] <- ElecRes/(1+discount_rate)^j
      track_years[track_years$n == j, "WoodCord"] <- WoodCord/(1+discount_rate)^j
      track_years[track_years$n == j, "WoodPellet"] <- WoodPellet/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_NG"] <- ASHP_NG/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_HO"] <- ASHP_HO/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_P"] <- ASHP_P/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_ElecRes"] <- ASHP_ElecRes/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_WoodCord"] <- ASHP_WoodCord/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_WoodPellet"] <- ASHP_WoodPellet/(1+discount_rate)^j
      track_years[track_years$n == j, "AC"] <- AC/(1+discount_rate)^j
      
      ## sub-parts of the net benefits
      #### Maintenance and Operations costs
      track_years[track_years$n == j, "NG_MO"] <-
        full_naturalgas_fuel_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "HO_MO"] <-
        full_heatingoil_fuel_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "P_MO"] <-
        full_propane_fuel_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "ElecRes_MO"] <-
        full_ElecRes_fuel_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "WoodCord_MO"] <-
        full_WoodCord_fuel_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "WoodPellet_MO"] <-
        full_WoodPellet_fuel_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_NG_MO"] <-
        (ASHP_NG_fuel_cost + backup_naturalgas_fuel_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_HO_MO"] <-
        (ASHP_HO_fuel_cost + backup_heatingoil_fuel_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_P_MO"] <-
        (ASHP_P_fuel_cost + backup_propane_fuel_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_ElecRes_MO"] <-
        (ASHP_ElecRes_fuel_cost + backup_ElecRes_fuel_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_WoodCord_MO"] <-
        (ASHP_WoodCord_fuel_cost + backup_WoodCord_fuel_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_WoodPellet_MO"] <-
        (ASHP_WoodPellet_fuel_cost + backup_WoodPellet_fuel_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "AC_MO"] <- 0
      #### emissions costs
      track_years[track_years$n == j, "NG_emissions_cost"] <-
        full_natgas_emissions_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "HO_emissions_cost"] <-
        full_heatingoil_emissions_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "P_emissions_cost"] <-
        full_propane_emissions_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "ElecRes_emissions_cost"] <-
        full_ElecRes_emissions_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "WoodCord_emissions_cost"] <-
        full_WoodCord_emissions_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "WoodPellet_emissions_cost"] <-
        full_WoodPellet_emissions_cost/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_NG_emissions_cost"] <-
        (ASHP_NG_emissions_cost + backup_naturalgas_emissions_cost + cooling_emissions_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_HO_emissions_cost"] <-
        (ASHP_HO_emissions_cost + backup_heatingoil_emissions_cost + cooling_emissions_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_P_emissions_cost"] <-
        (ASHP_P_emissions_cost + backup_propane_emissions_cost + cooling_emissions_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_ElecRes_emissions_cost"] <-
        (ASHP_ElecRes_emissions_cost + backup_ElecRes_emissions_cost + cooling_emissions_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_WoodCord_emissions_cost"] <-
        (ASHP_WoodCord_emissions_cost + backup_WoodCord_emissions_cost + cooling_emissions_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "ASHP_WoodPellet_emissions_cost"] <-
        (ASHP_WoodPellet_emissions_cost + backup_WoodPellet_emissions_cost + cooling_emissions_cost)/(1+discount_rate)^j
      track_years[track_years$n == j, "AC_emissions_cost"] <- cooling_emissions_cost/(1+discount_rate)^j
      ######################### end of "NPV" section ####################
    }
    track_years[is.na(track_years)] <- 0
    
    ## at the end of the 15 years, we then take the sum of the annual NPVs to find
    ## the total NPV
    track_trials[i, "NG"] <- sum(track_years$NG)
    track_trials[i, "HO"] <- sum(track_years$HO)
    track_trials[i, "P"] <- sum(track_years$P)
    track_trials[i, "ElecRes"] <- sum(track_years$ElecRes)
    track_trials[i, "WoodCord"] <- sum(track_years$WoodCord)
    track_trials[i, "WoodPellet"] <- sum(track_years$WoodPellet)
    track_trials[i, "ASHP_NG"] <- sum(track_years$ASHP_NG)
    track_trials[i, "ASHP_HO"] <- sum(track_years$ASHP_HO)
    track_trials[i, "ASHP_P"] <- sum(track_years$ASHP_P)
    track_trials[i, "ASHP_ElecRes"] <- sum(track_years$ASHP_ElecRes)
    track_trials[i, "ASHP_WoodCord"] <- sum(track_years$ASHP_WoodCord)
    track_trials[i, "ASHP_WoodPellet"] <- sum(track_years$ASHP_WoodPellet)
    track_trials[i, "AC"] <- sum(track_years$AC)
    
    ##subparts
    ## capital
    track_trials[i, "NG_capital"] <- track_years[1,"NG"]
    track_trials[i, "HO_capital"] <- track_years[1,"HO"]
    track_trials[i, "P_capital"] <- track_years[1,"P"]
    track_trials[i, "ElecRes_capital"] <- track_years[1,"ElecRes"]
    track_trials[i, "WoodCord_capital"] <- track_years[1,"WoodCord"]
    track_trials[i, "WoodPellet_capital"] <- track_years[1,"WoodPellet"]
    track_trials[i, "ASHP_NG_capital"] <- track_years[1,"ASHP_NG"]
    track_trials[i, "ASHP_HO_capital"] <- track_years[1,"ASHP_HO"]
    track_trials[i, "ASHP_P_capital"] <- track_years[1,"ASHP_P"]
    track_trials[i, "ASHP_ElecRes_capital"] <- track_years[1,"ASHP_ElecRes"]
    track_trials[i, "ASHP_WoodCord_capital"] <- track_years[1,"ASHP_WoodCord"]
    track_trials[i, "ASHP_WoodPellet_capital"] <- track_years[1,"ASHP_WoodPellet"]
    track_trials[i, "AC_capital"] <- track_years[1,"AC"]
    ## M&O
    track_trials[i, "NG_MO"] <- sum(track_years$NG_MO)
    track_trials[i, "HO_MO"] <- sum(track_years$HO_MO)
    track_trials[i, "P_MO"] <- sum(track_years$P_MO)
    track_trials[i, "ElecRes_MO"] <- sum(track_years$ElecRes_MO)
    track_trials[i, "WoodCord_MO"] <- sum(track_years$WoodCord_MO)
    track_trials[i, "WoodPellet_MO"] <- sum(track_years$WoodPellet_MO)
    track_trials[i, "ASHP_NG_MO"] <- sum(track_years$ASHP_NG_MO)
    track_trials[i, "ASHP_HO_MO"] <- sum(track_years$ASHP_HO_MO)
    track_trials[i, "ASHP_P_MO"] <- sum(track_years$ASHP_P_MO)
    track_trials[i, "ASHP_ElecRes_MO"] <- sum(track_years$ASHP_ElecRes_MO)
    track_trials[i, "ASHP_WoodCord_MO"] <- sum(track_years$ASHP_WoodCord_MO)
    track_trials[i, "ASHP_WoodPellet_MO"] <- sum(track_years$ASHP_WoodPellet_MO)
    track_trials[i, "AC_MO"] <- 0
    #### emissions costs
    track_trials[i, "NG_emissions_cost"] <- sum(track_years$NG_emissions_cost)
    track_trials[i, "HO_emissions_cost"] <- sum(track_years$HO_emissions_cost)
    track_trials[i, "P_emissions_cost"] <- sum(track_years$P_emissions_cost)
    track_trials[i, "ElecRes_emissions_cost"] <- sum(track_years$ElecRes_emissions_cost)
    track_trials[i, "WoodCord_emissions_cost"] <- sum(track_years$WoodCord_emissions_cost)
    track_trials[i, "WoodPellet_emissions_cost"] <- sum(track_years$WoodPellet_emissions_cost)
    track_trials[i, "ASHP_NG_emissions_cost"] <- sum(track_years$ASHP_NG_emissions_cost)
    track_trials[i, "ASHP_HO_emissions_cost"] <- sum(track_years$ASHP_HO_emissions_cost)
    track_trials[i, "ASHP_P_emissions_cost"] <- sum(track_years$ASHP_P_emissions_cost)
    track_trials[i, "ASHP_ElecRes_emissions_cost"] <- sum(track_years$ASHP_ElecRes_emissions_cost)
    track_trials[i, "ASHP_WoodCord_emissions_cost"] <- sum(track_years$ASHP_WoodCord_emissions_cost)
    track_trials[i, "ASHP_WoodPellet_emissions_cost"] <- sum(track_years$ASHP_WoodPellet_emissions_cost)
    track_trials[i, "AC_emissions_cost"] <- sum(track_years$AC_emissions_cost)
    #### emissions
    track_trials[i, "NG_emissions"] <- sum(track_years$NG_emissions_cost)/social_cost_of_CO2
    track_trials[i, "HO_emissions"] <- sum(track_years$HO_emissions_cost)/social_cost_of_CO2
    track_trials[i, "P_emissions"] <- sum(track_years$P_emissions_cost)/social_cost_of_CO2
    track_trials[i, "ElecRes_emissions"] <- sum(track_years$ElecRes_emissions_cost)/social_cost_of_CO2
    track_trials[i, "WoodCord_emissions"] <- sum(track_years$WoodCord_emissions_cost)/social_cost_of_CO2
    track_trials[i, "WoodPellet_emissions"] <- sum(track_years$WoodPellet_emissions_cost)/social_cost_of_CO2
    track_trials[i, "ASHP_NG_emissions"] <- sum(track_years$ASHP_NG_emissions_cost)/social_cost_of_CO2
    track_trials[i, "ASHP_HO_emissions"] <- sum(track_years$ASHP_HO_emissions_cost)/social_cost_of_CO2
    track_trials[i, "ASHP_P_emissions"] <- sum(track_years$ASHP_P_emissions_cost)/social_cost_of_CO2
    track_trials[i, "ASHP_ElecRes_emissions"] <- sum(track_years$ASHP_ElecRes_emissions_cost)/social_cost_of_CO2
    track_trials[i, "ASHP_WoodCord_emissions"] <- sum(track_years$ASHP_WoodCord_emissions_cost)/social_cost_of_CO2
    track_trials[i, "ASHP_WoodPellet_emissions"] <- sum(track_years$ASHP_WoodPellet_emissions_cost)/social_cost_of_CO2
    track_trials[i, "AC_emissions"] <- sum(track_years$AC_emissions_cost)/social_cost_of_CO2
    }
  geo[k, "heating_load"] <- heating_load
  geo[k, "cooling_load"] <- cooling_kWh
  geo[k,"NG_cool_NB"] <- mean(track_trials$ASHP_NG) - (mean(track_trials$NG) + mean(track_trials$AC))
  geo[k,"NG_cool_NB_capital"] <- mean(track_trials$ASHP_NG_capital) - (mean(track_trials$NG_capital) + mean(track_trials$AC_capital))
  geo[k,"NG_cool_NB_MO"] <- mean(track_trials$ASHP_NG_MO) - (mean(track_trials$NG_MO) + mean(track_trials$AC_MO))
  geo[k,"NG_cool_NB_emissions"] <- mean(track_trials$ASHP_NG_emissions_cost) - (mean(track_trials$NG_emissions_cost) + mean(track_trials$AC_emissions_cost))
  geo[k,"NG_cool_private"] <- geo[k,"NG_cool_NB_capital"] + geo[k,"NG_cool_NB_MO"]
  geo[k,"NG_cool_CO2e"] <- mean(track_trials$ASHP_NG_emissions) - (mean(track_trials$NG_emissions) + mean(track_trials$AC_emissions))
  geo[k,"NG_nocool_NB"] <- mean(track_trials$ASHP_NG) - mean(track_trials$NG)
  geo[k,"NG_nocool_NB_capital"] <- mean(track_trials$ASHP_NG_capital) - mean(track_trials$NG_capital)
  geo[k,"NG_nocool_NB_MO"] <- mean(track_trials$ASHP_NG_MO) - mean(track_trials$NG_MO)
  geo[k,"NG_nocool_NB_emissions"] <- mean(track_trials$ASHP_NG_emissions_cost) - mean(track_trials$NG_emissions_cost)
  geo[k,"NG_nocool_private"] <- geo[k,"NG_nocool_NB_capital"] + geo[k,"NG_nocool_NB_MO"]
  geo[k,"NG_nocool_CO2e"] <- mean(track_trials$ASHP_NG_emissions) - mean(track_trials$NG_emissions)
  geo[k,"HO_cool_NB"] <- mean(track_trials$ASHP_HO) - (mean(track_trials$HO) + mean(track_trials$AC))
  geo[k,"HO_cool_NB_capital"] <- mean(track_trials$ASHP_HO_capital) - (mean(track_trials$HO_capital) + mean(track_trials$AC_capital))
  geo[k,"HO_cool_NB_MO"] <- mean(track_trials$ASHP_HO_MO) - (mean(track_trials$HO_MO) + mean(track_trials$AC_MO))
  geo[k,"HO_cool_NB_emissions"] <- mean(track_trials$ASHP_HO_emissions_cost) - (mean(track_trials$HO_emissions_cost) + mean(track_trials$AC_emissions_cost))
  geo[k,"HO_cool_private"] <- geo[k,"HO_cool_NB_capital"] + geo[k,"HO_cool_NB_MO"]
  geo[k,"HO_cool_CO2e"] <- mean(track_trials$ASHP_HO_emissions) - (mean(track_trials$HO_emissions) + mean(track_trials$AC_emissions))
  geo[k,"HO_nocool_NB"] <- mean(track_trials$ASHP_HO) - mean(track_trials$HO)
  geo[k,"HO_nocool_NB_capital"] <- mean(track_trials$ASHP_HO_capital) - mean(track_trials$HO_capital)
  geo[k,"HO_nocool_NB_MO"] <- mean(track_trials$ASHP_HO_MO) - mean(track_trials$HO_MO)
  geo[k,"HO_nocool_NB_emissions"] <- mean(track_trials$ASHP_HO_emissions_cost) - mean(track_trials$HO_emissions_cost)
  geo[k,"HO_nocool_private"] <- geo[k,"HO_nocool_NB_capital"] + geo[k,"HO_nocool_NB_MO"]
  geo[k,"HO_nocool_CO2e"] <- mean(track_trials$ASHP_HO_emissions) - mean(track_trials$HO_emissions)
  geo[k,"P_cool_NB"] <- mean(track_trials$ASHP_P) - (mean(track_trials$P) + mean(track_trials$AC))
  geo[k,"P_cool_NB_capital"] <- mean(track_trials$ASHP_P_capital) - (mean(track_trials$P_capital) + mean(track_trials$AC_capital))
  geo[k,"P_cool_NB_MO"] <- mean(track_trials$ASHP_P_MO) - (mean(track_trials$P_MO) + mean(track_trials$AC_MO))
  geo[k,"P_cool_NB_emissions"] <- mean(track_trials$ASHP_P_emissions_cost) - (mean(track_trials$P_emissions_cost) + mean(track_trials$AC_emissions_cost))
  geo[k,"P_cool_private"] <- geo[k,"P_cool_NB_capital"] + geo[k,"P_cool_NB_MO"]
  geo[k,"P_cool_CO2e"] <- mean(track_trials$ASHP_P_emissions) - (mean(track_trials$P_emissions) + mean(track_trials$AC_emissions))
  geo[k,"P_nocool_NB"] <- mean(track_trials$ASHP_P) - mean(track_trials$P)
  geo[k,"P_nocool_NB_capital"] <- mean(track_trials$ASHP_P_capital) - mean(track_trials$P_capital)
  geo[k,"P_nocool_NB_MO"] <- mean(track_trials$ASHP_P_MO) - mean(track_trials$P_MO)
  geo[k,"P_nocool_NB_emissions"] <- mean(track_trials$ASHP_P_emissions_cost) - mean(track_trials$P_emissions_cost)
  geo[k,"P_nocool_private"] <- geo[k,"P_nocool_NB_capital"] + geo[k,"P_nocool_NB_MO"]
  geo[k,"P_nocool_CO2e"] <- mean(track_trials$ASHP_P_emissions) - mean(track_trials$P_emissions)
  geo[k,"ElecRes_cool_NB"] <- mean(track_trials$ASHP_ElecRes) - (mean(track_trials$ElecRes) + mean(track_trials$AC))
  geo[k,"ElecRes_cool_NB_capital"] <- mean(track_trials$ASHP_ElecRes_capital) - (mean(track_trials$ElecRes_capital) + mean(track_trials$AC_capital))
  geo[k,"ElecRes_cool_NB_MO"] <- mean(track_trials$ASHP_ElecRes_MO) - (mean(track_trials$ElecRes_MO) + mean(track_trials$AC_MO))
  geo[k,"ElecRes_cool_NB_emissions"] <- mean(track_trials$ASHP_ElecRes_emissions_cost) - (mean(track_trials$ElecRes_emissions_cost) + mean(track_trials$AC_emissions_cost))
  geo[k,"ElecRes_cool_private"] <- geo[k,"ElecRes_cool_NB_capital"] + geo[k,"ElecRes_cool_NB_MO"]
  geo[k,"ElecRes_cool_CO2e"] <- mean(track_trials$ASHP_ElecRes_emissions) - (mean(track_trials$ElecRes_emissions) + mean(track_trials$AC_emissions))
  geo[k,"ElecRes_nocool_NB"] <- mean(track_trials$ASHP_ElecRes) - mean(track_trials$ElecRes)
  geo[k,"ElecRes_nocool_NB_capital"] <- mean(track_trials$ASHP_ElecRes_capital) - mean(track_trials$ElecRes_capital)
  geo[k,"ElecRes_nocool_NB_MO"] <- mean(track_trials$ASHP_ElecRes_MO) - mean(track_trials$ElecRes_MO)
  geo[k,"ElecRes_nocool_NB_emissions"] <- mean(track_trials$ASHP_ElecRes_emissions_cost) - mean(track_trials$ElecRes_emissions_cost)
  geo[k,"ElecRes_nocool_private"] <- geo[k,"ElecRes_nocool_NB_capital"] + geo[k,"ElecRes_nocool_NB_MO"]
  geo[k,"ElecRes_nocool_CO2e"] <- mean(track_trials$ASHP_ElecRes_emissions) - mean(track_trials$ElecRes_emissions)
  geo[k,"wood_cool_NB"] <- (mean(track_trials$ASHP_WoodCord) + mean(track_trials$ASHP_WoodPellet))/2 - ((mean(track_trials$WoodCord) + mean(track_trials$WoodPellet))/2  + mean(track_trials$AC))
  geo[k,"wood_cool_NB_capital"] <- (mean(track_trials$ASHP_WoodCord_capital) + mean(track_trials$ASHP_WoodPellet_capital))/2 - ((mean(track_trials$WoodCord_capital) + mean(track_trials$WoodPellet_capital))/2  + mean(track_trials$AC_capital))
  geo[k,"wood_cool_NB_MO"] <- (mean(track_trials$ASHP_WoodCord_MO) + mean(track_trials$ASHP_WoodPellet_MO))/2 - ((mean(track_trials$WoodCord_MO) + mean(track_trials$WoodPellet_MO))/2  + mean(track_trials$AC_MO))
  geo[k,"wood_cool_NB_emissions"] <- (mean(track_trials$ASHP_WoodCord_emissions_cost) + mean(track_trials$ASHP_WoodPellet_emissions_cost))/2 - ((mean(track_trials$WoodCord_emissions_cost) + mean(track_trials$WoodPellet_emissions_cost))/2  + mean(track_trials$AC_emissions_cost))
  geo[k,"wood_cool_private"] <- geo[k,"wood_cool_NB_capital"] + geo[k,"wood_cool_NB_MO"]
  geo[k,"wood_cool_CO2e"] <- (mean(track_trials$ASHP_WoodCord_emissions) + mean(track_trials$ASHP_WoodPellet_emissions))/2 - ((mean(track_trials$WoodCord_emissions) + mean(track_trials$WoodPellet_emissions))/2  + mean(track_trials$AC_emissions))
  geo[k,"wood_nocool_NB"] <- (mean(track_trials$ASHP_WoodCord) + mean(track_trials$ASHP_WoodPellet))/2 - ((mean(track_trials$WoodCord) + mean(track_trials$WoodPellet))/2 )
  geo[k,"wood_nocool_NB_capital"] <- (mean(track_trials$ASHP_WoodCord_capital) + mean(track_trials$ASHP_WoodPellet_capital))/2 - ((mean(track_trials$WoodCord_capital) + mean(track_trials$WoodPellet_capital))/2 )
  geo[k,"wood_nocool_NB_MO"] <- (mean(track_trials$ASHP_WoodCord_MO) + mean(track_trials$ASHP_WoodPellet_MO))/2 - ((mean(track_trials$WoodCord_MO) + mean(track_trials$WoodPellet_MO))/2)
  geo[k,"wood_nocool_NB_emissions"] <- (mean(track_trials$ASHP_WoodCord_emissions_cost) + mean(track_trials$ASHP_WoodPellet_emissions_cost))/2 - ((mean(track_trials$WoodCord_emissions_cost) + mean(track_trials$WoodPellet_emissions_cost))/2)
  geo[k,"wood_nocool_private"] <- geo[k,"wood_nocool_NB_capital"] + geo[k,"wood_nocool_NB_MO"]
  geo[k,"wood_nocool_CO2e"] <- (mean(track_trials$ASHP_WoodCord_emissions) + mean(track_trials$ASHP_WoodPellet_emissions))/2 - ((mean(track_trials$WoodCord_emissions) + mean(track_trials$WoodPellet_emissions))/2)
  geo[k,c(127:ncol(geo))] <- geo[k,c(127:ncol(geo))]*-1
  geo[k,"perc_positive_NB_NG_cool"] <- mean((track_trials$ASHP_NG - (track_trials$NG + track_trials$AC)) < 0)
  geo[k,"perc_positive_private_NG_cool"] <- mean(((track_trials$ASHP_NG_capital + track_trials$ASHP_NG_MO) - (track_trials$NG_capital + track_trials$AC_capital + track_trials$NG_MO + track_trials$AC_MO)) < 0)
  geo[k,"perc_positive_NB_NG_nocool"] <- mean((track_trials$ASHP_NG - track_trials$NG) < 0)
  geo[k,"perc_positive_private_NG_nocool"] <- mean(((track_trials$ASHP_NG_capital + track_trials$ASHP_NG_MO) - (track_trials$NG_capital + track_trials$NG_MO)) < 0)
  geo[k,"perc_positive_NB_P_cool"] <- mean((track_trials$ASHP_P - (track_trials$P + track_trials$AC)) < 0)
  geo[k,"perc_positive_private_P_cool"] <- mean(((track_trials$ASHP_P_capital + track_trials$ASHP_P_MO) - (track_trials$P_capital + track_trials$AC_capital + track_trials$P_MO + track_trials$AC_MO)) < 0)
  geo[k,"perc_positive_NB_P_nocool"] <- mean((track_trials$ASHP_P - track_trials$P) < 0)
  geo[k,"perc_positive_private_P_nocool"] <- mean(((track_trials$ASHP_P_capital + track_trials$ASHP_P_MO) - (track_trials$P_capital + track_trials$P_MO)) < 0)
  geo[k,"perc_positive_NB_HO_cool"] <- mean((track_trials$ASHP_HO - (track_trials$HO + track_trials$AC)) < 0)
  geo[k,"perc_positive_private_HO_cool"] <- mean(((track_trials$ASHP_HO_capital + track_trials$ASHP_HO_MO) - (track_trials$HO_capital + track_trials$AC_capital + track_trials$HO_MO + track_trials$AC_MO)) < 0)
  geo[k,"perc_positive_NB_HO_nocool"] <- mean((track_trials$ASHP_HO - track_trials$HO) < 0)
  geo[k,"perc_positive_private_HO_nocool"] <- mean(((track_trials$ASHP_HO_capital + track_trials$ASHP_HO_MO) - (track_trials$HO_capital + track_trials$HO_MO)) < 0)
  geo[k,"perc_positive_NB_ElecRes_cool"] <- mean((track_trials$ASHP_ElecRes - (track_trials$ElecRes + track_trials$AC)) < 0)
  geo[k,"perc_positive_private_ElecRes_cool"] <- mean(((track_trials$ASHP_ElecRes_capital + track_trials$ASHP_ElecRes_MO) - (track_trials$ElecRes_capital + track_trials$AC_capital + track_trials$ElecRes_MO + track_trials$AC_MO)) < 0)
  geo[k,"perc_positive_NB_ElecRes_nocool"] <- mean((track_trials$ASHP_ElecRes - track_trials$ElecRes) < 0)
  geo[k,"perc_positive_private_ElecRes_nocool"] <- mean(((track_trials$ASHP_ElecRes_capital + track_trials$ASHP_ElecRes_MO) - (track_trials$ElecRes_capital + track_trials$ElecRes_MO)) < 0)
  geo[k,"perc_positive_NB_wood_cool"] <- mean(((track_trials$ASHP_WoodCord + track_trials$ASHP_WoodPellet)/2 - ((track_trials$WoodCord_capital + track_trials$WoodPellet_capital)/2 + track_trials$AC)) < 0)
  geo[k,"perc_positive_private_wood_cool"] <- mean((((track_trials$ASHP_WoodCord_capital + track_trials$ASHP_WoodPellet_capital)/2 + (track_trials$ASHP_WoodCord_MO + track_trials$ASHP_WoodPellet_MO)/2) - ((track_trials$WoodCord_capital + track_trials$WoodPellet_capital)/2 + track_trials$AC_capital + (track_trials$WoodCord_MO + track_trials$WoodPellet_MO)/2 + track_trials$AC_MO)) < 0)
  geo[k,"perc_positive_NB_wood_nocool"] <- mean(((track_trials$ASHP_WoodCord + track_trials$ASHP_WoodPellet)/2 - ((track_trials$WoodCord_capital + track_trials$WoodPellet_capital)/2)) < 0)
  geo[k,"perc_positive_private_wood_nocool"] <- mean((((track_trials$ASHP_WoodCord_capital + track_trials$ASHP_WoodPellet_capital)/2 + (track_trials$ASHP_WoodCord_MO + track_trials$ASHP_WoodPellet_MO)/2) - ((track_trials$WoodCord_capital + track_trials$WoodPellet_capital)/2 + (track_trials$WoodCord_MO + track_trials$WoodPellet_MO)/2)) < 0)
  print(k)
}

write.csv(geo, "final.csv", row.names = F)

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

# ## differences between systems per trial
# track_trials$NG_dif <- track_trials$NG - track_trials$ASHP_NG
# track_trials$HO_dif <- track_trials$HO - track_trials$ASHP_HO
# track_trials$P_dif <- track_trials$P - track_trials$ASHP_P
# 
# # means of differences between systems
# mean_NGdif <- mean(track_trials$NG_dif)
# mean_HOdif <- mean(track_trials$HO_dif)
# mean_Pdif <- mean(track_trials$P_dif)
# mean_NGdif
# mean_HOdif
# mean_Pdif
# 
# ## proportion positive differences between systems
# mean(track_trials$NG_dif > 0)
# mean(track_trials$HO_dif > 0)
# mean(track_trials$P_dif > 0)

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

  