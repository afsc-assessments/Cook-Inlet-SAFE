#Lukas DeFilippo, lukas.defilippo@noaa.gov, 781-572-8865
#11/21/2023
#The purpose of this script is to produce annual calculations for the Cook Inlet SAFE report for 'other' (non-Kenai) and 'all Chinook Tier 3
#Includes:
#(1) forecasting unknown quantities (i.e. run size, state harvest)
#(2) producing preseason and postseason management quantities as dictated in the FMP
#(3) determining appropriate buffers for management quantities 

#Load packages
library(forecast)
library(car)
library(boot)
source('Cook_Inlet_functions.R')

#Load Data
stock <- 'ALL Chinook'
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

#Function arguments
gen_lag <- 6
y_obj <- 2024
preseason <- TRUE
postseason <- FALSE
tier_3_buff <- seq(0.1, 0.9, 0.1)

#Data inputs
C_EEZ=Table['EEZ.Catch']
years=Table['Year']
C_total = rep(NA, length(C_EEZ))


#Perform Tier 3 Caclulations
Tier_3_Table <- Tier_3_fun(C_total=C_total , C_EEZ=C_EEZ, OFL=OFL, years=years,
                           gen_lag=gen_lag, y_obj=y_obj, buffer=tier_3_buff, catch_lag = nrow(Table), preseason=preseason, postseason=postseason)
