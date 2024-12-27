# Aaron Lambert, aaron.lambert@noaa.gov, 907-586-7270
# 12/27/2024

# The purpose of this script is to produce annual calculations for the Cook
# Inlet SAFE report for UCI Coho (Tier 3)
# Includes:
# (1) producing preseason and postseason management quantities as dictated in the FMP
# (2) determining appropriate buffers for management quantities 

# Load packages
library(forecast)
library(car)
library(boot)
source('Cook_Inlet_functions.R')

# Load Data
stock <- 'UCI Coho'
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

# Function arguments
buffer_window <- 10
gen_lag <- 4
y_obj <- 2025
preseason <- TRUE
postseason <- FALSE
F_state_forecast_method <- 'arima' #naive, or arima
run_forecast_method <- 'arima' #sibling or arima
tier_3_buff <- seq(0.1, 0.9, 0.1)

# Data inputs
C_total=Table['Total.Catch']
C_EEZ=Table['EEZ.Catch']
F_EEZ = Table$FEEZ
MFMT = Table$MFMT
ACL= Table$ACL
years=Table['Year']

# Perform Tier 3 Calculations
Tier_3_Table <- Tier_3_fun(C_total=C_total, 
                           C_EEZ=C_EEZ, 
                           years=years,
                           gen_lag=gen_lag,
                           y_obj=y_obj, 
                           buffer=tier_3_buff, 
                           catch_lag = nrow(Table),
                           preseason=preseason, 
                           postseason=postseason)



