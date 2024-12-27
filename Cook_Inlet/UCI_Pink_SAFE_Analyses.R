# Aaron Lambert, aaron.lambert@noaa.gov, 907-586-7270
# 12/27/2024

# The purpose of this script is to produce annual calculations for the Cook Inlet
# SAFE report for Upper Cook Inlet Pink salmon (Tier 3)
# Includes:
# (1) producing preseason and postseason management quantities as dictated in the FMP
# (2) determining appropriate buffers for management quantities 

# Load packages
library(forecast)
library(car)
library(boot)
library(zoo)
source('Cook_Inlet_functions.R')

# 1) Even year pinks ############################################################
# Load Data
stock <- 'UCI Pink Even'
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

# Function arguments
gen_lag <- 2
y_obj <- 2024
preseason <- TRUE
postseason <- TRUE
tier_3_buff <- seq(0, 0.9, 0.1)

# Data inputs
C_total=Table['Total.Catch']
C_EEZ=Table['EEZ.Catch']
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

# 2) Odd year pinks #############################################################
# Load Data
stock <- 'UCI Pink Odd'
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

# Function arguments
gen_lag <- 2
y_obj <- 2025
preseason <- TRUE
postseason <- FALSE
tier_3_buff <- seq(0.1, .9, 0.1)

# Data inputs
C_total=Table['Total.Catch']
C_EEZ=Table['EEZ.Catch']
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




