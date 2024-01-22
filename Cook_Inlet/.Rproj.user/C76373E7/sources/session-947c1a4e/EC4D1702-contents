#Lukas DeFilippo, lukas.defilippo@noaa.gov, 781-572-8865
#11/21/2023
#The purpose of this script is to produce annual calculations for the Cook Inlet SAFE report for UCI 'Other' sockeye (Tier 1 + Tier 3)
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
stock <- 'UCI Sockeye'
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

#Function arguments
ACL_buffer_window <- 5
gen_lag <- 5
y_obj <- 2021
preseason <- TRUE
postseason <- TRUE
F_state_forecast_method <- 'arima' #naive, or arima
run_forecast_method <- 'arima' #sibling or arima
tier_3_buff <- 0.9

#Data inputs
C_total=Table['Total.Catch']
C_EEZ=Table['EEZ.Catch']
F_EEZ = Table$FEEZ
MFMT = Table$MFMT
ACL= Table$ACL
Run=Table['Run']
Esc=Table['Esc']
Esc_goal=Table['Esc.Goal']
years=Table['Year']
sib_forecast <- NULL

#Calculate ACL Buffer
#Now execute the buffer function to emprically determine the buffer based on retrospective skill
buffer <- buffer_fun(ACL_buffer_window=ACL_buffer_window, y_obj=y_obj, sib_forecast=sib_forecast,
                     C_total=C_total, C_EEZ=C_EEZ,F_EEZ = F_EEZ, MFMT = MFMT,
                     ACL= ACL,Run=Run, Esc=Esc, Esc_goal=Esc_goal, years=years, 
                     gen_lag=gen_lag, F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method)

#Perform Tier 1 Calculations
Tier_1_Table <- Tier_1_fun(y_obj=y_obj, 
                           C_total=C_total, C_EEZ=C_EEZ,F_EEZ = F_EEZ, MFMT = MFMT,sib_forecast=sib_forecast,
                           ACL= ACL,Run=Run, Esc=Esc, Esc_goal=Esc_goal, years=years, ACL_buffer=buffer$buffer, preseason = preseason, postseason=postseason, 
                           gen_lag=gen_lag, F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method)

#Perform Tier 3 Caclulations
Tier_3_Table <- Tier_3_fun(C_total=C_total , C_EEZ=C_EEZ, OFL=OFL, years=years,
                           gen_lag=gen_lag, y_obj=y_obj, buffer=tier_3_buff, catch_lag = nrow(Table), preseason=preseason, postseason=postseason)
