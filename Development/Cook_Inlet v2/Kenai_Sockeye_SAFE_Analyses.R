#Lukas DeFilippo, lukas.defilippo@noaa.gov, 781-572-8865
#11/21/2023
#The purpose of this script is to produce annual calculations for the Cook Inlet SAFE report for Kenai sockeye (Tier 1 + Tier 3)
#Includes:
#(1) forecasting unknown quantities (i.e. run size, state harvest)
#(2) producing preseason and postseason management quantities as dictated in the FMP
#(3) determining appropriate buffers for management quantities 

#Load packages
library(forecast)
library(car)
library(boot)
library(tidyverse)
library(ggthemes)
library(ggpubr)
library(ggtext)
source('Cook_Inlet_functions.R')

#Load Data
stock <- 'Kenai Sockeye'
#Data <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Data.csv'))
Forecast <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Forecasts.csv'))
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

#Function arguments
buffer_window <- 10
gen_lag <- 5
y_obj <- 2024
preseason <- T
postseason <- F
F_state_forecast_method <- 'arima' #naive, or arima
run_forecast_method <- 'arima' #sibling or arima
# tier_3_buff <- seq(0.1, 0.9, 0.1)

#Data inputs
C_total=Table['Total.Kenai.R..Catch']
C_EEZ=Table['Kenai.R..EEZ.Catch']
Run=Table['Run']
Esc=Table['Escapement']

# SMSY
# Esc_goal = Table$Smsy[Table$Year==max(Table$Year)]
# Esc_goal_pre = Table$Smsy[Table$Year==max(Table$Year)]

# Lwr Bound
Esc_goal = Table$Lower.Bound.of.Goal
Esc_goal_pre = Table$Lower.Bound.of.Goal[Table$Year==max(Table$Year)]

years=Table['Year']
sib_forecast = Forecast$Kenai.Total.Run.Forecast[Forecast$Year==y_obj]/1000
sib_forecast_full = Forecast$Kenai.Total.Run.Forecast[Forecast$Year<=y_obj]/1000

#Calculate OFL to ABC Buffer
buffer_ABC <- buffer_fun_ABC(buffer_window=buffer_window,
                             y_obj=y_obj,
                             sib_forecast=sib_forecast,
                             C_total=C_total,
                             C_EEZ=C_EEZ,Run=Run, 
                             Esc=Esc,
                             Esc_goal=Esc_goal_pre,
                             years=years, 
                             gen_lag=gen_lag,
                             F_state_forecast_method=F_state_forecast_method,
                             run_forecast_method=run_forecast_method)

#Perform Tier 1 Calculations
Tier_1_Table <- Tier_1_fun(y_obj=y_obj, 
                           sib_forecast=sib_forecast, 
                           C_total=C_total, 
                           C_EEZ=C_EEZ,
                           Run=Run, 
                           Esc=Esc,
                           Esc_goal=Esc_goal, 
                           Esc_goal_pre=Esc_goal_pre, 
                           years=years, 
                           ABC_buffer=buffer_ABC$buffer, 
                           buffer_ABC = buffer_ABC,#ABC_buffer=0.478, 
                           preseason = preseason, 
                           postseason=postseason, 
                           plot = F,
                           # run_forecast_method= "arima",
                           gen_lag=gen_lag, 
                           F_state_forecast_method=F_state_forecast_method, 
                           run_forecast_method=run_forecast_method,
                           sib_forecast_full = sib_forecast_full)

#Perform Tier 3 Caclulations
# Tier_3_Table <- Tier_3_fun(C_total=C_total , 
#                            C_EEZ=C_EEZ, 
#                            years=years,
#                            gen_lag=gen_lag,
#                            y_obj=y_obj,
#                            buffer=tier_3_buff, 
#                            catch_lag = nrow(Table), 
#                            preseason=preseason, 
#                            postseason=postseason)
Tier_1_Table
