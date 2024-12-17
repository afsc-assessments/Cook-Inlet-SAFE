#Aaron Lambert, aaron.lambert@noaa.gov, 907-586-7270
#11/21/2023

# This is a reworking of code from Lukas DeFelipo 
#
#The purpose of this script is to produce annual calculations for the Cook Inlet SAFE report for Kasilof sockeye (Tier 1 + Tier 3)
#Includes:
#(1) forecasting unknown quantities (i.e. run size, state harvest)
#(2) producing preseason and postseason management quantities as dictated in the FMP
#(3) determining appropriate buffers for management quantities 

#Load packages
library(tidyverse)
library(forecast)
library(car)
library(boot)
library(zoo)
source('Cook_Inlet_functions.R')

#Load Data
stock <- 'Kasilof Sockeye'
# Data <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Data.csv'))
Forecast <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Forecasts.csv'))
data.df <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

#Function arguments
buffer_window <- 10
gen_lag <- 5
y_obj <- 2024
preseason <- TRUE
postseason <-TRUE
F_state_forecast_method <- 'arima' #naive, or arima
run_forecast_method <- 'arima' #sibling or arima
tier_3_buff <- seq(0.1, 0.9, 0.1)

# Data Section 

# This is the SSC prescribed lower bound of the escapement goal
Esc_goal_pre = (222000/1000)

# Join the State sibling forecast to the main dataframe
data.df <- data.df %>% 
  left_join( Forecast) %>% 
  rename(Sib.Forecast = Run.Forecast) # Rename forecast as sib forecast
  

# Preseason Analysis ###########################################################
# First, Calculate the Tier 1 Buffer ###########################################

# Post season OFL in the buffer windo
Postseason_OFL <- data.df$Potential.Yield.EEZ[data.df$Year %in% 
                                                (y_obj - buffer_window):(y_obj-1)]

Preseason_OFL <- vector(length = length(Postseason_OFL))

comp.years <- (y_obj-buffer_window):(y_obj-1)

i= 2014
for(i in comp.years){

  t.series <-data.df$Run[data.df$Year <= i]
  
  PF_run <- forecast(auto.arima(t.series), h = 1)
  
  PF_run$mean
 
  
}



# Loop over years to get the preseason OFL




##################################################################################




#Calculate OFL to ABC Buffer
buffer_ABC <- buffer_fun_ABC(buffer_window=buffer_window, 
                             y_obj=y_obj, #+1 if creating comparison plots
                             sib_forecast=sib_forecast,
                             C_total=C_total,
                             C_EEZ=C_EEZ,
                             Run=Run, Esc=Esc, 
                             Esc_goal=Esc_goal, 
                             years=years, 
                             gen_lag=gen_lag, 
                             F_state_forecast_method=F_state_forecast_method, 
                             run_forecast_method='arima')

#Perform Tier 1 Calculations
Tier_1_Table <- Tier_1_fun(y_obj=y_obj, 
                           sib_forecast=sib_forecast, 
                           C_total=C_total, 
                           C_EEZ=C_EEZ,Run=Run, 
                           Esc=Esc, 
                           Esc_goal=Esc_goal, 
                           Esc_goal_pre=Esc_goal_pre, 
                           years=years, 
                           ABC_buffer=0.694,
                           preseason = preseason, 
                           postseason=postseason, 
                           gen_lag=gen_lag, 
                           F_state_forecast_method=F_state_forecast_method, 
                           run_forecast_method='arima')


#Plot sibling vs arima model
#Do retrospective forecasting using the buffer function + 1 year
#Calculate OFL to ABC Buffer
buffer_ABC_retro <- buffer_fun_ABC(buffer_window=buffer_window, 
                                   y_obj=y_obj, #+1 if creating comparison plots
                                   sib_forecast=sib_forecast,
                                   C_total=C_total, 
                                   C_EEZ=C_EEZ,
                                   Run=Run, 
                                   Esc=Esc, 
                                   Esc_goal=Esc_goal, 
                                   years=years, 
                                   gen_lag=gen_lag, 
                                   F_state_forecast_method=F_state_forecast_method,
                                   run_forecast_method='arima')



#abundance
# png(file=paste0(getwd(),'/',stock,'/',y_obj,'_forecast_plot.png'))
# par(oma=c(5,1,5,0), mar=c(5,4,2,1), mfrow=c(1,2))
# plot(Tier_1_Table$PostSeason_Table$Year, 
#      Tier_1_Table$PostSeason_Table$Run, 
#      ylim=c(min(Tier_1_Table$PostSeason_Table$Run,
#                 buffer_ABC_retro$Preseason_run, 
#                 Forecast$Run.Forecast/1000), 
#             max(Tier_1_Table$PostSeason_Table$Run,buffer_ABC_retro$Preseason_run,
#                 Forecast$Run.Forecast/1000)), 
#      type='l', 
#      lty=3, 
#      ylab='Run size', 
#      xlab='Year')
# points(rev(buffer_ABC_retro$yr), 
#        buffer_ABC_retro$Preseason_run, 
#        type='o', 
#        col='red')
# points(Forecast$Year, 
#        Forecast$Run.Forecast/1000, 
#        col='blue', type='o')
# legend(x=Tier_1_Table$PostSeason_Table$Year[9.5], y=max(Tier_1_Table$PostSeason_Table$Run,buffer_ABC_retro$Preseason_run, Forecast$Run.Forecast/1000)*1.05, legend=c('Postseason', 'Sibling', 'Arima'), col=c('black', 'blue', 'red'), lty=c(3,1,1), pch=c(NA,1,1), bty='n')

#accuracy
sib_LAR <- buffer_fun_ABC(buffer_window=buffer_window, y_obj=y_obj+1, sib_forecast=sib_forecast,
                          C_total=C_total, C_EEZ=C_EEZ,Run=Run, Esc=Esc, Esc_goal=Esc_goal, years=years, 
                          gen_lag=gen_lag, F_state_forecast_method=F_state_forecast_method, run_forecast_method='sibling')


arima_LAR <- buffer_fun_ABC(buffer_window=buffer_window, y_obj=y_obj+1, sib_forecast=sib_forecast,
                            C_total=C_total, C_EEZ=C_EEZ,Run=Run, Esc=Esc, Esc_goal=Esc_goal, years=years, 
                            gen_lag=gen_lag, F_state_forecast_method=F_state_forecast_method, run_forecast_method='arima')
# plot(rev(sib_LAR$yr), sib_LAR$LAR, type='o', ylim=c(min(sib_LAR$LAR,arima_LAR$LAR), max(sib_LAR$LAR,arima_LAR$LAR)), xlab='Year', ylab='Log accuracy ratio (LAR)')
# points(rev(arima_LAR$yr), arima_LAR$LAR, type='o', col='red')
# abline(h=0)
# legend(x=rev(sib_LAR$yr)[5], y= max(sib_LAR$LAR,arima_LAR$LAR)*0.95, bty='n', lty=1, pch=1, col=c('black', 'red'), legend=c('Sibling', 'Arima'))
# main=paste0(stock)
# dev.off()
