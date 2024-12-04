#Lukas DeFilippo, lukas.defilippo@noaa.gov, 781-572-8865
#11/21/2023
#The purpose of this script is to produce annual calculations for the Cook Inlet SAFE report for Kasilof sockeye (Tier 1 + Tier 3)
#Includes:
#(1) forecasting unknown quantities (i.e. run size, state harvest)
#(2) producing preseason and postseason management quantities as dictated in the FMP
#(3) determining appropriate buffers for management quantities 


#Load packages
library(forecast)
library(car)
library(boot)
library(zoo)
library(tidyverse)
library(ggthemes)
library(ggtext)
library(ggpubr)
source('Cook_Inlet_functions.R')

#Load Data
stock <- 'Kasilof Sockeye'
# Data <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Data.csv'))
Forecast <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Forecasts.csv'))
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

#Function arguments
buffer_window <- 10
gen_lag <- 5
y_obj <- 2025
preseason <- TRUE
postseason <- FALSE
F_state_forecast_method <- 'arima' #naive, or arima
run_forecast_method <- 'arima' #sibling or arima
# tier_3_buff <- seq(0.1, 0.9, 0.1)

#Data inputs
C_total=Table['Total.Kasilof.R..Catch']
C_EEZ=Table['Kasilof.R..EEZ.Catch']
Run=Table['Run']
Esc=Table['Escapement']

Esc_goal=Table$Lower.Bound.of.Goal
Esc_goal_pre = Table$Lower.Bound.of.Goal[Table$Year==max(Table$Year)]
# SMSY
# Esc_goal = (222000/1000)
# Esc_goal_pre = (222000/1000)
#Esc_goal_pre = Esc_goal
years=Table['Year']
sib_forecast = Forecast$Run.Forecast[Forecast$Year==y_obj]/1000
# sib_forecast_full = Forecast
sib_forecast_full <- Forecast$Run.Forecast[Forecast$Year<=y_obj]/1000

#Calculate OFL to ABC Buffer
buffer_ABC <- buffer_fun_ABC(buffer_window=buffer_window, 
                             y_obj=y_obj, #+1 if creating comparison plots
                             sib_forecast=sib_forecast,
                             C_total=C_total,
                             C_EEZ=C_EEZ,
                             Run=Run, Esc=Esc, 
                             Esc_goal=Esc_goal_pre, 
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
                           ABC_buffer=buffer_ABC$buffer,
                           buffer_ABC = buffer_ABC,
                           preseason = preseason, 
                           postseason=postseason, 
                           gen_lag=gen_lag, 
                           F_state_forecast_method=F_state_forecast_method, 
                           run_forecast_method=run_forecast_method,
                           sib_forecast_full = sib_forecast_full)


#Plot sibling vs arima model
#Do retrospective forecasting using the buffer function + 1 year
#Calculate OFL to ABC Buffer
buffer_ABC_retro <- buffer_fun_ABC(buffer_window=buffer_window, 
                                   y_obj=y_obj+1, #+1 if creating comparison plots
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







# Plot comparing Sib forecast, AR1 forecast, and runsize
AR_PF <- data.frame("Year" = buffer_ABC$yr, 
                    "AR1" = buffer_ABC$Preseason_run)

Forecast_full <- left_join(Forecast, AR_PF)

Forecast_full <- left_join(Forecast_full, Table[,c("Year", "Run")])

Forecast_full$Run.Forecast <- Forecast_full$Run.Forecast/1000

Forecast_sub <- Forecast_full[Forecast_full$Year%in%buffer_ABC$yr,]

MAPE_AR <- 1/length(buffer_ABC$yr)*sum(abs((Forecast_sub$Run - Forecast_sub$AR1)/
                                         Forecast_sub$Run), na.rm = T)*100

MAPE_Sib <- 1/length(buffer_ABC$yr)*sum(abs((Forecast_sub$Run - Forecast_sub$Run.Forecast)/
                                              Forecast_sub$Run), na.rm = T)*100

#Color blind colors for plotting
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(Forecast_full, aes(x = Year, y = Run))+
  geom_point(aes(col = "Run Size"))+
  geom_line(aes(col = "Run Size"))+
  geom_point(aes(y = AR1, col = "AR1"))+
  geom_line(aes(y = AR1, col = "AR1"))+
  geom_point(aes(y = Run.Forecast, col = "Sib"))+
  geom_line(aes(y = Run.Forecast, col = "Sib"))+
  coord_cartesian(ylim = c(0,2000),
                  xlim = c(2012,2025))+
  scale_x_continuous(breaks = seq(from=2000, to = 2050, by = 5))+
  # scale_size_manual(guide = "none", values = c(2,4))+
  scale_color_manual(name="", 
                     values = colorBlindBlack8[c(1,2,3,7,7)],
                     breaks = c("AR1", "Sib", "Run Size"))+
  # guides(color = guide_legend(override.aes = list(size=c(2,4,2,2,2))))+
  theme_clean()+
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14), 
        plot.background = element_blank(), 
        legend.background = element_blank() )


#abundance
# png(file=paste0(getwd(),'/',stock,'/',y_obj,'_forecast_plot.png'))
par(oma=c(5,1,5,0), mar=c(5,4,2,1), mfrow=c(1,2))
plot(Tier_1_Table$PostSeason_Table$Year,
     Tier_1_Table$PostSeason_Table$Run,
     ylim=c(min(Tier_1_Table$PostSeason_Table$Run,
                buffer_ABC_retro$Preseason_run,
                Forecast$Run.Forecast/1000),
            max(Tier_1_Table$PostSeason_Table$Run,buffer_ABC_retro$Preseason_run,
                Forecast$Run.Forecast/1000)),
     xlim = c(1999, y_obj+1),
     type='l',
     lty=3,
     ylab='Run size',
     xlab='Year')
points(rev(buffer_ABC_retro$yr),
       buffer_ABC_retro$Preseason_run,
       type='o',
       col='red')
points(Forecast$Year,
       Forecast$Run.Forecast/1000,
       col='blue', type='o')
legend(x=Tier_1_Table$PostSeason_Table$Year[9.5], 
       y=max(Tier_1_Table$PostSeason_Table$Run,buffer_ABC_retro$Preseason_run,
             Forecast$Run.Forecast/1000)*1.05, 
       legend=c('Postseason', 'Sibling', 'Arima'), 
       col=c('black', 'blue', 'red'), 
       lty=c(3,1,1), 
       pch=c(NA,1,1),
       bty='n')

#accuracy
sib_LAR <- buffer_fun_ABC(buffer_window=buffer_window, 
                          y_obj=y_obj, 
                          sib_forecast=sib_forecast,
                          C_total=C_total, 
                          C_EEZ=C_EEZ,
                          Run=Run, 
                          Esc=Esc, 
                          Esc_goal=Esc_goal, 
                          years=years, 
                          gen_lag=gen_lag, 
                          F_state_forecast_method=F_state_forecast_method, 
                          run_forecast_method='sibling')


arima_LAR <- buffer_fun_ABC(buffer_window=buffer_window, 
                            y_obj=y_obj, 
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

plot(rev(sib_LAR$yr), sib_LAR$LAR, type='o', ylim=c(min(sib_LAR$LAR,arima_LAR$LAR), max(sib_LAR$LAR,arima_LAR$LAR)), xlab='Year', ylab='Log accuracy ratio (LAR)')
points(rev(arima_LAR$yr), arima_LAR$LAR, type='o', col='red')
abline(h=0)
legend(x=rev(sib_LAR$yr)[5], y= max(sib_LAR$LAR,arima_LAR$LAR)*0.95, bty='n', lty=1, pch=1, col=c('black', 'red'), legend=c('Sibling', 'Arima'))
main=paste0(stock)
dev.off()






buffer_ABC$Preseason_run
Table$Run
