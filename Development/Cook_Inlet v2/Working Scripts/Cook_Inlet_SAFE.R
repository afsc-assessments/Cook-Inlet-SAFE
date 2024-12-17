#Lukas DeFilippo, lukas.defilippo@noaa.gov, 781-572-8865
#11/01/2023
#The purpose of this script is to produce annual calculations for the Cook Inlet SAFE report
#Includes:
#(1) forecasting unknown quantities (i.e. run size, state harvest)
#(2) producing preseason and postseason management quantities as dictated in the FMP
#(3) determining appropriate buffers for management quantities 

#Load packages
library(forecast)
library(car)
library(boot)

#Load Tier 1 example data: Kenai sockeye
stock <- 'Kenai Sockeye'
Data <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Data.csv'))
Forecast <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Forecasts.csv'))
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

#Function to do all FMP calculations for Tier 1 stocks
Tier_1_fun <- function(C_total , C_EEZ, Run, Esc, Esc_goal, years, F_EEZ, MFMT, ACL, ACL_buffer=1,
                       gen_lag, y_obj, preseason=TRUE, postseason=TRUE, F_state_forecast_method=NULL, run_forecast_method=NULL){
  #Preseason calculations
  if(preseason==TRUE){
    #Re-construct table for calculations (this is in the event that the incoming data is not already packaged in a neat table from the state)
    #In practice, this could simply be loaded from the previous years' postseason table
    base_table <- cbind(years, Run, Esc, C_total, C_EEZ, F_EEZ, MFMT, ACL, Esc_goal)
    colnames(base_table) <- c('Year', 'Run', 'Esc', 'C_total', 'C_EEZ', 'F_EEZ', 'MFMT', 'ACL', 'Esc_goal')
    base_table$C_state <- base_table$C_total - base_table$C_EEZ
    base_table$F_state <- base_table$C_state/base_table$Run
    
    #Moving average of state harvest rate (option for forecasting)
    base_table$F_bar_state <- NA
    for(i in gen_lag:nrow(base_table)){
      base_table$F_bar_state[i] <- mean(base_table$F_state[(i-gen_lag+1):i])
    }
    
    #Withold any data from the year for which preseason calculations are being performed or later
    base_table <- subset(base_table, base_table$Year < y_obj) #This wouldn't be necessary in practice but for a retrospective exercise we need to withold 2021 values
    #Create column for potential yield
    base_table$Potential_Yield_EEZ <- base_table$Run - base_table$Esc_goal - (base_table$C_total - base_table$C_EEZ)
    base_table$Potential_Yield_EEZ[base_table$Potential_Yield_EEZ < 0] <- 0
    
    #Produce forecasts of state harvest (Fbar)
    if(F_state_forecast_method=='naive'){
        F_state_preseason <- base_table$F_bar_state[base_table$Year==y_obj-1]
    }
    if(F_state_forecast_method=='arima'){
      F_state_forecast <- forecast(auto.arima(logit(base_table$F_state), d=1), h=1)
      F_state_forecast_mean <- inv.logit(as.numeric(F_state_forecast$mean))
      F_state_forecast_80_CI <- c(inv.logit(as.numeric(F_state_forecast$lower))[1], inv.logit(as.numeric(F_state_forecast$upper))[1])
      F_state_forecast_95_CI <- c(inv.logit(as.numeric(F_state_forecast$lower))[2], inv.logit(as.numeric(F_state_forecast$upper))[2])
      F_state_preseason <- F_state_forecast_mean
    }

    #Produce/load forecast of run size
    #If a sibling forecast is possible (state data made available), (1) load the forecasted run size (use published value for this exercise)
    if(run_forecast_method=='sibling'){
      Forecast <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Forecasts.csv')) #Modify this to wherever we would store future sibling forecasts
      run_forecast_mean <- Forecast$Kenai.Total.Run.Forecast[Forecast$Year==y_obj]/1000 #Grab target year, adjust for units --> NEED TO MAKE COLUMN NAME GENERALIZABLE
      run_forcast_80_CI <- NULL #if/when we take over the sibling forecasting we will be able to pull the uncertainty estimates
      run_forcast_95_CI <- NULL
      run_preseason <- run_forecast_mean
    }
    #Otherwise, (2) perform arims forecast on run size
    if(run_forecast_method=='arima'){
      run_forecast <- forecast(auto.arima(log(base_table$Run), d=1), h=1)
      run_forecast_mean <-  exp(as.numeric(run_forecast$mean))
      run_forecast_80_CI <- c(exp(as.numeric(run_forecast$lower))[1], exp(as.numeric(run_forecast$upper))[1])
      run_forecast_95_CI <- c(exp(as.numeric(run_forecast$lower))[2], exp(as.numeric(run_forecast$upper))[2])
      run_preseason <- run_forecast_mean
    }
    
    #Calculate preseason potential yield for the upcoming year using the forecasted run size
    Potential_Yield_preseason <- (run_preseason - base_table$Esc_goal[length(base_table$Esc_goal)])
    Potential_Yield_EEZ_preseason <- Potential_Yield_preseason - (run_preseason*F_state_preseason)

    
    #Calculate preseason fishing mortality
    F_EEZ_preseason <- (sum(base_table$C_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + Potential_Yield_EEZ_preseason)/
                       (sum(base_table$Run[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + run_preseason)

    #Calculate ACL (preseason)
    ACL_preseason <- Potential_Yield_EEZ_preseason + sum(base_table$Potential_Yield_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)])
    ACL_preseason_no_buff <- ACL_preseason
    ACL_preseason <- ACL_preseason*ACL_buffer

    #Calculate OFL (preseason)
    OFL_preseason <- ACL_preseason - sum(base_table$C_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)])
    
    MFMT_preseason <- (sum(base_table$Potential_Yield_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + Potential_Yield_EEZ_preseason)/
                      (sum(base_table$Run[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + run_preseason)
   
   #Compile preseason quantities 
   Preseason_table <- cbind(F_state_preseason, run_preseason, Potential_Yield_preseason, Potential_Yield_EEZ_preseason, F_EEZ_preseason, ACL_preseason, ACL_preseason_no_buff, OFL_preseason, MFMT_preseason)
   colnames(Preseason_table) <-c('F_state_preseason', 'run_preseason', 'Potential_Yield', 'Potential_Yield_EEZ', 'F_EEZ', 'ACL', 'ACL_no_buffer', 'OFL', 'MFMT')
   
   #Write to .csv
   write.csv(Preseason_table, file=paste0(getwd(),'/',stock,'/', y_obj, '_preseason_table.csv'))
   write.csv(base_table, file=paste0(getwd(),'/',stock,'/', y_obj, '_historical_table.csv'))
   
   #Preseason plots
   pdf(file=paste0(getwd(),'/',stock,'/', y_obj, '_preseason_plots.pdf'))
   par(mfrow=c(3,2), mar=c(2,2,2,2), oma=c(2,2,2,2))
   # 1) State harvest
   if(F_state_forecast_method=='arima'){
     plot(base_table$Year, base_table$F_state, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
          ylim=c(min(base_table$F_state, F_state_forecast_95_CI[1]), max(base_table$F_state, F_state_forecast_95_CI[2])))
     points(y_obj, F_state_forecast_mean , col='red', pch=16, type='o', cex=1.1)
     arrows(x0=y_obj, x1=y_obj, y0= F_state_forecast_80_CI[1], y1= F_state_forecast_80_CI[2], col='red', length=0, lwd=2)
     arrows(x0=y_obj, x1=y_obj, y0= F_state_forecast_95_CI[1], y1= F_state_forecast_95_CI[2], col='red', length=0.05, lwd=1, angle=90, code=3)
     mtext(side=2, 'State waters harvest rate', line=2.5, cex=0.85)
   }
   if(F_state_forecast_method == 'naive'){
     plot(base_table$Year, base_table$F_state, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1))
     points(y_obj, F_state_preseason , col='red', pch=16, type='o', cex=1.1)
     points(base_table$Year, base_table$F_bar_state, type='l', lty=2)
     mtext(side=2, 'State waters harvest rate', line=2.5, cex=0.85)
   }

   # 2) Run forecast
   if(run_forecast_method=='arima'){
     plot(base_table$Year, base_table$Run, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
     ylim=c(min(base_table$Run, run_forecast_95_CI[1]), max(base_table$Run, run_forecast_95_CI[2])))
     points(y_obj, run_forecast_mean , col='red', pch=16, type='o', cex=1.1)
     arrows(x0=y_obj, x1=y_obj, y0= run_forecast_80_CI[1], y1= run_forecast_80_CI[2], col='red', length=0, lwd=2)
     arrows(x0=y_obj, x1=y_obj, y0= run_forecast_95_CI[1], y1= run_forecast_95_CI[2], col='red', length=0.05, lwd=1, angle=90, code=3)
     mtext(side=2, 'Run size', line=2.5, cex=0.85)
  }
  if(run_forecast_method=='sibling'){
    plot(base_table$Year, base_table$Run, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
         ylim=c(min(base_table$Run, run_preseason), max(base_table$Run, run_preseason)))
    points(y_obj, run_preseason , col='red', pch=16, type='o', cex=1.1)
    mtext(side=2, 'Run size', line=2.5, cex=0.85)
  }
  
   # 3) Potential Yield
   plot(base_table$Year, base_table$Potential_Yield_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
        ylim=c(min(base_table$Potential_Yield_EEZ, Potential_Yield_EEZ_preseason), max(base_table$Potential_Yield_EEZ, Potential_Yield_EEZ_preseason)))
   points(y_obj,Potential_Yield_EEZ_preseason , col='red', pch=16, type='o', cex=1.1)
   mtext(side=2, 'Potential Yield EEZ', line=2.5, cex=0.85)
   
   # 4) F_EEZ Preseason
   plot(base_table$Year, base_table$F_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
        ylim=c(min(base_table$F_EEZ, F_EEZ_preseason, na.rm=TRUE), max(base_table$F_EEZ, F_EEZ_preseason, na.rm=TRUE )))
   points(y_obj,F_EEZ_preseason , col='red', pch=16, type='o', cex=1.1)
   mtext(side=2, 'F EEZ', line=2.5, cex=0.85)
   
   # 5) ACL Preseason
   plot(base_table$Year, base_table$ACL, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
        ylim=c(min(base_table$ACL, ACL_preseason, na.rm=TRUE), max(base_table$ACL, ACL_preseason, na.rm=TRUE)))
   points(y_obj,ACL_preseason , col='red', pch=16, type='o', cex=1.1)
   mtext(side=2, 'ACL', line=2.5, cex=0.85)
   
   # 6) MFMT Preseason
   plot(base_table$Year, base_table$MFMT, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
        ylim=c(min(base_table$MFMT, MFMT_preseason, na.rm = TRUE), max(base_table$MFMT, MFMT_preseason, na.rm=TRUE)))
   points(y_obj,MFMT_preseason , col='red', pch=16, type='o', cex=1.1)
   mtext(side=2, 'MFMT', line=2.5, cex=0.85)
   dev.off()
  }
  #Postseason calculations
  if(postseason==TRUE){
    #Load preseason table to plot preseason values against actual values
    Preseason_table <- read.csv(file=paste0(getwd(),'/',stock,'/', y_obj, '_preseason_table.csv'))

     #Re-construct table for calculations (this is in the event that the incoming data is not already packaged in a neat table from the state)
     Postseason_table <- cbind(years, Run, Esc, C_total, C_EEZ, Esc_goal)
     colnames(Postseason_table) <- c('Year', 'Run', 'Esc', 'C_total', 'C_EEZ', 'Esc_goal')
     Postseason_table$C_state <- Postseason_table$C_total - Postseason_table$C_EEZ
     Postseason_table$F_state <- Postseason_table$C_state/Postseason_table$Run
     
     #Moving average of state harvest rate (option for forecasting)
     Postseason_table$F_bar_state <- NA
     for(i in gen_lag:nrow(Postseason_table)){
       Postseason_table$F_bar_state[i] <- mean(Postseason_table$F_state[(i-gen_lag+1):i])
     }
     
     Postseason_table <- subset(Postseason_table, Postseason_table$Year < (y_obj + 1))
    #Calculate potential/allowable yield in the EEZ
    Postseason_table$Potential_Yield_EEZ <- Postseason_table$Run - Postseason_table$Esc_goal - (Postseason_table$C_total - Postseason_table$C_EEZ)
    for(i in 1:nrow(Postseason_table)){
      Postseason_table$Potential_Yield_EEZ[i] <- max(Postseason_table$Potential_Yield_EEZ[i], 0)
    }

    #Calculate F EEZ
    Postseason_table$F_EEZ <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$F_EEZ[i] <- sum(Postseason_table$C_EEZ[(i-gen_lag+1):i])/sum(Postseason_table$Run[(i-gen_lag+1):i])
    }
    
    #Calculate MFMT
    Postseason_table$MFMT <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$MFMT[i] <- sum(Postseason_table$Potential_Yield_EEZ[(i-gen_lag+1):i])/
                            sum(Postseason_table$Run[(i-gen_lag+1):i])
    }
    
    #Calculate MSST
    Postseason_table$MSST <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$MSST[i] <- sum(Postseason_table$Esc_goal[(i-gen_lag+1):i])/2
    }
    
    #Calculate cumulative escapement
    Postseason_table$Cum_Esc <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$Cum_Esc[i] <- sum(Postseason_table$Esc[(i-gen_lag+1):i]) 
    }
    
    #Overfished? Compare MSST to spawning escapment
    Postseason_table$Overfished <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      if(Postseason_table$Cum_Esc[i] < Postseason_table$MSST[i]){
        Postseason_table$Overfished[i] <- 'YES'
      }else{
        Postseason_table$Overfished[i] <- 'NO'
      }
    }
    
    #Overfishing? Compare F_EEZ to MFMT
    Postseason_table$Overfishing <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      if(Postseason_table$F_EEZ[i] > Postseason_table$MFMT[i]){
        Postseason_table$Overfishing[i] <- 'YES'
      }else{
        Postseason_table$Overfishing[i] <- 'NO'
      }
    }
   
    #Calculate ACL (postseason)
    Postseason_table$ACL <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$ACL[i] <- sum(Postseason_table$Potential_Yield_EEZ[(i-gen_lag+1):i])  
    }
    
    #Calculate cumulative catch
    Postseason_table$Cum_Catch <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$Cum_Catch[i] <- sum(Postseason_table$C_EEZ[(i-gen_lag+1):i])  
    }
    
    #Was ACL exceeded?
    Postseason_table$ACL_exceeded <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      if(Postseason_table$Cum_Catch[i] > Postseason_table$ACL[i]){
        Postseason_table$ACL_exceeded[i] <- 'YES'
      }else{
        Postseason_table$ACL_exceeded[i] <- 'NO'
      }
    }
    
    #Write to csv
    write.csv(Postseason_table, file=paste0(getwd(),'/',stock,'/',y_obj,'_postseason_table.csv'))
    
    #Postseason plots
    pdf(file=paste0(getwd(),'/',stock,'/', y_obj,'_postseason_plots.pdf'))
    par(mfrow=c(3,2), mar=c(2,2,2,2), oma=c(2,2,2,2))
    # 1) State harvest
      plot(Postseason_table$Year, Postseason_table$F_state, type='o', col='black', pch=16, xlab=NA, ylab=NA,
           ylim=c(min(Postseason_table$F_state, Preseason_table$F_state_preseason), max(Postseason_table$F_state, Preseason_table$F_state_preseason)))
      points(y_obj, Preseason_table$F_state_preseason , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'Proportion state harvest', line=2.5, cex=0.85)

      # 2) Run
      plot(Postseason_table$Year, Postseason_table$Run, type='o', col='black', pch=16, xlab=NA, ylab=NA,
           ylim=c(min(Postseason_table$Run, Preseason_table$run_preseason), max(Postseason_table$Run, Preseason_table$run_preseason)))
      points(y_obj, Preseason_table$run_preseason , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'Run size', line=2.5, cex=0.85)
      
      # 3) Potential Yield
      plot(Postseason_table$Year, Postseason_table$Potential_Yield_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA,
           ylim=c(min(Postseason_table$Potential_Yield_EEZ, Preseason_table$Potential_Yield_EEZ, na.rm=TRUE), max(Postseason_table$Potential_Yield_EEZ, Preseason_table$Potential_Yield, na.rm=TRUE)))
      points(y_obj,Preseason_table$Potential_Yield , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'Potential Yield EEZ', line=2.5, cex=0.85)
      
      # 4) F_EEZ
      plot(Postseason_table$Year, Postseason_table$F_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, 
           ylim=c(min(Postseason_table$F_EEZ, Preseason_table$F_EEZ, na.rm=TRUE), max(Postseason_table$F_EEZ, Preseason_table$F_EEZ, na.rm=TRUE )))
      points(y_obj,Preseason_table$F_EEZ , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'F EEZ', line=2.5, cex=0.85)
      
      # 5) ACL Preseason
      plot(Postseason_table$Year, Postseason_table$ACL, type='o', col='black', pch=16, xlab=NA, ylab=NA, 
           ylim=c(min(Postseason_table$ACL, Preseason_table$ACL, na.rm=TRUE), max(Postseason_table$ACL, Preseason_table$ACL, na.rm=TRUE)))
      points(y_obj, Preseason_table$ACL , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'ACL', line=2.5, cex=0.85)
      
      # 6) MFMT Preseason
      plot(Postseason_table$Year, Postseason_table$MFMT, type='o', col='black', pch=16, xlab=NA, ylab=NA,
           ylim=c(min(Postseason_table$MFMT, Preseason_table$MFMT, na.rm = TRUE), max(Postseason_table$MFMT, Preseason_table$MFMT, na.rm=TRUE)))
      points(y_obj,Preseason_table$MFMT , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'MFMT', line=2.5, cex=0.85)
      dev.off()
    }
  #Create storage list to return output
  return_list <- list()
  if(preseason==TRUE){
    return_list$Preseason_Table <- Preseason_table
  }
  if(postseason==TRUE){
    return_list$PostSeason_Table <- Postseason_table
  }
  return(return_list)
}

Tier_1_Table <- Tier_1_fun(C_total=Table['Total.Kenai.R..Catch'], C_EEZ=Table['Kenai.R..EEZ.Catch'], F_EEZ = Table$FEEZ, MFMT = Table$MFMT,
                       ACL= Table$ACL, ACL_buffer=buffer$buffer,
                       Run=Table['Run'], Esc=Table['Escapement'], Esc_goal=Table['Lower.Bound.of.Goal'], years=Table['Year'], 
                       gen_lag=5, y_obj=2021, preseason=TRUE, postseason=FALSE, F_state_forecast_method='naive', run_forecast_method='arima')


#Determine buffer for tier 1 stocks based on retrospective preseason ACL relative to postseason realized ACL
buffer_fun <- function(ACL_buffer_window=10,y_obj=2021, gen_lag, F_state_forecast_method, run_forecast_method,
                       C_total, C_EEZ, F_EEZ, MFMT, ACL, Run, Esc, Esc_goal, years){

  #Compute postseason ACL
  Postseason <- Tier_1_fun(C_total=C_total, C_EEZ=C_EEZ, F_EEZ = F_EEZ, MFMT = MFMT, ACL=ACL, Run=Run, Esc=Esc, ACL_buffer=1,
                           Esc_goal=Esc_goal, years=years, gen_lag = gen_lag, y_obj=y_obj, preseason=FALSE, postseason=TRUE)  
                           
  Postseason_ACL <- Postseason$PostSeason_Table$ACL[(nrow(Postseason$PostSeason_Table)-ACL_buffer_window +1):nrow(Postseason$PostSeason_Table)]
  
  #Compute retrospective preseason ACL over the desired time window
  Preseason_ACL <- vector(length=length(Postseason_ACL))

  for(i in ACL_buffer_window:1){
    Preseason <- Tier_1_fun(C_total=C_total, C_EEZ=C_EEZ, F_EEZ = F_EEZ, MFMT = MFMT, ACL=ACL, Run=Run, Esc=Esc, ACL_buffer=1,
                            Esc_goal=Esc_goal, years=years, gen_lag = gen_lag,y_obj=(y_obj - i + 1), preseason=TRUE, postseason=FALSE, 
                            F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method)
    Preseason_ACL[(length(Preseason_ACL) - i +1)] <- Preseason$Preseason_Table[,'ACL']
  }


  #Log accuracy ratio/MSE for ACL
  LAR <- log(Preseason_ACL/Postseason_ACL)
  MSE <- 100*(exp(median(abs(LAR)))-1) #Gives median unsigned percentage error
  #Symmetric signed percentage bias
  SSPB <- sign(median(LAR))*MSE #Gives the sign of the bias

  #Compute buffer based on MSE/SSPB
  buffer <- min(((100 - SSPB)/100), 1)
  
  #Write to .csv
  buffer_mat <- cbind(MSE, SSPB, buffer)
  colnames(buffer_mat) <- c('MSE', 'SSPB', 'buffer')
  write.csv(buffer_mat, file=paste0(getwd(),'/',stock,'/',y_obj,'_buffer_table.csv'))
  
  #Compile list to return
  return_list <- list()
  return_list$MSE <- MSE
  return_list$SSPB <- SSPB
  return_list$buffer <- buffer
  
  return(return_list)
}
buffer <- buffer_fun(ACL_buffer_window=5, y_obj=2021, 
                       C_total=Table['Total.Kenai.R..Catch'], C_EEZ=Table['Kenai.R..EEZ.Catch'],F_EEZ = Table$FEEZ, MFMT = Table$MFMT,
                       ACL= Table$ACL,Run=Table['Run'], Esc=Table['Escapement'], Esc_goal=Table['Lower.Bound.of.Goal'], years=Table['Year'], 
                       gen_lag=5, F_state_forecast_method='naive', run_forecast_method='arima')
                       
#Function to do all FMP calculations for Tier 1 stocks
stock <- 'UCI Chum'
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))

#Function to do all FMP calculations for Tier 3 stocks
Tier_3_fun <- function(C_total , C_EEZ, OFL, MaxABC, years, catch_lag, buffer,
                       gen_lag, y_obj=2021, preseason=TRUE, postseason=TRUE){
  #Preseason calculations
  if(preseason==TRUE){
    #Re-construct table for calculations (this is in the event that the incoming data is not already packaged in a neat table from the state)
    Preseason_table <- data.frame(cbind(years, C_total, C_EEZ))
    colnames(Preseason_table) <- c('Year', 'C_total', 'C_EEZ')
    Preseason_table <- subset(Preseason_table, Preseason_table$Year < y_obj) #This wouldn't be necessary in practice but for a retrospective exercise we need to withold 2021 values
    
    #Calculate cumulative catch
    Preseason_table$Cum_Catch <- NA
    for(i in gen_lag:nrow(Preseason_table)){
      Preseason_table$Cum_Catch[i] <- sum(Preseason_table$C_EEZ[(i-gen_lag+1):i])  
    }
    
    #Determine OFL
    catch_lag <- nrow(Preseason_table)
    OFL <- max(Preseason_table$C_EEZ[(nrow(Preseason_table)-(catch_lag-1)):nrow(Preseason_table)])*gen_lag
    
    #Determine ACL
    ABC <- OFL*buffer
    
    #Determine ABC
    ACL <- ABC
    
    #Write to csv
    Preseason_SDC <- cbind(OFL, ABC, ACL)
    write.csv(Preseason_table, file=paste0(getwd(),'/',stock,'/',y_obj,'_preseason_table.csv'))
    write.csv(Preseason_SDC, file=paste0(getwd(),'/',stock,'/',y_obj,'_preseason_SDC.csv'))
    
  }
  #Postseason calculations
  if(postseason==TRUE){
    #Re-construct table for calculations (this is in the event that the incoming data is not already packaged in a neat table from the state)
    Postseason_table <- data.frame(cbind(years, C_total, C_EEZ))
    colnames(Postseason_table) <- c('Year', 'C_total', 'C_EEZ')
    Postseason_table <- subset(Postseason_table, Postseason_table$Year < (y_obj + 1))
    #Calculate cumulative catch
    Postseason_table$Cum_Catch <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$Cum_Catch[i] <- sum(Postseason_table$C_EEZ[(i-gen_lag+1):i])  
    }

    #Deermine OFL
    catch_lag <- nrow(Postseason_table)
    OFL <- max(Postseason_table$C_EEZ[(nrow(Postseason_table)-(catch_lag-1)):nrow(Postseason_table)])*gen_lag
    
    #Determine ACL
    ABC <- OFL*buffer
    
    #Determine ABC
    ACL <- ABC
    
    #ABC Exceeded
    ABC_exceed <- NULL
    if(Postseason_table$Cum_Catch[Postseason_table$Year==y_obj] > ABC){
      ABC_exceed <- 'YES'
    }else{
      ABC_exceed <- 'NO'
    }
      
  
    #Write to csv
    Postseason_SDC <- data.frame(cbind(OFL, ABC, ACL, ABC_exceed))
    write.csv(Postseason_table, file=paste0(getwd(),'/',stock,'/',y_obj,'_postseason_table.csv'))
    write.csv(Postseason_SDC, file=paste0(getwd(),'/',stock,'/',y_obj,'_postseason_SDC.csv'))
    
  }
  #Create storage list to return output
  return_list <- list()
  if(preseason==TRUE){
    return_list$Preseason_Table <- Preseason_SDC 
  }
  if(postseason==TRUE){
    return_list$PostSeason_Table <- Postseason_SDC 
  }
  return(return_list)
}

Tier_3_Table <- Tier_3_fun(C_total=Table$Total.Catch , C_EEZ=Table$EEZ.Catch, OFL=Table$OFL, MaxABC=Table$Max.ABC, years=Table$Year,
                           gen_lag=4, y_obj=2021, buffer=0.9, catch_lag = nrow(Table), preseason=FALSE, postseason=TRUE)
