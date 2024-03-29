#Lukas DeFilippo, lukas.defilippo@noaa.gov, 781-572-8865
#11/21/2023
#The purpose of this script is to produce functions used in performing tier 1 and tier 3 calculations for the Cook Inlet SAFE report

#Function to do all FMP calculations for Tier 1 stocks
Tier_1_fun <- function(C_total , C_EEZ, Run, Esc, Esc_goal, years, F_EEZ, MFMT, ACL, ACL_buffer=1, ABC_buffer=1, write=TRUE, plot=TRUE, sib_forecast=NULL,
                       gen_lag, y_obj, preseason=TRUE, postseason=TRUE, F_state_forecast_method=NULL, run_forecast_method=NULL){
  #Preseason calculations
  if(preseason==TRUE){
    #Re-construct table for calculations (this is in the event that the incoming data is not already packaged in a neat table from the state)
    #In practice, this could simply be loaded from the previous years' postseason table
    base_table <- cbind(years, Run, Esc, C_total, C_EEZ, F_EEZ, MFMT, ACL, Esc_goal)
    colnames(base_table) <- c('Year', 'Run', 'Esc', 'C_total', 'C_EEZ', 'F_EEZ', 'MFMT', 'ACL', 'Esc_goal')
    base_table$C_state <- base_table$C_total - base_table$C_EEZ
    base_table$F_state <- base_table$C_state/base_table$Run

    #Caclculate FEEZ (may be supplied already but good to calculate it ourselves)
    base_table$F_EEZ <- NA
    for(i in gen_lag:nrow(base_table)){
      base_table$F_EEZ[i] <- sum(base_table$C_EEZ[(i-gen_lag+1):i])/sum(base_table$Run[(i-gen_lag+1):i])
    }
    
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
      F_state_forecast <- forecast(auto.arima(logit(base_table$F_state), seasonal = FALSE, stepwise=FALSE, approximation=FALSE, ic='aicc'), h=1)
      F_state_forecast_mean <- inv.logit(as.numeric(F_state_forecast$mean))
      F_state_forecast_80_CI <- c(inv.logit(as.numeric(F_state_forecast$lower))[1], inv.logit(as.numeric(F_state_forecast$upper))[1])
      F_state_forecast_95_CI <- c(inv.logit(as.numeric(F_state_forecast$lower))[2], inv.logit(as.numeric(F_state_forecast$upper))[2])
      F_state_preseason <- F_state_forecast_mean
    }
    
    #Produce/load forecast of run size
    #If a sibling forecast is possible (state data made available), (1) load the forecasted run size (use published value for this exercise)
    if(run_forecast_method=='sibling'){
      run_forecast_mean <- sib_forecast
      run_forcast_80_CI <- NULL #if/when we take over the sibling forecasting we will be able to pull the uncertainty estimates
      run_forcast_95_CI <- NULL
      run_preseason <- run_forecast_mean
    }
    #Otherwise, (2) perform arims forecast on run size
    if(run_forecast_method=='arima'){
      run_forecast <- forecast(auto.arima(log(base_table$Run), seasonal = FALSE, stepwise=FALSE, approximation=FALSE, ic='aicc'), h=1)
      run_forecast_mean <-  exp(as.numeric(run_forecast$mean))
      run_forecast_80_CI <- c(exp(as.numeric(run_forecast$lower))[1], exp(as.numeric(run_forecast$upper))[1])
      run_forecast_95_CI <- c(exp(as.numeric(run_forecast$lower))[2], exp(as.numeric(run_forecast$upper))[2])
      run_preseason <- run_forecast_mean
    }
    
    #Calculate preseason potential yield for the upcoming year using the forecasted run size
    Potential_Yield_preseason <- max((run_preseason - base_table$Esc_goal[length(base_table$Esc_goal)]), 0)
    Potential_Yield_EEZ_preseason <- max(Potential_Yield_preseason - (run_preseason*F_state_preseason), 0)
    
    #Calculate tactical single season ACL
    alt_ACL <- Potential_Yield_EEZ_preseason*ACL_buffer
    
    #Calculate preseason fishing mortality
    F_EEZ_preseason <- max((sum(base_table$C_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + Potential_Yield_EEZ_preseason)/
      (sum(base_table$Run[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + run_preseason), 0)
    
    #Calculate ACL (preseason) --> generation-time level calculation --> Actally equivalent to ABC/OL
    ACL_preseason <- max(Potential_Yield_EEZ_preseason + sum(base_table$Potential_Yield_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]), 0)
    ACL_preseason_no_buff <- ACL_preseason
    ACL_preseason <- ACL_preseason*ABC_buffer
    
    #Calculate OFL (preseason) and single season ACL value (OFL without buffer)
    ACL_preseason_single <- max(ACL_preseason - sum(base_table$C_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]), 0)
    OFL_preseason <- max(ACL_preseason_no_buff - sum(base_table$C_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]), 0)
    
    MFMT_preseason <- max((sum(base_table$Potential_Yield_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + Potential_Yield_EEZ_preseason)/
      (sum(base_table$Run[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + run_preseason), 0)
    
    #Compile preseason quantities
    #Notes:
    #ACL_no_buff = OFL
    #ACL = ABC
    #ACL_single = ABC minus catch history
    Preseason_table <- cbind(F_state_preseason, run_preseason, Potential_Yield_preseason, Potential_Yield_EEZ_preseason, F_EEZ_preseason, ACL_preseason, ACL_preseason_no_buff, ACL_preseason_single, OFL_preseason, MFMT_preseason, alt_ACL)
    colnames(Preseason_table) <-c('F_state_preseason', 'run_preseason', 'Potential_Yield', 'Potential_Yield_EEZ', 'F_EEZ', 'ACL', 'ACL_no_buff','ACL_single', 'OFL', 'MFMT', 'alt_ACL')
    
    #Table for SSC
    Preseason_table_SSC <-data.frame(
      round(F_state_preseason , 3),
      round(run_preseason , 3),
      round(Potential_Yield_EEZ_preseason , 3),
      round(ACL_buffer, 3),
      round(alt_ACL , 3),
      round(F_EEZ_preseason , 3),
      round(MFMT_preseason , 3))
      colnames(Preseason_table_SSC) <- c('F_state preseason', 'run preseason', 'OFL_preseason', 'OFL to ABC_buffer', 'ABC_preseason', 'Potential F_EEZ', 'MFMT')

    
    #Write to .csv
    if(write==TRUE){
      write.csv(Preseason_table, file=paste0(getwd(),'/',stock,'/', y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method,'_preseason_table.csv'))
      write.csv(base_table, file=paste0(getwd(),'/',stock,'/', y_obj, '_historical_table.csv'))
      write.csv(Preseason_table_SSC, file=paste0(getwd(),'/',stock,'/', y_obj, '_Preseason_table_SSC.csv'))
                
    }
    #Preseason plots
    if(plot==TRUE){
      png(file=paste0(getwd(),'/',stock,'/', y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method, '_preseason_plots.png'))
      par(mfrow=c(3,2), mar=c(2,2,2,2), oma=c(2,2,2,2))
      # 1) State harvest
      if(F_state_forecast_method=='arima'){
        plot(base_table$Year, base_table$F_state, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
             ylim=c(min(base_table$F_state, F_state_forecast_95_CI[1]), max(base_table$F_state, F_state_forecast_95_CI[2])))
        points(y_obj, F_state_forecast_mean , col='red', pch=16, type='o', cex=1.1)
        points(base_table$Year, base_table$F_bar_state, type='l', lty=2)
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
      text(x=max(base_table$Year), y=0.9*c(max(base_table$F_state, F_state_forecast_95_CI[1])), 'A')
      
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
      text(x=max(base_table$Year), y=0.9*c(max(base_table$Run, run_preseason)), 'B')
      
      
      # 3) Potential Yield
      plot(base_table$Year, base_table$Potential_Yield_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
           ylim=c(min(base_table$Potential_Yield_EEZ, Potential_Yield_EEZ_preseason), max(base_table$Potential_Yield_EEZ, Potential_Yield_EEZ_preseason)))
      points(y_obj,Potential_Yield_EEZ_preseason , col='red', pch=16, type='o', cex=1.1)
      points(y_obj,alt_ACL, col='red', pch=1, type='o', cex=1.1)
      mtext(side=2, 'Potential Yield EEZ', line=2.5, cex=0.85)
      text(x=max(base_table$Year), y=0.9*c(max(base_table$Potential_Yield_EEZ, Potential_Yield_EEZ_preseason)), 'C')
      
      # 4) F_EEZ Preseason
      plot(base_table$Year, base_table$F_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
           ylim=c(min(base_table$F_EEZ, F_EEZ_preseason, na.rm=TRUE), max(base_table$F_EEZ, F_EEZ_preseason, na.rm=TRUE )))
      points(y_obj,F_EEZ_preseason , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'F EEZ', line=2.5, cex=0.85)
      text(x=max(base_table$Year), y=0.9*max(base_table$F_EEZ, F_EEZ_preseason, na.rm=TRUE), 'D')
      
      
      # 5) ACL/ABC Preseason (multigenerational)
      #plot(base_table$Year, base_table$ACL, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
           #ylim=c(min(base_table$ACL, ACL_preseason_no_buff, na.rm=TRUE), max(base_table$ACL, ACL_preseason_no_buff, na.rm=TRUE)))
      #points(y_obj,ACL_preseason_no_buff, col='red', pch=16, type='o', cex=1.1)
      #points(y_obj,ACL_preseason , col='red', pch=1, type='o', cex=1.1)
     #mtext(side=2, 'Aggregate potential yield', line=2.5, cex=0.85)
      
      # 6) MFMT Preseason
      plot(base_table$Year, base_table$MFMT, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
           ylim=c(min(base_table$MFMT, MFMT_preseason, na.rm = TRUE), max(base_table$MFMT, MFMT_preseason, na.rm=TRUE)))
      points(y_obj,MFMT_preseason , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'MFMT', line=2.5, cex=0.85)
      text(x=max(base_table$Year), y=0.9*max(base_table$MFMT, MFMT_preseason, na.rm = TRUE), 'E')
      
      dev.off()
    }
  }
  #Postseason calculations
  if(postseason==TRUE){
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
      Postseason_table$ACL[i] <- max(sum(Postseason_table$Potential_Yield_EEZ[(i-gen_lag+1):i]), 0)  
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
    if(write==TRUE){
      write.csv(Postseason_table, file=paste0(getwd(),'/',stock,'/',y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method,'_postseason_table.csv'))
      Postseason_table_SSC <- data.frame(
        Postseason_table$Year,
        round(Postseason_table$Run,0),
        round(Postseason_table$Esc,0),
        round(Postseason_table$Esc_goal,0),
        round(Postseason_table$C_total,0),
        round(Postseason_table$C_state,0),
        round(Postseason_table$F_state,3),
        round(Postseason_table$C_EEZ,0),
        round(Postseason_table$F_EEZ,3),
        round(Postseason_table$MFMT,3),
        round(Postseason_table$Potential_Yield_EEZ,0)
      )
      colnames(Postseason_table_SSC) <- c('Year', 'Run size', 'Escapement', 'Escapement goal', 'Total catch', 'State catch', 'F_state', 'EEZ Catch',' F_EEZ', 'MFMT', 'Potential Yield EEZ' )
      write.csv(Postseason_table_SSC,file=paste0(getwd(),'/',stock,'/',y_obj,'_Postseason_table_SSC.csv'))
    }
    #Postseason plots
    if(plot==TRUE){
      #Load preseason table to plot preseason values against actual values
      Preseason_table <- read.csv(file=paste0(getwd(),'/',stock,'/', y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method,'_preseason_table.csv'))
      
      png(file=paste0(getwd(),'/',stock,'/', y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method,'_postseason_plots.png'))
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
           ylim=c(min(Postseason_table$Potential_Yield_EEZ, Preseason_table$Potential_Yield_EEZ, na.rm=TRUE), max(Postseason_table$Potential_Yield_EEZ, Preseason_table$Potential_Yield_EEZ, na.rm=TRUE)))
      points(y_obj,Preseason_table$Potential_Yield_EEZ , col='red', pch=16, type='o', cex=1.1)
      points(y_obj,Preseason_table$alt_ACL , col='red', pch=1, type='o', cex=1.1)
      mtext(side=2, 'Potential Yield EEZ', line=2.5, cex=0.85)
      
      # 4) F_EEZ
      plot(Postseason_table$Year, Postseason_table$F_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, 
           ylim=c(min(Postseason_table$F_EEZ, Preseason_table$F_EEZ, na.rm=TRUE), max(Postseason_table$F_EEZ, Preseason_table$F_EEZ, na.rm=TRUE )))
      points(y_obj,Preseason_table$F_EEZ , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'F EEZ', line=2.5, cex=0.85)
      
      # 5) ACL Preseason
      #plot(Postseason_table$Year, Postseason_table$ACL, type='o', col='black', pch=16, xlab=NA, ylab=NA, 
           #ylim=c(min(Postseason_table$ACL, Preseason_table$ACL_no_buff, na.rm=TRUE), max(Postseason_table$ACL, Preseason_table$ACL_no_buff, na.rm=TRUE)))
      #points(y_obj, Preseason_table$ACL_no_buff , col='red', pch=16, type='o', cex=1.1)
      #points(y_obj, Preseason_table$ACL , col='blue', pch=16, type='o', cex=1.1)
      #mtext(side=2, 'ABC/OFL', line=2.5, cex=0.85)
      
      # 6) MFMT Preseason
      plot(Postseason_table$Year, Postseason_table$MFMT, type='o', col='black', pch=16, xlab=NA, ylab=NA,
           ylim=c(min(Postseason_table$MFMT, Preseason_table$MFMT, na.rm = TRUE), max(Postseason_table$MFMT, Preseason_table$MFMT, na.rm=TRUE)))
      points(y_obj,Preseason_table$MFMT , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'MFMT', line=2.5, cex=0.85)
      dev.off()
    }
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

#Determine buffer for tier 1 stocks based on retrospective preseason ACL (OFL) relative to postseason realized ACL
buffer_fun_ABC <- function(ACL_buffer_window=10,y_obj=2021, gen_lag, F_state_forecast_method, run_forecast_method, sib_forecast=NULL,
                       C_total, C_EEZ, F_EEZ, MFMT, ACL, Run, Esc, Esc_goal, years){

  #Compute postseason ACL (OFL)
  Postseason <- Tier_1_fun(C_total=C_total, C_EEZ=C_EEZ, F_EEZ = F_EEZ, MFMT = MFMT, ACL=ACL, Run=Run, Esc=Esc, ACL_buffer=1,
                           Esc_goal=Esc_goal, years=years, gen_lag = gen_lag, y_obj=y_obj, preseason=FALSE, postseason=TRUE, 
                           F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method, write=FALSE, plot=FALSE)  
  
  #Postseason_ACL <- Postseason$PostSeason_Table$Potential_Yield_EEZ[(nrow(Postseason$PostSeason_Table)-ACL_buffer_window +1):nrow(Postseason$PostSeason_Table)]
  Postseason_ACL <- Postseason$PostSeason_Table$ACL[Postseason$PostSeason_Table$Year %in% (y_obj-ACL_buffer_window):(y_obj-1)]
  
  #Compute retrospective preseason ACL (OFL) over the desired time window
  Preseason_ACL <- vector(length=length(Postseason_ACL))
  
  for(i in 1:ACL_buffer_window){
    Preseason <- Tier_1_fun(C_total=C_total, C_EEZ=C_EEZ, F_EEZ = F_EEZ, MFMT = MFMT, ACL=ACL, Run=Run, Esc=Esc, ACL_buffer=1, sib_forecast = sib_forecast,
                            Esc_goal=Esc_goal, years=years, gen_lag = gen_lag,y_obj=(y_obj - i), preseason=TRUE, postseason=FALSE, 
                            F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method, write=FALSE, plot=FALSE)
    Preseason_ACL[i] <- Preseason$Preseason_Table[,'ACL']
  }
  Preseason_ACL <- rev(Preseason_ACL)
  
  #Log accuracy ratio/MSE for ACL based on
  LAR <- log((Preseason_ACL+0.00000000000001)/(Postseason_ACL+0.00000000000001))
  MSA <- 100*(exp(median(abs(LAR), na.rm=TRUE))-1) #Gives median unsigned percentage error
  MSA_max <- 100*(exp(max(abs(LAR), na.rm=TRUE))-1)
  MSA_min <- 100*(exp(min(abs(LAR), na.rm=TRUE))-1)
  #Compute buffer based on MSA
  buffer <- max((100-MSA)/100, 0.01)
  buffer_max <- max((100-MSA_max)/100, 0.1)
  buffer_min <- max((100-MSA_min)/100, 0.1)

  #Write to .csv
  buffer_mat <- cbind(MSA, MSA_max, MSA_min, buffer, buffer_max, buffer_min)
  colnames(buffer_mat) <- c('MSA', 'MSA_max', 'MSA_min', 'buffer', 'buffer_max', 'buffer_min')
  write.csv(buffer_mat, file=paste0(getwd(),'/',stock,'/',y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method,'_ABC_buffer_table.csv'))
  
  #Compile list to return
  return_list <- list()
  return_list$MSA <- MSA
  return_list$MSA_max <- MSA_max
  return_list$MSA_min <- MSA_min
  
  return_list$buffer <- buffer
  return_list$buffer_max <- buffer_max
  return_list$buffer_min <- buffer_min
  
  
  return(return_list)
}

#Determine buffer for tier 1 stocks based on retrospective preseason ACL (OFL) relative to postseason realized ACL
buffer_fun_ACL <- function(ACL_buffer_window=10,y_obj=2021, gen_lag, F_state_forecast_method, run_forecast_method, sib_forecast=NULL,
                       C_total, C_EEZ, F_EEZ, MFMT, ACL, Run, Esc, Esc_goal, years){
  
  #Compute postseason ACL (OFL)
  Postseason <- Tier_1_fun(C_total=C_total, C_EEZ=C_EEZ, F_EEZ = F_EEZ, MFMT = MFMT, ACL=ACL, Run=Run, Esc=Esc, ACL_buffer=1,
                           Esc_goal=Esc_goal, years=years, gen_lag = gen_lag, y_obj=y_obj, preseason=FALSE, postseason=TRUE, 
                           F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method, write=FALSE, plot=FALSE)  
  
  #Postseason_ACL <- Postseason$PostSeason_Table$Potential_Yield_EEZ[(nrow(Postseason$PostSeason_Table)-ACL_buffer_window +1):nrow(Postseason$PostSeason_Table)]
  Postseason_ACL <- Postseason$PostSeason_Table$Potential_Yield_EEZ[Postseason$PostSeason_Table$Year %in% (y_obj-ACL_buffer_window):(y_obj-1)]
  
  #Compute retrospective preseason ACL (OFL) over the desired time window
  Preseason_ACL <- vector(length=length(Postseason_ACL))
  
  for(i in 1:ACL_buffer_window){
    Preseason <- Tier_1_fun(C_total=C_total, C_EEZ=C_EEZ, F_EEZ = F_EEZ, MFMT = MFMT, ACL=ACL, Run=Run, Esc=Esc, ACL_buffer=1, sib_forecast = sib_forecast,
                            Esc_goal=Esc_goal, years=years, gen_lag = gen_lag,y_obj=(y_obj - i), preseason=TRUE, postseason=FALSE, 
                            F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method, write=FALSE, plot=FALSE)
    Preseason_ACL[i] <- Preseason$Preseason_Table[,'Potential_Yield_EEZ']
  }
  Preseason_ACL <- rev(Preseason_ACL)
  
  #Log accuracy ratio/MSE for ACL based on
  #LAR <- log((Preseason_ACL+0.00000000000001)/(Postseason_ACL+0.00000000000001))
  LAR <- log((Preseason_ACL)/(Postseason_ACL))
  LAR <- na.omit(is.finite(LAR)*LAR)
  MSA <- 100*(exp(median(abs(LAR), na.rm=TRUE))-1) #Gives median unsigned percentage error
  MSA_max <- 100*(exp(max(abs(LAR), na.rm=TRUE))-1)
  MSA_min <- 100*(exp(min(abs(LAR), na.rm=TRUE))-1)
  #Compute buffer based on MSA
  buffer <- max((100-MSA)/100, 0.1)
  buffer_max <- max((100-MSA_max)/100, 0.1)
  buffer_min <- max((100-MSA_min)/100, 0.1)
  
  #Write to .csv
  buffer_mat <- cbind(MSA, MSA_max, MSA_min, buffer, buffer_max, buffer_min)
  colnames(buffer_mat) <- c('MSA', 'MSA_max', 'MSA_min', 'buffer', 'buffer_max', 'buffer_min')
  write.csv(buffer_mat, file=paste0(getwd(),'/',stock,'/',y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method,'ACL_buffer_table.csv'))
  
  #Compile list to return
  return_list <- list()
  return_list$MSA <- MSA
  return_list$MSA_max <- MSA_max
  return_list$MSA_min <- MSA_min
  
  return_list$buffer <- buffer
  return_list$buffer_max <- buffer_max
  return_list$buffer_min <- buffer_min
  
  
  return(return_list)
}

#Function to do all FMP calculations for Tier 3 stocks
Tier_3_fun <- function(C_total , C_EEZ, OFL, years, catch_lag, buffer,
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
      if(gen_lag > 1){
        Preseason_table$Cum_Catch[i] <- sum(Preseason_table$C_EEZ[(i-gen_lag+1):i])  
      }else{
        Preseason_table$Cum_Catch[i] <- Preseason_table$C_EEZ[i]
      }
    }
    
    #Determine OFL
    catch_lag <- nrow(Preseason_table)
    OFL <- max(Preseason_table$C_EEZ[(nrow(Preseason_table)-(catch_lag-1)):nrow(Preseason_table)])*gen_lag
    if(gen_lag > 1){
      OFL_pre <- OFL - sum(Preseason_table$C_EEZ[(nrow(Preseason_table)-gen_lag+2):nrow(Preseason_table)])
    }else{
      OFL_pre <- OFL
    }

    #Determine ACL
    ABC <- vector(length=length(buffer))
    ABC_pre <- vector(length=length(buffer))
    ACL <- vector(length=length(buffer))
    ACL_pre <- vector(length=length(buffer))
    
    for(i in 1:length(buffer)){
      ABC[i] <- OFL*buffer[i]
      ABC_pre[i] <- OFL_pre*buffer[i]
      
      #Determine ABC
      ACL[i] <- ABC[i]
      ACL_pre[i] <- ABC_pre[i]
    }
    
    #Write to csv
    Preseason_SDC <- cbind(buffer, rep(OFL,length(buffer)), ABC, rep(OFL_pre, length(buffer)), ABC_pre)
    colnames(Preseason_SDC) <- c('buffer', 'OFL', 'ABC', 'Preseason OFL', 'Preseason ABC')
    Preseason_table$C_total <- round(Preseason_table$C_total, 0)
    Preseason_table$C_EEZ<- round(Preseason_table$C_EEZ, 0)
    Preseason_table$Cum_Catch<- round(Preseason_table$Cum_Catch, 0)
    
    colnames(Preseason_table) <- c('Year', 'Total catch', 'EEZ catch', 'Cumulative EEZ catch' )
    
    write.csv(Preseason_table, file=paste0(getwd(),'/',stock,'/',y_obj,'_tier_3_preseason_table.csv'))
    write.csv(Preseason_SDC, file=paste0(getwd(),'/',stock,'/',y_obj,'_tier_3_preseason_SDC.csv'))
    
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
      if(gen_lag > 1){
        Postseason_table$Cum_Catch[i] <- sum(Postseason_table$C_EEZ[(i-gen_lag+1):i])  
      }else{
        Postseason_table$Cum_Catch[i] <- Postseason_table$C_EEZ[i]
      }
    }
    
    #Determine OFL
    catch_lag <- nrow(Postseason_table)
    OFL <- max(Postseason_table$C_EEZ[(nrow(Postseason_table)-(catch_lag-1)):nrow(Postseason_table)])*gen_lag
    
    #Determine ACL/ABC
    ABC <- vector(length=length(buffer))
    ACL <- vector(length=length(buffer))
    for(i in 1:length(buffer)){
      ABC[i] <- OFL*buffer[i]
      ACL[i] <- ABC[i]
    }
    
    #ABC Exceeded
    ABC_exceed <- vector(length=length(buffer))
    Cum_Catch <- Postseason_table$Cum_Catch[Postseason_table$Year==y_obj]
    for(i in 1:length(buffer)){
      if(Cum_Catch > ABC[i]){
        ABC_exceed[i] <- 'YES'
      }else{
        ABC_exceed[i] <- 'NO'
      }
    }

    #Write to csv
    Postseason_SDC <- data.frame(cbind(buffer, rep(OFL, length(buffer)), ABC, rep(Cum_Catch, length(buffer)), ABC_exceed))
    colnames(Postseason_SDC) <- c('buffer', 'OFL', 'ABC', 'Cumulative catch', 'ABC exceeded')
    Postseason_table$C_total <- round(Postseason_table$C_total, 0)
    Postseason_table$C_EEZ <- round(Postseason_table$C_EEZ, 0)
    Postseason_table$Cum_Catch <- round(Postseason_table$Cum_Catch, 0)
    
    #colnames(Postseason_table) <- c('Year', 'Total catch', 'EEZ_catch')
    write.csv(Postseason_table, file=paste0(getwd(),'/',stock,'/',y_obj,'_tier_3_postseason_table.csv'))
    write.csv(Postseason_SDC, file=paste0(getwd(),'/',stock,'/',y_obj,'_tier_3_postseason_SDC.csv'))
    
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
