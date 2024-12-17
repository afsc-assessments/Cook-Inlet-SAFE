#Lukas DeFilippo, lukas.defilippo@noaa.gov, 781-572-8865
#11/21/2023
#The purpose of this script is to produce functions used in performing tier 1 and tier 3 calculations for the Cook Inlet SAFE report

#Function to do all FMP calculations for Tier 1 stocks
Tier_1_fun <- function(C_total , 
                       C_EEZ, 
                       Run, 
                       Esc, 
                       Esc_goal,
                       Esc_goal_pre,
                       years, 
                       ABC_buffer=1, 
                       write=TRUE, 
                       plot=TRUE,
                       sib_forecast=NULL,
                       gen_lag, 
                       y_obj, 
                       preseason=TRUE,
                       postseason=TRUE, 
                       F_state_forecast_method=NULL, 
                       run_forecast_method=NULL, 
                       buffer_ABC=NULL,
                       sib_forecast_full=NULL){
  #Preseason calculations
  if(preseason==TRUE){
    #Re-construct table for calculations (this is in the event that the incoming data is not already packaged in a neat table from the state)
    #In practice, this could simply be loaded from the previous years' postseason table
    base_table <- cbind(years, Run, Esc, C_total, C_EEZ, Esc_goal)
    colnames(base_table) <- c('Year', 'Run', 'Esc', 'C_total', 'C_EEZ', 'Esc_goal')
    base_table$C_state <- base_table$C_total - base_table$C_EEZ
    base_table$F_state <- base_table$C_state/base_table$Run

    #Caclculate FEEZ
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
    base_table <- subset(base_table, base_table$Year < y_obj) #This wouldn't be necessary in practice but for a retrospective exercise we need to withold current year values
    #Create column for potential yield
    base_table$Potential_Yield_EEZ <- base_table$Run - base_table$Esc_goal - (base_table$C_total - base_table$C_EEZ)
    base_table$Potential_Yield_EEZ[base_table$Potential_Yield_EEZ < 0] <- 0
    
    #Produce forecasts of state harvest (Fbar)
    if(F_state_forecast_method=='naive'){
      F_state_preseason <- base_table$F_bar_state[base_table$Year==y_obj-1]
    }
    if(F_state_forecast_method=='arima'){
      F_state_arima <- auto.arima(logit(base_table$F_state), seasonal = FALSE, stepwise=FALSE, approximation=FALSE, ic='aicc')
      F_state_arima_terms <- F_state_arima$coef
      F_state_forecast <- forecast(F_state_arima, h=1)
      F_state_forecast_mean <- inv.logit(as.numeric(F_state_forecast$mean))
      F_state_forecast_80_CI <- c(inv.logit(as.numeric(F_state_forecast$lower))[1], inv.logit(as.numeric(F_state_forecast$upper))[1])
      F_state_forecast_95_CI <- c(inv.logit(as.numeric(F_state_forecast$lower))[2], inv.logit(as.numeric(F_state_forecast$upper))[2])
      F_state_preseason <- F_state_forecast_mean
    }
    
    #Produce/load forecast of run size
    #If a sibling forecast is possible (state data made available), (1) load the forecasted run size (use published value for this exercise)
    if(run_forecast_method=='sibling'){
      run_forecast_mean <- sib_forecast$Run.Forecast[sib_forecast$Year==y_obj]
      run_forcast_80_CI <- NULL #if/when we take over the sibling forecasting we will be able to pull the uncertainty estimates
      run_forcast_95_CI <- NULL
      run_preseason <- run_forecast_mean
    }
    #Otherwise, (2) perform arims forecast on run size
    if(run_forecast_method=='arima'){
      run_forecast_arima <- auto.arima(log(base_table$Run), seasonal = FALSE, stepwise=FALSE, approximation=FALSE, ic='aicc')
      run_forecast_arima_terms <-  run_forecast_arima$coef
      run_forecast <- forecast(run_forecast_arima, h=1)
      run_forecast_mean <-  exp(as.numeric(run_forecast$mean))
      run_forecast_80_CI <- c(exp(as.numeric(run_forecast$lower))[1], exp(as.numeric(run_forecast$upper))[1])
      run_forecast_95_CI <- c(exp(as.numeric(run_forecast$lower))[2], exp(as.numeric(run_forecast$upper))[2])
      run_preseason <- run_forecast_mean
    }
    
    #Calculate preseason potential yield for the upcoming year using the forecasted run size
    Potential_Yield_preseason <- max((run_preseason - Esc_goal_pre), 0)
    Potential_Yield_EEZ_preseason <- max(Potential_Yield_preseason - (run_preseason*F_state_preseason), 0)
    
    #Calculate OFL
    OFL_preseason <- Potential_Yield_EEZ_preseason
    ABC_preseason <- OFL_preseason*ABC_buffer

    #Calculate preseason fishing mortality
    F_EEZ_preseason <- max((sum(base_table$C_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + Potential_Yield_EEZ_preseason)/
      (sum(base_table$Run[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + run_preseason), 0)
    
    #Calculate preseason MFMT
    MFMT_preseason <- max((sum(base_table$Potential_Yield_EEZ[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + Potential_Yield_EEZ_preseason)/
      (sum(base_table$Run[(nrow(base_table)-gen_lag+2):nrow(base_table)]) + run_preseason), 0)
    
    #Table for SSC
    Preseason_table_SSC <-data.frame(
      round(F_state_preseason , 3),
      round(run_preseason , 3),
      round(Potential_Yield_EEZ_preseason , 3),
      round(ABC_buffer, 3),
      round(OFL_preseason , 3),
      round(ABC_preseason , 3),
      round(F_EEZ_preseason , 3),
      round(MFMT_preseason , 3))
      colnames(Preseason_table_SSC) <- c('F_state preseason', 'run_preseason', 'potential_yield_EEZ_preseason', 'OFL to ABC_buffer','OFL_preseason', 'ABC_preseason', 'Potential_F_EEZ', 'MFMT')

    #Write to .csv
    if(write==TRUE){
      write.csv(Preseason_table_SSC, file=paste0(getwd(),'/',stock,'/', y_obj, '_Preseason_table_SSC.csv'))
                
    }
    #Preseason plots
    if(plot==TRUE){
      # png(file=paste0(getwd(),'/',stock,'/', y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method, '_preseason_plots.png'))
      # par(mfrow=c(1,3), mar=c(0,4,0,0), oma=c(10,0.1,10,0.1))
      # 1) State harvest
      if(F_state_forecast_method=='arima'){
        # plot(base_table$Year, base_table$F_state, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
        #      ylim=c(min(base_table$F_state, F_state_forecast_95_CI[1]), max(base_table$F_state, F_state_forecast_95_CI[2])))
        # points(y_obj, F_state_forecast_mean , col='red', pch=16, type='o', cex=1.1)
        # points(base_table$Year, base_table$F_bar_state, type='l', lty=2)
        # arrows(x0=y_obj, x1=y_obj, y0= F_state_forecast_80_CI[1], y1= F_state_forecast_80_CI[2], col='red', length=0, lwd=2)
        # arrows(x0=y_obj, x1=y_obj, y0= F_state_forecast_95_CI[1], y1= F_state_forecast_95_CI[2], col='red', length=0.05, lwd=1, angle=90, code=3)
        # mtext(side=2, 'State waters harvest rate', line=2.5, cex=0.85)
        
        F_stateDF <- rbind(base_table[,c("Year","F_state")], c(y_obj, NA))
        F_stateDF$upr95 <- c(rep(NA,length(run_forecast$fitted)), F_state_forecast_95_CI[2])
        F_stateDF$lwr95 <- c(rep(NA,length(run_forecast$fitted)), F_state_forecast_95_CI[1])
        F_stateDF$upr80 <- c(rep(NA,length(F_state_forecast$fitted)), F_state_forecast_80_CI[2])
        F_stateDF$lwr80 <- c(rep(NA,length(F_state_forecast$fitted)), F_state_forecast_80_CI[1])
        F_stateDF$fitted <- c(inv.logit(F_state_forecast$fitted), NA)
        F_stateDF$mean <- c(rep(NA,length(F_state_forecast$fitted)), F_state_forecast_mean)
        F_stateDF$type <- factor(c(rep( "Obs", length(F_state_forecast$fitted)), "ARIMA"))
        
        Fmape <- round(mape(actual = base_table$F_state[base_table$Year%in%buffer_ABC$yr], 
                            predicted = buffer_ABC$Preseason_f),3)*100
        
        
        
        #Color blind colors for plotting
        colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        #Years used in assessment
        n_years <- length(base_table$Year)
        
        
        
        #GG plot 
        Fstate_plot <- ggplot(F_stateDF, aes(x = Year, y = F_state))+
          geom_errorbar(aes(ymax = upr95, ymin  = lwr95, col = "95% CI"), linewidth = 1)+
          geom_errorbar(aes(ymax = upr80, ymin  = lwr80, col = "80% CI"), linewidth = 1.5, width = 0)+
          geom_line(aes(col = "Obs"),size = 1.2)+
          geom_line(aes(y = fitted, col = "ARIMA"),size = 1,linetype = 2)+
          geom_point(aes(y = mean, col = "Pred"), size = 4)+
          geom_point(aes(col = type, size = type))+
          labs(x = "Year", y = bquote("State harvest rate ( "~F[state]~")"))+
          geom_richtext(aes(x = y_obj-4, y = max(.8, na.rm = T), 
                            label = paste("MAPE =", Fmape,"%"), 
                            label.colour = "white"), 
                        inherit.aes = F)+
          coord_cartesian(ylim=c(0,1),xlim=c(1999, y_obj+1))+
          scale_x_continuous(breaks = seq(from=2000, to = 2030, by = 5))+
          scale_size_manual(guide = "none", values = c(2,2))+
          scale_color_manual(name="", 
                             values = colorBlindBlack8[c(1,3,7,7,7)],
                             breaks = c("Obs", "ARIMA", "Pred", "80% CI","95% CI"))+
          # guides(color = guide_legend(override.aes = list(size=c(2,4,2,2,2))))+
          theme_clean()+
          theme(legend.position = "top",
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14), 
                plot.background = element_blank(), 
                legend.background = element_blank() )
        
        
        
      }
      if(F_state_forecast_method == 'naive'){
        # plot(base_table$Year, base_table$F_state, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1))
        # points(y_obj, F_state_preseason , col='red', pch=16, type='o', cex=1.1)
        # points(base_table$Year, base_table$F_bar_state, type='l', lty=2)
        # mtext(side=2, 'State waters harvest rate', line=2.5, cex=0.85)
      }
      # text(x=max(base_table$Year), y=0.9*c(max(base_table$F_state, F_state_forecast_95_CI[1])), 'A')
      
      # 2) Run forecast
      if(run_forecast_method=='arima'){
        # plot(base_table$Year, base_table$Run, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
        #      ylim=c(min(base_table$Run, run_forecast_95_CI[1]), max(base_table$Run, run_forecast_95_CI[2])))
        # points(y_obj, run_forecast_mean , col='red', pch=16, type='o', cex=1.1)
        # arrows(x0=y_obj, x1=y_obj, y0= run_forecast_80_CI[1], y1= run_forecast_80_CI[2], col='red', length=0, lwd=2)
        # arrows(x0=y_obj, x1=y_obj, y0= run_forecast_95_CI[1], y1= run_forecast_95_CI[2], col='red', length=0.05, lwd=1, angle=90, code=3)
        # mtext(side=2, 'Run size', line=2.5, cex=0.85)
        runsizeDF<- rbind(base_table[,c("Year","Run")], c(y_obj, NA))
        runsizeDF$upr95 <- c(rep(NA,length(run_forecast$fitted)), run_forecast_95_CI[2])
        runsizeDF$lwr95 <- c(rep(NA,length(run_forecast$fitted)), run_forecast_95_CI[1])
        runsizeDF$upr80 <- c(rep(NA,length(run_forecast$fitted)), run_forecast_80_CI[2])
        runsizeDF$lwr80 <- c(rep(NA,length(run_forecast$fitted)), run_forecast_80_CI[1])
        runsizeDF$fitted <- c(exp(run_forecast$fitted), NA)
        runsizeDF$type <- factor(c(rep( "Obs", n_years),"ARIMA"))
        runsizeDF$mean <- c(rep(NA,length(run_forecast$fitted)), run_forecast_mean)
        # runsizeDF$sibforecast <- c(Forecast$Run.Forecast, NA)
        # runsizeDF<-left_join(runsizeDF, Forecast[,c(1,2)])
        runsizeDF$sib.forecast <- c(sib_forecast_full,NA)
        
        temp.df<-data.frame("Year"=buffer_ABC$yr, "ARIMA_OOS"=buffer_ABC$Preseason_run)
        
        runsizeDF <- left_join(runsizeDF,temp.df, by = "Year")
        
        # Subset the df to get 10 year window with retrospective ARIMA projections
        runsizeDF_MAPE <- runsizeDF[runsizeDF$Year %in% buffer_ABC$yr,]
        
        mape.sib <- round((1/length(runsizeDF_MAPE$Year))*sum(abs((runsizeDF_MAPE$Run - runsizeDF_MAPE$sib.forecast)/ runsizeDF_MAPE$Run),na.rm = TRUE)*100, 1)
        mape.arima <- round((1/length(runsizeDF_MAPE$Year))*sum(abs((runsizeDF_MAPE$Run - runsizeDF_MAPE$ARIMA_OOS)/ runsizeDF_MAPE$Run),na.rm = TRUE)*100, 1)
        
        
        # Compare observed run size to ARIMA fit
        runsize_ARFIT_plot <- ggplot(runsizeDF, aes(x = Year, y = Run))+
          geom_errorbar(aes(ymax = upr95, ymin  = lwr95, col = "95% CI"), linewidth = 1)+
          geom_errorbar(aes(ymax = upr80, ymin  = lwr80, col = "80% CI"), linewidth = 1.5, width = 0)+
          # geom_ribbon(aes(ymin = lwr80, ymax = upr80))+
          geom_line(aes(col = "Obs"),size = 1.2)+
          geom_line(aes(y = fitted, col = "ARIMA"),size = 1.2,linetype = 2)+
          geom_point(aes(y = fitted, col = "ARIMA"), size = 2)+
          geom_point(aes(col = type, size = type))+
          geom_point(aes(y = mean, col = "Pred"), size = 4)+
          # geom_point(aes(y = Forecast, col = "Sib"))+
          # geom_line(aes(y = Forecast, col = "Sib"))+
          # geom_richtext(aes(x = 2015, y = 2000, label = paste("MAPE<sub>ARIMA</sub> =", mape.arima,"%"), label.colour = "white"), inherit.aes = F)+
          # geom_richtext(aes(x = 2015, y = 1800, label = paste("MAPE<sub>Sibling</sub> =", mape.sib, "%")),label.colour = "white", inherit.aes = F)+
          labs(x = "Year", y = "Run size (000's)")+
          coord_cartesian(xlim=c(1999, y_obj+1))+
          scale_x_continuous(breaks = seq(from=2000, to = 2030, by = 5))+
          scale_size_manual(guide = "none", values = c(2,2))+
          scale_color_manual(name="", 
                             values = colorBlindBlack8[c(1,3,7,7,7,7)],
                             breaks = c("Obs","ARIMA","Pred","80% CI", "95% CI"))+
          # guides(color = guide_legend(override.aes = list(size=c(2,4,2,2,2))))+
          theme_clean()+
          theme(legend.position = "top",
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.background = element_blank(), 
                legend.background = element_blank()
          )
        
        # Compare out of sample ARIMA to sib
        forecast_comp_plot <- ggplot(runsizeDF_MAPE, aes(x = Year, y = Run))+
          # geom_errorbar(aes(ymax = upr95, ymin  = lwr95, col = "95% CI"), linewidth = 1)+
          # geom_errorbar(aes(ymax = upr80, ymin  = lwr80, col = "80% CI"), linewidth = 1.5, width = 0)+
          geom_ribbon(aes(ymin = lwr80, ymax = upr80))+
          geom_line(aes(col = "Obs."),size = 1.2)+
          geom_point(aes(col = "Obs."), size = 2)+
          geom_line(aes(y = ARIMA_OOS, col = "ARIMA"),size = 1.2,linetype = 2)+
          geom_point(aes(y = ARIMA_OOS, col = "ARIMA"), size = 2)+
          # geom_point(aes(col = type, size = type))+
          # geom_point(aes(y = mean, col = "ARIMA Projected"), size = 4)+
          geom_point(aes(y = sib.forecast, col = "Sib."), size = 2)+
          geom_line(aes(y = sib.forecast, col = "Sib."),size=1.2, linetype = 4)+
          geom_richtext(aes(x = y_obj-2, y = max(runsizeDF$Run, na.rm = T), label = paste("MAPE<sub>ARIMA</sub> =", mape.arima,"%"), label.colour = "white"), inherit.aes = F)+
          geom_richtext(aes(x = y_obj-2, y = (max(runsizeDF$Run, na.rm = T)-(.1*max(runsizeDF$Run, na.rm = T))), label = paste("MAPE<sub>Sibling</sub> =", mape.sib, "%")),label.colour = "white", inherit.aes = F)+
          labs(x = "Year", y = "Run size (000's)")+
          coord_cartesian(xlim=c(min(runsizeDF_MAPE$Year), y_obj-1))+
          scale_x_continuous(breaks = seq(from=2000, to = 2030, by = 1))+
          scale_size_manual(guide = "none", values = c(2,2))+
          scale_color_manual(name="", 
                             values = colorBlindBlack8[c(1,3,8,7,7,7)],
                             breaks = c("Obs.","ARIMA", "Sib.","ARIMA Projected","80% CI", "95% CI"))+
          # guides(color = guide_legend(override.aes = list(size=c(2,4,2,2,2))))+
          theme_clean()+
          theme(legend.position = "top",
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                plot.background = element_blank(), 
                legend.background = element_blank()
          )
        
        
        
        
      }
      if(run_forecast_method=='sibling'){
        plot(base_table$Year, base_table$Run, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
             ylim=c(min(base_table$Run, run_preseason), max(base_table$Run, run_preseason)))
        points(y_obj, run_preseason , col='red', pch=16, type='o', cex=1.1)
        mtext(side=2, 'Run size', line=2.5, cex=0.85)
      }
      # text(x=max(base_table$Year), y=0.9*c(max(base_table$Run, run_preseason)), 'B')
      
      # 3) Potential Yield
      # plot(base_table$Year, base_table$Potential_Yield_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
      #      ylim=c(min(base_table$Potential_Yield_EEZ, Potential_Yield_EEZ_preseason), max(base_table$Potential_Yield_EEZ, Potential_Yield_EEZ_preseason)))
      # points(y_obj,Potential_Yield_EEZ_preseason , col='red', pch=16, type='o', cex=1.1)
      # points(y_obj,ABC_preseason, col='red', pch=1, type='o', cex=1.1)
      # mtext(side=2, 'Potential Yield EEZ', line=2.5, cex=0.85)
      # text(x=max(base_table$Year), y=0.9*c(max(base_table$Potential_Yield_EEZ, Potential_Yield_EEZ_preseason)), 'C')
      
      
      yieldDF <- rbind(base_table[,c("Year","Potential_Yield_EEZ")], c(y_obj, NA))
      yieldDF$PotYield_Pred <- c(rep(NA,length(run_forecast$fitted)), Potential_Yield_EEZ_preseason) # Potential Yield EEZ
      # yieldDF$type <- factor(c(rep( "Obs", n_years),"Pred"))
      yieldDF$ABC <- c(rep(NA,length(run_forecast$fitted)), ABC_preseason) #ABC ? OFL reduced to using buffer
      
      # Plot potential yield over time and this years preseason OFL and ABC
      yield_plot <- ggplot(yieldDF, aes(x = Year, y = Potential_Yield_EEZ))+
        geom_line(aes(col = "Obs"),size = 1.2)+
        # geom_line(aes(y = fitted, col = "Model fit"),size = 1.2,linetype = 2)+
        # geom_point(aes(col = type), size = c(rep(2,n_years),7))+
        geom_point(aes(col = "Obs"), size = 2)+
        geom_point(aes(y = PotYield_Pred, col = "Projected"), size = 4)+
        geom_point(aes(y = ABC, col = "ABC"), size = 4)+
        labs(x = "Year", y = "Potential Yield EEZ")+
        coord_cartesian(xlim=c(1999,y_obj+1))+
        # scale_size_manual(guide = "none", values = c(2,4))+
        scale_color_manual(name="",
                           values = colorBlindBlack8[c(1,2,3)],
                           breaks = c("Obs", "Projected", "ABC"))+
        scale_x_continuous(breaks = seq(from=2000, to = 2030, by = 5))+
        # guides(color = guide_legend(override.aes = list(size=c(2,4,2,2,2))))+
        theme_clean()+
        theme(legend.position = "top",
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14),
              plot.background = element_blank(), 
              legend.background = element_blank())
      
      png(file=paste0(getwd(),'/',stock,'/', y_obj,'_Fstate_forecast=',run_forecast_method, '_preseason_GGplots.png'),
          width = 600,  height = 400)
      print(Fstate_plot)
      dev.off()
      png(file=paste0(getwd(),'/',stock,'/', y_obj,'_run_forecast=',run_forecast_method, '_preseason_GGplots.png'),
          width = 600,  height = 400)
      print(runsize_ARFIT_plot)
      dev.off()
      png(file=paste0(getwd(),'/',stock,'/','_run_forecast=',run_forecast_method, '_RetroSpectivepreseason_GGplots.png'),
          width = 600,  height = 400)
      
      print(forecast_comp_plot)
      dev.off()
      
      zz <- ggarrange(Fstate_plot,runsize_ARFIT_plot, labels = c("a","b"), common.legend = T, vjust = c(0.1,0.1))
      
      comb.plot <- ggarrange(zz,forecast_comp_plot, ncol = 1, labels = c("","c"))
      
      png(file=paste0(getwd(),'/',stock,'/','_run_forecast=',run_forecast_method, '_CombinedPlot_GGplots.png'),
          width = 600,  height = 500)
      
      print(comb.plot)
      dev.off()
      # 4) F_EEZ Preseason
      #plot(base_table$Year, base_table$F_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, xlim=c(min(base_table$Year), max(base_table$Year)+1),
           #ylim=c(min(base_table$F_EEZ, F_EEZ_preseason, na.rm=TRUE), max(base_table$F_EEZ, F_EEZ_preseason, na.rm=TRUE )))
      #points(y_obj,F_EEZ_preseason , col='red', pch=16, type='o', cex=1.1)
      #mtext(side=2, 'F EEZ', line=2.5, cex=0.85)
      #text(x=max(base_table$Year), y=0.9*max(base_table$F_EEZ, F_EEZ_preseason, na.rm=TRUE), 'D')
      #dev.off()
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
    
    #Calculate OFL (postseason)
    Postseason_table$OFL_post <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$OFL_post[i] <- max(sum(Postseason_table$Potential_Yield_EEZ[(i-gen_lag+1):i]), 0)  
    }
    
    #Calculate cumulative catch
    Postseason_table$Cum_Catch <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      Postseason_table$Cum_Catch[i] <- sum(Postseason_table$C_EEZ[(i-gen_lag+1):i])  
    }
    
    #Was OFL exceeded?
    Postseason_table$OFL_exceeded <- NA
    for(i in gen_lag:nrow(Postseason_table)){
      if(Postseason_table$Cum_Catch[i] > Postseason_table$OFL_post[i]){
        Postseason_table$OFL_exceeded[i] <- 'YES'
      }else{
        Postseason_table$OFL_exceeded[i] <- 'NO'
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
        round(Postseason_table$Potential_Yield_EEZ,0),
        round(Postseason_table$Cum_Esc,0),
        round(Postseason_table$MSST,0)
        
      )
      colnames(Postseason_table_SSC) <- c('Year', 'Run size', 'Escapement', 'Escapement goal', 'Total catch', 'State catch', 'F_state', 'EEZ Catch',' F_EEZ', 'MFMT', 'Potential Yield EEZ', 'Cumulative Esc.','MSST' )
      write.csv(Postseason_table_SSC,file=paste0(getwd(),'/',stock,'/',y_obj,'_Postseason_table_SSC.csv'), row.names = FALSE)
    }
    #Postseason plots
    if(plot==TRUE){
      #Load preseason table to plot preseason values against actual values
      Preseason_table <- read.csv(file=paste0(getwd(),'/',stock,'/', y_obj,'_Preseason_table_SSC.csv'))
      
      png(file=paste0(getwd(),'/',stock,'/', y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method,'_postseason_plots.png'))
      par(mfrow=c(3,2), mar=c(2,2,2,2), oma=c(2,2,2,2))
      # 1) State harvest
      plot(Postseason_table$Year, Postseason_table$F_state, type='o', col='black', pch=16, xlab=NA, ylab=NA,
           ylim=c(min(Postseason_table$F_state, Preseason_table$F_state.preseason), max(Postseason_table$F_state, Preseason_table$F_state.preseason)))
      points(y_obj, Preseason_table$F_state.preseason , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'Proportion state harvest', line=2.5, cex=0.85)
      
      # 2) Run
      plot(Postseason_table$Year, Postseason_table$Run, type='o', col='black', pch=16, xlab=NA, ylab=NA,
           ylim=c(min(Postseason_table$Run, Preseason_table$run_preseason), max(Postseason_table$Run, Preseason_table$run_preseason)))
      points(y_obj, Preseason_table$run_preseason , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'Run size', line=2.5, cex=0.85)
      
      # 3) Potential Yield
      plot(Postseason_table$Year, Postseason_table$Potential_Yield_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA,
           ylim=c(min(Postseason_table$Potential_Yield_EEZ, Preseason_table$potential_yield_EEZ_preseason, na.rm=TRUE), max(Postseason_table$Potential_Yield_EEZ, Preseason_table$potential_yield_EEZ_preseason , na.rm=TRUE)))
      points(y_obj,Preseason_table$potential_yield_EEZ_preseason  , col='red', pch=16, type='o', cex=1.1)
      points(y_obj,Preseason_table$ABC_preseason , col='red', pch=1, type='o', cex=1.1)
      mtext(side=2, 'Potential Yield EEZ', line=2.5, cex=0.85)
      
      # 4) F_EEZ
      plot(Postseason_table$Year, Postseason_table$F_EEZ, type='o', col='black', pch=16, xlab=NA, ylab=NA, 
           ylim=c(min(Postseason_table$F_EEZ, Preseason_table$Potential_F_EEZ, na.rm=TRUE), max(Postseason_table$F_EEZ, Preseason_table$Potential_F_EEZ, na.rm=TRUE )))
      points(y_obj,Preseason_table$Potential_F_EEZ , col='red', pch=16, type='o', cex=1.1)
      mtext(side=2, 'F EEZ', line=2.5, cex=0.85)
      
      # 5) MFMT Preseason
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
    return_list$Preseason_Table <- Preseason_table_SSC
    if(F_state_forecast_method=='arima'){
      return_list$F_state_arima_terms <- F_state_arima
    } 
    if(run_forecast_method=='arima'){
      return_list$run_forecast_arima_terms <- run_forecast_arima
    } 
  }

  if(postseason==TRUE){
    return_list$PostSeason_Table <- Postseason_table
  }
  return(return_list)
}

#Determine buffer for tier 1 stocks based on retrospective preseason ABC (OFL) relative to postseason realized ABC
buffer_fun_ABC <- function(buffer_window=10,y_obj=2021, gen_lag, F_state_forecast_method, run_forecast_method, sib_forecast=NULL,
                       C_total, C_EEZ, Run, Esc, Esc_goal, years){
  
  #Compute postseason OFL
  Postseason <- Tier_1_fun(C_total=C_total, C_EEZ=C_EEZ, Run=Run, Esc=Esc, ABC_buffer=1,
                           Esc_goal=Esc_goal, Esc_goal_pre = Esc_goal, years=years, gen_lag = gen_lag, y_obj=y_obj, preseason=FALSE, postseason=TRUE, 
                           F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method, write=FALSE, plot=FALSE)  
  
  Postseason_OFL <- Postseason$PostSeason_Table$Potential_Yield_EEZ[Postseason$PostSeason_Table$Year %in% (y_obj-buffer_window):(y_obj-1)]
  
  #Compute retrospective preseason ABC (OFL) over the desired time window
  Preseason_OFL <- vector(length=length(Postseason_OFL))
  Preseason_run <- vector(length=length(Postseason_OFL))
  Preseason_f   <- vector(length=length(Postseason_OFL))
  
  yr <- vector(length=buffer_window)
  
  for(i in 1:buffer_window){
    Preseason <- Tier_1_fun(C_total=C_total, C_EEZ=C_EEZ, Run=Run, Esc=Esc, ABC_buffer=1, sib_forecast = sib_forecast,
                            Esc_goal=Esc_goal,Esc_goal_pre = Esc_goal, years=years, gen_lag = gen_lag,y_obj=(y_obj - i), preseason=TRUE, postseason=FALSE, 
                            F_state_forecast_method=F_state_forecast_method, run_forecast_method=run_forecast_method, write=FALSE, plot=FALSE)
    Preseason_OFL[i] <- Preseason$Preseason_Table[,'potential_yield_EEZ_preseason']
    Preseason_run[i] <- Preseason$Preseason_Table[,'run_preseason']
    Preseason_f[i]   <- Preseason$Preseason_Table[,'F_state preseason']
    
    yr[i] <- (y_obj - i)
  }
  Preseason_OFL <- rev(Preseason_OFL)
  Preseason_run <- rev(Preseason_run)
  Preseason_f   <- rev(Preseason_f)
  
  yr <- rev(yr)
  
  #Log accuracy ratio/MSE for ABC based on
  LAR <- log((Preseason_OFL+0.00000000000001)/(Postseason_OFL+0.00000000000001))
  LAR <- na.omit(is.finite(LAR)*LAR)
  LAR_MSA <- LAR[LAR>0] #Only calculate MSA/buffer based on positve errors (overforecast)
  MSA <- 100*(exp(median(abs(LAR_MSA), na.rm=TRUE))-1) #Gives median unsigned percentage error

  #Compute buffer based on MSA
  buffer <- max((100-MSA)/100, 0.1)

  #Write to .csv
  buffer_mat <- cbind(MSA, buffer)
  colnames(buffer_mat) <- c('MSA', 'buffer_factor')
  write.csv(buffer_mat, file=paste0(getwd(),'/',stock,'/',y_obj,'_F_state_forecast=',F_state_forecast_method,'_run_forecast=',run_forecast_method,'ABC_buffer_table.csv'))
  
  #Compile list to return
  return_list <- list()
  return_list$MSA <- MSA
  return_list$buffer <- buffer
  return_list$LAR <- LAR
  return_list$LAR_MSA <- LAR_MSA
  return_list$yr <- yr
  
  return_list$Preseason_run <- Preseason_run
  return_list$Preseason_OFL <- Preseason_OFL
  return_list$Preseason_f   <- Preseason_f  
  
  return(return_list)
}

#Function to do all FMP calculations for Tier 3 stocks
Tier_3_fun <- function(C_total , C_EEZ, years, catch_lag, buffer,
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
    OFL_pre <- max(rollmean(x=Preseason_table$C_EEZ, k=gen_lag))
    OFL <- max(rollsum(x = Preseason_table$C_EEZ, k = gen_lag))
    # if(gen_lag > 1){
    #   OFL_pre <- OFL - sum(Preseason_table$C_EEZ[(nrow(Preseason_table)-gen_lag+2):nrow(Preseason_table)])
    # }else{
    #   OFL_pre <- OFL
    # }
    
    #Determine ACL
    ABC <- vector(length=length(buffer))
    # ABC_pre <- vector(length=length(buffer))
    # ACL <- vector(length=length(buffer))
    # ACL_pre <- vector(length=length(buffer))
    
    for(i in 1:length(buffer)){
      # ABC[i] <- OFL*buffer[i]
      ABC[i] <- OFL_pre*(1-buffer[i])
      
      #Determine ABC
      # ACL[i] <- ABC[i]
      # ACL_pre[i] <- ABC_pre[i]
    }
    
    #Write to csv
    Preseason_SDC <- cbind(paste0(buffer*100,"%"), round(rep(OFL_pre, length(buffer)),0), round(ABC,0),rep(OFL,length(buffer)))
    colnames(Preseason_SDC) <- c('Buffer', 'Preseason OFL', 'ABC', 'OFL')
    Preseason_table$C_total <- round(Preseason_table$C_total, 0)
    Preseason_table$C_EEZ<- round(Preseason_table$C_EEZ, 0)
    Preseason_table$Cum_Catch<- round(Preseason_table$Cum_Catch, 0)
    
    colnames(Preseason_table) <- c('Year', 'Total catch', 'EEZ catch', 'Cumulative EEZ catch' )
    
    write.csv(Preseason_table, file=paste0(getwd(),'/',stock,'/',y_obj,'_tier_3_preseason_table.csv'))
    write.csv(Preseason_SDC, file=paste0(getwd(),'/',stock,'/',y_obj,'_tier_3_preseason_SDC.csv'), row.names = F)
    
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
    # OFL <- max(Postseason_table$C_EEZ[(nrow(Postseason_table)-(catch_lag-1)):nrow(Postseason_table)])*gen_lag
    OFL <- max(rollsum(x=Postseason_table$C_EEZ, k=gen_lag))
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








