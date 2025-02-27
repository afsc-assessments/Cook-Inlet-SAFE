# Project Name: Upper Cook Inlet Bayesian PF and harvest specs
# Creator: Aaron Lambert - NOAA
# Date: 9-20-2024

# Version: Retrospective testing of Bayesian model 

# Purpose: To assess the performance of models and look at the probability of 
#          over fishing given ABC buffers


# 1) Read in Data
# 2) Preprocess Data
# 3) Call Stan Model (Loop over years)
# 4) Analysis/Plots


# Notes:




# Package
require(rstan)
require(bayesplot)
require(tidyverse)
require(mgcv)
require(ggthemes)
library(ggpubr)
# require(viridis)
require(shinystan)
# require(lubridate)
# require(reshape2)
library(Metrics)
require(tidybayes)


# Parallelize for optimum model run time (Speeds things up...)
rstan_options(auto_write = TRUE) 
mc.cores = parallel::detectCores()

# Define Workflow Paths ============================================

# set to working directory (Set to directory with all the necessary folders)
wd <- getwd()

# setwd(wd)

# Objects used to save/load data, outputs, or stan/R scripts
# To use this, one must have folders with these names in the working directory
# dir.output <- file.path(wd,"output")
# dir.figs <- file.path(wd,"figs")
dir.stan <- file.path(wd,"stan")
# dir.data <- file.path(wd,"data")
# dir.R <- file.path(wd,"R")


######### Import Data ###############
stock <- 'Kasilof Sockeye'
# Data <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Data.csv'))
Forecast <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Forecasts.csv'))
# Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Table.csv'))
Table <- read.csv(file=paste0(getwd(),'/',stock,'/', 'Total Run Size Long.csv'))

# Control Section ##############################################################

# Model version
model.version <- "AR1_betaFit_long"

# MCMC Parameters
n.chains = 4
n.iter = 10000
n.thin = 2

# First year to include in PF
start.year <- 1979

# Smsy reference point 
if(stock == "Kenai Sockeye"){
Esc_goal_smsy <- (1212000)
}

if(stock == "Kasilof Sockeye"){
Esc_goal_smsy = (222000)
}

# Data processing ############################################################

# Years used in retro testing
testyears <- c(1999:2024)

# Realized potential yield
Table$Pot_yield_SMSY <- Table$Run - Esc_goal_smsy - (Table$Total.Catch - Table$EEZCatch)
Table$Pot_yield_lwr <- Table$Run - Table$Lower - (Table$Total.Catch - Table$EEZCatch)

# # Fstate
Table$C_state <- Table$Total.Catch - Table$EEZCatch
Table$F_state <- Table$C_state/Table$Run

# Function to get beta dist parameters from mean and variance
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
  
}

# Data frame to hold results
retroDF <- data.frame("Year" = testyears,
                      "Stock"  = stock,
                      "Model" = model.version,
                      "Iterations" = n.iter,
                      "median.runsize" = NA,
                      "median.stateF" = NA,
                      "median.OFLpre.SMSY" = NA,
                      "median.OFLpre.lwr" = NA,
                      "Buff_smsy"= NA,
                      "Buff_lwr" = NA,
                      "ABC_Smsy_cumprob" = NA,
                      "ABC_lwr_cumprob" = NA,
                      "OFL_true_smsy" = Table$Pot_yield_SMSY[Table$Year%in%testyears],
                      "OFL_true_lwr" = Table$Pot_yield_lwr[Table$Year%in%testyears])


# List to hold plots
plot.list <- list()

# Color blind palette for plotting
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Retrospective loop over testyears
for(y in testyears){
  
  # uncomment to check code for a single year
  # y <- 2016
  
# Years Used in Run size forecast
years <- Table$Year[Table$Year < y & 
                      Table$Year>= start.year]

# Number of years used in run size forecast
n.years <- length(years)

# Realized past run sizes
runsize <- Table$Run[Table$Year < y &
                       Table$Year >= start.year]

# The predicted years truth for plotting
real.PF <- Table$Run[Table$Year == y]

# Historic F
F_state <- Table$F_state[Table$Year < max(Table$Year) &
                           Table$Year>=1999]

# Number of years Fstate
n.years.F <- length(F_state)

# True F for plotting
real.F <- Table$F_state[Table$Year == y]

# Only used in certain models (not necessary in future iterations)
params <-estBetaParams(mu=mean(Table$F_state,na.rm = T), var = sd(Table$F_state,na.rm = T)^2)

A <- params$alpha
B <- params$beta

# Inits list (not currently needed)
# inits <-function(){
#   #   
#   list("alpha_R" = runif(1,0,20),
#        "beta_R" = runif(1,0,1),
#        "theta" = runif(1,-1,0),
#        "mu" = runif(1,-1,1),
#        "sigma" = runif(1,0,1),
#        "sigma_F"=runif(1,0,1)
#        
#        
#   )
# }

# inits_ll <- list(inits(), inits(), inits(), inits())

# Call the Stan model
fit <- stan(file = file.path(dir.stan,paste("UCI_",
                                            model.version ,
                                            ".stan", 
                                            sep = "")), 
            data = list(n_years = n.years, 
                        n_years_F = n.years.F,
                        run_hist = runsize,
                        Fstate = F_state,
                        A = A,
                        B=B),
            # init = inits_ll,
            chains = n.chains,
            iter = n.iter, 
            thin = n.thin, 
            cores = mc.cores,
            # adjust this for treedepth warnings/div chains warnings
            # control = list(max_treedepth = 25, adapt_delta = 0.99),
            verbose = F,
            save_warmup = F
            
)



# Extract parameter estimates for plotting & analysis ##########################
pars <- rstan::extract(fit)

# Calculate OFLpre
# USing SMSY
OFL_pre_smsy <- (pars$post_curr_predRunsize - Esc_goal_smsy ) - 
  (pars$post_curr_predRunsize * pars$post_curr_predFstate)

# Using lower bound
OFL_pre_lwr <- (pars$post_curr_predRunsize - (Table$Lower[Table$Year==y]) ) - 
  (pars$post_curr_predRunsize * pars$post_curr_predFstate)

# Get the CDF to calculate cum prob
OFL_cdf_smsy <- ecdf(OFL_pre_smsy)
OFL_cdf_lwr <- ecdf(OFL_pre_lwr)

# Record the buffer 
retroDF$Buff_smsy[retroDF$Year==y] <- (OFL_cdf_smsy(0))
retroDF$Buff_lwr[retroDF$Year==y] <- (OFL_cdf_lwr(0))

# Use the cum prob of overfished to set abc
retroDF$ABC_Smsy_cumprob[retroDF$Year==y] <- round(median(OFL_pre_smsy) * (1-OFL_cdf_smsy(0)))
retroDF$ABC_lwr_cumprob[retroDF$Year==y] <- round(median(OFL_pre_lwr) * (1-OFL_cdf_lwr(0)))

# Get the OFLpre point value (median of OFLpre dist)
retroDF$median.OFLpre.SMSY[retroDF$Year == y] <- median(OFL_pre_smsy)
retroDF$median.OFLpre.lwr[retroDF$Year == y] <- median(OFL_pre_lwr)

# Get the point est of PF and Fstate
retroDF$median.runsize[retroDF$Year == y] <- median(pars$post_curr_predRunsize)
retroDF$median.stateF[retroDF$Year == y] <- median(pars$post_curr_predFstate)

# Plots of the PF fit
# Calculate quantiles for data
quant.predRun <- apply(X = exp(pars$ln_predRunsize),
                       MARGIN = 2,
                       FUN = quantile,
                       probs=c(0.025, 0.25, 0.5, 0.75, 0.975), 
                       na.rm = T)

# Add to df for plotting
plot.df <- data.frame(years,
                      runsize,
                      t(quant.predRun),
                      "Obs.")

# Name the columns
names(plot.df) <- c("Year","Run","low95","low50","median",
                    "up50","up95","cat")

# Get the current years prediction and CI
curr <- quantile(pars$post_curr_predRunsize,probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

# Put in DF
curr.df <- data.frame(Year = y,
                      Run =  real.PF,
                      low95 = unname(curr[1]),
                      low50 =  unname(curr[2]),
                      median =  unname(curr[3]),
                      up50 =  unname(curr[4]),
                      up95 =  unname(curr[5]),
                      cat = "Curr. Year")

# bind to model fits
plot.df <- rbind(plot.df, curr.df)

# Plot the run size prediction
PF.plot <- plot.df %>% 
  ggplot(aes(x = Year, y = Run))+
  # geom_point(aes(col = "Run Size"))+
  geom_ribbon(aes(ymin = low95, ymax = up95, fill = "95% CI"), alpha = .6)+
  geom_ribbon(aes(ymin = low50, ymax = up50, fill = "50% CI"), alpha = .6)+
  geom_line(aes(y = median, col = "Median"), linewidth = 1.22)+
  geom_line(aes(col = cat))+
  geom_point(aes(col = cat), size = 2)+
  # scale_fill_colorblind(name = "")+
  # scale_color_colorblind(name ="")+
  scale_color_manual(name="", 
                     values = colorBlindBlack8[c(1,3,7,7,7)],
                     breaks = c("Median", "Obs.", "Curr. Year"))+
  scale_fill_manual(name="", 
                    values = colorBlindBlack8[c(7,2)],
                    breaks = c("50% CI", "95% CI"))+
  ggtitle(label = paste0("PF for year=",y))+
  labs(y="Total run size (000's)")+
  theme_clean()+
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.background = element_blank(), 
        legend.background = element_blank()
  )



# Densities of posterior current predictions for plotting
run.df <- data.frame("par" = "RunSize", "value" = pars$post_curr_predRunsize)

# Fstate.df <- data.frame("par" = "F_state", "value" = pars$post_curr_predFstate)
Fstate.df <- data.frame("par" = "F_state", "value" = pars$post_curr_predFstate)

# OFLpre.df <- data.frame("par" = "OFLpre", "value" = OFL_pre_smsy)

# plot.df <- rbind(run.df, Fstate.df, OFLpre.df)

run.plot <- ggplot(run.df, aes(x = value/1000))+
  geom_density(fill = colorBlindBlack8[6], alpha = .7)+
  xlab("Predicted Run Size (Thousands of Salmon)")+
  ylab("")+
  coord_cartesian(xlim = c(0,ifelse(stock=="Kasilof Sockeye",3500,12000)))+
  scale_x_continuous(n.breaks = 10)+
  # ggtitle(label = paste0("Retrospective plots for year=",y))+
  theme_classic()+
  theme(legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), 
        plot.margin = margin(t = 1, b = 0, l = 1,unit = "cm"),
        plot.background = element_blank(), 
        panel.border = element_blank(), 
        legend.background = element_blank() )


Fstate.plot <- ggplot(Fstate.df, aes(x = value, ..scaled..))+
  geom_density(fill = colorBlindBlack8[7], alpha = .7)+
  xlab("Predicted State Harvest Rate")+
  ylab("")+
  coord_cartesian(xlim=c(0,1))+
  theme_classic()+
  theme(legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), 
        plot.background = element_blank(),
        plot.margin = margin(t = 1, b = 0, l = 1,unit = "cm"),
        panel.border = element_blank(), 
        legend.background = element_blank() )


# get the density for OFL
estDensity <- density(OFL_pre_smsy)

dense.df <- data.frame(x = estDensity$x,
                       y = estDensity$y,
                       cat = ifelse(estDensity$x<=0, "No", "yes"))

# Calculate ABC
abc <- (median(OFL_pre_smsy)*(1- OFL_cdf_smsy(0)))/1000

# ABC line for plotting
abc.line <- data.frame(x1 = abc,
                       x2 = abc,
                       y1 = 0, 
                       y2 = max(dense.df$y[dense.df$x>=abc])*.95)

# Predicted median OFL for plotting
ofl.line <- data.frame(x1 = median(OFL_pre_smsy)/1000,
                       x2 = median(OFL_pre_smsy)/1000,
                       y1 = 0,
                       y2 = max(dense.df$y[dense.df$x>=median(OFL_pre_smsy)]))

# Realized oFL for plotting
real.ofl.line <- data.frame(x1 = retroDF$OFL_true_smsy[retroDF$Year==y]/1000, 
                            x2 = retroDF$OFL_true_smsy[retroDF$Year==y]/1000, 
                            y1 = 0,
                            y2 = ifelse(retroDF$OFL_true_smsy[retroDF$Year==y]>= dense.df$x[dense.df$y==max(dense.df$y)],
                                        max(dense.df$y[dense.df$x>=retroDF$OFL_true_smsy[retroDF$Year==y]]),
                                        max(dense.df$y[dense.df$x<=retroDF$OFL_true_smsy[retroDF$Year==y]])))

# Plot the OFL density
OFLpre.plot <- ggplot(dense.df)+
  geom_line( aes(x = x/1000, y = y))+
  geom_ribbon(aes(x = x/1000, 
                  ymin = 0,
                  ymax = y, fill = cat))+
  geom_segment(data = abc.line, aes(x = x1, xend = x2, y = y1, yend = y2, col = "ABC"),
               linewidth = 1.2, 
               linetype = 3)+
  geom_segment(data = ofl.line, aes(x = x1, xend = x2, y = y1, yend = y2, col = "OFLpre"),
               linewidth = 1.2, 
               linetype = 1)+
  geom_segment(data = real.ofl.line, aes(x = x1, xend = x2, y = y1, yend = y2, col = "Realized OFLpre"),
               linetype = 2,
               linewidth = 1.2)+
  xlab("OFLpre (Thousands of Salmon)")+
  ylab("")+
  # coord_cartesian(xlim=c(-300,1300))+
  coord_cartesian(xlim = c(ifelse(stock=="Kasilof Sockeye",-300,-500),
                           ifelse(stock=="Kasilof Sockeye",1200,4000)))+
  scale_fill_manual(values = colorBlindBlack8[c(1,2)],
                    name = "Surplus EEZ Yield?")+
  scale_color_manual(values = colorBlindBlack8[c(3,4,7)],
                     name = "SDC")+
  theme_classic()+
  theme(legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), 
        plot.margin = margin(t = 1, b = 0, l = 1,unit = "cm"),
        plot.background = element_blank(),
        panel.border = element_blank(), 
        legend.key.width = unit(1.1,"cm"),
        legend.background = element_blank() )

# Combine all density plots
pp <- ggarrange(run.plot,
                Fstate.plot,
                OFLpre.plot, ncol = 1,
                align = "v", labels = c("a","b","c"), vjust = 0.9)

# Annotate so we know which year and model the plots are for
comb.plot <- annotate_figure(pp, 
                             top = text_grob(paste0(stock," ",y, " Retrospective Plots \n",model.version),
                                             size = 15,
                                             face = "bold"))

# Store plots in the plot list
plot.list[[paste0("pf_",y)]] <- PF.plot
plot.list[[paste0("DensPlot_",y)]] <- comb.plot

# Print statement to see how loop is progressing
print(paste0("Finally done with year = ",y))

} # Close loop

# Save plots as PDF
pdf(file = paste0(getwd(),"/",stock,"/Retro plots_",model.version,".pdf"), 
    width =9, height = 9)
plot.list
dev.off()

# Join with true run size and Fstate
retroDF <- left_join(retroDF, Table[c('Year','Run','F_state')])

# Save Output to compare with other models/year range runs below
# saveRDS(object = retroDF, file = paste0(getwd(),"/",stock,"/",model.version,"_retro_results_24Feb2025.RDS"))

# Model comparison ########################################################################

Kenai_b <- readRDS(file = paste0(getwd(),"/Kenai Sockeye/AR1_beta_long_retro_results_24Feb2025.RDS"))
Kasilof_b <- readRDS(file = paste0(getwd(),"/Kasilof Sockeye/AR1_beta_long_retro_results_24Feb2025.RDS"))
Kenai_bf <- readRDS(file = paste0(getwd(),"/Kenai Sockeye/AR1_betaFit_long_retro_results_24Feb2025.RDS"))
Kasilof_bf <- readRDS(file = paste0(getwd(),"/Kasilof Sockeye/AR1_betaFit_long_retro_results_24Feb2025.RDS"))
# Kenai_PDO <- readRDS(file = paste0(getwd(),"/Kenai Sockeye/AR1_logit_WN_PDO_retro_results.RDS"))
# Kasilof_MM <- readRDS(file = paste0(getwd(),"/Kasilof Sockeye/AR1_logit_retro_results.RDS"))
# Kasilof_WN <- readRDS(file = paste0(getwd(),"/Kasilof Sockeye/AR1_logit_WN_retro_results.RDS"))
# Kasilof_PDO <- readRDS(file = paste0(getwd(),"/Kasilof Sockeye/AR1_MA_PDO_retro_results.RDS"))

# Add model runs here to assess against each other
allStockDF <- rbind(Kenai_b, Kenai_bf, Kasilof_b, Kasilof_bf)

# Plot showing MAPE of models 
allStockDF %>% 
  group_by(Stock,Model) %>% 
  summarise(Run_MAPE = mape(predicted = median.runsize, actual = Run),
            F_MAPE = mape(predicted = median.stateF, actual = F_state)) %>% 
  pivot_longer(cols = c(Run_MAPE,F_MAPE)) %>% 
  ggplot(aes(x = factor(Stock), y = value))+
    geom_point(aes(col = Model),size = 2, position = "Jitter")+
    facet_wrap(~factor(name))


# # CalculateOFL_true_smsy# Calculate the ABC across a range of buffers
# allStockDF$buffer <- NA
# allStockDF$ABC_lwr <- NA
# allStockDF$ABC_smsy <- NA
# allStockDF$err_lwr <- NA
# allStockDF$err_smsy <- NA
# allStockDF$perc_err_lwr <- NA
# allStockDF$perc_err_smsy <- NA
# 
# buffer <- seq(.1,.9,.1)
# 
# for (b in 1:length(buffer)) {
#   # b <- 1
#   
#   buff <- buffer[b]
#   
#   tempDF <- allStockDF
#   
#   tempDF$ABC_lwr <- tempDF$median.OFLpre.lwr*(1-buff)
#   
#   tempDF$ABC_smsy <- tempDF$median.OFLpre.SMSY*(1-buff)
#   
#   tempDF$err_lwr <- tempDF$ABC_lwr - tempDF$OFL_true_lwr
#   
#   tempDF$err_smsy <- tempDF$ABC_smsy - tempDF$OFL_true_smsy
#   
#   tempDF$perc_err_lwr <- (tempDF$err_lwr/tempDF$OFL_true_lwr)*100
#   
#   tempDF$perc_err_smsy <- (abs(tempDF$err_smsy)/(abs(tempDF$OFL_true_smsy)))*100
#   
#   tempDF$buffer <- buff*100
#   
#   if(b==1){finalDF <- tempDF}else{finalDF <- rbind(finalDF, tempDF)}
#   
# }

# Calculate the probability of overforecasting
# finalDF$yes_over <- NA
# 
# finalDF %>% 
#   group_by(Stock) %>% 
#   mutate(yes_over)
# 
# 
# propDF <- finalDF %>% 
#   group_by(buffer,Stock) %>% 
#   summarise(sum(yes_over)/length(testyears))

# The ABC error 
allStockDF$smsy_err <- allStockDF$ABC_Smsy_cumprob - allStockDF$OFL_true_smsy
allStockDF$lwr_err <- allStockDF$ABC_lwr_cumprob - allStockDF$OFL_true_lwr

# Record if error is over or under to calculate the prob of overfishing given an ABC
allStockDF$yes_over_smsy <- ifelse(allStockDF$smsy_err>0,1,0)
allStockDF$yes_over_lwr <- ifelse(allStockDF$lwr_err>0,1,0)

# Table of probability of overfishing given ABC
allStockDF %>% 
  group_by(Stock, Model) %>% 
  summarise("Prob_smsy" = sum(yes_over_smsy)/length(yes_over_smsy),
            "Prob_lwr" = sum(yes_over_lwr)/length(yes_over_lwr))


# png(filename = paste0(getwd(),"/Figures/2025 OFL vs OFLpred Bayesian.png"))

# Plot OFL verse OFLpre 
allStockDF %>% 
  ggplot(aes(x = Year, y = OFL_true_smsy))+
  geom_col(aes(fill = "Obs"))+
  geom_point(aes(y = median.OFLpre.SMSY, col = "Median Pred"), size = 2)+
  geom_line(aes(y = median.OFLpre.SMSY, col = "Median Pred"))+
  scale_fill_manual(values=colorBlindBlack8[1], name = "")+
  scale_color_manual(values=colorBlindBlack8[2], name = "")+
  scale_x_continuous(breaks = unique(allStockDF$Year))+
  # scale_y_continuous(breaks = seq(-200,1000, 200))+
  # facet_wrap(~Stock)+
  facet_grid(Stock~Model)+
  labs(y = "OFL (Thousands of sockeye salmon)")+
  theme_clean()+
  theme(plot.background = element_blank(),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.background  = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle=90))
dev.off()

# Scratch ######################################################################
# allStockDF %>% 
#   ggplot(aes(x = Year, y = Run))+
#   geom_col(aes(fill = "Obs"))+
#   geom_point(aes(y = median.runsize, col = "Median Pred"), size = 2)+
#   scale_fill_manual(values=colorBlindBlack8[1], name = "")+
#   scale_color_manual(values=colorBlindBlack8[2], name = "")+
#   scale_x_continuous(breaks = unique(finalDF$Year))+
#   scale_y_continuous(breaks = seq(0,5000, 500))+
#   # facet_wrap(~Model)+
#   facet_grid(Stock~Model)+
#   labs(y = "OFL (Thousands of sockeye salmon)")+
#   theme_clean()+
#   theme(plot.background = element_blank(),
#         axis.text = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.background  = element_blank(),
#         legend.position = "top",
#         axis.text.x = element_text(angle=90))
# 
# err.plot <- finalDF %>% 
#   ggplot(aes(x = Year, y = err_smsy, fill = factor(buffer)))+
#   geom_col(position = "dodge")+
#   labs(y = "Error (000's fish)")+
#   scale_fill_manual(name = "", values = colorBlindBlack8,
#                     labels = c("10%", "20%","30%", "40%", "50%","60%", "70%","80%","90%"))+
#   scale_y_continuous(breaks = seq(-2000,500, 200))+
#   scale_x_continuous(breaks = unique(finalDF$Year))+
#   # facet_grid(Stock~Model)+
#   facet_wrap(~Stock)+
#   theme_clean()+
#   theme(plot.background = element_blank(),
#         axis.text = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.position = "right",
#         axis.text.x = element_text(angle=90))
# 
# 
# perc.err.plot <- finalDF %>% 
#   ggplot(aes(x = Year, y = perc_err_smsy, fill = factor(buffer)))+
#   geom_col(position = "dodge")+
#   labs(y = "Percent Error")+
#   scale_fill_manual(name = "", values = colorBlindBlack8,
#                     labels = c("10%", "20%","30%", "40%", "50%","60%", "70%","80%","90%"))+  scale_y_continuous(breaks = seq(-200,900, 100))+
#   scale_x_continuous(breaks = unique(finalDF$Year))+
#   # facet_grid(Stock~Model)+
#   facet_wrap(~Stock)+
#   theme_clean()+
#   theme(plot.background = element_blank(),
#         axis.text = element_text(size=14),
#         axis.title = element_text(size=14),
#         legend.position = "right",
#         axis.text.x = element_text(angle=90))


# pdf(file = paste0(wd,"/",stock,"/",model.version,"_",stock,"_retroplots.pdf"))
# 
# err.plot
# lwr.per.err.plot
# Smsy.per.err.plot
# plot.list
# 
# dev.off()

# Probability of over forecasting
tt <- finalDF %>% 
  group_by(Stock,buffer,Year) %>% 
  reframe(rank = ifelse(err_smsy>0,1,0))
tt %>% 
  group_by(Stock,buffer) %>% 
  reframe(prop = sum(rank==1)/length(Year))

allStockDF %>% 
  group_by(Model) %>% 
  mutate(err = median.OFLpre.SMSY - OFL_true_smsy) %>% 
  # filter( OFL_true_smsy>0) %>% 
  reframe(MAPE.oFL = mape(predicted = median.OFLpre.SMSY, actual = OFL_true_smsy),
          MAPE.run = mape(predicted = median.runsize, actual = Run),
          RMSE.run = rmse(predicted = median.runsize, actual = Run))

Kenai_WN$median.OFLpre.SMSY



#Log accuracy ratio/MSE for ABC based on
LAR <- log((retroDF$median.OFLpre.SMSY+0.00000000000001)/(retroDF$OFL_true_smsy+0.00000000000001))
LAR <- na.omit(is.finite(LAR)*LAR)
LAR_MSA <- LAR[LAR>0] #Only calculate MSA/buffer based on positve errors (overforecast)
MSA <- 100*(exp(median(abs(LAR_MSA), na.rm=TRUE))-1) #Gives median unsigned percentage error

#Compute buffer based on MSA
buffer <- min((MSA)/100, 0.9)

# MAPE
(1/length(retroDF$Year))*sum(abs((retroDF$OFL_true_lwr- retroDF$median.OFLpre.lwr)/retroDF$OFL_true_lwr))
# 
# # Look at model fit to data
# 
# # Calulate quantiles for data
# quant.predRun <- apply(X = exp(pars$ln_predRunsize),
#                        MARGIN = 2,
#                        FUN = quantile,
#                        probs=c(0.025, 0.25, 0.5, 0.75, 0.975), 
#                        na.rm = T)
# 
# plot.df <- data.frame(years,
#                       runsize,
#                       t(quant.predRun))
# 
# names(plot.df) <- c("Year","Run","low95","low50","median",
#                     "up50","up95")
# 
# curr <- quantile(pars$post_curr_predRunsize,probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
# 
# curr.df <- data.frame(Year = myYear,
#                       Run =  NA,
#                       low95 = unname(curr[1]),
#                       low50 =  unname(curr[2]),
#                       median =  unname(curr[3]),
#                       up50 =  unname(curr[4]),
#                       up95 =  unname(curr[5]))
# 
# plot.df <- rbind(plot.df, curr.df)
# 
# # Plot the run size prediction
# plot.df %>% 
#   ggplot(aes(x = Year, y = Run))+
#   geom_point(aes(col = "Run Size"))+
#   geom_line(aes(col = "Run Size"))+
#   geom_line(aes(y = median, col = "Median"))+
#   geom_ribbon(aes(ymin = low95, ymax = up95, fill = "95% CI"), alpha = .2)+
#   geom_ribbon(aes(ymin = low50, ymax = up50, fill = "50% CI"), alpha = .2)+
#   scale_fill_colorblind(name = "")+
#   scale_color_colorblind(name ="")+
#   labs(y="Total run size (000's)")+
#   theme_clean()+
#   theme(legend.position = "top",
#         plot.background = element_blank())
# 
# # FSTATE plots ################################################################
# # Calulate quantiles for data
# quant.predF <- apply(X = (pars$pred_Fstate),
#                      MARGIN = 2,
#                      FUN = quantile,
#                      probs=c(0.025, 0.25, 0.5, 0.75, 0.975), 
#                      na.rm = T)
# 
# plot.F.df <- data.frame(years,
#                         F_state,
#                         t(quant.predF))
# 
# names(plot.F.df) <- c("Year","Fstate","low95","low50","median",
#                       "up50","up95")
# 
# curr.Fstate <- quantile(pars$post_curr_predFstate,
#                         probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
# 
# curr.F.df <- data.frame(Year = myYear,
#                         Fstate =  NA,
#                         low95 = unname(curr.Fstate[1]),
#                         low50 =  unname(curr.Fstate[2]),
#                         median =  unname(curr.Fstate[3]),
#                         up50 =  unname(curr.Fstate[4]),
#                         up95 =  unname(curr.Fstate[5]))
# 
# plot.F.df <- rbind(plot.F.df, curr.F.df)
# 
# 
# plot.F.df %>% 
#   ggplot(aes(x = Year, y = Fstate))+
#   geom_point(aes(col = "Fstate"))+
#   geom_line(aes(col = "Fstate"))+
#   geom_line(aes(y = median, col = "Median"))+
#   geom_ribbon(aes(ymin = low95, ymax = up95, fill = "95% CI"), alpha = .2)+
#   geom_ribbon(aes(ymin = low50, ymax = up50, fill = "50% CI"), alpha = .2)+
#   coord_cartesian(ylim = c(0,1))+
#   scale_fill_colorblind(name="")+
#   scale_color_colorblind(name="")+
#   theme_clean()+
#   theme(legend.position = "top",
#         plot.background = element_blank())
# 
# 
# 
# # Calculate the OFL
# OFL_pre <- (pars$post_curr_predRunsize - Esc_goal_pre ) - (pars$post_curr_predRunsize * pars$post_curr_predFstate)
# 
# # Get the CDF to calculate probabilies for statements
# OFL_cdf <- ecdf(OFL_pre)
# 
# 
# # Pallete for plotting
# colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
#                        "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# # Densities of posterior current predictions for plotting
# run.df <- data.frame("par" = "RunSize", "value" = pars$post_curr_predRunsize)
# 
# Fstate.df <- data.frame("par" = "F_state", "value" = pars$post_curr_predFstate)
# 
# OFLpre.df <- data.frame("par" = "OFLpre", "value" = OFL_pre)
# 
# plot.df <- rbind(run.df, Fstate.df, OFLpre.df)
# 
# run.plot <- ggplot(run.df, aes(x = value))+
#   geom_density(fill = colorBlindBlack8[3], alpha = .7)+
#   xlab("Predicted Run Size")+
#   ylab("Relative probability")+
#   coord_cartesian(xlim = c(0,11000))+
#   theme(legend.position = "top",
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 14), 
#         plot.background = element_blank(), 
#         panel.border = element_blank(), 
#         legend.background = element_blank() )+
#   theme_classic()
# 
# Fstate.plot <- ggplot(Fstate.df, aes(x = value))+
#   geom_density(fill = colorBlindBlack8[4], alpha = .7)+
#   xlab("Predicted F_state")+
#   ylab("Relative probability")+
#   coord_cartesian(xlim=c(0,1))+
#   theme(legend.position = "top",
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 14), 
#         plot.background = element_blank(),
#         panel.border = element_blank(), 
#         legend.background = element_blank() )+
#   theme_classic()
# 
# OFLpre.plot <- ggplot(OFLpre.df, aes(x = value))+
#   geom_density(fill = colorBlindBlack8[5], alpha = .7)+
#   xlab("OFLpre")+
#   ylab("Relative probability")+
#   coord_cartesian(xlim=c(min(OFLpre.df$value),5000))+
#   theme(legend.position = "top",
#         axis.text = element_text(size = 12),
#         axis.title = element_text(size = 14), 
#         plot.background = element_blank(),
#         panel.border = element_blank(), 
#         legend.background = element_blank() )+
#   theme_classic()
# 
# ggarrange(run.plot,
#           Fstate.plot,
#           OFLpre.plot, ncol = 1,
#           align = "v")
# 
# mean(pars$post_curr_predFstate)
# median(OFL_pre)
# 
# # Credible intervals
# quantile(OFL_pre, probs = c(0.025, .1,0.25, 0.5, 0.75,.9, 0.975))
# 
# 1-OFL_cdf(3000)


