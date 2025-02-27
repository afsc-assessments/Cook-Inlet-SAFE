# Project Name: Upper Cook Inlet Bayesian PF and harvest specs
# Creator: Aaron Lambert - NOAA
# Date: 2-27-2025

# Version: This version is for the long timeseries PF and Fstate predicted from
#          distribution parameterized by historical observations.

# Purpose: To generate an AR1 PF and calculate ABC, Fstate, OFL, MFMT

# 1) Read in Data
# 2) Preprocess Data
# 3) Call Stan Model
# 4) Analysis/Plots


# Notes:




# Packages

require(rstan)
require(bayesplot)
require(tidyverse)
require(mgcv)
require(ggthemes)
library(ggpubr)
library(LaplacesDemon)
# require(viridis)
require(shinystan)
# require(lubridate)
# require(reshape2)
# require(dplyr)
require(tidybayes)


# Parralize for optimum model run time (Speeds things up...)
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
model.version = "AR1_betaFit_long"

# MCMC Parameters
n.chains = 4
n.iter = 10000
n.thin = 2

# Year projection is made
myYear <- 2025
start.year <- 1979

# Smsy point est 
if(stock == "Kenai Sockeye"){
  Esc_goal_smsy <- (1212000)
}

if(stock == "Kasilof Sockeye"){
  Esc_goal_smsy = (222000)
}


# Fstate
Table$C_state <- Table$Total.Catch- Table$EEZCatch
Table$F_state <- Table$C_state/(Table$Run)

# Only used on AR1_beta_long stan script.
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

params <-estBetaParams(mu=mean(Table$F_state,na.rm = T), var = sd(Table$F_state,na.rm = T)^2)

A <- params$alpha
B <- params$beta

# Run size years used
years <- Table$Year[Table$Year < myYear & 
                      Table$Year>=start.year]

# Number of run size years used
n.years <- length(years)

# Number of Fstate years used to fit to Beta dist
years.F <- Table$Year[Table$Year<myYear &
                          Table$Year>=2015]
n.years.F <-length(years.F)

# Realized past run sizes
runsize <- Table$Run[Table$Year < myYear &
                       Table$Year>= start.year]

# Realized Fstate
F_state <- Table$F_state[Table$Year < myYear&
                           Table$Year>=2015]

# Inits list
# inits <-function(){
#   #   
#     list("alpha_R" = runif(1,0,20),
#          "beta_R" = runif(1,-2,2),
#          "theta" = runif(1,-1,1),
#          "mu" = runif(1,-1,1),
#          "sigma" = runif(1,0,5),
#          "sigma_F"=runif(1,0,5),
#          "Fstate_hat_logit" = 1
#          
# 
#     )
#   }
#   
#   inits_ll <- list(inits(), inits(), inits(), inits())

# Call the stan model
fit <- stan(file = file.path(dir.stan,paste("UCI_",
                                            model.version ,
                                            ".stan", 
                                            sep = "")), 
            data = list(n_years = n.years,
                        n_years_F = n.years.F,
                        A=A,
                        B=B,
                        Fstate = F_state,
                        run_hist = runsize),
            # init = inits_ll,
            chains = n.chains,
            iter = n.iter, 
            thin = n.thin, 
            cores = mc.cores,
            # adjust this for treedepth warnings/div chains warnings
            # control = list(max_treedepth = 25, adapt_delta = 0.99),
            verbose = F,
            save_warmup = T
            
)


# Trace plots to check for convergence parameters of interest
traceplot(object = fit, c(
  "alpha_R",
  "beta_R",
  # "Fstate",
  "A",
  "B"
  # "sigma_F"
))
  
# Launch shiny app (Eady way to look at diagnostic plots) ######################
# shinystan::launch_shinystan(as.shinystan(fit)) # Uncomment to use

# Extract parameter estimates for plotting & analysis ##########################
pars <- rstan::extract(fit)

# Look at model fit to data

# Calculate quantiles for data
quant.predRun <- apply(X = exp(pars$ln_predRunsize),
                       MARGIN = 2,
                       FUN = quantile,
                       probs=c(0.025, 0.25, 0.5, 0.75, 0.975), 
                       na.rm = T)

# Data frame for plotting
plot.df <- data.frame(years,
                      runsize,
                      t(quant.predRun))

# Name the columns
names(plot.df) <- c("Year","Run","low95","low50","median",
                    "up50","up95")

# The current year (the one we are predicting)
curr <- quantile(pars$post_curr_predRunsize,probs = c(0.025, 0.25, 0.5, 0.75, 0.975))

# Data frame for plotting the current year
curr.df <- data.frame(Year = myYear,
                      Run =  NA,
                      low95 = unname(curr[1]),
                      low50 =  unname(curr[2]),
                      median =  unname(curr[3]),
                      up50 =  unname(curr[4]),
                      up95 =  unname(curr[5]))

# join the data frames
plot.df <- rbind(plot.df, curr.df)

# Plot the run size prediction
plot.df %>% 
  ggplot(aes(x = Year, y = Run))+
  geom_point(aes(col = "Run Size"))+
  geom_line(aes(col = "Run Size"))+
  geom_line(aes(y = median, col = "Median Pred"))+
  geom_ribbon(aes(ymin = low95, ymax = up95, fill = "95% CI"), alpha = .2)+
  geom_ribbon(aes(ymin = low50, ymax = up50, fill = "50% CI"), alpha = .2)+
  scale_fill_colorblind(name = "")+
  scale_color_colorblind(name ="")+
  labs(y="Total run size (000's)")+
  theme_clean()+
  theme(legend.position = "top",
        plot.background = element_blank())


# Calculate the OFLpre
OFL_pre <- (pars$post_curr_predRunsize - (Esc_goal_smsy) ) - (pars$post_curr_predRunsize * pars$post_curr_predFstate)

# Get the CDF to calculate ABC buffer (cum prob of overfishing)
OFL_cdf <- ecdf(OFL_pre)


# Colors for plotting
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Densities of posterior current predictions for plotting
run.df <- data.frame("par" = "RunSize", "value" = pars$post_curr_predRunsize)

# Fstate.df <- data.frame("par" = "F_state", "value" = pars$post_curr_predFstate)
Fstate.df <- data.frame("par" = "F_state", "value" = pars$post_curr_predFstate)

# OFLpre.df <- data.frame("par" = "OFLpre", "value" = OFL_pre)

# plot.df <- rbind(run.df, Fstate.df, OFLpre.df)

run.plot <- ggplot(run.df, aes(x = value/1000))+
  geom_density(fill = colorBlindBlack8[6], alpha = .7)+
  xlab("Predicted Run Size (Thousands of Salmon)")+
  ylab("Relative probability")+
  coord_cartesian(xlim = c(0,max(run.df$value)/1000))+
  theme_classic()+
  theme(legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), 
        plot.background = element_blank(), 
        panel.border = element_blank(), 
        legend.background = element_blank() )


Fstate.plot <- ggplot(Fstate.df, aes(x = value))+
  geom_density(fill = colorBlindBlack8[7], alpha = .7)+
  xlab("Predicted State Harvest Rate")+
  ylab("Relative probability")+
  coord_cartesian(xlim=c(0,1))+
  theme_classic()+
  theme(legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), 
        plot.background = element_blank(),
        panel.border = element_blank(), 
        legend.background = element_blank() )

# Calculate density for OFL
estDensity <- density(OFL_pre)

# Data frame for plotting OFL density
dense.df <- data.frame(x = estDensity$x,
                       y = estDensity$y,
                       cat = ifelse(estDensity$x<=0, "No", "yes"))#This is to color the area under the curve below and above 0

# ABC for plotting
abc <- (median(OFL_pre)*(1-OFL_cdf(0)))/1000

# ABC line
abc.line <- data.frame(x1 = abc,
                       x2 = abc,
                       y1 = 0, 
                       y2 = max(dense.df$y[dense.df$x>=abc]))

# OFL line
ofl.line <- data.frame(x1 = median(OFL_pre)/1000, 
                       x2 = median(OFL_pre)/1000, 
                       y1 = 0,
                       y2 = max(dense.df$y[dense.df$x>=median(OFL_pre)]))

# Plot the OFL density
OFLpre.plot <- ggplot(dense.df)+
  geom_line( aes(x = x/1000, y = y))+
  geom_ribbon(aes(x = x/1000, 
                  ymin = 0,
                  ymax = y, fill = cat))+
  geom_segment(data = abc.line, aes(x = x1, xend = x2, y = y1, yend = y2, col = "ABC"),linewidth = 1.5)+
  geom_segment(data = ofl.line, aes(x = x1, xend = x2, y = y1, yend = y2, col = "OFLpre"), linewidth = 1.5)+
  xlab("OFLpre (Thousands of Salmon)")+
  ylab("Relative probability")+
  coord_cartesian(xlim=c(min(OFLpre.df$value/1000),3500))+
  scale_fill_manual(values = colorBlindBlack8[c(1,2)],
                    name = "Surplus EEZ Yield?")+
  scale_color_manual(values = colorBlindBlack8[c(3,4)],
                     name = "SDC")+
  theme_classic()+
  theme(legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), 
        plot.background = element_blank(),
        panel.border = element_blank(), 
        legend.background = element_blank() )

# Combine all the plots together
ggarrange(run.plot,
          Fstate.plot,
          OFLpre.plot, ncol = 1,
          align = "v", labels = c("a","b","c"), vjust = 0.9)


# Scratch work ###############################################################
median(pars$post_curr_predFstate)
median(pars$post_curr_predRunsize)
median(OFL_pre)

mean(pars$post_curr_predRunsize)
mean(pars$post_curr_predFstate)

# Credible intervals
quantile(pars$post_curr_predRunsize, probs = c(0.025, .1,0.25, 0.5, 0.75,.9, 0.975))
quantile(pars$post_curr_predFstate, probs = c(0.025, .1,0.25, 0.5, 0.75,.9, 0.975))
quantile(OFL_pre, probs = c(0.025, .1,0.25, 0.5, 0.75,.9, 0.975))
1-OFL_cdf(0)

OFL_cdf(0)
median(OFL_pre)*(1-OFL_cdf(0))

OFL_cdf(384233)                   

sd(OFL_pre)/mean(OFL_pre)

abc.df <- data.frame("par" = "ABC",
           "value" = OFL_pre*(OFL_cdf(384233)))

comb.df <- rbind(OFLpre.df,abc.df)

ggplot(comb.df, aes(x = value, fill = par))+
  geom_density( alpha = .3)+
  xlab("OFLpre")+
  ylab("Relative probability")+
  geom_vline(xintercept = median(OFL_pre))+
  geom_vline(xintercept = median(OFL_pre*(1-OFL_cdf(0))))+
  coord_cartesian(xlim=c(min(OFLpre.df$value),3500000))+
  theme_classic()+
  theme(legend.position = "top",
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18), 
        plot.background = element_blank(),
        panel.border = element_blank(), 
        legend.background = element_blank() )

Table %>% 
  ggplot(aes(x = Run, y = F_state))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_text(aes(label = Year))

Table %>% 
  ggplot(aes(x = Run, y = C_state))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_text(aes(label = Year))