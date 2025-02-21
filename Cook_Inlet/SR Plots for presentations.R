# Aaron Lambert, aaron.lambert@noaa.gov, 907-586-7270
# 1/22/2025

# The purpose of this script is to generate figures for CI SAFE Presentations


library(readxl)
library(tidyverse)
library(ggthemes)

# Color blind palette for plotting
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Kenai Late Run SR curves ######################################################
# Data for Kenai recruit-spawner curves
KNSOCK <- read_excel("Kenai Sockeye/Mckinley 2024 Kenai Data.xlsx")

# Create recruit/spawners
KNSOCK$RS <- Mckinley$Returns/Mckinley$Escapement

# Subset to use the year range used in the ADFG Mckinley et al 2024 analysis
KNSOCK <- KNSOCK[KNSOCK$year>=1979,]

png(filename = paste0(getwd(),"/Figures/Kenai log(RS) Present Plot.png"), width = 500, height =450)
# PLot log(R/S)
KNSOCK %>% 
  ggplot(aes(x = Escapement/1000000, y = log(RS)))+
  geom_point(size = 2)+
  # geom_text(aes(label =ifelse(Escapement > 1212000,year,"")), nudge_y = .05)+
  geom_smooth(method = "lm", color = colorBlindBlack8[8])+
  geom_hline(aes(linetype = "Replacement line",yintercept = 0))+
  geom_vline(aes(xintercept = 0.75, colour = "Lwr bound"), linetype = 2, size = 1)+
  geom_vline(aes(xintercept = 1.3, colour = "Upr bound"), linetype = 2, size = 1)+
  labs(y="Log(R/S)")+
  # scale_x_continuous(expand = c(0,0), limits = c(0,NA))+
  # scale_y_continuous(expand = c(0,0), limits = c(0,NA))+
  # # coord_cartesian(xlim = c(0,25000))
  scale_linetype_manual(values = 2, name = "")+
  scale_color_manual(name = "", values = colorBlindBlack8[c(2,4,6)])+
  labs(x = "Escapement (Millions)")+
  theme_clean()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = "top",
        plot.background = element_blank(), 
        legend.background = element_blank())
dev.off()

# Get range of values to profile over to plot the R~S curve
spn <- seq(0,5,.01)

# Get the cooresponding points
rets <- spn*exp((1.860)- (0.604)*spn)

# Put in DF to plot
df <- data.frame("Escapement" = spn, "Returns" = rets )

# Lable for plotting
KNSOCK$Level <- ifelse(KNSOCK$Escapement>=750000 & KNSOCK$Escapement <=1212000,
                       "Above LB",ifelse(KNSOCK$Escapement>1212000,
                                         "Above SMSY-POINT","Below Target"))

# Plot the Kenai Late Sockeye recruit spawner curve
# png(filename = paste0(getwd(),"/Figures/Kenai SR Present Plot.png"), width = 500, height =450)
ggplot()+
  # geom_text(data = KNSOCK, aes(x = Escapement/1000000, y = Returns/1000000,label = year, vjust = -1))+
  geom_vline( linetype = 2, aes(xintercept = 1.212, col = "SMSY-POINT"), size = 1.5)+
  geom_vline( linetype = 2, aes(xintercept = .75,col = "LB"), size = 1.5)+
  geom_vline( linetype = 2, aes(xintercept = 1.3,col = "UB"), size = 1.5)+
  geom_line(data = df,aes(x = Escapement, y = Returns), size = 1.2)+
  geom_point(data = KNSOCK,aes(x = Escapement/1000000, y = Returns/1000000, 
                               colour = Level), size =3)+
  
  geom_line(aes(x = c(0:5), y = c(0:5)))+
  
  # scale_color_colorblind(name = "")+
  scale_color_manual(name = "",
                     values = colorBlindBlack8[c(7,3,4,6,8,9)])+
  coord_cartesian(ylim = c(0,11), xlim=c(0,3))+
  labs(y = "Returns (millions of sockeye salmon)",
       x = "Escapement (millions of sockeye salmon)")+
  theme_clean()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = "top",
        plot.background = element_blank(), 
        legend.background = element_blank())
# dev.off()    


# Kasilof Sockeye spawn recruit analysis
# Data for Kenai recruit-spawner curves
KASOCK <- read.csv("Kasilof Sockeye/Mckinley 2024 Kasilof Sockeye RS data.csv")

KASOCK2 <- KASOCK
KASOCK2$Escapement <- KASOCK2$Escapement/1000
fit <- lm(log(KASOCK2$RS)~(KASOCK2$Escapement))

esc <- c(0:1100)

sr <- fit$coefficients[1] + fit$coefficients[2]*esc 

df <- data.frame("esc" = esc,
                 "sr" = sr)

ES <-  c(968,933,1050)

SR <- fit$coefficients[1] + fit$coefficients[2]*ES 

temp.df <- data.frame("Year" = c(2022,2023,2024),
                      "Escapement" = c(968,933,1050),
                      "SR" = SR)

png(filename = paste0(getwd(),"/Figures/Kasilof log(RS) Present Plot.png"), width = 825, height =650)
# PLot log(R/S)
KASOCK %>% 
  ggplot(aes(x = Escapement/1000, y = log(RS)))+
  geom_point(size = 4)+
  # geom_text(aes(label =ifelse(Escapement > 1212000,year,"")), nudge_y = .05)+
  geom_smooth(method = "lm", color = colorBlindBlack8[8])+
  geom_line(data = df, aes(x = esc, y = sr))+
  geom_hline(aes(linetype = "Replacement line",yintercept = 0))+
  geom_vline(aes(xintercept = 140, colour = "Lwr bound"), linetype = 2, size = 1)+
  geom_vline(aes(xintercept = 320, colour = "Upr bound"), linetype = 2, size = 1)+
  geom_vline(aes(xintercept = 222, colour = "SMSY-POINT"), linetype = 2, size = 1)+
  geom_point(data = temp.df, aes(x = Escapement, y = SR, color = "Recent Esc."), size = 8)+
  geom_text(data = temp.df,aes(x = Escapement, y = SR, label = Year), nudge_y = c(-.15,.15,.15),
            size.unit = "pt", size = 24)+
  labs(y="Log(R/S)")+
  # scale_x_continuous(expand = c(0,0), limits = c(0,NA))+
  # scale_y_continuous(expand = c(0,0), limits = c(0,NA))+
  coord_cartesian(xlim = c(0,1050))+
  scale_linetype_manual(values = 2, name = "")+
  scale_color_manual(name = "", values = colorBlindBlack8[c(2,4,6,8)], limits = c("Lwr bound",
                                                                                  "SMSY-POINT",
                                                                                  "Upr bound",
                                                                                  "Recent Esc."))+
  labs(x = "Escapement (thousands)")+
  theme_clean()+
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 21),
        legend.position = "top",
        plot.background = element_blank(), 
        legend.background = element_blank())
dev.off()

# Get range of values to profile over to plot the R~S curve
spn <- seq(0,10,.001)

# Get the cooresponding points
# rets <- spn*exp((1.95)- (0.32)*spn)+0.604
rets <- spn*exp((2.03)- (0.318)*spn)

# Put in DF to plot
df <- data.frame("Escapement" = spn, "Returns" = rets )

# Lable for plotting
KASOCK$Level <- ifelse(KASOCK$Escapement>=140000 & KASOCK$Escapement <=222000,
                       "Above LB",ifelse(KASOCK$Escapement>222000,
                                         "Above SMSY-POINT","Below Target"))

# Plot the Kenai Late Sockeye recruit spawner curve
png(filename = paste0(getwd(),"/Figures/Kasilof SR Present Plot.png"), width = 500, height =450)
ggplot()+
  # geom_text(data = KNSOCK, aes(x = Escapement/1000000, y = Returns/1000000,label = year, vjust = -1))+
  geom_vline( linetype = 2, aes(xintercept = 222, col = "SMSY-POINT"), size = 1.5)+
  geom_vline( linetype = 2, aes(xintercept = 140,col = "LB"), size = 1.5)+
  geom_vline( linetype = 2, aes(xintercept = 320,col = "UB"), size = 1.5)+
  geom_line(data = df,aes(x = Escapement*100, y = Returns*100), size = 1.2)+
  geom_line(aes(x = c(0:1000), y = c(0:1000)))+
  geom_point(data = KASOCK,aes(x = Escapement/1000, y = Returns/1000, 
                               colour = Level), size =3)+
  
  geom_line(aes(x = c(0:1), y = c(0:1)))+
  
  # scale_color_colorblind(name = "")+
  scale_color_manual(name = "",
                     values = colorBlindBlack8[c(7,3,4,6,8,9)])+
  # coord_cartesian(ylim = c(0,11), xlim=c(0,3))+
  labs(y = "Returns (thousands of sockeye salmon)",
       x = "Escapement (thousands of sockeye salmon)")+
  theme_clean()+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = "top",
        plot.background = element_blank(), 
        legend.background = element_blank())
dev.off()





reg.conf.intervals <- function(x, y) {
  n <- length(y) # Find length of y to use as sample size
  lm.model <- lm(y ~ x) # Fit linear model
  
  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- 1:max(x)
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_new2 <- 1:max(x + 100)
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit2 + t.val * se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower, slope.upper))
  colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
  
  # Plot the fitted linear regression line and the computed confidence bands
  plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
  lines(y.fit2, col = 'black', lwd = 2)
  lines(bands[1], col = 'blue', lty = 2, lwd = 2)
  lines(bands[2], col = 'blue', lty = 2, lwd = 2)
  
  return(bands)
}

conf.intervals <- reg.conf.intervals(df$, cars$dist)
