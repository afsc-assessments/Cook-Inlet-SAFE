# Aaron Lambert - NOAA
# UCI Harvest plots
# 9/27/2024


# The purpose of this scrip is to generate plots summary harvest plots for SAFE
# report.

library(ggpubr)
library(tidyverse)
library(gtools)
library(ggthemes)
library(readxl)
library(lubridate)

# load in all the harvest data for plotting'
Table_coho <- read.csv(file=paste0(getwd(),'/','UCI Coho/Table.csv'))
Table_coho$Stock <- "Coho"
Table_coho$EEZ.Catch <- Table_coho$EEZ.Catch/1000
Table_coho$ABC_2024 <- 35769/1000
Table_coho$OFLpre_2025 <- 67013/1000
Table_coho$ABC_2025 <- 6701/1000

Table_chum <- read.csv(file=paste0(getwd(),'/','UCI Chum/Table.csv'))
Table_chum$Stock <- "Chum"
Table_chum$EEZ.Catch <- Table_chum$EEZ.Catch/1000
Table_chum$ABC_2024 <- 110432/1000
Table_chum$OFLpre_2025 <- 97508/1000
Table_chum$ABC_2025 <- 82882/1000  


Table_Pink_Odd <- read.csv(file=paste0(getwd(),'/','UCI Pink Odd/Table.csv'))
Table_Pink_Odd$Stock <- "Pink Odd"
Table_Pink_Odd$EEZ.Catch <- Table_Pink_Odd$EEZ.Catch/1000
Table_Pink_Odd$OFLpre_2025 <- 58174/1000
Table_Pink_Odd$ABC_2025 <-52357/1000

Table_Pink_even <- read.csv(file=paste0(getwd(),'/','UCI Pink Even/Table.csv'))
Table_Pink_even$Stock <- "Pink Even"
Table_Pink_even$EEZ.Catch <- Table_Pink_even$EEZ.Catch/1000
Table_Pink_even$ABC_2024 <- 135218/1000
Table_Pink_even$ABC_2025 <- NA

Table_Sockeye <- read.csv(file=paste0(getwd(),'/','UCI Sockeye/Table.csv'))
# Table_Sockeye$Stock <- "Other Sockeye"
Table_Sockeye$EEZ.Catch <- Table_Sockeye$EEZ.Catch/1000
Table_Sockeye$ABC_2024 <- 177493/1000
Table_Sockeye$OFLpre_2025 <- 181351/1000
Table_Sockeye$ABC_2025 <- 154148/1000
# Table_Sockeye[nrow(Table_Sockeye)+1,]<-NA
# Table_Sockeye[nrow(Table_Sockeye), "Year"] <- 2024 #workaround for plotting
# Table_Sockeye[nrow(Table_Sockeye), "EEZ.Catch"] <- 84102/1000 #workaround for plotting
Table_Sockeye$Stock <- "Other Sockeye"

Table_Kasilof <- read.csv(file=paste0(getwd(),'/','Kasilof Sockeye/Table.csv'))
Table_Kasilof <- Table_Kasilof %>% 
  rename(EEZ.Catch = Kasilof.R..EEZ.Catch)
Table_Kasilof$ABC_2024 <- 375512/1000
Table_Kasilof$OFLpre_2025 <- 664294/1000
Table_Kasilof$ABC_2025 <- 130701/1000
# Table_Kasilof[nrow(Table_Kasilof)+1,]<-NA
# Table_Kasilof[nrow(Table_Kasilof), "Year"] <- 2024 #workaround for plotting
# Table_Kasilof[nrow(Table_Kasilof), "EEZ.Catch"] <- 28900/1000 #workaround for plotting
Table_Kasilof$Stock <- "Kasilof Sockeye"

Table_Kenai <- read.csv(file=paste0(getwd(),'/','Kenai Sockeye/Table.csv'))
# Table_Kenai$Stock <- "Kenai Sockeye"
Table_Kenai <- Table_Kenai %>% 
  rename(EEZ.Catch = Kenai.R..EEZ.Catch)
Table_Kenai$ABC_2024 <- 431123/1000
Table_Kenai$OFLpre_2025 <- 514761/1000
Table_Kenai$ABC_2025 <- 168485/1000
# Table_Kenai[nrow(Table_Kenai)+1,]<-NA
# Table_Kenai[nrow(Table_Kenai), "Year"] <- 2024 #workaround for plotting
# Table_Kenai[nrow(Table_Kenai), "EEZ.Catch"] <- 211717/1000 #workaround for plotting
Table_Kenai$Stock <- "Kenai Sockeye"

Table_Chinook <- read.csv(file=paste0(getwd(),'/','All Chinook/Table.csv'))
Table_Chinook$Stock <- "Chinook"
Table_Chinook$EEZ.Catch <- Table_Chinook$EEZ.Catch/1000
Table_Chinook$ABC_2024 <- 270/1000
Table_Chinook$OFLpre_2025 <- 373/1000
Table_Chinook$ABC_2025 <- 261/1000

# 2024 Inseason Catch Data
Catch_2024_master <- read_xlsx(path  = paste0(getwd(),'/','Inseason_Catch/CIS_data_2024_FINAL_20Nov24.xlsx'))
Catch_2024 <- read.csv( file = paste0(getwd(),"/Inseason_Catch/2024 EEZ Catch.csv"))

# Convert date to date object
Catch_2024_master <-  Catch_2024_master %>% 
  mutate(OPENER_DATE = as.Date(OPENER_DATE), format = "%d-%M-%y")  

Catch_2024 <- Catch_2024 %>% 
  mutate(OPENER_DATE = as.Date(OPENER_DATE), format = "%d-%M-%y") 

Catch_2024$SPECIES <- factor(Catch_2024$SPECIES)

levels(Catch_2024$SPECIES) <- c("Chinook",
                                "Chum",
                                "Coho",
                                "Kasilof sockeye",
                                "Kenai sockeye",
                                "Other sockeye",
                                "Pink",
                                "Sockeye"
                                )

# Calculate cumulative catch
Catch_2024 <- Catch_2024 %>% 
  group_by(SPECIES) %>% 
  mutate(CUMULATIVE_CATCH = cumsum(CATCH_PER_DAY)) %>% 
  as.data.frame()

Catch_2024 <- Catch_2024 %>% 
  group_by(SPECIES) %>% 
  mutate(Perc_Tac = (max(CUMULATIVE_CATCH)/TAC_2024)*100) %>% 
  data.frame()
# Catch_2024$SPECIES <- factor(Catch_2024$SPECIES, levels = c("Sockeye",
#                                                             "Chinook",
#                                                             "Coho","Chum",
#                                                             "Pink"))
# Colorblind color scheme
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")




# Plot of Annual estimated EEZ harvest and the current and past OFL/ABC ########
masterDF <- smartbind(Table_Chinook, 
                      Table_coho, 
                      Table_chum,
                      Table_Kasilof,
                      Table_Kenai,
                      Table_Pink_even,
                      Table_Pink_Odd,
                      Table_Sockeye)

# Add factor identifying hist est vs known catch
masterDF$Catch_status <- ifelse(masterDF$Year<=2023,"Estimated", "Known")

masterDF$ABC_2024 <- as.numeric(masterDF$ABC_2024)
masterDF$OFLpre_2025 <- as.numeric(masterDF$OFLpre_2025)
masterDF$ABC_2025 <- as.numeric(masterDF$ABC_2025)

masterDF <- masterDF[,c("Year","EEZ.Catch", "ABC_2024","Stock","OFLpre_2025","ABC_2025","Catch_status")]

masterDF$Stock <- factor(masterDF$Stock, levels = c("Kenai Sockeye",
                                                    "Kasilof Sockeye",
                                                    "Other Sockeye",
                                                    "Chinook",
                                                    "Coho",
                                                    "Chum",
                                                    "Pink Even",
                                                    "Pink Odd"))


Catch.trend.plot <- ggplot(masterDF, aes(x = Year, y = EEZ.Catch))+
  geom_line()+
  geom_point(aes(shape = Catch_status),size=2)+
  labs(y = "EEZ Catch ('000's of fish)")+
  geom_hline(aes(yintercept = ABC_2024, 
                 linetype = "ABC 2024", 
                 color = "ABC 2024"),
             size = 1)+
  geom_hline(aes(yintercept = OFLpre_2025, 
                 linetype = "OFLpre 2025", 
                 color = "OFLpre 2025"), 
             size = 1)+
  theme_classic()+
  geom_hline(aes(yintercept = ABC_2025, 
                 linetype = "Recomended ABC 2025", 
                 color = "Recomended ABC 2025"), 
             size = 1.2)+
  theme_classic()+
  scale_color_manual(name = "",
                     values = c(colorBlindBlack8[c(2,4,6)]))+
  scale_linetype_manual(values = c(2,3,4),
                        name = "")+
  scale_shape(name = "")+
  coord_cartesian(xlim = c(1999,2025))+
  facet_wrap(~Stock, scales = "free_y", nrow = 4)+
  theme(
    # plot.margin = unit(c(5.5,10,5.5,5.5),"points"),
    axis.text.x = element_text(angle = 90),
    legend.position = "top",
    legend.background = element_blank(),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
    )


png(filename = paste0(getwd(),'/Figures/2025 Catch vs ABC all species.png'),
    width = 625, height = 400)
Catch.trend.plot
dev.off()

# Catch by species in 2025 ####################################################

# Save all catch plots as pdf
# pdf(file = paste0(getwd(),'/Figures/Catch By Day 2025.pdf'),
#     width = 10,
#     height = 10)



# 1. all species on one plot
catch.all.plot <- Catch_2024[Catch_2024$SPECIES != "Sockeye",] %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY/1000, fill = SPECIES))+
  geom_col()+
  # scale_fill_colorblind()
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6:8)], name = "")+
  labs(x = "Opener date", y = "Catch (000's)")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        axis.title = element_text(face = "plain", size = 12),
        text = element_text(size = 14))

png(filename = paste0(getwd(),'/Figures/All species catch 2024.png'))
catch.all.plot
dev.off()

# 2. Chinook 
catch.chinook.plot <- Catch_2024 %>% 
  filter(SPECIES  == "Chinook") %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY))+
  # geom_col(fill = colorBlindBlack8[4])+
  geom_col()+
  # scale_fill_colorblind()
  # scale_fill_manual(values = colorBlindBlack8[c(1:4,6)], name = "")+
  labs(x = "Opener date", y = "EEZ Catch (number of fish)")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))


# png(filename = paste0(getwd(),'/Figures/Chinook catch 2025.png'))
catch.chinook.plot
# dev.off()

cumcatch.chinook.plot <- Catch_2024 %>% 
  filter(SPECIES  =="Chinook") %>%
  ggplot(aes(x = OPENER_DATE, y = CUMULATIVE_CATCH))+
  # geom_col(aes(fill = SPECIES))+
  geom_line(size = 1.7)+
  geom_hline(aes(yintercept = TAC_2024, col = "TAC 2024"),linetype= 2)+
  geom_label(aes(label = paste0("TAC utilized = ",
                               round(Perc_Tac,1),"%"), 
                x = OPENER_DATE[7], 
                y  = TAC_2024*1.05), label.size = 0)+
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6)],
                    name = "", guide = "none")+
  labs(x = "Opener date", y = "Cumulative EEZ Catch (number of fish)")+
  theme_classic()+
  scale_color_manual(name = "", values = "red")+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  # guides(
  #        col = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position.inside = c(.87,.25),
        legend.position = "top",
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
# facet_wrap(~SPECIES, scales = "free_y", nrow = 3)

png(filename = paste0(getwd(),'/Figures/Chinook catch 2024.png'), width = 625,height =500)
ggarrange(catch.chinook.plot,cumcatch.chinook.plot, labels = c("a","b"))# cumcatch.pink.plot
dev.off()

# Chinook Catch Timseries 
Catch.trend.chinook.plot <- ggplot(masterDF[masterDF$Stock == "Chinook",],
                                aes(x = Year, y = EEZ.Catch*1000))+
  geom_line()+
  geom_point(size=2.5, aes(shape=Catch_status))+
  labs(y = "EEZ Catch (number of fish)")+
  geom_hline(aes(yintercept = ABC_2024*1000, 
                 linetype = "ABC 2024", 
                 color = "ABC 2024"),
             size = 1.2)+
  geom_hline(aes(yintercept = OFLpre_2025*1000, 
                 linetype = "OFLpre 2025", 
                 color = "OFLpre 2025"), 
             size = 1.2)+
  geom_hline(aes(yintercept = ABC_2025*1000, 
                 linetype = "Recomended ABC 2025", 
                 color = "Recomended ABC 2025"), 
             size = 1.2)+
  theme_classic()+
  scale_color_manual(name = "",
                     values = c(colorBlindBlack8[c(2,4,6)]))+
  scale_linetype_manual(values = c(2,3,4),
                        name = "")+
  scale_shape(name="")+
  coord_cartesian(xlim = c(1999,2025))+
  # facet_wrap(~Stock, scales = "free_y", nrow = 4)+
  theme(
    plot.margin = unit(c(5.5,10,5.5,5.5),"points"),
    axis.text.x = element_text(angle = 90),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


png(filename = paste0(getwd(),'/Figures/Chinook timeseries 2025.png'),width = 600,
    height = 400)
Catch.trend.chinook.plot
dev.off()

# 3. Chum ###################################################################
catch.chum.plot <- Catch_2024 %>% 
  filter(SPECIES  == "Chum") %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY))+
  # geom_col(fill = colorBlindBlack8[4])+
  geom_col()+
  # scale_fill_colorblind()
  # scale_fill_manual(values = colorBlindBlack8[c(1:4,6)], name = "")+
  labs(x = "Opener date", y = "EEZ Catch")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# png(filename = paste0(getwd(),'/Figures/Chum catch 2025.png'))
catch.chum.plot
# dev.off()

cumcatch.chum.plot <- Catch_2024 %>% 
  filter(SPECIES  =="Chum") %>%
  ggplot(aes(x = OPENER_DATE, y = CUMULATIVE_CATCH/1000))+
  # geom_col(aes(fill = SPECIES))+
  geom_line(size = 1.7)+
  geom_hline(aes(yintercept = TAC_2024/1000, col = "TAC 2024"),linetype= 2)+
  geom_label(aes(label = paste0("TAC utilized = ",
                                round(Perc_Tac,1),"%"), 
                 x = OPENER_DATE[7], 
                 y  = (TAC_2024/1000)*1.05), label.size = 0)+
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6)],
                    name = "", guide = "none")+
  labs(x = "Opener date", y = "Cumulative EEZ Catch (000's)")+
  theme_classic()+
  scale_color_manual(name = "", values = "red")+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  # guides(
  #        col = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position.inside = c(.87,.25),
        legend.position = "top",
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
# facet_wrap(~SPECIES, scales = "free_y", nrow = 3)

png(filename = paste0(getwd(),'/Figures/Chum catch 2024.png'), width = 625,height =500)
ggarrange(catch.chum.plot,cumcatch.chum.plot, labels = c("a","b"))# cumcatch.pink.plot
dev.off()

# Pink Catch Timseries 
Catch.trend.chum.plot <- ggplot(masterDF[masterDF$Stock == "Chum",],
                                aes(x = Year, y = EEZ.Catch))+
  geom_line()+
  geom_point(size=2.5, aes(shape=Catch_status))+
  labs(y = "EEZ Catch (000's)")+
  geom_hline(aes(yintercept = ABC_2024, 
                 linetype = "ABC 2024", 
                 color = "ABC 2024"),
             size = 1.2)+
  geom_hline(aes(yintercept = OFLpre_2025, 
                 linetype = "OFLpre 2025", 
                 color = "OFLpre 2025"), 
             size = 1.2)+
  geom_hline(aes(yintercept = ABC_2025, 
                 linetype = "Recomended ABC 2025", 
                 color = "Recomended ABC 2025"), 
             size = 1.2)+
  theme_classic()+
  scale_color_manual(name = "",
                     values = c(colorBlindBlack8[c(2,4,6)]))+
  scale_linetype_manual(values = c(2,3,4),
                        name = "")+
  scale_shape(name="")+
  coord_cartesian(xlim = c(1999,2025))+
  # facet_wrap(~Stock, scales = "free_y", nrow = 4)+
  theme(
    plot.margin = unit(c(5.5,10,5.5,5.5),"points"),
    axis.text.x = element_text(angle = 90),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


png(filename = paste0(getwd(),'/Figures/Chum timeseries 2025.png'),width = 600,
height = 400)
Catch.trend.chum.plot
dev.off()
# 4. Coho #####################################################################
catch.coho.plot <- Catch_2024 %>% 
  filter(SPECIES  == "Coho") %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY))+
  # geom_col(fill = colorBlindBlack8[4])+
  geom_col()+
  # scale_fill_colorblind()
  # scale_fill_manual(values = colorBlindBlack8[c(1:4,6)], name = "")+
  labs(x = "Opener date", y = "EEZ Catch")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# png(filename = paste0(getwd(),'/Figures/Coho catch 2025.png'))
catch.coho.plot
# dev.off()
# Cumulative Pink catch plots 2024

cumcatch.coho.plot <- Catch_2024 %>% 
  filter(SPECIES  =="Coho") %>%
  ggplot(aes(x = OPENER_DATE, y = CUMULATIVE_CATCH/1000))+
  # geom_col(aes(fill = SPECIES))+
  geom_line(size = 1.7)+
  geom_hline(aes(yintercept = TAC_2024/1000, col = "TAC 2024"),linetype= 2)+
  geom_label(aes(label = paste0("TAC utilized = ",
                                round(Perc_Tac,1),"%"), 
                 x = OPENER_DATE[7], 
                 y  = (TAC_2024/1000)*1.05), label.size = 0)+
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6)],
                    name = "", guide = "none")+
  labs(x = "Opener date", y = "Cumulative EEZ Catch (000's)")+
  theme_classic()+
  scale_color_manual(name = "", values = "red")+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  # guides(
  #        col = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position.inside = c(.87,.25),
        legend.position = "top",
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
# facet_wrap(~SPECIES, scales = "free_y", nrow = 3)

png(filename = paste0(getwd(),'/Figures/Coho catch 2024.png'), width = 625,height =500)
ggarrange(catch.coho.plot,cumcatch.coho.plot, labels = c("a","b"))# cumcatch.pink.plot
dev.off()

# Coho Catch Timeseries 
Catch.trend.coho.plot <- ggplot(masterDF[masterDF$Stock == "Coho",],
                                aes(x = Year, y = EEZ.Catch))+
  geom_line()+
  geom_point(size=2.5, aes(shape=Catch_status))+
  labs(y = "EEZ Catch (000's)")+
  geom_hline(aes(yintercept = ABC_2024, 
                 linetype = "ABC 2024", 
                 color = "ABC 2024"),
             size = 1.2)+
  geom_hline(aes(yintercept = OFLpre_2025, 
                 linetype = "OFLpre 2025", 
                 color = "OFLpre 2025"), 
             size = 1.2)+
  geom_hline(aes(yintercept = ABC_2025, 
                 linetype = "Recomended ABC 2025", 
                 color = "Recomended ABC 2025"), 
             size = 1.2)+
  theme_classic()+
  scale_color_manual(name = "",
                     values = c(colorBlindBlack8[c(2,4,6)]))+
  scale_linetype_manual(values = c(2,3,4),
                        name = "")+
  scale_shape(name="")+
  coord_cartesian(xlim = c(1999,2025))+
  # facet_wrap(~Stock, scales = "free_y", nrow = 4)+
  theme(
    plot.margin = unit(c(5.5,10,5.5,5.5),"points"),
    axis.text.x = element_text(angle = 90),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


png(filename = paste0(getwd(),'/Figures/Coho timeseries 2025.png'),,width = 600,
    height = 400)
Catch.trend.coho.plot
dev.off()

# 5. Pink  ##################################################################

# Cacth by day in 2024
catch.pink.plot <- Catch_2024 %>% 
  filter(SPECIES  == "Pink") %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY))+
  # geom_col(fill = colorBlindBlack8[4])+
  geom_col()+
  # scale_fill_colorblind()
  # scale_fill_manual(values = colorBlindBlack8[c(1:4,6)], name = "")+
  labs(x = "Opener date", y = "EEZ Catch")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# png(filename = paste0(getwd(),'/Figures/Pink catch by day 2024.png'))
# catch.pink.plot
# dev.off()

# Cumulative Pink catch plots 2024
cumcatch.pink.plot <- Catch_2024 %>% 
  filter(SPECIES  =="Pink") %>%
  ggplot(aes(x = OPENER_DATE, y = CUMULATIVE_CATCH/1000))+
  # geom_col(aes(fill = SPECIES))+
  geom_line(size = 1.7)+
  geom_hline(aes(yintercept = TAC_2024/1000, col = "TAC 2024"),linetype= 2)+
  geom_label(aes(label = paste0("TAC utilized = ",
                                round(Perc_Tac,1),"%"), 
                 x = OPENER_DATE[7], 
                 y  = (TAC_2024/1000)*1.05), label.size = 0)+
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6)],
                    name = "", guide = "none")+
  labs(x = "Opener date", y = "Cumulative EEZ Catch (000's)")+
  theme_classic()+
  scale_color_manual(name = "", values = "red")+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  # guides(
  #        col = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position.inside = c(.87,.25),
        legend.position = "top",
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
  # facet_wrap(~SPECIES, scales = "free_y", nrow = 3)

png(filename = paste0(getwd(),'/Figures/Pink catch 2024.png'), width = 625,height =500)
ggarrange(catch.pink.plot,cumcatch.pink.plot, labels = c("a","b"))# cumcatch.pink.plot
dev.off()

# Pink Catch Timseries 
Catch.trend.pink.plot <- ggplot(masterDF[masterDF$Stock %in% c("Pink Even","Pink Odd"),],
                                aes(x = Year, y = EEZ.Catch))+
  geom_line()+
  geom_point(size=2.5, aes(shape=Catch_status))+
  labs(y = "EEZ Catch (000's)")+
  geom_hline(aes(yintercept = ABC_2024, 
                 linetype = "ABC 2024", 
                 color = "ABC 2024"),
             size = 1.2)+
  geom_hline(aes(yintercept = OFLpre_2025, 
                 linetype = "OFLpre 2025", 
                 color = "OFLpre 2025"), 
             size = 1.2)+
  geom_hline(aes(yintercept = ABC_2025, 
                 linetype = "Recomended ABC 2025", 
                 color = "Recomended ABC 2025"), 
             size = 1.2)+
  theme_classic()+
  scale_color_manual(name = "",
                     values = c(colorBlindBlack8[c(2,4,6)]))+
  scale_linetype_manual(values = c(2,3,4),
                        name = "")+
  scale_shape(name="")+
  coord_cartesian(xlim = c(1999,2025))+
  facet_wrap(~Stock, scales = "free_y", nrow = 4)+
  theme(
    plot.margin = unit(c(5.5,10,5.5,5.5),"points"),
    axis.text.x = element_text(angle = 90),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


png(filename = paste0(getwd(),'/Figures/Pink timeseries 2025.png'),width = 600,
    height = 400)
Catch.trend.pink.plot
dev.off()





# 6. All Sockeye
catch.sockeye.plot <- Catch_2024 %>% 
  filter(SPECIES  == "Sockeye") %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY/1000))+
  geom_col(fill = colorBlindBlack8[6])+
  # scale_fill_colorblind()
  # scale_fill_manual(values = colorBlindBlack8[c(1:4,6)], name = "")+
  labs(x = "Opener date", y = "Sockeye (000's)")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        axis.title = element_text(face = "plain", size = 12),
        text = element_text(size = 14))

# png(filename = paste0(getwd(),'/Figures/Sockeye catch 2025.png'))
catch.sockeye.plot
# dev.off()

# dev.off() # PDF save

# Cumulative catch plots 

cumcatch.all.plot <- Catch_2024 %>% 
  # filter(SPECIES  == "Sockeye salmon") %>% 
  ggplot(aes(x = OPENER_DATE, y = CUMULATIVE_CATCH/1000))+
  # geom_col(aes(fill = SPECIES))+
  geom_col()+
  geom_hline(aes(yintercept = TAC_2024/1000, col = "TAC 2024"),linetype= 2)+
  # scale_fill_colorblind()
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6)],
                    name = "", guide = "none")+
  labs(x = "Opener date", y = "EEZ Catch (000's)")+
  theme_classic()+
  scale_color_manual(name = "", values = "red")+
  # scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  # guides(
  #        col = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position.inside = c(.87,.25),
        legend.position = "top",
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        axis.title = element_text(face = "plain", size = 12),
        text = element_text(size = 14))+
  facet_wrap(~SPECIES, scales = "free_y", nrow = 3)

# png(filename = paste0(getwd(),'/Figures/Cumulative Catch All Species 2025.png'))
cumcatch.all.plot
# dev.off()

png(filename = paste0(getwd(),'/Figures/Comb catch plot 2025.png'),
    units = "in",
    res=72,
    width = 9,
    height = 6)
ggarrange(cumcatch.all.plot,Catch.trend.plot)
dev.off()


# Kenai Sockeye #############################################################
# Cacth by day in 2024
catch.kenai.plot <- Catch_2024 %>% 
  filter(SPECIES  == "Kenai sockeye") %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY))+
  # geom_col(fill = colorBlindBlack8[4])+
  geom_col()+
  # scale_fill_colorblind()
  # scale_fill_manual(values = colorBlindBlack8[c(1:4,6)], name = "")+
  labs(x = "Opener date", y = "EEZ Catch")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# png(filename = paste0(getwd(),'/Figures/Pink catch by day 2024.png'))
catch.kenai.plot
# dev.off()

# Cumulative Pink catch plots 2024
cumcatch.kenai.plot <- Catch_2024 %>% 
  filter(SPECIES  =="Kenai sockeye") %>%
  ggplot(aes(x = OPENER_DATE, y = CUMULATIVE_CATCH/1000))+
  # geom_col(aes(fill = SPECIES))+
  geom_line(size = 1.7)+
  geom_hline(aes(yintercept = TAC_2024/1000, col = "TAC 2024"),linetype= 2)+
  geom_label(aes(label = paste0("TAC utilized = ",
                                round(Perc_Tac,1),"%"), 
                 x = OPENER_DATE[7], 
                 y  = (TAC_2024/1000)*1.05), label.size = 0)+
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6)],
                    name = "", guide = "none")+
  labs(x = "Opener date", y = "Cumulative EEZ Catch (000's)")+
  theme_classic()+
  scale_color_manual(name = "", values = "red")+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  # guides(
  #        col = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position.inside = c(.87,.25),
        legend.position = "top",
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
# facet_wrap(~SPECIES, scales = "free_y", nrow = 3)

png(filename = paste0(getwd(),'/Figures/Kenai sockeye catch 2024.png'), width = 625,height =500)
ggarrange(catch.kenai.plot,cumcatch.kenai.plot, labels = c("a","b"))# cumcatch.pink.plot
dev.off()

# Kenai Catch Timseries 
Catch.trend.kenai.plot <- ggplot(masterDF[masterDF$Stock == "Kenai Sockeye",],
                                aes(x = Year, y = EEZ.Catch))+
  geom_line()+
  geom_point(size=2.5, aes(shape=Catch_status))+
  labs(y = "EEZ Catch (000's)")+
  geom_hline(aes(yintercept = ABC_2024, 
                 linetype = "ABC 2024", 
                 color = "ABC 2024"),
             size = 1.2)+
  geom_hline(aes(yintercept = OFLpre_2025, 
                 linetype = "OFLpre 2025", 
                 color = "OFLpre 2025"), 
             size = 1.2)+
  geom_hline(aes(yintercept = ABC_2025, 
                 linetype = "Recomended ABC 2025", 
                 color = "Recomended ABC 2025"), 
             size = 1.2)+
  theme_classic()+
  scale_color_manual(name = "",
                     values = c(colorBlindBlack8[c(2,4,6)]))+
  scale_linetype_manual(values = c(2,3,4),
                        name = "")+
  scale_shape(name="")+
  coord_cartesian(xlim = c(1999,2025))+
  # facet_wrap(~Stock, scales = "free_y", nrow = 4)+
  theme(
    plot.margin = unit(c(5.5,10,5.5,5.5),"points"),
    axis.text.x = element_text(angle = 90),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


png(filename = paste0(getwd(),'/Figures/Kenai Sockeye timeseries 2025.png'),width = 600,
    height = 400)
Catch.trend.kenai.plot
dev.off()


# kasilof Sockeye ##############################################################
# Cacth by day in 2024
catch.kasilof.plot <- Catch_2024 %>% 
  filter(SPECIES  == "Kasilof sockeye") %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY))+
  # geom_col(fill = colorBlindBlack8[4])+
  geom_col()+
  # scale_fill_colorblind()
  # scale_fill_manual(values = colorBlindBlack8[c(1:4,6)], name = "")+
  labs(x = "Opener date", y = "EEZ Catch")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# png(filename = paste0(getwd(),'/Figures/Pink catch by day 2024.png'))
catch.kasilof.plot
# dev.off()

# Cumulative Pink catch plots 2024
cumcatch.kasilof.plot <- Catch_2024 %>% 
  filter(SPECIES  =="Kasilof sockeye") %>%
  ggplot(aes(x = OPENER_DATE, y = CUMULATIVE_CATCH/1000))+
  # geom_col(aes(fill = SPECIES))+
  geom_line(size = 1.7)+
  geom_hline(aes(yintercept = TAC_2024/1000, col = "TAC 2024"),linetype= 2)+
  geom_label(aes(label = paste0("TAC utilized = ",
                                round(Perc_Tac,1),"%"), 
                 x = OPENER_DATE[7], 
                 y  = (TAC_2024/1000)*1.05), label.size = 0)+
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6)],
                    name = "", guide = "none")+
  labs(x = "Opener date", y = "Cumulative EEZ Catch (000's)")+
  theme_classic()+
  scale_color_manual(name = "", values = "red")+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  # guides(
  #        col = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position.inside = c(.87,.25),
        legend.position = "top",
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
# facet_wrap(~SPECIES, scales = "free_y", nrow = 3)

png(filename = paste0(getwd(),'/Figures/Kasilof sockeye catch 2024.png'), width = 625,height =500)
ggarrange(catch.kasilof.plot,cumcatch.kasilof.plot, labels = c("a","b"))# cumcatch.pink.plot
dev.off()

# Kasilof Catch Timseries 
Catch.trend.kasilof.plot <- ggplot(masterDF[masterDF$Stock == "Kasilof Sockeye",],
                                 aes(x = Year, y = EEZ.Catch))+
  geom_line()+
  geom_point(size=2.5, aes(shape=Catch_status))+
  labs(y = "EEZ Catch (000's)")+
  geom_hline(aes(yintercept = ABC_2024, 
                 linetype = "ABC 2024", 
                 color = "ABC 2024"),
             size = 1.2)+
  geom_hline(aes(yintercept = OFLpre_2025, 
                 linetype = "OFLpre 2025", 
                 color = "OFLpre 2025"), 
             size = 1.2)+
  geom_hline(aes(yintercept = ABC_2025, 
                 linetype = "Recomended ABC 2025", 
                 color = "Recomended ABC 2025"), 
             size = 1.2)+
  theme_classic()+
  scale_color_manual(name = "",
                     values = c(colorBlindBlack8[c(2,4,6)]))+
  scale_linetype_manual(values = c(2,3,4),
                        name = "")+
  scale_shape(name="")+
  coord_cartesian(xlim = c(1999,2025))+
  # facet_wrap(~Stock, scales = "free_y", nrow = 4)+
  theme(
    plot.margin = unit(c(5.5,10,5.5,5.5),"points"),
    axis.text.x = element_text(angle = 90),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


png(filename = paste0(getwd(),'/Figures/Kasilof Sockeye timeseries 2025.png'),width = 600,
    height = 400)
Catch.trend.kasilof.plot
dev.off()

# Other Sockeye #####################################################
# Cacth by day in 2024
catch.other.plot <- Catch_2024 %>% 
  filter(SPECIES  == "Other sockeye") %>% 
  ggplot(aes(x = OPENER_DATE, y = CATCH_PER_DAY))+
  # geom_col(fill = colorBlindBlack8[4])+
  geom_col()+
  # scale_fill_colorblind()
  # scale_fill_manual(values = colorBlindBlack8[c(1:4,6)], name = "")+
  labs(x = "Opener date", y = "EEZ Catch")+
  theme_classic()+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  guides(fill = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.position.inside = c(.8,.85),
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# png(filename = paste0(getwd(),'/Figures/Pink catch by day 2024.png'))
catch.other.plot
# dev.off()

# Cumulative Pink catch plots 2024
cumcatch.other.plot <- Catch_2024 %>% 
  filter(SPECIES  =="Other sockeye") %>%
  ggplot(aes(x = OPENER_DATE, y = CUMULATIVE_CATCH/1000))+
  # geom_col(aes(fill = SPECIES))+
  geom_line(size = 1.7)+
  geom_hline(aes(yintercept = TAC_2024/1000, col = "TAC 2024"),linetype= 2)+
  geom_label(aes(label = paste0("TAC utilized = ",
                                round(Perc_Tac,1),"%"), 
                 x = OPENER_DATE[7], 
                 y  = (TAC_2024/1000)*1.05), label.size = 0)+
  scale_fill_manual(values = colorBlindBlack8[c(1:4,6)],
                    name = "", guide = "none")+
  labs(x = "Opener date", y = "Cumulative EEZ Catch (000's)")+
  theme_classic()+
  scale_color_manual(name = "", values = "red")+
  scale_x_date(date_labels = "%d-%b", breaks = unique(Catch_2024$OPENER_DATE))+
  # guides(
  #        col = guide_legend(position = "inside"))+
  coord_cartesian(xlim = c(min(Catch_2024$OPENER_DATE), max(Catch_2024$OPENER_DATE)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        # legend.position.inside = c(.87,.25),
        legend.position = "top",
        panel.grid.major.y = element_line(linetype = 3, size = 1),
        # panel.grid.minor = element_blank(),
        # panel. = element_blank(),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))
# facet_wrap(~SPECIES, scales = "free_y", nrow = 3)

png(filename = paste0(getwd(),'/Figures/Other sockeye catch 2024.png'), width = 625,height =500)
ggarrange(catch.other.plot,cumcatch.other.plot, labels = c("a","b"))# cumcatch.pink.plot
dev.off()

# Other sockeye Catch Timseries 
Catch.trend.other.plot <- ggplot(masterDF[masterDF$Stock == "Other Sockeye",],
                                 aes(x = Year, y = EEZ.Catch))+
  geom_line()+
  geom_point(size=2.5, aes(shape=Catch_status))+
  labs(y = "EEZ Catch (000's)")+
  geom_hline(aes(yintercept = ABC_2024, 
                 linetype = "ABC 2024", 
                 color = "ABC 2024"),
             size = 1.2)+
  geom_hline(aes(yintercept = OFLpre_2025, 
                 linetype = "OFLpre 2025", 
                 color = "OFLpre 2025"), 
             size = 1.2)+
  geom_hline(aes(yintercept = ABC_2025, 
                 linetype = "Recomended ABC 2025", 
                 color = "Recomended ABC 2025"), 
             size = 1.2)+
  theme_classic()+
  scale_color_manual(name = "",
                     values = c(colorBlindBlack8[c(2,4,6)]))+
  scale_linetype_manual(values = c(2,3,4),
                        name = "")+
  scale_shape(name="")+
  coord_cartesian(xlim = c(1999,2025))+
  # facet_wrap(~Stock, scales = "free_y", nrow = 4)+
  theme(
    plot.margin = unit(c(5.5,10,5.5,5.5),"points"),
    axis.text.x = element_text(angle = 90),
    legend.position = "top",
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


png(filename = paste0(getwd(),'/Figures/Other Sockeye timeseries 2025.png'),width = 600,
    height = 400)
Catch.trend.other.plot
dev.off()



# Vessels fishing per day
Catch_2024_master %>% 
  ggplot(aes(x = OPENER_DATE, y = VESSEL_COUNT, fill = SPECIES))+
  geom_col()
