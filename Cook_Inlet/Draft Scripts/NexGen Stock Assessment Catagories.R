# Aaron Lambert - NOAA
# 11/18/2024
# Next Generation Table and Plot for SAFE


# Create catagrories for rating
cats <- c("Catch","Size/Age","Abundance","Life History","Ecosystem")

# Create  data frame with ratings for each stock
NexGen <- data.frame("Cat" = rep(cats, times = 7),
                     "Stock" = c(rep("Kenai Sockeye Salmon",5),
                                 rep("Kasilof Sockeye Salmon",5),
                                 rep("Aggregate Other Sockeye Salmon",5),
                                 rep("Aggregate Chinook Salmon",5),
                                 rep("Aggregate Chum Salmon",5),
                                 rep("Aggregate Coho Salmon",5),
                                 rep("Aggregate Pink Salmon",5)),
                     "Rating" = c(4,4,4,4,0,
                                  4,4,4,4,0,
                                  4,3,3,4,0,
                                  4,3,3,4,0,
                                  4,3,3,4,0,
                                  4,3,3,4,0,
                                  4,5,2,4,0))

# Calculate percent of total rating
NexGen<- NexGen %>% group_by(Stock) %>% mutate(Perc = sum(Rating)/25)

# get the difference for plotting
NexGen$Target <- 5-NexGen$Rating

# Pivot long for plotting 
NexGen_long<-NexGen %>% pivot_longer(cols = c(Rating, Target)) 

# Order factors for target/rating and stock
NexGen_long$name <- factor(NexGen_long$name, levels = c("Target","Rating"))

NexGen_long$Stock <- factor(NexGen_long$Stock, levels = c("Kenai Sockeye Salmon",
                                                          "Kasilof Sockeye Salmon",
                                                          "Aggregate Other Sockeye Salmon",
                                                          "Aggregate Chinook Salmon",
                                                          "Aggregate Coho Salmon",
                                                          "Aggregate Chum Salmon",
                                                          "Aggregate Pink Salmon"))

# Plot the ratings for SAFE 
png(file=paste0(getwd(),'/Figures/NexGen_GGplots.png'),
    width = 600,  height = 400)
NexGen_long %>% 
  ggplot(aes(x = Cat, y = value, fill = name))+
  geom_col()+
  geom_text(aes(x = 3, y = 5.5, label = paste0(Perc*100,"% of Target Goal")))+
  facet_wrap(~Stock)+
  labs(y="Rating",x="Catagory")+
  coord_cartesian(ylim = c(0,6))+
  scale_y_continuous(breaks = c(0:5))+
  scale_fill_colorblind(name ="")+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "top")

dev.off()

# Save a table for the report
NexGen_table <-pivot_wider(NexGen[,c(1:3)],names_from = Cat, values_from = Rating)

write.csv(NexGen_table, file=paste0(getwd(),'/Figures/NexGen_table_SSC.csv'))
