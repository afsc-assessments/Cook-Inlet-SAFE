# Aaron Lambert NOAA
# 10/23/2024
# Upper Cook Inlet SAFE
# The purpose of this script is to document data wrangling 


library(tidyverse)
library(readxl)
library(lubridate)



# 2024 Catch data from AKFIN
# Read in the catch data
CIS_Catch_2024 <- read_xlsx(path  = paste0(getwd(),'/','Inseason_Catch/CIS_data_2024_FINAL_20Nov24.xlsx'))

# Calculating the 2024 Kenai, Kasilof and Other salmon catch by day
# Get total catch for the season
# sp.DF <- CIS_Catch_2024 %>%group_by(SPECIES) %>% summarise(tot = sum(CATCH_PER_DAY)) 

# Break out Kenai, Kasilof, and other by day
temp.catch <- CIS_Catch_2024 %>% 
  group_by(OPENER_DATE) %>% 
  summarise("Kenai Sockeye salmon"= CATCH_PER_DAY[SPECIES == 'Sockeye salmon']*.538, #Kenai proportion
            "Kasilof Sockeye salmon" = CATCH_PER_DAY[SPECIES == 'Sockeye salmon']*.24, #Kasilof proportion
            "Other Sockeye salmon" = CATCH_PER_DAY[SPECIES == 'Sockeye salmon']*(1-0.538-0.24)) %>% 
  pivot_longer(cols = c('Kenai Sockeye salmon', 'Kasilof Sockeye salmon','Other Sockeye salmon'), names_to = "SPECIES", values_to = "CATCH_PER_DAY") 

# Subset the original DF for binding
tt <- CIS_Catch_2024[,c(2:4)]

# Bind 
test <- rbind(tt, temp.catch)

# Arrange by date
test <- test %>% group_by(OPENER_DATE) %>% arrange(OPENER_DATE) %>% as.data.frame()

# Arrange
test2 <- test %>%group_by(OPENER_DATE) %>% arrange(OPENER_DATE) %>% as.data.frame()

# Get the Species and TAC
TAC.DF<- unique(CIS_Catch_2024[,c("SPECIES","TAC_2024")])

# Calculate the TAC by Sockeye stocks
Kenai.TAC <- TAC.DF$TAC_2024[TAC.DF$SPECIES == "Sockeye salmon"]*0.538 # From google drive Kenai data
Kasilof.TAC <- TAC.DF$TAC_2024[TAC.DF$SPECIES == "Sockeye salmon"]*0.24 # From google drive Kasilof data
Other.TAC <- TAC.DF$TAC_2024[TAC.DF$SPECIES == "Sockeye salmon"]*(1-0.538-0.24) # Remaining tac

# Put TAC by species into df
Temp.TAC <- data.frame("SPECIES" = c("Kenai Sockeye salmon","Kasilof Sockeye salmon", "Other Sockeye salmon"),
                       "TAC_2024" = c(Kenai.TAC,Kasilof.TAC,Other.TAC))

# bind
totTACDF <- rbind(TAC.DF,Temp.TAC)

totTACDF$SPECIES <- factor(totTACDF$SPECIES)

# Left join to rest of the data
test3 <-left_join(test2, totTACDF)

# Save as csv
# write.csv(test3,file = paste0(getwd(),"/Inseason_Catch/2024 EEZ Catch.csv"),row.names = F)
