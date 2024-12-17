#Lukas DeFilippo, lukas.defilippo@noaa.gov, 781-572-8865
#12/11/2023
#The purpose of this script is to read in disparate data sources to construct post-2021 harvest and escapement estimates for cook inlet salmon stocks

#1) 2022
#read in data
catch_dat_2022 <- read.csv('Complete_EEZ_StateWaterSplits_ToNMFS_2022_2022.csv')
catch_dat_2022 <- subset(catch_dat_2022, catch_dat_2022$DOL.Year==2022)
unique(catch_dat_2022$DOL.Year)

#By species, pull (a) total harvest, (b) EEZ harvest, (c) state harvest
species_vec <- unique(catch_dat_2022$Species)[1:5]
n_species <- length(species_vec)
catch_list <- replicate(length(species_vec), list(replicate(3, vector())))

for(i in 1:n_species){
  # (a) total harvest
  C_total <- as.numeric(catch_dat_2022$Number.Of.Animals..sum.[catch_dat_2022$Species==species_vec[i]])
  catch_list[[i]][1] <- sum(C_total)
  # (b) EEZ harvest
  #C_EEZ <- as.numeric(catch_dat_2022$EEZ_Num[catch_dat_2022$Species==species_vec[i]]) #Using supplied EEZ numbers
  C_EEZ <- C_total*catch_dat_2022$EEZ.[catch_dat_2022$Species==species_vec[i]] #Multiply total by EEZ fraction (ensures summation to total w state harvest)
  catch_list[[i]][2] <- round(sum(C_EEZ), 0)
  
  # (c) state harvest
  #C_state <- (catch_dat_2022$StateWater_Num[catch_dat_2022$Species==species_vec[i]])
  C_state <- C_total*catch_dat_2022$StateWater.[catch_dat_2022$Species==species_vec[i]]
  catch_list[[i]][3] <- round(sum(C_state),0)
}

#For sockeye, need to get estimated proportions of (1) Kenai, (2) Kasilof, and (3) 'Other sockeye'
kenai_sock_prop_2022 <- 0.652
kasilof_sock_prop_2022 <- 0.087
other_sock_prop_2022 <- 1- (kenai_sock_prop_2022 + kasilof_sock_prop_2022)

#For Chinook, need to get estimated proportions of (1) Kenai late run, (2) 'Other Chinook'
kenai_chin_prop_2022 <- 0.402
other_chin_prop_2022 <- 1 - kenai_chin_prop_2022 
