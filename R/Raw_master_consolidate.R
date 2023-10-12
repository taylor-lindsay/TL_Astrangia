# Title: Raw_master_consolidate.R
# Author: Taylor Lindsay
# Date: 10.10.23
# Input files:
#  master dataset, new cage info 
# Output files: 
#  new cage info merged with master dataset 
# Notes 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)


I made manual changes to this document after writing this script, see AP23_Raw_master.csv

# Data -------------------------------------------------------------------

# Apo sym data set  
#master <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Raw_Data/AP23_Raw_apo_sym.csv') 

# cage grid sample numbers dataset 
#new <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Raw_Data/AP23_grid_numbers_start.csv')      ####


# pivot cage grid longer 
#new_long <- pivot_longer(new,cols = c(G1, G2, G4, G5, D1, D2, D4,D5, S1, S2, S4, S5),
                         names_to = "cage", values_to = "sample")      

#new_long <- new_long %>% mutate(sample_id = paste(cage, spot, sample, sep="-"))

#new_both <- merge(new_long, master, all=TRUE)

# write master 
#write.csv(new_both, '~/Desktop/GITHUB/TL_Astrangia/Raw_Data/AP23_Raw_master.csv')
