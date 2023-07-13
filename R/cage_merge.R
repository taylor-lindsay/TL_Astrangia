# Title: Cage_Merge.R
# Author: Taylor Lindsay
# Date: 7.6.23
# Input files:
#  master dataset, new cage info 
# Output files: 
#  new cage info merged with master dataset 
# Notes 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Data -------------------------------------------------------------------

# Master data set 
master <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Raw_Data/AP23_Raw2.csv')

# New data set in cage format 
new <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Raw_Data/AP23_grid_survival_7.12.23.csv')     ####


# Manipulate cage data set ------------------------------------------------

# pivot longer 
new_long <- pivot_longer(new,cols = c(G1, G2, G4, G5, D1, D2, D4,D5, S1, S2, S4, S5),
                         names_to = "cage", values_to = "survival_7.12.23")                 ####

# Merge with master 

fin <- full_join(master, new_long) %>%
  .[,-1]                                                                          

# write the file 
write.csv(fin, '~/Desktop/GITHUB/TL_Astrangia/Raw_Data/AP23_Raw2.csv')

