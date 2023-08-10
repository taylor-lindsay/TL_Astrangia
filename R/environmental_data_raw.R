# Title: environmental_data_raw.R
# Author: Taylor Lindsay
# Date: 7.6.23
# Input files:
#  master dataset, new files
# Output files: 
#  master dataset  
# Notes 
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggplot2)

# Data -------------------------------------------------------------------

# Master data set 
master <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/Environmental_Data_Merged.csv')

#A <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/21741107-A_7.5.23.csv') %>%
  .[,c(1:4)] 
colnames(A)[2] ="datetime"
colnames(A)[3] ="temp"
colnames(A)[4] ="light"

B <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/21741109-B_7.5.23.csv') %>%
  .[,c(1:4)] 
colnames(B)[2] ="datetime"
colnames(B)[3] ="temp"
colnames(B)[4] ="light"

C <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/21741108-C_7.5.23.csv') %>%
  .[,c(1:4)] 
colnames(C)[2] ="datetime"
colnames(C)[3] ="temp"
colnames(C)[4] ="light"

# Merge loggers  ----------------------------------------------------------

merged <- full_join(A[,2:4],B[,2:4], by="datetime",suffix = c(".a", ".b"))
merged <- full_join(merged,C[,2:4], by="datetime",suffix = c(".c", ".c"))

# clean ends 
#merged_clean <- merged[-c(1:12,1449:1456),]          #####

# append to master 

appended <- rbind(master, merged_clean)

# write csv 
write.csv(appended, '~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/Environmental_Data_Merged.csv')


#### Manipulate dataset 

# make it datetime format 
master$datetime <- mdy_hm(master$datetime,tz=Sys.timezone())

long <- pivot_longer(master,cols = c(temp.a,temp.b,temp.c,light.a,light.b,light.c),
                          names_to = "treatment", values_to = "value") %>%
  separate(.,treatment, c("variable","treatment"))

long$treatment <- long$treatment %>% 
  gsub("a", "Control", .)  %>%
  gsub("b", "Shaded", .)  %>%
  gsub("c", "Deep", .)

long_temp <- long %>% filter(variable == "temp")
long_temp$datetime <- as.Date(long_temp$datetime)
daily_temp <-long_temp %>% group_by(datetime,treatment) %>%
  summarize(mean_temp = mean(value))

long_light <- long %>% filter(variable == "light")
long_light$datetime <- as.Date(long_light$datetime)
daily_light <-long_light %>% group_by(datetime,treatment) %>%
  summarize(mean_light = mean(value))

# graphs  ---------------------------------------------------

  # full temp data 
ggplot(long_temp, aes(x=datetime,y=temperature, color=treatment, group=3)) +
  geom_line()

daily_temp_plot <- ggplot(daily_temp, aes(x=datetime,y=mean_temp, color=treatment)) +
  geom_line() +
  labs(y= "Mean Daily Temperature (˚C)", x = "Date 2023")

ggsave("daily_temp.jpg", plot = daily_temp_plot, path = '~/Desktop/')

##### Light 
ggplot(long_light, aes(x=datetime,y=light, color=treatment, group=3)) +
  geom_line()

daily_light_plot <- ggplot(daily_light, aes(x=datetime,y=mean_light, color=treatment)) +
  geom_line() +
  labs(y= "Mean Daily Light (lum)", x = "Date")

ggsave("daily_light.jpg", plot = daily_light_plot, path = '~/Desktop/')

# current means 
colMeans(merged_clean[,2:7])

#temp.a     temp.b       temp.c    
#65.711478  65.610485   65.204969  
# control     shades    deep 

# light.a     light.b   light.c 
# 14.465877   6.708217  2.917827 
# control     shades    deep 

