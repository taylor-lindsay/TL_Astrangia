## # Title: quad_envi_merging.R
# Author: TL
# Date: 08.28.2023
# Input files: 
#  2x hobo files
# Output files: 
#  
# Notes


# Libraries ---------------------------------------------------------------

library(tidyverse)
#library(dplyr)
#library(ggpubr)
library(ggplot2)

# Data -------------------------------------------------------------------

x <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706002-X-2023-10-03.csv')
z <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706004-Z-2023-10-03.csv')
#x <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706005-V_2023-09-29.csv')
#z <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706006-W_2023-09-29.csv')

# Merge the data

merged <- rbind(x,z)

#in_water <- merged %>%
#  filter(.$Date.Time..EDT. >= "09/22/2023 10:00:00") %>%
#  filter(.$Date.Time..EDT. <= "09/22/2023 12:30:00") 

# Temp graph
#ggplot(in_water, aes(x= Date.Time..EDT., y=Ch.1...Temperature.....C.)) + geom_point()


# light graph 
#ggplot(in_water, aes(x= Date.Time..EDT., y=Ch.2...Light....lux.)) +  geom_point()

# specific quadrat values 

new <- merged %>%
  filter(.$Date.Time..EDT. >= "10/03/2023 09:29:10") %>%
  filter(.$Date.Time..EDT. <= "10/03/2023 09:33:00") %>%
  mutate(zscore = (.$Ch.2...Light....lux. - mean(.$Ch.2...Light....lux.))/sd(.$Ch.2...Light....lux.)) # %>% filter(abs(zscore)<2) #%>% filter(Ch.2...Light....lux. >= 3000)

ggplot(new, aes(x= Date.Time..EDT., y=new$Ch.2...Light....lux.)) +
  geom_point()

mean(new$Ch.2...Light....lux.)
mean(new$Ch.1...Temperature.....C.)

# MERGE TO MASTER DATA SET ------------------------------------------------

# once the data has been entered for today, merge it to the master 
raw <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_2023-10-03.csv') %>%
  filter(quality_control == "good") %>%
  select(date,algae,corrected_depth,mean_sym,mean_apo,light,temp)

# load in master 
master <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_master.csv')%>%
  select(!X)
  
# merge step
merged<- rbind(master,raw)

# write it 
write.csv(merged, '~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_master.csv')

# graph for light 
ggplot(merged) +
  geom_point(aes(x=corrected_depth,y=light)) + 
  geom_smooth(aes(x=corrected_depth,y=light)) 

# apo sym 
ggplot(merged) +
  geom_point(aes(x=corrected_depth,y=mean_apo)) +
  geom_smooth(aes(x=corrected_depth,y=mean_apo))+
  geom_point(aes(x=corrected_depth,y=mean_sym), color = "red") +
  geom_smooth(aes(x=corrected_depth,y=mean_sym), color = "red") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# algae
ggplot(merged) +
  geom_point(aes(x=algae,y=mean_apo)) +
  geom_smooth(aes(x=algae,y=mean_apo))+
  geom_point(aes(x=algae,y=mean_sym), color = "red") +
  geom_smooth(aes(x=algae,y=mean_sym), color = "red") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

# algae
ggplot(merged) +
  geom_point(aes(x=algae,y=mean_apo)) +
  geom_smooth(aes(x=algae,y=mean_apo))+
  geom_point(aes(x=algae,y=mean_sym), color = "red") +
  geom_smooth(aes(x=algae,y=mean_sym), color = "red") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

ggplot(merged)+
  geom_point(aes(x=corrected_depth, y=algae)) + 
  geom_smooth(aes(x=corrected_depth, y=algae))



