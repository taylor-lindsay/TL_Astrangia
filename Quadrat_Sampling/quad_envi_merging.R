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

x <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706002-X_2023-08-28.csv')
z <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706004-Z_2023-08-28.csv')

# Merge the data

merged <- rbind(x,z)

in_water <- merged %>%
  filter(.$Date.Time..EDT. >= "08/28/2023 10:24:00") %>%
  filter(.$Date.Time..EDT. <= "08/28/2023 11:20:00") 

# Temp graph
ggplot(in_water, aes(x= Date.Time..EDT., y=Ch.1...Temperature.....C.)) +
  geom_point()


# light graph 
ggplot(in_water, aes(x= Date.Time..EDT., y=Ch.2...Light....lux.)) +
  geom_point()

# specific quadrat values 

new <- merged %>%
  filter(.$Date.Time..EDT. >= "08/28/2023 11:17:00") %>%
  filter(.$Date.Time..EDT. <= "08/28/2023 11:20:00") %>%
  mutate(zscore = (.$Ch.2...Light....lux. - mean(.$Ch.2...Light....lux.))/sd(.$Ch.2...Light....lux.)) %>%
  filter(abs(zscore)<2)

ggplot(new, aes(x= Date.Time..EDT., y=new$Ch.2...Light....lux.)) +
  geom_point()

mean(new$Ch.2...Light....lux.)
mean(new$Ch.1...Temperature.....C.)
