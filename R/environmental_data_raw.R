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
master <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/Environmental_Data_Merged(C).csv')

A <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/21741107-A-9.20.23.csv') %>%
  .[,c(1:4)] 
colnames(A)[2] ="datetime"
colnames(A)[3] ="temp"
colnames(A)[4] ="light"

B <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/21741109-B-9.20.23.csv') %>%
  .[,c(1:4)] 
colnames(B)[2] ="datetime"
colnames(B)[3] ="temp"
colnames(B)[4] ="light"

C <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/21741108-C-9.20.23.csv') %>%
  .[,c(1:4)] 
colnames(C)[2] ="datetime"
colnames(C)[3] ="temp"
colnames(C)[4] ="light"

# Merge loggers  ----------------------------------------------------------

merged <- full_join(A[,2:4],B[,2:4], by="datetime",suffix = c(".a", ""))
merged <- full_join(merged,C[,2:4], by="datetime",suffix = c(".b",".c"))

# clean ends 
merged_clean <- merged[-c(1:15,4030:4037),]          #####

# append to master 
appended <- rbind(master, merged_clean)

# write csv 
#write.csv(appended, '~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/Environmental_Data_Merged.csv')


# full dataset  -----------------------------------------------------------


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
ggplot(long_temp, aes(x=datetime,y=value, color=treatment, group=3)) +
  geom_line()

# box plot 
ggplot(long_temp, aes(x=treatment, y=value)) +
  geom_boxplot()

daily_temp_plot <- ggplot(daily_temp, aes(x=datetime,y=mean_temp, color=treatment)) +
  geom_line() +
  labs(y= "Mean Daily Temperature (˚C)", x = "Date 2023")
daily_temp_plot

ggsave("daily_temp.jpg", plot = daily_temp_plot, path = '~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/Results/')

##### Light 
ggplot(long_light, aes(x=datetime,y=value, color=treatment, group=3)) +
  geom_line()

# box plot 
ggplot(long_light, aes(x=treatment, y=value)) +
  geom_boxplot()

daily_light_plot <- ggplot(daily_light, aes(x=datetime,y=mean_light, color=treatment)) +
  geom_line() +
  labs(y= "Mean Daily Light (lum)", x = "Date")
daily_light_plot

ggsave("daily_light.jpg", plot = daily_light_plot, path = '~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/Results/')



# functional data set  ----------------------------------------------------

long_light_full <- long_light %>%
  filter(.$datetime <= "2023-08-08")
  
long_temp_full <- long_temp %>%
  filter(.$datetime <= "2023-08-08")


daily_light_full <- daily_light %>%
  filter(.$datetime <= "2023-08-08")

daily_temp_full <- daily_temp %>%
  filter(.$datetime <= "2023-08-08")

# full temp data 
ggplot(long_temp_full, aes(x=datetime,y=value, color=treatment, group=3)) +
  geom_line()

# box plot 
ggplot(long_temp_full, aes(x=treatment, y=value)) +
  geom_boxplot()

daily_temp_plot <- ggplot(daily_temp_full, aes(x=datetime,y=mean_temp, color=treatment)) +
  geom_line() +
  labs(y= "Mean Daily Temperature (˚C)", x = "Date 2023")
daily_temp_plot

ggsave("daily_temp_functional.jpg", plot = daily_temp_plot, path = '~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/Results/')

##### Light 
ggplot(long_light_full, aes(x=datetime,y=value, color=treatment, group=3)) +
  geom_line()

# box plot 
ggplot(long_light_full, aes(x=treatment, y=value)) +
  geom_boxplot()

daily_light_plot <- ggplot(daily_light_full, aes(x=datetime,y=mean_light, color=treatment)) +
  geom_line() +
  labs(y= "Mean Daily Light (lum)", x = "Date")
daily_light_plot

ggsave("daily_light_functional.jpg", plot = daily_light_plot, path = '~/Desktop/GITHUB/TL_Astrangia/Environmental_Data/Results/')




# Stats -------------------------------------------------------------------

### Means 

# current means 
colMeans(master[,2:7])

#temp.a     temp.b       temp.c    
#67.634480  67.511428   66.677816  
# control     shades    deep 

# light.a     light.b   light.c 
# 13.327452   4.396765  3.056081 
# control     shades    deep 

### T Tests

# paired t-tests 
temp.a <- master$temp.a
temp.b <- master$temp.b
temp.c <- master$temp.c
light.a <- master$light.a
light.b <- master$light.b
light.c <- master$light.c

t.test(temp.a, temp.b, paired = TRUE) 
t.test(temp.a, temp.c, paired = TRUE) 
t.test(temp.b, temp.c, paired = TRUE) 

t.test(light.a, light.b, paired = TRUE)
t.test(light.a, light.c, paired = TRUE)
t.test(light.b, light.c, paired = TRUE)

# Light vs. temp

lightvtemp <- full_join(daily_temp, daily_light, by=c("datetime","treatment"),suffix = c(".light", ".temp"))
ggplot(lightvtemp, aes(x=mean_temp, y=mean_light))+
  geom_point()

 
