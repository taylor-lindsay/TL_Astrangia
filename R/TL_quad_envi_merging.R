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
library(scales)
library(gridExtra)
theme_set(theme_bw())

# Data -------------------------------------------------------------------

x <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706002-X-2023-11-13.csv')
z <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706004-Z-2023-11-13.csv')
#x <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706005-V-2023-11-05.csv')
#z <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_hobo_21706006-W-2023-11-05.csv')

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
  filter(.$Date.Time..EST. >= "11/13/2023 10:35:20") %>%
  filter(.$Date.Time..EST. <= "11/13/2023 10:37:30") %>%
  mutate(zscore = (.$Ch.2...Light....lux. - mean(.$Ch.2...Light....lux.))/sd(.$Ch.2...Light....lux.)) %>% 
  filter(abs(zscore)<2) #%>% filter(Ch.2...Light....lux. >= 500)

ggplot(new, aes(x= Date.Time..EST., y=new$Ch.2...Light....lux.)) +
  geom_point()

mean(new$Ch.2...Light....lux.)
mean(new$Ch.1...Temperature.....C.)


# once the data has been entered for today, merge it to the master 
raw <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_2023-11-13.csv') %>%
  filter(quality_control == "good") %>%
  select(date,algae,corrected_depth,mean_sym,mean_apo,light,temp)

# load in master 
master <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_master.csv')%>%
  select(!X)

# merge step
merged <- rbind(master,raw)

# write it 
#write.csv(merged, '~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_master.csv')


# Mutate Data (bins, m2, etc.) ------------------------------------------------

# START HERE

## LOAD DATA 
master <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_master.csv')%>%
  select(!X)

## make new column for total colonies
updated <- master %>%
  mutate(all_colonies = mean_sym + mean_apo)

# make bin category 
master <- master %>% mutate(bins = cut(corrected_depth, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)))
# summarize by bins 
summary_merged <- master %>% group_by(bins) %>% summarise_at(c("mean_apo", "mean_sym"), mean, na.rm = TRUE)
# create new df of numbers for break points 
break_points <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
#add to summary df 
summary_merged$breaks = break_points
# pivot longer for stacked bar graph 
summary_merged_longer <- summary_merged %>% 
  pivot_longer(cols = c(mean_apo, mean_sym), names_to = "colony_type", values_to = "value")

# how many quadrats have i done? 
quad_counts <- master %>% group_by(bins) %>% summarise(count = n()) 
quad_counts$breaks = break_points

# Ratios of apo:sym 
master_ratios <- master %>%
  mutate(ratio = 
           case_when(
             mean_apo==mean_sym ~ 0,
             mean_sym==0 ~ 1,
             mean_apo==0 ~ -1,
             mean_apo<mean_sym ~ (mean_apo/mean_sym*(-1)),
             mean_apo>mean_sym ~ mean_sym/mean_apo))

# convert to m^2
master <- master %>%
  mutate(mean_sym_m2 = mean_sym*4) %>%
  mutate(mean_apo_m2 = mean_apo*4)

# Percent cover based on sizes 
# colonies were 0.053629137 m^2 

master <- master %>%
  mutate(percent_sym = mean_sym*0.0053629137*100) %>%
  mutate(percent_apo = mean_apo*0.0053629137*100)

# Graphing ------------------------------------------------

# PLOT Light x Depth 
LightxDepth <- ggplot(master) +
  geom_point(aes(x=corrected_depth,y=light), color = "orange", alpha = 0.6) + 
  geom_smooth(aes(x=corrected_depth,y=light), color= "orange", span=0.5,) +
  labs(y= "Light (Lux)", x = "Depth (ft below MLLW)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) 
LightxDepth
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_LightxDepth.jpg', LightxDepth, width = 4, height = 5)

# PLOT Morphs x Depth 
MorphsxDepth <- ggplot(master) +
  geom_point(aes(x = corrected_depth, y = mean_apo_m2), color = "#5CA8DB", alpha = 0.6) +
  geom_smooth(aes(x = corrected_depth, y = mean_apo_m2, color = "Mean Apo"), alpha = 0.6) +
  geom_point(aes(x = corrected_depth, y = mean_sym_m2), color = "#A63566", alpha = 0.6) +
  geom_smooth(aes(x = corrected_depth, y = mean_sym_m2, color = "Mean Sym"), alpha = 0.6) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(y = "Colonies per m^2", x = "Depth (ft below MLLW)") +
  scale_color_manual(
    values = c("Mean Apo" = "#5CA8DB", "Mean Sym" = "#A63566"),
    name = ""
  )
MorphsxDepth 
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_MorphsxDepth.jpg', MorphsxDepth, width = 10, height = 6)

# PLOT Morphs x Algae
MorphsxAlgae <-ggplot(master) +
  geom_point(aes(x=algae,y=mean_apo_m2), color = "#5CA8DB", alpha = 0.6) +
  geom_smooth(aes(x=algae,y=mean_apo_m2), color = "#5CA8DB", alpha = 0.6)+
  geom_point(aes(x=algae,y=mean_sym_m2), color = "#A63566", alpha = 0.6) +
  geom_smooth(aes(x=algae,y=mean_sym_m2), color = "#A63566", alpha = 0.6) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  labs(y= "Colonies per m^2", x = "Algae % Cover") 
MorphsxAlgae
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_MorphsxAlgae.jpg', MorphsxAlgae, width = 8, height = 6)

# PLOT Algae x depth
AlgaexDepth <- ggplot(master)+
  geom_point(aes(x=corrected_depth, y=algae), color = "#1B6B22", alpha = 0.6) + 
  geom_smooth(aes(x=corrected_depth, y=algae), color = "#1B6B22") +
  labs(y= "Algae % Cover", x = "Depth (ft below MLLW)")  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
AlgaexDepth
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_AlgaexDepth.jpg', AlgaexDepth, width = 4, height = 5)

# PLOT Colonies x Depth 
ColoniesxDepth <- ggplot(updated) +
  geom_point(aes(x=corrected_depth,y=all_colonies)) +
  geom_smooth(aes(x=corrected_depth,y=all_colonies),span=0.5)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), sec.axis = sec_axis(~ . * 2, name = "Secondary Y-Axis"))
ColoniesxDepth
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_ColoniesxDepth.jpg', ColoniesxDepth, width = 8, height = 6)

# PLOT Colonies x Depth + Algae 
ColoniesxDepthxAlgae <- ggplot(updated, aes(x = corrected_depth)) +
  geom_point(aes(y = algae*1.6), color = "#1B6B22") +
  geom_smooth(aes(y=algae*1.6),span=0.5, color = "#1B6B22", fill = "transparent")+
  geom_point(aes(y = all_colonies), color = "black") +
  geom_smooth(aes(y=all_colonies),span=0.5, color = "black", fill = "transparent")+
  scale_y_continuous(
    name = "Colonies / m2",
    sec.axis = sec_axis(~ . / 1.6, name = "Algae percent cover"),
    limits = c(0, 160))
ColoniesxDepthxAlgae
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_ColoniesxDepthxAlgae.jpg', ColoniesxDepthxAlgae, width = 8, height = 6)

# PLOT Colonies x Depth + Algae 
ColoniesxDepthxAlgae <- ggplot(master, aes(x = corrected_depth)) +
  #geom_point(aes(y = algae*1.6), color = "#1B6B22") +
  geom_smooth(aes(y = algae * 1.6, color = "Algae"), span = 0.5, fill = "transparent") +
  #geom_point(aes(x=corrected_depth,y=mean_apo_m2), color= "#5CA8DB", alpha = 0.6) +
  geom_smooth(aes(x = corrected_depth, y = mean_apo_m2, color = "Mean Apo"), span = 0.7, alpha = 0.6, fill = "transparent") +
  #geom_point(aes(x=corrected_depth,y=mean_sym_m2), color = "#A63566", alpha = 0.6) +
  geom_smooth(aes(x = corrected_depth, y = mean_sym_m2, color = "Mean Sym"), span = 0.5, alpha = 0.6, fill = "transparent") +
  scale_y_continuous(
    name = "Colonies per m^2",
    sec.axis = sec_axis(~ . / 1.6, name = "Algae percent cover"),
    limits = c(0, 160)) +
  scale_color_manual(
    values = c("Algae" = "#1B6B22", "Mean Apo" = "#5CA8DB", "Mean Sym" = "#A63566"),
    name = ""
  ) +
  theme_minimal()

ColoniesxDepthxAlgae
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_ColoniesxDepthxAlgae.jpg', ColoniesxDepthxAlgae, width = 10, height = 6)


# PLOT Bar graph of Morphs 
bar_Morphs <- ggplot(summary_merged_longer, aes(fill=colony_type, y=value, x=breaks)) + 
  geom_bar(position="stack", stat="identity")
bar_Morphs
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_bar_Morphs.jpg', bar_Morphs, width = 8, height = 6)

# PLOT counts of quadrats in bins 
bar_counts <- ggplot(quad_counts, aes(x=breaks, y=count)) +
  geom_bar(stat = "identity")
bar_counts
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_bar_counts.jpg', bar_counts, width = 8, height = 6)

# PLOT Ratios
ggplot(master_ratios) +
  geom_point(aes(x=corrected_depth,y=ratio)) +
  geom_smooth(aes(x=corrected_depth,y=ratio),span=0.5)
           

# AP percent cover ---------------------------------------------------------

# mean per bin 
percent_cover_bins <- master %>%
  group_by(bins) %>%
  summarise_at(c("mean_apo", "mean_sym", "algae"), mean, na.rm = TRUE) %>% 
  mutate(other=100-(mean_apo+mean_sym+algae)) %>%
  pivot_longer(cols = c(mean_apo, mean_sym, algae), names_to = "type", values_to = "value")

percent_cover <- ggplot(percent_cover_bins, aes(bins, weight = value, fill=factor(type, levels=c("other", "algae", "mean_apo","mean_sym")))) +
  geom_bar() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y= "Percent Cover", x="Depth Bins (ft)", fill="Cover Type")

percent_cover

ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_percent_cover_stacked_bar.jpg', percent_cover, width = 10, height = 6)






