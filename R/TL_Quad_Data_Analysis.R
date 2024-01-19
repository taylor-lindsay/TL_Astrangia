## # Title: quad_envi_merging.R
# Author: TL
# Date: 12.23.2023
# Input files: 
#  
# Output files: 
#  
# Notes


# Libraries ---------------------------------------------------------------

library(tidyverse)
#library(dplyr)
#library(ggpubr)
library(ggplot2)
#library(scales)
#library(gridExtra)
theme_set(theme_classic())

# Data -------------------------------------------------------------------

# read and edit data 
data <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_master.csv') %>%
  select(algae, corrected_depth_m, mean_sym, mean_apo, light) %>%
  mutate(all_colonies = mean_sym + mean_apo) %>%
  mutate(bins = cut(corrected_depth_m, breaks = c(0,3,6,9,12,15,18,21,24))) 

# convert to m^2
data <- data %>%
  mutate(mean_sym_m2 = mean_sym*4) %>%
  mutate(mean_apo_m2 = mean_apo*4)

# Calculate means by bins 

# summarize by bins 
summary_merged <- data %>% group_by(bins) %>% summarise_at(c("mean_apo", "mean_sym"), mean, na.rm = TRUE)
# create new df of numbers for break points 
break_points <- c(0,3,6,9,12,15,18,21)
#add to summary df 
summary_merged$breaks = break_points
# pivot longer for stacked bar graph 
summary_merged <- summary_merged %>% 
  pivot_longer(cols = c(mean_apo, mean_sym), names_to = "colony_type", values_to = "value")

# how many quadrats have i done? 
quad_counts <- data %>% group_by(bins) %>% summarise(count = n()) 
quad_counts$breaks = break_points


# Percent cover based on sizes 
# colonies were 0.053629137 m^2 
data <- data %>%
  mutate(percent_sym = mean_sym*0.0053629137*100) %>%
  mutate(percent_apo = mean_apo*0.0053629137*100)

# Graphing ------------------------------------------------

# PLOT Light x Depth 
LightxDepth <- ggplot(data) +
  geom_point(aes(x=corrected_depth_m,y=light), color = "orange", alpha = 0.6) + 
  geom_smooth(aes(x=corrected_depth_m,y=light), color= "orange", span=0.5,) +
  labs(y= "Light (Lux)", x = "Depth (m below MLLW)") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) 
LightxDepth
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_LightxDepth.jpg', LightxDepth, width = 4, height = 5)

# PLOT Morphs x Depth 
MorphsxDepth <- ggplot(data) +
  theme_classic() + 
  geom_point(aes(x = corrected_depth_m, y = mean_apo_m2), color = "#5CA8DB",size = 0.5, alpha = 0.6) +
  geom_smooth(aes(x = corrected_depth_m, y = mean_apo_m2, color = "Mean Apo"), span = 0.5, se=F) +
  geom_point(aes(x = corrected_depth_m, y = mean_sym_m2), color = "#A63566", size = 0.5, alpha = 0.4) +
  geom_smooth(aes(x = corrected_depth_m, y = mean_sym_m2, color = "Mean Sym"), span = 0.5, se=F) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(y = "Colonies per m^2", x = "Depth (m below MLLW)") +
  scale_color_manual(
    values = c("Mean Apo" = "#5CA8DB", "Mean Sym" = "#A63566"),
    name = "Colony Morph"
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
AlgaexDepth <- ggplot(data)+
  geom_point(aes(x=corrected_depth_m, y=algae), color = "#1B6B22", alpha = 0.6) + 
  geom_smooth(aes(x=corrected_depth_m, y=algae), color = "#1B6B22") +
  labs(y= "Algae % Cover", x = "Depth (m below MLLW)")  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
AlgaexDepth
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_AlgaexDepth.jpg', AlgaexDepth, width = 4, height = 5)

# PLOT Colonies x Depth 
ColoniesxDepth <- ggplot(data) +
  geom_point(aes(x=corrected_depth_m,y=all_colonies)) +
  geom_smooth(aes(x=corrected_depth_m,y=all_colonies),span=0.5)+
  labs(y= "Total colonies / m2", x = "Depth (m below MLLW)")  +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
ColoniesxDepth
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_ColoniesxDepth.jpg', ColoniesxDepth, width = 8, height = 6)

# PLOT Colonies x Depth + Algae 
ColoniesxDepthxAlgae <- ggplot(data, aes(x = corrected_depth_m)) +
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
ColoniesxDepthxAlgae <- ggplot(data, aes(x = corrected_depth_m)) +
  #geom_point(aes(y = algae*1.6), color = "#1B6B22") +
  geom_smooth(aes(y = algae * 1.6, color = "Algae"), span = 0.5, se=F) +
  #geom_point(aes(x=corrected_depth,y=mean_apo_m2), color= "#5CA8DB", alpha = 0.6) +
  geom_smooth(aes(x = corrected_depth_m, y = mean_apo_m2, color = "Mean Apo"), span = 0.7, alpha = 0.6, se=F) +
  #geom_point(aes(x=corrected_depth,y=mean_sym_m2), color = "#A63566", alpha = 0.6) +
  geom_smooth(aes(x = corrected_depth_m, y = mean_sym_m2, color = "Mean Sym"), span = 0.5, alpha = 0.6, se=F) +
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
bar_Morphs <- ggplot(summary_merged, aes(fill=colony_type, y=value, x=breaks)) + 
  geom_bar(position="stack", stat="identity")
bar_Morphs
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_bar_Morphs.jpg', bar_Morphs, width = 8, height = 6)

# PLOT counts of quadrats in bins 
bar_counts <- ggplot(quad_counts, aes(x=breaks, y=count)) +
  geom_bar(stat = "identity")
bar_counts
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_bar_counts.jpg', bar_counts, width = 8, height = 6)

# AP percent cover ---------------------------------------------------------

# mean per bin 
percent_cover_bins <- data %>%
  group_by(bins) %>%
  summarise_at(c("mean_apo", "mean_sym", "algae"), mean, na.rm = TRUE) %>% 
  mutate(other=100-(mean_apo+mean_sym+algae)) %>%
  mutate(percent_total = mean_apo+mean_sym) 
longer_sep <- percent_cover_bins %>%
  pivot_longer(cols = c(mean_apo, mean_sym, algae), names_to = "type", values_to = "value")
longer_total <- percent_cover_bins %>%
  pivot_longer(cols = c(percent_total, algae), names_to = "type", values_to = "value")

percent_cover_sep <- ggplot(longer_sep, aes(bins, weight = value, fill=factor(type, levels=c("algae", "mean_apo","mean_sym")))) +
  geom_bar() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y= "Percent Cover", x="Depth Bins (m)", fill="Cover Type")
percent_cover_sep
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_percent_cover_stacked_bar.jpg', percent_cover_sep, width = 10, height = 6)

percent_cover_total <- ggplot(longer_total, aes(bins, weight = value, fill=factor(type, levels=c("algae","percent_total")))) +
  geom_bar() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y= "Percent Cover", x="Depth Bins (m)", fill="Cover Type")
percent_cover_total
ggsave('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Results/Plot_TLAP_percent_cover_stacked_bar.jpg', percent_cover_total, width = 10, height = 6)
 




