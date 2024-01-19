## # Title: quad_modeling.R
# Author: TL
# Date: 11.10.2023
# Input files: 
#  master quadrat data 
# Output files: 
#  
# Notes


# libraries ---------------------------------------------------------------


library(tidyverse)
#library(dplyr)
#library(ggpubr)
library(ggplot2)
#library(scales)
library(gridExtra) # for making graph arrangements 
library(mgcv)   #for Generalized additive models 
library(gam)    #for Generalized additive models 


# Data entry & manipulation  ----------------------------------------------

# upload data 
master <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Quadrat_Sampling/quad_data_raw_master.csv')%>%
  select(!X)

# make new column for total colonies
master <- master %>%
  mutate(all_colonies = mean_sym + mean_apo)

# make bin category 
master <- master %>% mutate(bins = cut(corrected_depth, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)))
# summarize by bins 
bins_merged <- master %>% group_by(bins) %>% summarise_at(c("mean_apo", "mean_sym"), mean, na.rm = TRUE)
# create new df of numbers for break points 
break_points <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
#add to summary df 
bins_merged$breaks = break_points
# pivot longer for stacked bar graph 
#summary_merged_longer <- summary_merged %>% pivot_longer(cols = c(mean_apo, mean_sym), names_to = "colony_type", values_to = "value")

# how many quadrats have i done? 
quad_counts <- master %>% group_by(bins) %>% summarise(count = n()) 
quad_counts$breaks = break_points

# Add ratios 
master<- master %>%
  mutate(ratio = 
           case_when(
             mean_apo==mean_sym ~ 0,
             mean_sym==0 ~ 1,
             mean_apo==0 ~ -1,
             mean_apo<mean_sym ~ (mean_apo/mean_sym*(-1)),
             mean_apo>mean_sym ~ mean_sym/mean_apo))


# Modeling  ---------------------------------------------------------------

sym_smooth <- gam(mean_sym ~ s(corrected_depth, k=12), data = master)
gam.check(sym_smooth)
summary(sym_smooth)
plot.gam(sym_smooth, shade=TRUE, shade.col = "#A63566", main= "Symbiotic Colonies", xlab="Depth", ylab = "Predicted Number of Colonies") 

apo_smooth <- gam(mean_apo ~ s(corrected_depth), data = master)
gam.check(apo_smooth)
summary(apo_smooth)
plot.gam(apo_smooth, shade=TRUE, shade.col = "#5CA8DB", main= "Aposymbiotic Colonies", xlab="Depth", ylab = "Predicted Number of Colonies")

# this will plot them together, but you have to change the gam command to gamm 
sym_smooth2 <- gamm(mean_sym ~ s(corrected_depth, k=12), data = master)
apo_smooth2 <- gamm(mean_apo ~ s(corrected_depth), data = master)

Apo <- apo_smooth2$gam
Sym <- sym_smooth2$gam
plot <- compare_smooths(Apo,Sym) %>% draw()
plot + labs(x="Depth", y="Estimate of Colony Number") + scale_color_manual(values = c("#5CA8DB", "#A63566")) +  scale_fill_manual(values = c("#5CA8DB", "#A63566")) + ggtitle("Generalized Additive Models predict colony distribution") 


