# Title: AP23 Analysis
# Author: Taylor Lindsay (updated from Putnam Lab)
# Date: 12.13.2023
# Input files:
# Raw Data File
# Output files:
# Graphs 
# Notes 

library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)

# read data
master <- read_csv("~/Desktop/GITHUB/TL_Astrangia/Raw_Data/AP23_ALL_Results.csv") 

# Define groups and settings 
treatment_comparisons <- list( c("apo_deep","sym_deep"), c("apo_control","sym_control"), c("apo_shade","sym_shade"), c("apo_control","apo_shade"), c("apo_control","apo_deep"), c("sym_control","sym_shade"), c("sym_control","sym_deep"))
treatment_comparisons_combined <- list( c("control","shade"), c("control","deep"))
measurement_order <- c('control','shade','deep') 
x_order <- c('apo_control','sym_control','apo_shade', 'sym_shade', 'apo_deep', 'sym_deep') 
x_order_combined <- c('control','shade', 'deep') 
pj <- position_jitterdodge(jitter.width=0.1, seed=9,
                           jitter.height = 0)

# GRAPHS: 

#CHL
AP23_CHL <- ggplot(master, aes(x=factor(full_treatment, level=x_order), y=chla.ug.cm2, fill=factor(treatment, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(treatment, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('Control', 'Shade', 'Deep')) + 
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(y= "CHL", x = "Depth Treatment", fill='Treatment') + 
  scale_x_discrete(labels=c('Apo', 'Sym', 'Apo', 'Sym', 'Apo', 'Sym')) +
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "t.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", "")))
AP23_CHL
ggsave("AP23_CHL_boxplot.jpg", plot = AP23_CHL, path = '~/Desktop/GITHUB/TL_Astrangia/Results/', width = 6,
       height = 6)

#AO
master_AO <- master%>% filter(!is.na(full_treatment))
AP23_AO <- ggplot(master_AO, aes(x=factor(full_treatment, level=x_order), y=cre.umol, fill=factor(treatment, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(full_treatment, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('Control', 'Shade', 'Deep')) + 
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(x = "Treatment", y = "Copper Reducing Equivalents (µmol/mg protein)",
       title = "Total antixodidant capacity", fill = "Treatment") + 
  scale_x_discrete(labels=c('Apo', 'Sym', 'Apo', 'Sym', 'Apo', 'Sym')) +
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "t.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", ""))) 

AP23_AO
ggsave("AP23_AO_boxplot_full.jpg", plot = AP23_AO, path = '~/Desktop/GITHUB/TL_Astrangia/Results/', width = 6,
       height = 6)

# combine sym and apo 
AP23_AO_combined <- ggplot(master_AO, aes(x=factor(treatment, level=x_order_combined), y=cre.umol)) + 
  geom_boxplot(alpha=0.5, outlier.size=0) + 
  labs(x = "Treatment", y = "Copper Reducing Equivalents (µmol/mg protein)",
       title = "Total antixodidant capacity", fill = "Treatment") +
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons_combined, method = "t.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", ""))) 
AP23_AO_combined
ggsave("AP23_AO_boxplot_full_combined.jpg", plot = AP23_AO_combined, path = '~/Desktop/GITHUB/TL_Astrangia/Results/', width = 6,
       height = 6)


#AO/prot
master_cre <- master %>%
  filter(!is.na(cre.umol.mgprot)) %>%
  mutate(zscore = (.$cre.umol.mgprot - mean(.$cre.umol.mgprot))/sd(.$cre.umol.mgprot)) %>%
  filter(abs(zscore)<3) 

AP23_AO_prot <- ggplot(master_cre, aes(x=factor(full_treatment, level=x_order), y=cre.umol.mgprot, fill=factor(treatment, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(full_treatment, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('Control', 'Shade', 'Deep')) + 
  scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(x = "full_treatment", y = "Copper Reducing Equivalents (µmol/mg protein)",
       title = "Total antixodidant capacity", fill = "Treatment") + 
  #scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "t.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", ""))) 

AP23_AO_prot
ggsave("AP23_AO_prot_boxplot.jpg", plot = AP23_AO_prot, path = '~/Desktop/GITHUB/TL_Astrangia/Results/', width = 6,
       height = 6)

# remove sym/apo distinction
simple_comparisons <- list( c("control","deep"), c("control","shade"))
measurement_order <- c('control','shade','deep') 
x_order <- c('apo_control','sym_control','apo_shade', 'sym_shade', 'apo_deep', 'sym_deep') 
pj <- position_jitterdodge(jitter.width=0.1, seed=9,
                           jitter.height = 0)

ggplot(master_cre, aes(x=treatment, y=cre.umol.mgprot)) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  #geom_point(aes(color=factor(full_treatment, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  #scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('Control', 'Shade', 'Deep')) + 
  #scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(x = "treatment", y = "Copper Reducing Equivalents (µmol/mg protein)",
       title = "Total antixodidant capacity", fill = "Treatment") + 
  #scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = simple_comparisons, method = "t.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", ""))) 

# protein
AP23_prot <- ggplot(master, aes(x=factor(full_treatment, level=x_order), y=prot_mg.cm2, fill=factor(treatment, level=measurement_order))) +
  geom_boxplot(alpha=0.5, outlier.size=0) +
  geom_point(aes(color=factor(treatment, level=measurement_order)), position = pj, size=1, show.legend = FALSE)+
  #scale_fill_manual(values = c("#EBB134", "#ED6B5F", "#6060A8"), labels=c('Control', 'Shade', 'Deep')) + 
  #scale_color_manual(values = c("#EBB134", "#ED6B5F", "#6060A8")) + 
  labs(y= "CHL", x = "Depth Treatment", fill='Treatment') + 
  #scale_x_discrete(labels=c('Deep', 'Shallow', 'Deep', 'Shallow', 'Deep', 'Shallow')) + 
  theme_classic() +
  stat_compare_means(comparisons = treatment_comparisons, method = "t.test", 
                     symnum.args = list(cutpoints = c(0, 0.001 ,0.01, 0.05, Inf), symbols = c("***", "**", "*", ""))) 

AP23_prot
ggsave("AP23_protein_boxplot.jpg", plot = AP23_prot, path = '~/Desktop/GITHUB/TL_Astrangia/Results/', width = 6,
       height = 6)

