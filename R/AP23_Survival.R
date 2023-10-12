# Title: AP23_Survival.R
# Author: Taylor Lindsay
# Date: 10.11.23
# Input files:
#  Master data set with days_alive info 
# Output files: 
#  survival? 
# Notes 
# Libraries ---------------------------------------------------------------
#load required packages
library(tidyr)
library(dplyr)
#library(lubridate)
library("survival")
library("survminer")

# Data ---------------------------------------------------------------------

# load data and filter out only dead and alive individuals 
raw <- read.csv('~/Desktop/GITHUB/TL_Astrangia/Raw_Data/AP23_Raw_all_survival_01.csv') %>%
  filter(survival_10.10.23 == 0 | survival_10.10.23 == 1) 

# make the final column numeric 
raw$survival_10.10.23 <- as.numeric(raw$survival_10.10.23)

# create a surv object 
surv_object <- Surv(raw$days_alive, raw$survival_10.10.23)

# plot by treatment 
fit2 <- survfit(surv_object ~ treatment, data = raw)
plot_treatment <- ggsurvplot(fit2, data = raw, pval = TRUE, legend = "bottom", legend.title = "Treatment", legend.labs = c("Control", "Deep", "Shade"))
plot_treatment <- plot_treatment + xlab("Days")
plot_treatment

# plot by apo/sym
fit2 <- survfit(surv_object ~ apo_sym, data = raw)
plot_sym_apo <- ggsurvplot(fit2, data = raw, pval = TRUE, legend = "bottom", legend.title = "Status", legend.labs = c("Apo", "Sym"))
plot_sym_apo <- plot_sym_apo + xlab("Days")
plot_sym_apo



