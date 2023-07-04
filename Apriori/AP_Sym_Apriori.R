#AP_Apriori

library(tidyverse)
library(ggpubr)

ap_raw <- read.csv('~/Desktop/GITHUB/TL_Astrangia/AP_Sym_Apriori.csv')

no_mixed <- ap_raw %>%
  filter(Morph != "Mixed")

x <- ggplot(no_mixed, aes(x=Morph, y=Cells.cm2)) +
  geom_boxplot() +
  labs(y= "Symbiont cells per cm2", x = "Colony Morph")


y = x + stat_compare_means(method = "t.test", size = 2)

ggsave("apriori.jpg", plot = y, path = '~/Desktop/')

y

# find means to explain the % difference
white <- ap_raw %>%
  filter(color == "WH") %>%
  summarise(mean = mean(Cells.cm2))
WH <- 193224.7
brown <- ap_raw %>%
  filter(color == "BR") %>%
  summarise(mean = mean(Cells.cm2))
BR <- 1255414

WH/BR
# White colonies had 15.4% of the symbionts as brown 
