####################################
# Figures for Jeremy's scaling paper
####################################

# load packages
library(dplyr)
library(ggplot2)
library(readxl)

# load data
#d_full <- read.csv("Cetacea model output NULL_EXTANT.csv")
d_full <- read.csv("Cetacea model output BOUT_EXTANT.csv")
#d_full <- read.csv("Cetacea model output NULL_ALL_ENP.csv")

d_sp <- read.csv("Stats by species.csv")



############
# Figure 2B
############
fig_2b <- ggplot(d_sp, aes(DT.max...TADL, log.value, color = Group, shape = log.of.that)) +
  geom_point() + 
  geom_smooth(data = filter(d_sp, Group == "Rorqual"), aes(group = log.of.that), method = lm, se = FALSE) +
  geom_smooth(data = filter(d_sp, Group == "Odontocete"), aes(group = log.of.that), method = lm, se = FALSE) +
  theme_bw() + guides(size=FALSE, color=FALSE) +
  labs(x = "log[Energy  (kJ)]", y = "log[Foraging Efficiency]")
fig_2b


############
# Figure 2C
############
fig_2c <- ggplot(data = d_sp, aes(DT.max...TADL, logEff_max.0.75, color = Group)) +
  geom_point() +
  geom_smooth(aes(group = Group), method = lm, se = FALSE) +
  theme_bw() + guides(size=FALSE, color=FALSE) +
  labs(x = "Maximum dive time - Theoretical dive time", y = "log[Foraging Efficiency]")
fig_2c

