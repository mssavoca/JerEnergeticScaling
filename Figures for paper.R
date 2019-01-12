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

d_ind <- read.csv("Stats by individual.csv")

d_sp <- read.csv("Stats by species.csv")

fig_4_data <- read.csv("Figure 4 data.csv")
  fig_4_data$MR <- as.factor(fig_4_data$MR)
  fig_4_data$Calc.MR <- as.factor(fig_4_data$Calc.MR)

############
# Figure 2A
############
fig_2a <- ggplot(d_ind, aes(DT_max.TADL.25, FE_max, color = Grouping, shape = Grouping)) + # Change color from Group to Grouping for different plot types
    geom_point() + 
    geom_smooth(aes(group = Grouping), method = lm, se = TRUE) +       # Change group from Group to Grouping for different plot types
    # single line
    theme_bw() + guides(size=FALSE, color=FALSE) +
    labs(x = "Max dive duration - TADL", y = "Max # feeding events per dive")
fig_2a 
  
  
  # + annotate("text", x = 2, y = 20, label = c("y == 0.2204x ^ 1.2438", "italic(R) ^ 2 == 0.3387"))
  annotate("text", x = 10:10, y = 20:25, label = c("y == 0.2204x ^ 1.2438", "italic(R) ^ 2 == 0.3387"))


("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))

y = 0.2204x1.2438 R² = 0.3387

  
  
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


##########
# Figure 3
##########
# get silhouette images for figure
imgOo <- png::readPNG("./Orcinus-orca.png")
rastOo <- grid::rasterGrob(imgOo, interpolate = T)
imgBp <- png::readPNG("./Balaenoptera-physalus.png")
rastBp <- grid::rasterGrob(imgBp, interpolate = T)
fig_3 <- ggplot(data = d_full, aes(x = log(M..kg.), y = log(E_divesurf_max), color = Group, shape = MR.exponent)) +
  geom_point(aes(size = Percent, group = MR.exponent), alpha = 0.3) +  
  geom_smooth(data = filter(d_full, MR.exponent == 0.45), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.61), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.68), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.75), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +  
  annotation_custom(rastOo, ymin = -1, ymax = 0.5, xmin = 3.5, xmax = 5.5) +
  annotation_custom(rastBp, ymin = -1, ymax = 0.5, xmin = 10.25, xmax = 11.5) +
  theme_bw() + guides(size=FALSE, color=FALSE) +
  labs(x = "Log (Mass [kg])", y = "Log (Energetic Efficiency [max])")
fig_3


###########
# Figure 4
###########
fig_4 <- ggplot(data = fig_4_data, aes(logMC, log.of.MR, color = MR, shape = Group)) +
  geom_point(data = filter(fig_4_data, Group == "Rorqual" & Status == "fossil"), aes(group=log.of.MR), size = 2) +
  geom_point(data = filter(fig_4_data, Group == "Rorqual" & Status == "hypothetical"), aes(group=log.of.MR), size = 2) +
  geom_point(data = filter(fig_4_data, Group == "Rorqual" & Status == "extant"), aes(group=log.of.MR), size = 2) +
  geom_point(data = filter(fig_4_data, Group == "Odontocete"), aes(x = logMC, y = Calc.value, group=log.of.MR), size = 2) +
  geom_point(data = filter(fig_4_data, Group == "Balaenid"), aes(group=log.of.MR), size = 2) +
  geom_line(data = filter(fig_4_data, Group == "Rorqual" & Status == "fossil"), linetype = "dashed") +
  geom_line(data = filter(fig_4_data, Group == "Rorqual" & Status == "hypothetical"), linetype = "dashed") +
  geom_line(data = filter(fig_4_data, Group == "Rorqual" & Status == "extant"), linetype = "solid") +
  geom_line(data = filter(fig_4_data, Group == "Odontocete"), aes(x = logMC, y = Calc.value)) +
  geom_line(data = filter(fig_4_data, Group == "Balaenid")) +
  theme_bw() +
  labs(x = "log [Body mass (kg)]", y = "log[Energetic efficiency]")
fig_4

