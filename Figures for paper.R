####################################
# Figures for Jeremy's scaling paper
####################################

# load packages
library(dplyr)
library(ggplot2)
library(ggsci)
library(readxl)

# load data
#d_full <- read.csv("Cetacea model output NULL_EXTANT.csv")
d_full <- read.csv("Cetacea model output BOUT_EXTANT.csv")
#d_full <- read.csv("Cetacea model output NULL_ALL_ENP.csv")
  d_full$MR.exponent = as.factor(d_full$MR.exponent)
  d_full$M..kg. <- as.numeric(d_full$M..kg.)
  d_full$Prey.W..g. <- as.numeric(d_full$Prey.W..g.)
  d_full$Group <- ifelse(d_full$Family == "Balaenopteridae", "Rorqual", 
                         ifelse(d_full$Family == "Balaenidae", "Balaenid", "Odontocete"))

d_ind <- read.csv("Stats by individual.csv")

d_sp <- read.csv("Stats by species.csv")

fig_4_data <- read.csv("Figure 4 data.csv")
  fig_4_data$MR <- as.factor(fig_4_data$MR)
  fig_4_data$Calc.MR <- as.factor(fig_4_data$Calc.MR)
  
# get silhouette images for figures
  imgOo <- png::readPNG("./Orcinus-orca.png")
  rastOo <- grid::rasterGrob(imgOo, interpolate = T)
  imgBp <- png::readPNG("./Balaenoptera-physalus.png")
  rastBp <- grid::rasterGrob(imgBp, interpolate = T)
  imgBm <- png::readPNG("./Balaena-mysticetus.png")
  rastBm <- grid::rasterGrob(imgBm, interpolate = T)
  imgMn <- png::readPNG("./Megaptera-novaeangliae.png")
  rastMn <- grid::rasterGrob(imgMn, interpolate = T)
  imgfm <- png::readPNG("./fossil-mysticete.png")
  rastfm <- grid::rasterGrob(imgfm, interpolate = T)
  imgZsp <- png::readPNG("./Ziphius-sp.png")
  rastZsp <- grid::rasterGrob(imgZsp, interpolate = T)
  imgBw <- png::readPNG("./Balaenoptera-musculus.png")
  rastBw <- grid::rasterGrob(imgBw, interpolate = T)
  imgPm <- png::readPNG("./Physeter-macrocephalus.png")
  rastPm <- grid::rasterGrob(imgPm, interpolate = T)
  imgBa <- png::readPNG("./Balaenoptera-acutorostrata.png")
  rastBa <- grid::rasterGrob(imgBa, interpolate = T)
  imgPp <- png::readPNG("./Phocoena-phocoena.png")
  rastPp <- grid::rasterGrob(imgPp, interpolate = T)
  

############
# Figure 2A
############
fig_2a <- ggplot(d_ind, aes(DT_max.TADL, FE_max, color = Grouping, shape = Grouping)) + # Change color from Group to Grouping for different plot types
    geom_point() + 
    geom_smooth(aes(group = Grouping), method = lm, se = TRUE, size=1.25) +       # Change group from Group to Grouping for different plot types
    geom_smooth(data = d_ind, aes(x = DT_max.TADL, y = FE_max), color = "black",  method = lm, size=0.5, inherit.aes = FALSE) +
    geom_vline(xintercept=0, linetype="dashed", color = "gray50") +
    theme_bw() +
    annotation_custom(rastBp, ymin = 14, ymax = 20, xmin = -24, xmax = -3) +
    annotation_custom(rastPp, ymin = 2, ymax = 8, xmin = -3.5, xmax = 2.5) +
    annotation_custom(rastZsp, ymin = 30, ymax = 34, xmin = 20, xmax = 32) +
    annotation_custom(rastPm, ymin = 25, ymax = 29, xmin = 43, xmax = 60) +
    # annotate("text", x = 10, y = 20, label = expression("y=0.2204x^1.2438")) + #c("y == 0.2204x ^ 1.2438", "italic(R) ^ 2 == 0.3387")) +
    labs(x = "Max dive duration - TADL", y = "Max # feeding events per dive") 
fig_2a + scale_color_npg()
  
  
  # + annotate("text", x = 2, y = 20, label = c("y == 0.2204x ^ 1.2438", "italic(R) ^ 2 == 0.3387"))
  #annotate("text", x = 10:10, y = 20:25, label = c("y == 0.2204x ^ 1.2438", "italic(R) ^ 2 == 0.3387"))


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
  labs(x = "log[Energy (kJ)]", y = "log[Foraging Efficiency]")
  # annotation_custom(rastOo, ymin = 2, ymax = 3, xmin = 35, xmax = 45) +
  # annotation_custom(rastBp, ymin = 1.5, ymax = 1.75, xmin = 2.5, xmax = 4) +
fig_2b + scale_color_npg()


############
# Figure 2C
############
fig_2c <- ggplot(data = d_sp, aes(DT.max...TADL, logEff_max.0.75, color = Group)) +
  geom_point() +
  geom_smooth(aes(group = Group), method = lm, se = FALSE) +
  theme_bw() + guides(size=FALSE, color=FALSE) +
  labs(x = "Maximum dive time - Theoretical dive time", y = "log[Foraging Efficiency]")
fig_2c + scale_color_npg()


##########
# Figure 3
##########
fig_3 <- ggplot(data = d_full, aes(x = log(M..kg.), y = log(E_divesurf_max), color = Group, shape = MR.exponent)) +
  geom_point(aes(size = (Percent)*10, group = MR.exponent), alpha = 0.3) +  
  geom_smooth(data = filter(d_full, MR.exponent == 0.45), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.61), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.68), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.75), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
  annotation_custom(rastOo, ymin = -1, ymax = 0.5, xmin = 3.5, xmax = 5.5) +
  annotation_custom(rastBp, ymin = 5, ymax = 6.5, xmin = 7, xmax = 10) +
  annotation_custom(rastBm, ymin = -2, ymax = -1, xmin = 10, xmax = 12.5) +
  theme_bw() + guides(size=FALSE, color=FALSE) + ylim(-4,8) + xlim(2.75,12.75) +
  labs(x = "log[Mass (kg)]", y = "log[Energetic Efficiency (max)]")
fig_3 + scale_color_npg()


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
  annotation_custom(rastOo, ymin = -0.15, ymax = 0.25, xmin = 1.75, xmax = 2.75) +
  annotation_custom(rastBp, ymin = 1.5, ymax = 1.75, xmin = 2.5, xmax = 4) +
  annotation_custom(rastBm, ymin = 0, ymax = 0.5, xmin = 4.55, xmax = 5.75) +
  theme_bw() +
  labs(x = "log[Body mass (kg)]", y = "log[Energetic efficiency]")
fig_4 + scale_color_npg()

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
  annotation_custom(rastOo, ymin = -0.15, ymax = 0.25, xmin = 1.75, xmax = 2.75) +
  annotation_custom(rastBp, ymin = 1.5, ymax = 1.75, xmin = 2.5, xmax = 4) +
  annotation_custom(rastBm, ymin = 0, ymax = 0.5, xmin = 4.55, xmax = 5.75) +
  theme_bw() +
  labs(x = "log[Body mass (kg)]", y = "log[Energetic efficiency]")
fig_4 + scale_color_npg()

