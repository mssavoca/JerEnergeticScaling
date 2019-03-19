####################################
# Figures for Jeremy's scaling paper
####################################

# load packages
library(dplyr)
library(ggplot2)
library(ggsci)
library(readxl)
library(tidyverse)

###########
# load data
###########
#d_full <- read.csv("Cetacea model output NULL_EXTANT.csv")
d_full <- read.csv("Cetacea model output BOUT_EXTANT.csv")
#d_full <- read.csv("Cetacea model output NULL_ALL_ENP.csv")
  d_full$MR.exponent = as.factor(d_full$MR.exponent)
  d_full$M..kg. <- as.numeric(d_full$M..kg.)
  d_full$Prey.W..g. <- as.numeric(d_full$Prey.W..g.)
  d_full$Group <- ifelse(d_full$Family == "Balaenopteridae", "Rorqual", 
                         ifelse(d_full$Family == "Balaenidae", "Balaenid", "Odontocete"))
  d_full$Grouping <- ifelse(d_full$Family == "Balaenopteridae", "Balaenopteridae", 
                            ifelse(d_full$Family %in% c("Delphinidae", "Phocoenidae"), "Delphinidae and Phocoenidae",
                                   ifelse(d_full$Family %in% c("Physeteridae", "Ziphiidae"), "Physeteridae and Ziphiidae",
                                          "Balaenidae")))
  
  
d_full_3.18.19 <- read.csv("Cetacea model output BOUT_EXTANT_final_3.18.19.csv")
  d_full_3.18.19$MR.exponent = as.factor(d_full_3.18.19$MR.exponent)
  d_full_3.18.19$Group <- ifelse(d_full_3.18.19$Family == "Balaenopteridae", "Rorqual", 
                       ifelse(d_full_3.18.19$Family == "Balaenidae", "Balaenid", "Odontocete"))
  d_full_3.18.19$Grouping <- ifelse(d_full_3.18.19$Family == "Balaenopteridae", "Balaenopteridae", 
                          ifelse(d_full_3.18.19$Family %in% c("Delphinidae", "Phocoenidae"), "Delphinidae and Phocoenidae",
                                 ifelse(d_full_3.18.19$Family %in% c("Physeteridae", "Ziphiidae"), "Physeteridae and Ziphiidae",
                                        "Balaenidae")))
  
d_full_final <- read.csv("Cetacea model output BOUT_EXTANT_w_hypotheticals.csv")    # EARLY 2019 file, d_full_3.18.19 is newer
  d_full_final <- subset(d_full_final, select = c(Family:MR.exponent))
  d_full_final$MR.exponent = as.factor(d_full_final$MR.exponent)
  d_full_final$Percent = as.numeric(d_full_final$Percent)
  d_full_final$M..kg. <- as.numeric(d_full_final$M..kg.)
  d_full_final$Prey.W..g. <- as.numeric(d_full_final$Prey.W..g.)
  d_full_final$TL..m. <- as.numeric(d_full_final$TL..m.)
  d_full_final$Energy..kJ.g. <- as.numeric(d_full_final$Energy..kJ.g.)
  d_full_final$Energy..kJ. <- as.numeric(d_full_final$Energy..kJ.)
  d_full_final$E_divesurf_med <- as.numeric(d_full_final$E_divesurf_med)
  d_full_final$E_divesurf_max <- as.numeric(d_full_final$E_divesurf_max)
  d_full_final$Group = as.factor(d_full_final$Group <- ifelse(d_full_final$Family == "Balaenopteridae", "Rorqual", 
                         ifelse(d_full_final$Family == "Balaenidae", "Balaenid", 
                                ifelse(d_full_final$Family == "Fossil", "Fossil", 
                                       ifelse(d_full_final$Family == "Hypothetical", "Hypothetical", "Odontocete")))))
  
  

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
fig_2a <- ggplot(d_ind, aes(DT_max.TADL, FE_max, color = Group, shape = Species)) + # Change shape from Group to Grouping for different plot types
    geom_point(aes(group = Group, size = MXD..m.)) + 
    geom_smooth(aes(group = Group), method = lm, se = TRUE, size=1.25) +       # Change group from Group to Grouping for different plot types
  #  geom_smooth(data = d_ind, aes(x = DT_max.TADL, y = FE_max), color = "black",  method = lm, size=0.5, inherit.aes = FALSE) +
    geom_vline(xintercept=0, linetype="dashed", color = "gray50") +
    scale_shape_manual(name = "Species",                      
                       labels = c("Balaenoptera bonaerensis","Balaenoptera musculus","Balaenoptera physalus","Berardius bairdii",
                                  "Globicephala macrorhynchus", "Globicephala melas","Grampus griseus", "Megaptera novaeangliae",
                                  "Mesoplodon densirostris","Orcinus orca","Phocoena phocoena", "Physeter macrocephalus", "Ziphius cavirostris"),                     
                       values = c(0,1,2,3,4,5,6,7,8,9,10,12,13,14)) +
    theme_bw() + 
    theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
    # annotation_custom(rastfm, ymin = -50, ymax = -34, xmin = 15, xmax = 32) +
    # annotation_custom(rastBp, ymin = 16.5, ymax = 24.5, xmin = -24, xmax = -2) +
    # annotation_custom(rastPp, ymin = 1, ymax = 7, xmin = -3.5, xmax = 2.5) +
    # annotation_custom(rastZsp, ymin = 25, ymax = 29, xmin = 43, xmax = 55) +
    # annotation_custom(rastPm, ymin = 30, ymax = 35.5, xmin = 12, xmax = 31) +
    # annotate("text", x = 10, y = 20, label = expression("y=0.2204x^1.2438")) + #c("y == 0.2204x ^ 1.2438", "italic(R) ^ 2 == 0.3387")) +
    labs(x = "Maximum dive time - theoretical dive time (min)", y = "Max # feeding events per dive", size = "Max. depth (m)") + 
    scale_x_continuous(breaks=seq(-25,50,25))
fig_2a +scale_color_manual(values = c("#4DBBD5FF","#E64B35FF")) + theme(legend.position="none")

ggsave("fig2a_withoutlegend.eps", width = 13, height = 8, units = "in")
#dev.copy2pdf(file="fig2a_withlegend.pdf", width=13, height=8)



fig_2_final <- ggplot(d_ind, aes(DT_med..min., FE_med, color = Group, shape = Species)) + # Change shape from Group to Grouping for different plot types
  geom_point(aes(group = Group, size = MXD..m.)) + 
  geom_smooth(aes(group = Group), method = lm, se = TRUE, size=1.25) +       # Change group from Group to Grouping for different plot types
  #  geom_smooth(data = d_ind, aes(x = DT_max.TADL, y = FE_max), color = "black",  method = lm, size=0.5, inherit.aes = FALSE) +
  scale_shape_manual(name = "Species",                      
                     labels = c("Balaenoptera bonaerensis","Balaenoptera musculus","Balaenoptera physalus","Berardius bairdii",
                                "Globicephala macrorhynchus", "Globicephala melas","Grampus griseus", "Megaptera novaeangliae",
                                "Mesoplodon densirostris","Orcinus orca","Phocoena phocoena", "Physeter macrocephalus", "Ziphius cavirostris"),                     
                     values = c(0,1,2,3,4,5,6,7,8,9,10,12,13,14)) +
  theme_bw() + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  # annotation_custom(rastfm, ymin = -50, ymax = -34, xmin = 15, xmax = 32) +
  # annotation_custom(rastBp, ymin = 16.5, ymax = 24.5, xmin = -24, xmax = -2) +
  # annotation_custom(rastPp, ymin = 1, ymax = 7, xmin = -3.5, xmax = 2.5) +
  # annotation_custom(rastZsp, ymin = 25, ymax = 29, xmin = 43, xmax = 55) +
  # annotation_custom(rastPm, ymin = 30, ymax = 35.5, xmin = 12, xmax = 31) +
  # annotate("text", x = 10, y = 20, label = expression("y=0.2204x^1.2438")) + #c("y == 0.2204x ^ 1.2438", "italic(R) ^ 2 == 0.3387")) +
  labs(x = "Median dive duration", y = "Median # feeding events per dive", size = "Max. depth (m)") + 
  scale_x_continuous(breaks=seq(-25,50,25))
fig_2_final +scale_color_manual(values = c("#4DBBD5FF","#E64B35FF"))

ggsave("fig_2_final.tiff", width = 13, height = 8, units = "in")
dev.copy2pdf(file="fig_2_final.pdf", width=13, height=8)



m_fig_2_final = lm(data =filter(d_ind, Group == "Rorquals"), FE_med~DT_med..min.)
summary(m_fig_2_final)

  
############
# Figure 2B
############
#COLORS DARK/LIGHT RED, DARK/LIGHT BLUE

fig_2b_final <- ggplot(d_sp, aes(DT.max...TADL, log.value, color = log.of.that, shape = Species)) +
    geom_point(data = filter(d_sp, log.of.that == "in" & Group == "Rorqual"), colour = "#E64B35FF", size = 3) +
    geom_point(data = filter(d_sp, log.of.that == "out" & Group == "Rorqual"), colour = "darkred", size = 3) +
    geom_point(data = filter(d_sp, log.of.that == "in" & Group == "Odontocete"), color = "#4DBBD5FF", size = 3) +
    geom_point(data = filter(d_sp, log.of.that == "out" & Group == "Odontocete"), color = "darkblue", size = 3) +
    geom_smooth(data = filter(d_sp, Group == "Rorqual" & log.of.that == "in"), aes(group = log.of.that), color = "#E64B35FF", method = lm, se = FALSE) +
    geom_smooth(data = filter(d_sp, Group == "Rorqual" & log.of.that == "out"), aes(group = log.of.that), color = "firebrick", method = lm, se = FALSE) +
    geom_smooth(data = filter(d_sp, Group == "Odontocete" & log.of.that == "in"), aes(group = log.of.that), color = "#4DBBD5FF", method = lm, se = FALSE) +
    geom_smooth(data = filter(d_sp, Group == "Odontocete" & log.of.that == "out"), aes(group = log.of.that), color = "dodgerblue4", method = lm, se = FALSE) +
  geom_vline(xintercept=0, linetype="dashed", color = "gray50") +
  scale_shape_manual(name = "Species",                      
                     labels = c("Balaenoptera bonaerensis","Balaenoptera musculus","Balaenoptera physalus","Berardius bairdii",
                                "Globicephala macrorhynchus", "Globicephala melas","Grampus griseus", "Megaptera novaeangliae",
                                "Mesoplodon densirostris","Orcinus orca","Phocoena phocoena", "Physeter macrocephalus", "Ziphius cavirostris"),                     
                     values = c(0,1,2,3,4,5,6,7,8,9,10,12,13,14)) +
  guides(size=FALSE, color=FALSE) +  theme_bw() + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "Maximum dive time - theoretical dive time (min)", y = "log[Energy(kJ)]") +
  # annotation_custom(rastOo, ymin = 2.5, ymax = 3.5, xmin = 30, xmax = 40) +
  # annotation_custom(rastBp, ymin = 5.5, ymax = 6, xmin = -8, xmax = 12) +
  scale_x_continuous(breaks=seq(-25,50,25))
fig_2b_final + scale_color_npg()  + theme(legend.position="none")

# Save plots
ggsave("fig2b_final.eps", width = 13, height = 8, units = "in")
#Save pdf of plot
#dev.copy2pdf(file="fig2b_final.pdf", width=13, height=8)



############
# Figure 2C
############
fig_2c <- ggplot(data = d_sp, aes(DT.max...TADL, logEff_max.0.75, color = Group, shape = Species)) +
  geom_point(data = filter(d_sp, Group == "Rorqual"), aes(group = log.of.that), color = "#E64B35FF", size = 3) +
  geom_point(data = filter(d_sp, Group == "Odontocete"), aes(group = log.of.that), color = "#4DBBD5FF", size = 3) +
  geom_smooth(data = filter(d_sp, Group == "Rorqual"), aes(group = Group), color = "#E64B35FF", method = lm, se = FALSE) +
  geom_smooth(data = filter(d_sp, Group == "Odontocete"), aes(group = Group), color = "#4DBBD5FF", method = lm, se = FALSE) +
  geom_vline(xintercept=0, linetype="dashed", color = "gray50") +
  scale_shape_manual(name = "Species",                      
                     labels = c("Balaenoptera bonaerensis","Balaenoptera musculus","Balaenoptera physalus","Berardius bairdii",
                                "Globicephala macrorhynchus", "Globicephala melas","Grampus griseus", "Megaptera novaeangliae",
                                "Mesoplodon densirostris","Orcinus orca","Phocoena phocoena", "Physeter macrocephalus", "Ziphius cavirostris"),                     
                     values = c(0,1,2,3,4,5,6,7,8,9,10,12,13,14)) +
  theme_bw() + guides(size=FALSE, color=FALSE) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "Maximum dive time - theoretical dive time (min)", y = "log[Foraging Efficiency + 1]") +
  # annotation_custom(rastOo, ymin = 0.25, ymax = 0.75, xmin = 30, xmax = 40) +
  # annotation_custom(rastBp, ymin = 1.25, ymax = 1.75, xmin = -8, xmax = 12) +
  scale_x_continuous(breaks=seq(-25,50,25))
fig_2c + scale_color_npg()  + theme(legend.position="none")

# Save plots
ggsave("fig2c.eps", width = 13, height = 8, units = "in")

#dev.copy2pdf(file="fig2c.pdf", width=13, height=8) 

#plot(lm(data = filter(d_sp, Group=="Rorqual"), logEff_max.0.75~DT.max...TADL))



##########
# Figure 3
##########

fig_3a <- ggplot(data = filter(d_full_3.18.19, Family != "Balaenidae"), aes(x = log10(M..kg.), y=log10(Energy..kJ.), color = Group)) +
  geom_point(aes(size = (Percent)*10), alpha = 0.5) +  
  geom_smooth(data = filter(d_full_final, Group == "Odontocete"), aes(weight = Percent), method = lm) +
  geom_smooth(data = filter(d_full_final, Group == "Rorqual"), aes(weight = Percent), method = lm) +
  geom_abline(intercept = 0, slope = 1, linetype ="dashed", size = 1.15) + 
  # annotation_custom(rastOo, ymin = -50, ymax = -45, xmin = -24, xmax = -2) + #Otherwise the ggsave has transparent first silhouette
  # annotation_custom(rastOo, xmin = 2.5, xmax = 3.25,  ymin = 1, ymax = 1.75) +
  # annotation_custom(rastBp, xmin = 4, xmax = 6, ymin = 6.15, ymax = 7.6) +
  # annotation_custom(rastPp, xmin = 1.35, xmax = 1.75, ymin = 3.5, ymax = 4) +
  # annotation_custom(rastZsp, xmin = 3.5, xmax = 4.5, ymin = 0.75, ymax = 1.5) +
  # annotation_custom(rastPm, xmin = 4.45, xmax = 5.95, ymin = 2, ymax = 3.75) +
  # annotation_custom(rastBa, xmin = 3.35, xmax = 4.25, ymin = 4.5, ymax = 5.25) +
  theme_bw() + guides(size=FALSE, color=FALSE) + 
  ylim(1,7) + xlim(1,6) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Prey Energy (kJ)]")
cols <- c("Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF", "Balaenid" = "darkgreen", "Hypothetical" = "orange", "Fossil" = "black", "Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF")
fig_3a + scale_color_manual(values = cols)

# Save plots
ggsave("fig3a.tiff", width = 14, height = 8, units = "in")

dev.copy2pdf(file="fig3a.pdf", width=14, height=8)

m_fig_3a = lm(data =filter(d_full_3.18.19, Group == "Rorqual"), log10(Energy..kJ.)~log10(M..kg.), weights = Percent)
summary(m_fig_3a)


#################################
# Density plot of diet, Figure 3B 
#################################

d_full_3.18.19$Grouping <- as.factor(fct_relevel(d_full_3.18.19$Grouping, "Delphinidae and Phocoenidae", "Physeteridae and Ziphiidae", "Balaenopteridae"))
fig3b <- d_full_3.18.19 %>% filter(Grouping != "Balaenidae") %>% 
  ggplot(aes(x = Energy..kJ., fill = paste(Genus, Species))) + 
  geom_density(aes(weight = Percent), alpha = 0.4) + 
  scale_x_log10(labels = scales::comma) + 
  # scale_x_continuous(labels = scales::comma) +
  facet_wrap(~ Grouping) + 
  xlab("log[Prey energy (kJ)]") + ylab("frequency") +
  theme_classic() + labs(fill="Species") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(hjust = 0.5, size = 16), 
        strip.text.x = element_text(size = 14)) 
fig3b

# Save plots
ggsave("fig3b.tiff", width = 14, height = 8, units = "in")
#dev.copy2pdf(file="fig3b.pdf", width=14, height=8)


##########
# Figure 4
##########

d_other <- filter(d_full_final, Group == "Balaenid")

fig_4 <- ggplot(data = d_full_3.18.19, aes(x = log10(M..kg.), y = log10(E_divesurf_med), color = Group)) +
  geom_point(aes(size = (Percent)*10, shape = MR.exponent), alpha = 0.5) + 
  geom_point(data = d_other, aes(size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) +
  geom_smooth(data = filter(d_full_3.18.19, Group == "Odontocete"), aes(weight = Percent, group = MR.exponent, linetype = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full_3.18.19, Group == "Rorqual"), aes(weight = Percent, group = MR.exponent, linetype = MR.exponent), method = lm) +
  scale_linetype_manual(values=c("solid", "dashed", "dotdash", "dotted")) +
  # annotation_custom(rastOo, ymin = -50, ymax = -45, xmin = -24, xmax = -2) + #Otherwise the ggsave has transparent first silhouette
  # annotation_custom(rastOo, xmin = 2.65, xmax = 3.15,  ymin = -1.5, ymax = -0.6) +
  # annotation_custom(rastBp, xmin = 3.9, xmax = 5.25, ymin = 2.9, ymax = 4) +
  # annotation_custom(rastPp, xmin = 2.4, xmax = 2.65, ymin = 1.5, ymax = 2) +
  # annotation_custom(rastZsp, xmin = 3.25, xmax = 3.9, ymin = -2, ymax = -0.75) +
  # annotation_custom(rastPm, xmin = 4.25, xmax = 5.25, ymin = -2, ymax = -0.5) +
  # annotation_custom(rastBa, xmin = 3.6, xmax = 4.1, ymin = 2.05, ymax = 2.5) +
  # annotation_custom(rastBm, xmin = 4.6, xmax = 5.7, ymin = -0.75, ymax = 0.45) +
  # annotation_custom(rastfm, xmin = 3.15, xmax = 3.6, ymin = 1.5, ymax = 2.1) +
  # annotation_custom(rastBw,  xmin = 5, xmax = 6.75, ymin = 1.3, ymax = 2.5) +
  theme_bw() + guides(size=FALSE, color=FALSE) + 
  ylim(-2,4) + xlim(1,6) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Energetic Efficiency]")
cols <- c("Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF", "Balaenid" = "darkgreen", "Hypothetical" = "orange", "Fossil" = "black", "Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF")
fig_4 + scale_color_manual(values = cols) + theme(legend.position="none")

# Save plots
ggsave("fig4.tiff", width = 14, height = 8, units = "in")
dev.copy2pdf(file="fig4.pdf", width=14, height=8)


####################
# Extended Figure 3b
####################

fig_3b_extended <- ggplot(data = filter(d_full_final, !Group %in% c("Odontocete", "Balaenid")), 
                          aes(x = log10(M..kg.), y = log10(E_divesurf_max), color = Group)) +
  geom_point(aes(size = (Percent)*10, shape = MR.exponent), alpha = 0.5) + 
  geom_smooth(data = filter(d_full_final, Group == "Rorqual"), aes(group = MR.exponent, linetype = MR.exponent), method = "lm", se = FALSE) +
  geom_line(data = filter(d_full_final, Group == "Fossil"), aes(group = MR.exponent, linetype = MR.exponent)) +
  geom_line(data = filter(d_full_final, Group == "Hypothetical"), aes(group = MR.exponent, linetype = MR.exponent)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotdash", "dotted")) +
  theme_bw() + guides(size=FALSE, color=FALSE) + 
  ylim(-2,4) + xlim(1,6) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Energetic Efficiency (max)]")
cols <- c("Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF", "Balaenid" = "darkgreen", "Hypothetical" = "orange", "Fossil" = "black", "Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF")
fig_3b_extended + scale_color_manual(values = cols) + theme(legend.position="none")

ggsave("fig3b_extended.tiff", width = 14, height = 8, units = "in")
#dev.copy2pdf(file="fig3b.pdf", width=14, height=8)





###########################
# older figures drafts here
###########################

# 
# fig_2b <- ggplot(d_sp, aes(DT.max...TADL, log.value, color = Group, shape = Species)) +
#   geom_point(data = filter(d_sp, Group == "Rorqual"), aes(group = log.of.that), color = "#E64B35FF", size = 3) +
#   geom_point(data = filter(d_sp, Group == "Odontocete"), aes(group = log.of.that), color = "#4DBBD5FF", size = 3) +
#   geom_smooth(data = filter(d_sp, Group == "Rorqual"), aes(group = log.of.that), color = "#E64B35FF", method = lm, se = FALSE) +
#   geom_smooth(data = filter(d_sp, Group == "Odontocete"), aes(group = log.of.that), color = "#4DBBD5FF", method = lm, se = FALSE) +
#   geom_vline(xintercept=0, linetype="dashed", color = "gray50") +
#   scale_shape_manual(name = "Species",                      
#                      labels = c("Balaenoptera bonaerensis","Balaenoptera musculus","Balaenoptera physalus","Berardius bairdii",
#                                 "Globicephala macrorhynchus", "Globicephala melas","Grampus griseus", "Megaptera novaeangliae",
#                                 "Mesoplodon densirostris","Orcinus orca","Phocoena phocoena", "Physeter macrocephalus", "Ziphius cavirostris"),                     
#                      values = c(0,1,2,3,4,5,6,7,8,9,10,12,13,14)) +
#   guides(size=FALSE, color=FALSE) +  theme_bw() + 
#   theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
#   labs(x = "Maximum dive time - theoretical dive time (min)", y = "log[energy(kJ)]") +
#   annotation_custom(rastOo, ymin = 2.5, ymax = 3.5, xmin = 30, xmax = 40) +
#   annotation_custom(rastBp, ymin = 5.5, ymax = 6, xmin = -8, xmax = 12) +
#   scale_x_continuous(breaks=seq(-25,50,25))
# fig_2b + scale_color_npg()




# fig_4 <- ggplot(data = fig_4_data, aes(logMC, log.of.MR, color = MR, shape = Group)) +
#   geom_point(data = filter(fig_4_data, Group == "Rorqual" & Status == "fossil"), aes(group=log.of.MR), size = 3.25) +
#   geom_point(data = filter(fig_4_data, Group == "Rorqual" & Status == "hypothetical"), aes(group=log.of.MR), size = 3.25) +
#   geom_point(data = filter(fig_4_data, Group == "Rorqual" & Status == "extant"), aes(group=log.of.MR), size = 3.25) +
#   geom_point(data = filter(fig_4_data, Group == "Odontocete"), aes(x = logMC, y = Calc.value, group=log.of.MR), size = 3) +
#   geom_point(data = filter(fig_4_data, Group == "Balaenid"), aes(group=log.of.MR), size = 3) +
#   geom_line(data = filter(fig_4_data, Group == "Rorqual" & Status == "fossil"), linetype = "dotted", size = 1.5) +
#   geom_line(data = filter(fig_4_data, Group == "Rorqual" & Status == "hypothetical"), linetype = "dotted", size = 1.5) +
#   geom_smooth(data = filter(fig_4_data, Group == "Rorqual" & Status == "extant"), linetype = "solid", se = FALSE, method = lm, size = 1.5) +
#   geom_line(data = filter(fig_4_data, Group == "Odontocete"), aes(x = logMC, y = Calc.value)) +
#   geom_line(data = filter(fig_4_data, Group == "Balaenid")) +
#   annotation_custom(rastPp, ymin = 1, ymax = 1.25, xmin = 1.25, xmax = 1.65) +
#   annotation_custom(rastOo, ymin = -0.15, ymax = 0.25, xmin = 2, xmax = 2.75) +
#   annotation_custom(rastBp, ymin = 2, ymax = 2.25, xmin = 3.5, xmax = 5) +
#   annotation_custom(rastBm, ymin = -0.05, ymax = 0.45, xmin = 4.55, xmax = 5.75) +
#   annotation_custom(rastfm, ymin = 1.25, ymax = 1.75, xmin = 2.75, xmax = 3.5) +
#   annotation_custom(rastPm, ymin = -0.6, ymax = -0.05, xmin = 3.15, xmax = 4.25) +
#   annotation_custom(rastBw, ymin = 0.95, ymax = 1.45, xmin = 5.5, xmax = 7.25) +
#   theme_bw() + ylim(-0.75,2.5) + xlim(1.15,7) +
#   theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
#   labs(x = "log[Body mass (kg)]", y = "log[Energetic efficiency]")
# fig_4 + scale_color_npg()

# # sweet tidy code from Max
# d_full_final %>% group_by(Genus, Species) %>% summarize(wgtMean = weighted.mean(Prey.W..g., Percent))
# d_full_final %>% filter(MR.exponent == "0.75") %>%  group_by(Genus, Species) %>% summarize(wgtMean = log10(weighted.mean(E_divesurf_max)))

# fig_3 <- ggplot(data = d_full, aes(x = log(M..kg.), y = log(E_divesurf_max), color = Group, shape = MR.exponent)) +
#   geom_point(aes(size = (Percent)*10, group = MR.exponent), alpha = 0.5) +  
#   geom_smooth(data = filter(d_full, MR.exponent == 0.45), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
#   geom_smooth(data = filter(d_full, MR.exponent == 0.61), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
#   geom_smooth(data = filter(d_full, MR.exponent == 0.68), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
#   geom_smooth(data = filter(d_full, MR.exponent == 0.75), aes(weight = Percent, group = Group, color = MR.exponent), method = lm) +
#   annotation_custom(rastOo, ymin = -2, ymax = -1, xmin = 5.25, xmax = 6.5) +
#   annotation_custom(rastBp, ymin = 6.5, ymax = 8.5, xmin = 9.5, xmax = 12.5) +
#   annotation_custom(rastBm, ymin = -0.5, ymax = 0.5, xmin = 10.55, xmax = 12.75) +
#   annotation_custom(rastPp, ymin = 6.5, ymax = 8, xmin = 3.15, xmax = 3.9) +
#   annotation_custom(rastZsp, ymin = 3.5, ymax = 5, xmin = 7.25, xmax = 8.5) +
#   annotation_custom(rastPm, ymin = -3.5, ymax = -2.25, xmin = 9.5, xmax = 11.5) +
#   annotation_custom(rastBa, ymin = 4.5, ymax = 5.5, xmin = 8.5, xmax = 9.75) +
#   theme_bw() + guides(size=FALSE, color=FALSE) + ylim(-4,8) + xlim(2.75,12.5) +
#   theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
#   labs(x = "log[Mass (kg)]", y = "log[Energetic Efficiency (max)]")
# fig_3 + scale_color_npg()



