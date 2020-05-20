####################################
# Figures for Jeremy's scaling paper
####################################

# load packages
library(dplyr)
library(ggplot2)
library(ggsci)
library(readxl)
library(tidyverse)
library(ggpubr)
library(lme4)
library(lmerTest)
library(MCMCglmm)
library(viridis)



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
  
  
d_full_9.5.19 <- read.csv("Cetacea model output v15.2 to savoca.csv")
  d_full_9.5.19$MR.exponent = as.factor(d_full_9.5.19$MR.exponent)
  d_full_9.5.19$Group <- ifelse(d_full_9.5.19$Family == "Balaenopteridae", "Rorqual", 
                       ifelse(d_full_9.5.19$Family == "Balaenidae", "Balaenid", "Odontocete"))
  d_full_9.5.19$Grouping <- ifelse(d_full_9.5.19$Family == "Balaenopteridae", "Balaenopteridae", 
                          ifelse(d_full_9.5.19$Family %in% c("Delphinidae", "Phocoenidae"), "Delphinidae and Phocoenidae",
                                 ifelse(d_full_9.5.19$Family %in% c("Physeteridae", "Ziphiidae"), "Physeteridae and Ziphiidae",
                                        "Balaenidae")))
  
d_full_final <- read.csv("Cetacea model output v15.2 to savoca.csv")   # Jeremy game me this file during the revisions stage on 9.5.19
  d_full_final <- subset(d_full_final, select = c(Family:MR.exponent))
  d_full_final$MR.exponent = as.factor(d_full_final$MR.exponent)
  d_full_final$Percent = as.numeric(d_full_final$Percent)
 # d_full_final$M..kg. <- as.numeric(d_full_final$M..kg.)
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
  
# # get silhouette images for figures
#   imgOo <- png::readPNG("./Orcinus-orca.png")
#   rastOo <- grid::rasterGrob(imgOo, interpolate = T)
#   imgBp <- png::readPNG("./Balaenoptera-physalus.png")
#   rastBp <- grid::rasterGrob(imgBp, interpolate = T)
#   imgBm <- png::readPNG("./Balaena-mysticetus.png")
#   rastBm <- grid::rasterGrob(imgBm, interpolate = T)
#   imgMn <- png::readPNG("./Megaptera-novaeangliae.png")
#   rastMn <- grid::rasterGrob(imgMn, interpolate = T)
#   imgfm <- png::readPNG("./fossil-mysticete.png")
#   rastfm <- grid::rasterGrob(imgfm, interpolate = T)
#   imgZsp <- png::readPNG("./Ziphius-sp.png")
#   rastZsp <- grid::rasterGrob(imgZsp, interpolate = T)
#   imgBw <- png::readPNG("./Balaenoptera-musculus.png")
#   rastBw <- grid::rasterGrob(imgBw, interpolate = T)
#   imgPm <- png::readPNG("./Physeter-macrocephalus.png")
#   rastPm <- grid::rasterGrob(imgPm, interpolate = T)
#   imgBa <- png::readPNG("./Balaenoptera-acutorostrata.png")
#   rastBa <- grid::rasterGrob(imgBa, interpolate = T)
#   imgPp <- png::readPNG("./Phocoena-phocoena.png")
#   rastPp <- grid::rasterGrob(imgPp, interpolate = T)
#   




##############
# Fig 2 Final
##############

#https://stackoverflow.com/questions/12410908/combine-legends-for-color-and-shape-into-a-single-legend
  
  
fig_2_final <- d_ind %>% 
  mutate(Species = factor(str_replace(Species, "_", " "))) %>%
  mutate(Species = fct_relevel(Species, "Balaenoptera bonaerensis", "Megaptera novaeangliae", "Balaenoptera physalus", "Balaenoptera musculus", 
                               "Phocoena phocoena", "Grampus griseus", "Globicephala macrorhynchus", "Globicephala melas", "Orcinus orca",       
                               "Ziphius cavirostris", "Mesoplodon densirostris", "Berardius bairdii", 
                               "Physeter macrocephalus")) %>% 
  ggplot(aes(DT_med..min., 
             FE_med, 
             #group = Group,
             shape = Species)) + # Change shape from Group to Grouping for different plot types
  geom_point(aes(size = MXD..m., color = Group), alpha = 0.75) + 
  # geom_smooth(method = lm, 
  #             se = TRUE, 
  #             size = 1.25) +       # Change group from Group to Grouping for different plot types
  scale_shape_manual(name = "Species", labels = c("Balaenoptera bonaerensis", "Megaptera novaeangliae", "Balaenoptera physalus", "Balaenoptera musculus", 
                                                  "Phocoena phocoena", "Grampus griseus", "Globicephala macrorhynchus", "Globicephala melas", "Orcinus orca",       
                                                  "Ziphius cavirostris", "Mesoplodon densirostris", "Berardius bairdii", 
                                                  "Physeter macrocephalus"), values = c(16,16,16,16,16,17,17,17,17,18,18,18,15)) +
  scale_radius(range = c(0.5, 8)) +
  scale_x_continuous(breaks=seq(-25,50,25)) +
  scale_color_manual(name = "Species",
                     labels = c("Balaenoptera bonaerensis", "Megaptera novaeangliae", "Balaenoptera physalus", "Balaenoptera musculus", 
                                "Phocoena phocoena", "Grampus griseus", "Globicephala macrorhynchus", "Globicephala melas", "Orcinus orca",       
                                "Ziphius cavirostris", "Mesoplodon densirostris", "Berardius bairdii", 
                                "Physeter macrocephalus"),
                     values = c("#4DBBD5FF", "#E64B35FF")) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16, face="bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  labs(x = "Median dive duration", 
       y = "Median # feeding events per dive", 
       size = "Max. depth (m)",
       color = "") +
  guides(shape = guide_legend(override.aes = list(size = 3),
                              label.theme = element_text(face = "italic")))
fig_2_final 

ggsave("fig_2_final.tiff", width = 13, height = 8, units = "in")
ggsave("fig_2_final.eps", width = 13, height = 8, units = "in")
ggsave("fig_2_final.jpg", width = 13, height = 8, units = "in")
dev.copy2pdf(file="fig_2_final.pdf", width=13, height=8)



m_fig_2_final = lm(data =filter(d_ind, Group == "Rorquals"), FE_med~DT_med..min.)
summary(m_fig_2_final)

  


##########
# Figure 3
##########

data_from_DW <- readRDS("Figure3_smydata.rds")
data_from_DW_bootstrap <- readRDS("Figure3_bootstrap_b.rds")
data_from_DW_OLS_param <- readRDS("Figure3_m_ols_param.rds")
data_from_DW_preds <- readRDS("Figure3_bootstrap_preds.rds")

DW_model <- as.data.frame(t(data_from_DW_preds)) %>% 
  cbind(select(data_from_DW, x_mean, y_mean, species, Group)) %>% 
  mutate(slope = if_else(Group == "Rorqual", 
                         data_from_DW_OLS_param$slope.rorq,
                         data_from_DW_OLS_param$slope.od),
         intercept = if_else(Group == "Rorqual", 
                             data_from_DW_OLS_param$intercept.rorq,
                             data_from_DW_OLS_param$intercept.od),
         y_model = x_mean * slope + intercept) %>% 
  rename(ci_lower = `2.5%`,
         ci_upper = `97.5%`)
write_csv(DW_model, "DW_model_fig3.csv")


fig_3a <- ggplot() +
  geom_point(data = filter(d_full_9.5.19, Family != "Balaenidae", MR.exponent == "0.45"), 
             aes(x = log10(M..kg.), y=log10(Energy..kJ.), color = Group, size = Percent)) +  
  # geom_smooth(aes(weight = Percent, color = Group),
  #             d_full_final, 
  #             method = lm,
  #             se = TRUE) +
  #geom_smooth(data = filter(d_full_final, Group == "Rorqual"), aes(weight = Percent), method = lm) +
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") +
  # geom_segment(data = data_from_DW_OLS_param, aes(x = 1.5, 
  #                                                xend = 4.25, 
  #                                                y = 1.5*slope.od + intercept.od, 
  #                                                yend = 4.25*slope.od + intercept.od),
  #              color = "#4DBBD5FF") +
  geom_line(aes(x_mean, y_model, color = Group),
            DW_model, size = 1.1,
            inherit.aes = FALSE) +
  geom_ribbon(aes(x_mean, ymin = ci_lower, ymax = ci_upper, group = Group),
              DW_model,
              alpha = 0.25,
              inherit.aes = FALSE) +
  # annotation_custom(rastOo, ymin = -50, ymax = -45, xmin = -24, xmax = -2) + #Otherwise the ggsave has transparent first silhouette
  # annotation_custom(rastOo, xmin = 2.5, xmax = 3.25,  ymin = 1, ymax = 1.75) +
  # annotation_custom(rastBp, xmin = 4, xmax = 6, ymin = 6.15, ymax = 7.6) +
  # annotation_custom(rastPp, xmin = 1.35, xmax = 1.75, ymin = 3.5, ymax = 4) +
  # annotation_custom(rastZsp, xmin = 3.5, xmax = 4.5, ymin = 0.75, ymax = 1.5) +
  # annotation_custom(rastPm, xmin = 4.45, xmax = 5.95, ymin = 2, ymax = 3.75) +
  # annotation_custom(rastBa, xmin = 3.35, xmax = 4.25, ymin = 4.5, ymax = 5.25) +
  theme_bw() + 
  guides(size = FALSE, color = FALSE) + 
  ylim(1,7) + 
  xlim(1,6) +
  scale_radius(range = c(0.5, 8)) +
  scale_color_manual(values = c("Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF")) +
  theme(axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16, face = "bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Prey Energy (kJ)]")
fig_3a

# Save plots
ggsave("fig3a_options/fig3a_points.pdf", width = 14, height = 8, units = "in")

ggsave("fig3.tiff", width = 14, height = 8, units = "in")
ggsave("fig3.jpg", width = 14, height = 8, units = "in")
dev.copy2pdf(file="fig3a.pdf", width=14, height=8)

f########################################################
# Figure 3 modeling for Jeremy's scaling paper revisions----
########################################################

# load packages and data
library(dplyr)
library(readxl)
library(tidyverse)
library(ggpubr)
library(lme4)
library(lmerTest)
library(MCMCglmm)

d_full_9.5.19 <- read.csv("Cetacea model output v15.2 to savoca.csv")
d_full_9.5.19$MR.exponent = as.factor(d_full_9.5.19$MR.exponent)
d_full_9.5.19$Group <- ifelse(d_full_9.5.19$Family == "Balaenopteridae", "Rorqual", 
                              ifelse(d_full_9.5.19$Family == "Balaenidae", "Balaenid", "Odontocete"))
d_full_9.5.19$Grouping <- ifelse(d_full_9.5.19$Family == "Balaenopteridae", "Balaenopteridae", 
                                 ifelse(d_full_9.5.19$Family %in% c("Delphinidae", "Phocoenidae"), "Delphinidae and Phocoenidae",
                                        ifelse(d_full_9.5.19$Family %in% c("Physeteridae", "Ziphiidae"), "Physeteridae and Ziphiidae",
                                               "Balaenidae")))

# basic glmm in lme4
# MCMCglmm so that we can get a distribution of parameter estimates and thus a confidence interval of the slope
MCMCglmm_fig_3 <- MCMCglmm(log10(Energy..kJ.) ~  log10(M..kg.)*Order,
                           random = ~ Species,
                           data = filter(d_full_9.5.19, Family != "Balaenidae" & MR.exponent == "0.45"), 
                           family = "gaussian",
                           nitt = 10000, thin = 1, burnin = 1000,
                           pr = TRUE, # To save the posterior mode of for each level or category in your random effect(s) we use pr = TRUE, which saves them in the $Sol part of the model output.
                           verbose = TRUE)
summary(MCMCglmm_fig_3)

model_param_values <- as.data.frame(MCMCglmm_fig_3$Sol) %>% #turning the table of parameter estimates into a dataframe
  mutate(Rorqual_slope = `log10(M..kg.)` + `log10(M..kg.):OrderMysticeti`)


# plot parameter distributions
slope_distributions <- ggplot(model_param_values) +
  geom_density(aes(`log10(M..kg.)`), color = "#4DBBD5FF") +
  geom_density(aes(Rorqual_slope), color = "#E64B35FF") +
  labs(x = "slope parameter distribution") +
  geom_vline(xintercept = 1,  linetype = "dashed") +
  theme_classic()
slope_distributions 

dev.copy2pdf(file="slope_distributions.pdf", width=4, height=5)


# means and 95% intervals on the two slope parameter distributions
#for Odontocetes
mean(model_param_values$`log10(M..kg.)`)
quantile(model_param_values$`log10(M..kg.)`, probs = c(0.025, 0.975))

#for Rorquals
mean(model_param_values$Rorqual_slope)
quantile(model_param_values$Rorqual_slope, probs = c(0.025, 0.975))


# check out package BRMS for stan modeling in R


m_fig_3a = lm(data =filter(d_full_9.5.19, Group == "Rorqual"), log10(Energy..kJ.)~log10(M..kg.), weights = Percent)
summary(m_fig_3a)


#################################
# Density plot of diet, Figure 3B 
#################################

d_full_9.5.19$Grouping <- as.factor(fct_relevel(d_full_9.5.19$Grouping, "Delphinidae and Phocoenidae", "Physeteridae and Ziphiidae", "Balaenopteridae"))

order_binom <- function(g, s, m) {
  sprintf("%s. %s", str_sub(g, 1, 1), s) %>% 
  factor %>% 
    fct_reorder(m)
}

# boxplot/violin
fig3b <- d_full_9.5.19 %>%
  filter(Grouping != "Balaenidae", 
         MR.exponent == "0.75") %>% 
  mutate(Species = recode(Species,
                          bonarensis = "bonaerensis",
                          Phocaena = "phocoena"),
         binomial = order_binom(Genus, Species, M..kg.)) %>%
  uncount(Percent) %>%
  ggplot(aes(x = binomial, y = log10(Energy..kJ.), color = Grouping)) +
  #geom_boxplot() +
  geom_violin(width = 1.5,
              adjust = 2) +
  scale_y_continuous(labels = scales::comma) + 
  labs(y = "log[Prey energy (kJ)]") +
  #coord_flip() +
  #ylab("frequency") +
  theme_classic(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.8)) 
fig3b

# histogram
fig3b <- d_full_9.5.19 %>%
  filter(Grouping != "Balaenidae") %>% 
  mutate(Species = recode(Species,
                          bonarensis = "bonaerensis",
                          Phocaena = "phocoena"),
         binomial = order_binom(Genus, Species, M..kg.)) %>%
  uncount(Percent) %>%
  ggplot(aes(log10(Energy..kJ.), color = binomial, fill = binomial)) +
  geom_histogram(binwidth = 0.5, alpha = 0.5, position = position_identity()) +
  scale_x_continuous(labels = scales::comma) + 
  labs(x = "log[Prey energy (kJ)]",
       y = "count") +
  facet_wrap(~ Grouping) +
  theme_classic(base_size = 18) +
  theme(axis.title = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = c(0.7, 0.7)) 
fig3b

# Save plots
ggsave("fig3b_options/fig3b_violin.pdf", width = 14, height = 8, units = "in")
#dev.copy2pdf(file="fig3b.pdf", width=14, height=8)


##########
# Figure 4----
##########

#new data from Danuta's modeling
data_from_DW_045 <- read_rds("Figure4_45_smydata.rds")
data_from_DW_bootstrap_045 <- readRDS("Figure4_45_bootstrap_b.rds")
data_from_DW_OLS_param_045 <- readRDS("Figure4_45_m_ols_param.rds")
data_from_DW_preds_045 <- readRDS("Figure4_45_bootstrap_preds.rds")

DW_model_045 <- as.data.frame(t(data_from_DW_preds_045)) %>% 
  cbind(select(data_from_DW_045, x_mean, y_mean, species, Group)) %>% 
  mutate(slope = if_else(Group == "Rorqual", 
                         data_from_DW_OLS_param_045$slope.rorq,
                         data_from_DW_OLS_param_045$slope.od),
         intercept = if_else(Group == "Rorqual", 
                             data_from_DW_OLS_param_045$intercept.rorq,
                             data_from_DW_OLS_param_045$intercept.od),
         y_model = x_mean * slope + intercept) %>% 
  rename(ci_lower = `2.5%`,
         ci_upper = `97.5%`)
write_csv(DW_model_045, "DW_model_045.csv")


data_from_DW_061 <- read_rds("Figure4_61_smydata.rds")
data_from_DW_bootstrap_061 <- readRDS("Figure4_61_bootstrap_b.rds")
data_from_DW_OLS_param_061 <- readRDS("Figure4_61_m_ols_param.rds")
data_from_DW_preds_061 <- readRDS("Figure4_61_bootstrap_preds.rds")

DW_model_061 <- as.data.frame(t(data_from_DW_preds_061)) %>% 
  cbind(select(data_from_DW_061, x_mean, y_mean, species, Group)) %>% 
  mutate(slope = if_else(Group == "Rorqual", 
                         data_from_DW_OLS_param_061$slope.rorq,
                         data_from_DW_OLS_param_061$slope.od),
         intercept = if_else(Group == "Rorqual", 
                             data_from_DW_OLS_param_061$intercept.rorq,
                             data_from_DW_OLS_param_061$intercept.od),
         y_model = x_mean * slope + intercept) %>% 
  rename(ci_lower = `2.5%`,
         ci_upper = `97.5%`)
write_csv(DW_model_061, "DW_model_061.csv")


data_from_DW_068 <- read_rds("Figure4_68_smydata.rds")
data_from_DW_bootstrap_068 <- readRDS("Figure4_68_bootstrap_b.rds")
data_from_DW_OLS_param_068 <- readRDS("Figure4_68_m_ols_param.rds")
data_from_DW_preds_068 <- readRDS("Figure4_68_bootstrap_preds.rds")

DW_model_068 <- as.data.frame(t(data_from_DW_preds_068)) %>% 
  cbind(select(data_from_DW_068, x_mean, y_mean, species, Group)) %>% 
  mutate(slope = if_else(Group == "Rorqual", 
                         data_from_DW_OLS_param_068$slope.rorq,
                         data_from_DW_OLS_param_068$slope.od),
         intercept = if_else(Group == "Rorqual", 
                             data_from_DW_OLS_param_068$intercept.rorq,
                             data_from_DW_OLS_param_068$intercept.od),
         y_model = x_mean * slope + intercept) %>% 
  rename(ci_lower = `2.5%`,
         ci_upper = `97.5%`)
write_csv(DW_model_068, "DW_model_068.csv")


data_from_DW_075 <- read_rds("Figure4_75_smydata.rds")
data_from_DW_bootstrap_075 <- readRDS("Figure4_75_bootstrap_b.rds")
data_from_DW_OLS_param_075 <- readRDS("Figure4_75_m_ols_param.rds")
data_from_DW_preds_075 <- readRDS("Figure4_75_bootstrap_preds.rds")

DW_model_075 <- as.data.frame(t(data_from_DW_preds_075)) %>% 
  cbind(select(data_from_DW_075, x_mean, y_mean, species, Group)) %>% 
  mutate(slope = if_else(Group == "Rorqual", 
                         data_from_DW_OLS_param_075$slope.rorq,
                         data_from_DW_OLS_param_075$slope.od),
         intercept = if_else(Group == "Rorqual", 
                             data_from_DW_OLS_param_075$intercept.rorq,
                             data_from_DW_OLS_param_075$intercept.od),
         y_model = x_mean * slope + intercept) %>% 
  rename(ci_lower = `2.5%`,
         ci_upper = `97.5%`)
write_csv(DW_model_075, "DW_model_075.csv")




d_other <- filter(d_full_final, Group == "Balaenid")

fig_4 <- ggplot() +
  geom_point(data = d_full_9.5.19, 
             aes(x = log10(M..kg.), y = log10(E_divesurf_med), 
                 size = (Percent)*10, shape = MR.exponent, color = Group),
             alpha = 0.5) + 
  geom_point(data = d_other, 
             aes(x = log10(M..kg.), y = log10(E_divesurf_med), 
                 size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) +
  geom_line(aes(x_mean, y_model, color = Group),
            DW_model_045, size = 1.1,
            inherit.aes = FALSE) +
  geom_ribbon(aes(x_mean, ymin = ci_lower, ymax = ci_upper, group = Group),
              DW_model_045,
              alpha = 0.25,
              inherit.aes = FALSE) +
  geom_line(aes(x_mean, y_model, color = Group),
            DW_model_061, size = 1.1,
            inherit.aes = FALSE,
            linetype = "dashed") +
  geom_ribbon(aes(x_mean, ymin = ci_lower, ymax = ci_upper, group = Group),
              DW_model_061,
              alpha = 0.25,
              inherit.aes = FALSE) +
  geom_line(aes(x_mean, y_model, color = Group),
            DW_model_068, size = 1.1,
            inherit.aes = FALSE,
            linetype = "dotdash") +
  geom_ribbon(aes(x_mean, ymin = ci_lower, ymax = ci_upper, group = Group),
              DW_model_068,
              alpha = 0.25,
              inherit.aes = FALSE) +
  geom_line(aes(x_mean, y_model, color = Group),
            DW_model_075, size = 1.1,
            inherit.aes = FALSE,
            linetype = "dotted") +
  geom_ribbon(aes(x_mean, ymin = ci_lower, ymax = ci_upper, group = Group),
              DW_model_075,
              alpha = 0.25,
              inherit.aes = FALSE) +
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
  ylim(-2,5) + xlim(1,7) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Energetic Efficiency]")
cols <- c("Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF", "Balaenid" = "turquoise", "Hypothetical" = "orange", "Fossil" = "red", "Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF")
fig_4 + scale_color_manual(values = cols) + theme(legend.position="none")


# Save plots
ggsave("fig4.tiff", width = 14, height = 8, units = "in")
ggsave("fig4.jpg", width = 14, height = 8, units = "in")
dev.copy2pdf(file="fig4.pdf", width=14, height=8)



####################
# Figure S2   ----
####################

cols <- c("Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF", "Balaenid" = "darkgreen", "Hypothetical" = "orange", "Fossil" = "black", "Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF")

# fig_4_Ein <- ggplot(data = d_full_9.5.19, aes(x = log10(M..kg.), y = log10(Total_Energy_in..J./1000), color = Group)) +
#   geom_point(aes(size = (Percent)*10, shape = MR.exponent), alpha = 0.5) + 
#   geom_smooth(data = filter(d_full_9.5.19, Group == "Odontocete"), aes(weight = Percent, group = MR.exponent, linetype = MR.exponent), method = lm) +
#   geom_smooth(data = filter(d_full_9.5.19, Group == "Rorqual"), aes(weight = Percent, group = MR.exponent, linetype = MR.exponent), method = lm) +
#   scale_linetype_manual(values=c("solid", "dashed", "dotdash", "dotted")) +
#   theme_bw() + guides(size=FALSE, color=FALSE) + 
#   #ylim(-2,5) + xlim(1,7) +
#   theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
#   labs(x = "log[Mass (kg)]", y = "log[Total Energy In]") +
#   scale_color_manual(values = cols) + theme(legend.position="none")
# fig_4_Ein

fig_S2 <- ggplot(data = d_full_9.5.19, aes(color = Group)) +
  geom_point(data = filter(d_full_9.5.19, Group == "Odontocete"),
             aes(x = log10(M..kg.), y = log10(Total_Energy_in..J./1000), 
             size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) + 
  geom_point(data = filter(d_full_9.5.19, Group == "Odontocete"),
             aes(x = log10(M..kg.), y = log10(Total_E_out..J./1000), 
             size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) + 
  geom_point(data = filter(d_full_9.5.19, Group == "Rorqual"),
             aes(x = log10(M..kg.), y = log10(Total_Energy_in..J./1000), 
             size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) + 
  geom_point(data = filter(d_full_9.5.19, Group == "Rorqual"),
             aes(x = log10(M..kg.), y = log10(Total_E_out..J./1000), 
             size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) +
  geom_point(data = filter(d_full_9.5.19, Group == "Balaenid"),
             aes(x = log10(M..kg.), y = log10(Total_Energy_in..J./1000), 
                 size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) +
  geom_point(data = filter(d_full_9.5.19, Group == "Balaenid"),
             aes(x = log10(M..kg.), y = log10(Total_E_out..J./1000), 
                 size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) +

  geom_smooth(data = filter(d_full_9.5.19, Group == "Odontocete"),
              aes(x = log10(M..kg.), y = log10(Total_Energy_in..J./1000),
                  linetype = "dashed"), method = lm, se = FALSE) +
  geom_smooth(data = filter(d_full_9.5.19, Group == "Odontocete"),
              aes(x = log10(M..kg.), y = log10(Total_E_out..J./1000),
                  linetype = "solid"), method = lm, se = FALSE) +
  geom_smooth(data = filter(d_full_9.5.19, Group == "Rorqual"), 
              aes(x = log10(M..kg.), y = log10(Total_Energy_in..J./1000),
                  linetype = "dashed"), method = lm, se = FALSE) +
  geom_smooth(data = filter(d_full_9.5.19, Group == "Rorqual"), 
              aes(x = log10(M..kg.), y = log10(Total_E_out..J./1000),
                  linetype = "solid"), method = lm, se = FALSE) +
  geom_smooth(data = filter(d_full_9.5.19, Group == "Balaenid"), 
              aes(x = log10(M..kg.), y = log10(Total_Energy_in..J./1000),
                  linetype = "dashed"), method = lm, se = FALSE) +
  geom_smooth(data = filter(d_full_9.5.19, Group == "Balaenid"), 
              aes(x = log10(M..kg.), y = log10(Total_E_out..J./1000),
                  linetype = "solid"), method = lm, se = FALSE) +
  facet_grid(.~MR.exponent) +
  theme_bw() + 
  guides(size=FALSE, color=FALSE) + 
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Total Energy (kJ)]") +
  scale_color_manual(values = cols) + 
  theme(legend.position="none",
        strip.text = element_text(size = 12))
fig_S2



# Save plots
ggsave("fig_4_extended.tiff", width = 14, height = 8, units = "in")
dev.copy2pdf(file="fig_4_extended.pdf", width=14, height=8)


fig_4_Eout <- ggplot(data = d_full_9.5.19, aes(x = log10(M..kg.), y = log10(Total_E_out..J./1000), color = Group)) +
  geom_point(aes(size = (Percent)*10, shape = MR.exponent), alpha = 0.5) + 
  geom_smooth(data = filter(d_full_9.5.19, Group == "Odontocete"), aes(weight = Percent, group = MR.exponent, linetype = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full_9.5.19, Group == "Rorqual"), aes(weight = Percent, group = MR.exponent, linetype = MR.exponent), method = lm) +
  scale_linetype_manual(values=c("solid", "dashed", "dotdash", "dotted")) +
  theme_bw() + guides(size=FALSE, color=FALSE) + 
  #ylim(-2,5) + xlim(1,7) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Total Energy Out]") +
  scale_color_manual(values = cols) + theme(legend.position="none")
fig_4_Eout + scale_color_manual(values = cols) + theme(legend.position="none")

# Save plots STILL NEED TO UPDATE
ggsave("fig4.tiff", width = 14, height = 8, units = "in")
dev.copy2pdf(file="fig4.pdf", width=14, height=8)


ggarrange(fig_4_Ein, fig_4_Eout, 
          labels = c("A", "B"), # THIS IS SO COOL!!
          legend = "none",
          ncol = 2, nrow = 1)


####################
# Extended Figure 3b----
####################

fig_3b_extended <- ggplot(data = filter(d_full_final, !Group %in% c("Odontocete", "Balaenid")), 
                          aes(x = log10(M..kg.), y = log10(E_divesurf_med), color = Group)) +
  geom_point(aes(size = (Percent)*10, shape = MR.exponent), alpha = 0.5) + 
  geom_smooth(data = filter(d_full_final, Group == "Rorqual"), aes(group = MR.exponent, linetype = MR.exponent), se = FALSE) +
  geom_line(data = filter(d_full_final, Group == "Fossil"), aes(group = MR.exponent, linetype = MR.exponent)) +
  geom_line(data = filter(d_full_final, Group == "Hypothetical"), aes(group = MR.exponent, linetype = MR.exponent)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotdash", "dotted")) +
  theme_bw() + guides(size=FALSE, color=FALSE) + 
  ylim(-2,4) + xlim(1,7) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Energetic Efficiency (max)]")
cols <- c("Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF", "Balaenid" = "darkgreen", "Hypothetical" = "orange", "Fossil" = "black", "Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF")
fig_3b_extended + scale_color_manual(values = cols) + theme(legend.position="none")

ggsave("fig3b_extended.tiff", width = 14, height = 8, units = "in")
#dev.copy2pdf(file="fig3b.pdf", width=14, height=8)

####################
# Med-med regression
####################
med_med_lm <- lm(formula = log10(FE_med) ~ log10(DT_med..min.), data = d_ind)
med_med_slope <- signif(coef(med_med_lm)[2], 3)
med_med_int <- signif(coef(med_med_lm)[1], 3)
med_med_r2 <- signif(summary(med_med_lm)$r.squared, 3)
med_med_form_lbl <- str_glue("log[10](FE_med) == {med_med_slope} %*%",
                             " log[10](DT_med) + {med_med_int}")
med_med_r2_lbl <- str_glue("R^2 == {med_med_r2}")
ggplot(d_ind, aes(log10(DT_med..min.), log10(FE_med))) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  annotate(geom = "text", 
           label = med_med_form_lbl,
           x = -0.5, y = 1.5,
           hjust = 0,
           parse = TRUE) +
  annotate(geom = "text", 
           label = med_med_r2_lbl,
           x = -0.5, y = 1.35,
           hjust = 0,
           parse = TRUE) +
  labs(x = expression(log[10](DT_med)),
       y = expression(log[10](FE_med))) +
  theme_classic(base_size = 14)
ggsave("med_med_reg.eps", width = 14, height = 8, units = "in")








###########################
# older figures drafts here
###########################

# Original figure 2
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

fig_4 <- ggplot(data = d_full_9.5.19, aes(x = log10(M..kg.), y = log10(E_divesurf_med), color = Group)) +
  geom_point(aes(size = (Percent)*10, shape = MR.exponent), alpha = 0.5) + 
  geom_point(data = d_other, aes(size = (Percent)*10, shape = MR.exponent, alpha = 0.5)) +
  geom_smooth(data = filter(d_full_9.5.19, Group == "Odontocete"), aes(weight = Percent, group = MR.exponent, linetype = MR.exponent), method = lm) +
  geom_smooth(data = filter(d_full_9.5.19, Group == "Rorqual"), aes(weight = Percent, group = MR.exponent, linetype = MR.exponent), method = lm) +
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
  ylim(-2,5) + xlim(1,7) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Energetic Efficiency]")
cols <- c("Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF", "Balaenid" = "tan2", "Hypothetical" = "orange", "Fossil" = "red", "Odontocete" = "#4DBBD5FF", "Rorqual" = "#E64B35FF")
fig_4 + scale_color_manual(values = cols) + theme(legend.position="none")






