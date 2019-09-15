########################################################
# Figure 3 modeling for Jeremy's scaling paper revisions----
########################################################

# load packages and data----
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

# basic glmm in lme4----
glmm_fig_3 <- lmer(log10(Energy..kJ.) ~ log10(M..kg.)*Order + (1|Species),
                   data = filter(d_full_9.5.19, Family != "Balaenidae" & MR.exponent == "0.45"))
summary(glmm_fig_3) #to get slope for mysticetes it is simply the log10(Mass) term, 1.65. For Odontocetes it is the log10(Mass) term PLUS the interaction term (-1.24) to give a final Odontocete slope of 0.4



# MCMCglmm so that we can get a distribution of parameter estimates and thus a confidence interval of the slope---- 
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


# means and 95% intervals on the two slope parameter distributions----
#for Odontocetes
mean(model_param_values$`log10(M..kg.)`)
quantile(model_param_values$`log10(M..kg.)`, probs = c(0.025, 0.975))

#for Rorquals
mean(model_param_values$Rorqual_slope)
quantile(model_param_values$Rorqual_slope, probs = c(0.025, 0.975))


