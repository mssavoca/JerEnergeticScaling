##################################
## Jeremy's Energetic Scaling paper preliminary visualizations
##################################

# load packages
library(dplyr)
library(ggplot2)


# load data
d_full <- read.csv("Odontoceti model output v9.4.csv")
d_full$MR.exponent = as.factor(d_full$MR.exponent)

#create weighted values
d_full$Weighted_E_divesurf_max <- d_full$Percent*d_full$E_divesurf_max  #creates a column for E_divesurf_max that is weighted by Percent diet
d_full$Weighted_E_divesurf_med <- d_full$Percent*d_full$E_divesurf_med  #creates a column for E_divesurf_med that is weighted by Percent diet


#preliminary plots
p1_TL_dive_med <- ggplot(d_full, aes(TL..m., E_dive_med, color = Species)) +
  geom_point() + geom_smooth(method = loess) + facet_wrap(.~d_full$MR.exponent)

p1_TL_dive_med


p1_TL_dive_max <- ggplot(d_full, aes(TL..m., E_dive_max, color = Species)) +
  geom_point() + geom_smooth(method = loess) + facet_wrap(.~d_full$MR.exponent)

p1_TL_dive_max


p1_TL_divesurf_med <- ggplot(d_full, aes(TL..m., E_divesurf_med, color = Species)) +
  geom_point() + 
  geom_smooth(aes(group = MR.exponent), inherit.aes = T, method = loess) + 
  facet_wrap(.~d_full$MR.exponent)

p1_TL_divesurf_med


p1_TL_divesurf_max <- ggplot(d_full, aes(x = TL..m., y = log(E_divesurf_max), color = Species)) +
  geom_point(inherit.aes=T) + 
  facet_wrap(.~d_full$MR.exponent)  + 
  geom_smooth(aes(group = MR.exponent), inherit.aes = T, method = loess) 

p1_TL_divesurf_max

##################
#making figures with weighted proportions of diet
##################

p1_TL__weighted_divesurf_max <- ggplot(d_full, aes(x = TL..m., y = Weighted_E_divesurf_max, color = Species)) +
  geom_point(inherit.aes=T) +  
  geom_smooth(aes(group = MR.exponent), inherit.aes = T, method = loess) +
  facet_grid(.~d_full$MR.exponent)

p1_TL__weighted_divesurf_max

p1_TL__weighted_divesurf_med <- ggplot(d_full, aes(x = TL..m., y = Weighted_E_divesurf_med, color = Species)) +
  geom_point(inherit.aes=T) +  
  geom_smooth(aes(group = MR.exponent), inherit.aes = T, method = loess) +
  facet_grid(.~d_full$MR.exponent)

p1_TL__weighted_divesurf_med

