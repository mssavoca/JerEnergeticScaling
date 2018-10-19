##################################
## Jeremy's Energetic Scaling paper preliminary visualizations
##################################

# load packages
library(dplyr)
library(ggplot2)


# load data
d_full <- read.csv("Odontoceti model output v9.4.csv")
d_full$MR.exponent = as.factor(d_full$MR.exponent)


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
