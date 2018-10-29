##################################
## Jeremy's Energetic Scaling paper preliminary visualizations
##################################

# load packages
library(dplyr)
library(ggplot2)
#library(readxl)

# load data
d_full <- read.csv("Odontoceti model output v9.4.csv")
#d_full <- read_excel("Odontoceti model output v9.4.xlsx", sheet = 1)

d_full$MR.exponent = as.factor(d_full$MR.exponent)

#create weighted values
d_full$Weighted_E_divesurf_max <- d_full$Percent*d_full$E_divesurf_max  #creates a column for E_divesurf_max that is weighted by Percent diet
d_full$Weighted_E_divesurf_med <- d_full$Percent*d_full$E_divesurf_med  #creates a column for E_divesurf_med that is weighted by Percent diet

##################
# Simulatiing data/bootstrapping code
##################

rn <- function(mean){
  return(rnorm(100,mean,sd(d_full$E_divesurf_max)))
}

#d_new<-rnorm(100,d_full$Weighted_E_divesurf_max,sd(d_full$E_divesurf_max))
d_new<-lapply(d_full$Weighted_E_divesurf_max,rn)
hist(d_new[[1]],breaks=50)

#d_strapped<-(rep(d_full[1,],100))
d_strapped<-d_full %>% slice(rep(1:n(), each = 100))

d_strapped$Weighted_E_divesurf_max[1:100]<-d_new[[1]]

for(i in 1:(dim(d_full)[1])){
  d_strapped$Weighted_E_divesurf_max[(100*i-99):(100*i)]<-as.integer(d_new[[i]])
}
d_strapped$Weighted_E_divesurf_max[d_strapped$Weighted_E_divesurf_max<0] <- 0

Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.61") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max_gamm$gam)
plot(Eff_dive_max_gamm$gam)

Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.68") %>% gamm(Weighted_E_divesurf_max ~ M..kg., family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max_gamm$gam)

Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.75") %>% gamm(Weighted_E_divesurf_max ~ M..kg., family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max_gamm$gam)

##################
# premliminary figures with weighted proportions of diet
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


##################
#preliminary plots
##################

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




