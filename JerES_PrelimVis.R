##################################
## Jeremy's Energetic Scaling paper preliminary visualizations
##################################

# load packages
library(dplyr)
library(ggplot2)
library(mgcv)
library(readxl)

# load data
d_full <- read.csv("Cetacea model output v9.6.csv")
#d_full <- read.csv("Odontoceti model output v9.5.csv")
#d_full <- read_excel("Odontoceti model output v9.5.xlsx")

d_full$MR.exponent = as.factor(d_full$MR.exponent)

# add group column
d_full$Group <- ifelse(d_full$Family == "Balaenopteridae", "Rorqual", "Odontocete")

#create weighted values
d_full$Weighted_E_divesurf_max <- d_full$Percent*d_full$E_divesurf_max  #creates a column for E_divesurf_max that is weighted by Percent diet
d_full$Weighted_E_divesurf_med <- d_full$Percent*d_full$E_divesurf_med  #creates a column for E_divesurf_med that is weighted by Percent diet

##################
# Simulatiing data/bootstrapping code
##################

sd_species <- d_full %>% group_by(., Species) %>% summarise(., sd_Species=sd(E_divesurf_max))
sd_species.med <- d_full %>% group_by(., Species) %>% summarise(., sd_Species=sd(E_divesurf_med))

mean_sd_species<-mean(sd_species$sd_Species)
mean_sd_species.med<-mean(sd_species.med$sd_Species)

rn <- function(mean){
  return(rnorm(100,mean,mean_sd_species))
}

rn.med <- function(mean){
  return(rnorm(100,mean,mean_sd_species.med))
}

#d_new<-rnorm(100,d_full$Weighted_E_divesurf_max,sd(d_full$E_divesurf_max))
d_new<-lapply(d_full$Weighted_E_divesurf_max,rn)
d_new.med<-lapply(d_full$Weighted_E_divesurf_med,rn.med)

hist(d_new.med[[1]],breaks=50)

#d_strapped<-(rep(d_full[1,],100))
d_strapped<-d_full %>% slice(rep(1:n(), each = 100))

d_strapped$Weighted_E_divesurf_max[1:100]<-d_new[[1]]
d_strapped$Weighted_E_divesurf_med[1:100]<-d_new.med[[1]]

for(i in 1:(dim(d_full)[1])){
  d_strapped$Weighted_E_divesurf_max[(100*i-99):(100*i)]<-as.integer(d_new[[i]])
  d_strapped$Weighted_E_divesurf_med[(100*i-99):(100*i)]<-as.integer(d_new.med[[i]])
}

d_strapped$Weighted_E_divesurf_max[d_strapped$Weighted_E_divesurf_max<0] <- 0
d_strapped$Weighted_E_divesurf_med[d_strapped$Weighted_E_divesurf_med<0] <- 0

# plotting the strapped data as a sanity check
p1_logM__weighted_divesurf_max_strapped <- ggplot(d_strapped, aes(x = log(M..kg.), y = Weighted_E_divesurf_max, color = Species)) +
  geom_point(inherit.aes=T, alpha = 0.3) +  
  geom_smooth(aes(group = MR.exponent), color = "black", inherit.aes = T, method = loess) +
  facet_grid(.~d_strapped$MR.exponent)

p1_logM__weighted_divesurf_max_strapped


Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.45") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max_gamm$gam)
plot(Eff_dive_max_gamm$gam)

Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.61") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max_gamm$gam)
plot(Eff_dive_max_gamm$gam)

Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.68") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max_gamm$gam)

Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.75") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max_gamm$gam)


Eff_dive_med_gamm<- filter(d_strapped, MR.exponent == "0.61") %>% gamm(Weighted_E_divesurf_med ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_med_gamm$gam)
plot(Eff_dive_med_gamm$gam)

Eff_dive_med_gamm<- filter(d_strapped, MR.exponent == "0.68") %>% gamm(Weighted_E_divesurf_med ~ M..kg., family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_med_gamm$gam)

Eff_dive_med_gamm<- filter(d_strapped, MR.exponent == "0.75") %>% gamm(Weighted_E_divesurf_med ~ M..kg., family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_med_gamm$gam)

###################
# looking at prey data with < 20% occurrence
###################
d_strapped_20 <- filter(d_strapped, Percent >= 20)
p1_logM__weighted_divesurf_max_strapped20 <- ggplot(d_strapped_20, aes(x = log(M..kg.), y = Weighted_E_divesurf_max, color = Species)) +
  geom_point(inherit.aes=T, alpha = 0.3) +  
  geom_smooth(aes(group = MR.exponent), color = "black", inherit.aes = T, method = loess) +
  facet_grid(.~d_strapped_20$MR.exponent)

p1_logM__weighted_divesurf_max_strapped20


# running models with prey data with < 20% occurrence
Eff_dive_max20_gamm<- filter(d_strapped_20, MR.exponent == "0.61") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max20_gamm$gam)
plot(Eff_dive_max20_gamm$gam)

Eff_dive_max20_gamm<- filter(d_strapped_20, MR.exponent == "0.68") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max20_gamm$gam)

Eff_dive_max20_gamm<- filter(d_strapped_20, MR.exponent == "0.75") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max20_gamm$gam)




##################
# premliminary figures with weighted proportions of diet
##################

p1_logM__weighted_divesurf_max <- ggplot(d_full, aes(x = log(M..kg.), y = Weighted_E_divesurf_max, color = Species)) +
  geom_point(inherit.aes=T) +  
  geom_smooth(aes(group = MR.exponent), color = "black", inherit.aes = T, method = loess) +
  facet_grid(d_full$Group~d_full$MR.exponent, scales = "free")

p1_logM__weighted_divesurf_max

p1_logM__weighted_divesurf_med <- ggplot(d_full, aes(x = log(M..kg.), y = Weighted_E_divesurf_med, color = Species)) +
  geom_point(inherit.aes=T) +  
  geom_smooth(aes(group = MR.exponent), color = "black", inherit.aes = T, method = loess) +
  facet_grid(.~d_full$MR.exponent)

p1_logM__weighted_divesurf_med

p1_TL__weighted_divesurf_max <- ggplot(d_full, aes(x = TL..m., y = Weighted_E_divesurf_max, color = Species)) +
  geom_point(inherit.aes=T) +  
  geom_smooth(aes(group = MR.exponent), color = "black", inherit.aes = T, method = loess) +
  facet_grid(.~d_full$MR.exponent)

p1_TL__weighted_divesurf_max

p1_TL__weighted_divesurf_med <- ggplot(d_full, aes(x = TL..m., y = Weighted_E_divesurf_med, color = Species)) +
  geom_point(inherit.aes=T) +  
  geom_smooth(aes(group = MR.exponent), color = "black", inherit.aes = T, method = loess) +
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


p1_TL_divesurf_med <- ggplot(d_full, aes(M..kg., E_divesurf_med, color = Species)) +
  geom_point() + 
  geom_smooth(aes(group = MR.exponent), inherit.aes = T, method = loess) + 
  facet_wrap(.~d_full$MR.exponent)

p1_TL_divesurf_med


p1_TL_divesurf_max <- ggplot(d_full, aes(x = M..kg., y = E_divesurf_max, color = Species)) +
  geom_point(inherit.aes=T) + 
  facet_wrap(.~d_full$MR.exponent)  + 
  geom_smooth(aes(group = MR.exponent), inherit.aes = T, method = loess) 

p1_TL_divesurf_max




