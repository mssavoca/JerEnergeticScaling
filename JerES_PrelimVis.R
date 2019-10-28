##################################
## Jeremy's Energetic Scaling paper preliminary visualizations
##################################

# load packages
library(dplyr)
library(ggplot2)
library(mgcv)
library(readxl)

# load data
#d_full <- read.csv("Cetacea model output NULL_EXTANT.csv")
d_full <- read.csv("Cetacea model output BOUT_EXTANT.csv")
#d_full <- read.csv("Cetacea model output NULL_ALL_ENP.csv")

#d_full <- read_excel("Cetacea model output v10.10.xlsx", sheet = 1)

d_full$MR.exponent = as.factor(d_full$MR.exponent)
d_full$M..kg. <- as.numeric(d_full$M..kg.)
d_full$Prey.W..g. <- as.numeric(d_full$Prey.W..g.)
d_full$Group <- ifelse(d_full$Family == "Balaenopteridae", "Rorqual", 
                       ifelse(d_full$Family == "Balaenidae", "Balaenid", "Odontocete"))

# Makes the group when fossil species are included in NULL_ALL_ENP
#d_full$Group[d_full$Family == "Balaenopteridae" | d_full$Family == "Fossil"] <- "Rorqual"
#d_full$Group[d_full$Family != "Balaenopteridae" & d_full$Family != "Fossil"] <- "Odontocete"

d_full$MR.exponent = as.factor(d_full$MR.exponent)


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

saveRDS(d_strapped, file="d_strapped_12042018.RDS")
d_strapped <- readRDS(file="d_strapped_12042018.RDS")

# plotting the strapped data as a sanity check
# p1_logM__weighted_divesurf_max_strapped <- ggplot(d_strapped, aes(x = log(M..kg.), y = Weighted_E_divesurf_max, color = Species)) +
#   geom_point(inherit.aes=T, alpha = 0.3) +  
#   geom_smooth(aes(group = MR.exponent), color = "black", inherit.aes = T, method = loess) +
#   facet_grid(.~d_strapped$MR.exponent)
# 
# p1_logM__weighted_divesurf_max_strapped

# GAMMs with both Odontocetes and Mysticetes in the model
Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.68" & Family != "Balaenidae") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Eff_dive_max_gamm$gam)
par(mfrow=c(1,2))
plot(Eff_dive_max_gamm$gam, select=1, xlab="Whale mass (kg)", ylab="Foraging efficiency",shade=F, cex.lab =1.5,cex.axis =1.5)
plot(Eff_dive_max_gamm$gam, select=2, xlab="Prey engulfment capacity (g)", ylab="Foraging efficiency",shade=F, cex.lab =1.5, cex.axis =1.5)

#save the model output from R console to a text file to refer to later
sink("Both_NULL_GAMM_0.68.txt")
print(summary(Eff_dive_max_gamm$gam))
sink()



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


##EXPLORE Odont v. Myst GAMMs
Odont_Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.75") %>% filter(., Group=="Odontocete") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Odont_Eff_dive_max_gamm$gam)
plot(Odont_Eff_dive_max_gamm$gam)

#save the model output from R console to a text file to refer to later
sink("Odont_NULL_GAMM_0.75.txt")
print(summary(Odont_Eff_dive_max_gamm$gam))
sink()

Myst_Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.45") %>% filter(., Group=="Rorqual") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5), family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(Myst_Eff_dive_max_gamm$gam)
plot((Myst_Eff_dive_max_gamm$gam))

#save the model output from R console to a text file to refer to later
sink("Mys_NULL_GAMM_0.45.txt")
print(summary(Myst_Eff_dive_max_gamm$gam))
sink()


group_Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.68") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5)+Group, family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(group_Eff_dive_max_gamm$gam)
plot(group_Eff_dive_max_gamm$gam)

group_Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.61") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5)+Group, family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(group_Eff_dive_max_gamm$gam)
plot(group_Eff_dive_max_gamm$gam)

group_Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.75") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5)+Group, family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(group_Eff_dive_max_gamm$gam)
plot(group_Eff_dive_max_gamm$gam)

group_Eff_dive_max_gamm<- filter(d_strapped, MR.exponent == "0.45") %>% gamm(Weighted_E_divesurf_max ~ s(M..kg.,k=5)+s(Prey.W..g., k=5)+Group, family=poisson(link='log'), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(group_Eff_dive_max_gamm$gam)
plot(group_Eff_dive_max_gamm$gam)



#### MORE EXPLORATION CRAP

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
<<<<<<< HEAD

=======
>>>>>>> d46a069026b06c18878dce1e2641ae600f3dec3d
# get silhouette images for figure
imgOo <- png::readPNG("./Orcinus-orca.png")
rastOo <- grid::rasterGrob(imgOo, interpolate = T)
imgBp <- png::readPNG("./Balaenoptera-physalus.png")
rastBp <- grid::rasterGrob(imgBp, interpolate = T)

p1_logM_divesurf_max <- ggplot(data = filter(d_full, d_full$MR.exponent == "0.45"),
                               aes(x = log(M..kg.), y = E_divesurf_max, color = Group)) +
  geom_point(aes(size =Percent), alpha = 0.3) +  
  geom_smooth(data = filter(d_full, Group == "Rorqual")) +
  geom_smooth(data = filter(d_full, Group == "Odontocete")) +
  #geom_smooth(aes(group = MR.exponent), color = "black", method = loess) +
  #facet_grid(.~d_full$MR.exponent, scales = "free") +
  annotation_custom(rastOo, ymin = 200, ymax = 275, xmin = -1) +
  annotation_custom(rastBp, ymin = 300, ymax = 375, xmin = 8, xmax = 12.5) +
  theme_bw() +
  labs(x = "Log (Mass in kg)", y = "Energetic Efficiency (max)")

p1_logM_divesurf_max

# plot removing the hypothetically huge blue whale
d_obs <- filter(d_full, MR.exponent == "0.68") #& Species != "huge")

p1_logM_divesurf_max_obs <- ggplot(data = d_full, aes(x = log(M..kg.), y = log(E_divesurf_max), color = Group, shape = MR.exponent)) +
  geom_point(aes(size = Percent, group = MR.exponent), alpha = 0.3) +  
  geom_smooth(data = filter(d_full, MR.exponent == 0.45), aes(weight = Percent, group = Group), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.61), aes(weight = Percent, group = Group), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.68), aes(weight = Percent, group = Group), method = lm) +
  geom_smooth(data = filter(d_full, MR.exponent == 0.75), aes(weight = Percent, group = Group), method = lm) +  
  annotation_custom(rastOo, ymin = -1, ymax = 0.5, xmin = 3.5, xmax = 5.5) +
  annotation_custom(rastBp, ymin = -1, ymax = 0.5, xmin = 10.25, xmax = 11.5) +
  theme_bw() + guides(size=FALSE, color=FALSE) +
  labs(x = "Log (Mass [kg])", y = "Log (Energetic Efficiency [max])")

p1_logM_divesurf_max_obs

<<<<<<< HEAD
=======
# code to extract values for each model, needed for Jeremy's paper, change filter as necessary
d_model <- filter(d_full, Species != "huge" & Group == "Rorqual" & MR.exponent == "0.75")  #        
model <- lm((log(E_divesurf_max)~log(M..kg.)), weights = Percent, data = d_model)
summary(model)


>>>>>>> d46a069026b06c18878dce1e2641ae600f3dec3d

p1_logM__weighted_divesurf_max <- ggplot(d_full, aes(x = log(M..kg.), y = Weighted_E_divesurf_max, color = Species)) +
  geom_point(inherit.aes=T) +  
  geom_smooth(aes(group = MR.exponent), color = "black", inherit.aes = T, method = loess) +
  facet_grid(.~d_full$MR.exponent)

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




## POTENTIAL FIGS
par(mfrow=c(2,2))
plot(Odont_Eff_dive_max_gamm$gam, shade=T)
plot(Myst_Eff_dive_max_gamm$gam, shade=T)

plot(Odont_Eff_dive_max_gamm$gam, col="red", shade.col="grey", shade=F, residuals=TRUE,select=1, xlab="", ylab="", xlim=c(0,250000), ylim=c(-2,2)) #residuals=TRUE,
par(new=T)
plot(Myst_Eff_dive_max_gamm$gam, col="blue", shade.col="grey", shade=F, residuals=TRUE,select=1, xlim=c(0,250000), ylim=c(-2,2)) #residuals=TRUE,

plot(Odont_Eff_dive_max_gamm$gam, col="red", shade.col="grey", shade=F, select=2, xlab="", ylab="", xlim=c(0,250000), ylim=c(-2,2)) #residuals=TRUE,
par(new=T)
plot(Myst_Eff_dive_max_gamm$gam, col="blue", shade.col="grey", shade=F, select=2, xlim=c(0,250000), ylim=c(-2,2)) #residuals=TRUE,





## GGPLOT for GAM exploration - https://stackoverflow.com/questions/49471300/gam-plots-with-ggplot ##
## scale size of point by # diet
## https://mfasiolo.github.io/mgcViz/articles/mgcviz.html - explore other approach ##
## For plotting in response - https://stats.stackexchange.com/questions/31502/plotting-gam-model-output-not-component-smooth-functions
## pull out pieces of a GAM for "fancy plot" - https://rforge.wordpress.com/2009/06/16/how2plot-nicer-gam-curves/

library(ISLR)
library(mgcv)
library(voxel)
library(tidyverse)
library(gridExtra)
#data(College)

map(vars, function(x){
  p <- plotGAM(group_Eff_dive_max_gamm$gam, smooth.cov = x) #plot customization goes here
  g <- ggplotGrob(p)
}) %>%
{grid.arrange(grobs = (.), ncol = 2, nrow = 3)}

map(vars, function(x){
  p <- plotGAM(group_Eff_dive_max_gamm$gam, smooth.cov = x, groupCovs = "Group") +
    geom_point(data = filter(d_strapped, MR.exponent == "0.75"), aes_string(y = "Weighted_E_divesurf_med", x = x, color= "Group"), alpha = 0.2) +
    geom_rug(data = filter(d_strapped, MR.exponent == "0.75"), aes_string(y = "Weighted_E_divesurf_med", x = x, color= "Group"  ), alpha = 0.2) +
    scale_color_manual("Group", values = c("#868686FF", "#0073C2FF")) +
    theme(legend.position="none")
  g <- ggplotGrob(p)
}) %>%
{grid.arrange(grobs = (.), ncol = 3, nrow = 2)}




## predict GAM
Myst_Eff_dive_max_gamm$gam
Odont_Eff_dive_max_gamm$gam

newmass <- data.frame(M..kg.=seq(from=0, to=max(d_strapped$M..kg.), by=50))
newprey <- data.frame(Prey.W..g.=seq(from=0, to=max(d_strapped$Prey.W..g.), by=1000))
newmass$Prey.W..g. <- mean(d_strapped$Prey.W..g.)
newprey$M..kg. <- mean(d_strapped$M..kg.)

fitsmass <- predict(gam1, newdata=newdat, type="response", se.fit=TRUE)
M_fitsprey <- predict(Myst_Eff_dive_max_gamm$gam, newdata=newprey, type="response", se.fit=TRUE)
M_fitsmass <- predict(Myst_Eff_dive_max_gamm$gam, newdata=newmass, type="response", se.fit=TRUE)
O_fitsprey <- predict(Odont_Eff_dive_max_gamm$gam, newdata=newprey, type="response", se.fit=TRUE)
O_fitsmass <- predict(Odont_Eff_dive_max_gamm$gam, newdata=newmass, type="response", se.fit=TRUE)

plot(Myst_Eff_dive_max_gamm$gam, select=2)
lines(newprey$Prey.W..g., M_fitsprey$fit-mean(predict(Myst_Eff_dive_max_gamm$gam, newdata=d_strapped)), col="red")

summary(M_fitsprey$fit)
summary(M_fitsmass$fit)

M_predictprey<-cbind(newprey, M_fitsprey$fit)
O_predictprey<-cbind(newprey, O_fitsprey$fit)
M_predictprey$Group="Rorqual"
O_predictprey$Group="Odontocete"

names(M_predictprey)[3]<-"E_divesurf_max"
names(O_predictprey)[3]<-"E_divesurf_max"

M_predictmass<-cbind(newmass, M_fitsmass$fit)
O_predictmass<-cbind(newmass, O_fitsmass$fit)
M_predictmass$Group="Rorqual"
O_predictmass$Group="Odontocete"

names(M_predictmass)[3]<-"E_divesurf_max"
names(O_predictmass)[3]<-"E_divesurf_max"


# get silhouette images for figure with gam curves - this needs more work!!!
imgOo <- png::readPNG("./Orcinus-orca.png")
rastOo <- grid::rasterGrob(imgOo, interpolate = T)
imgBp <- png::readPNG("./Balaenoptera-physalus.png")
rastBp <- grid::rasterGrob(imgBp, interpolate = T)

p1_logM_divesurf_max <- ggplot(data = filter(d_full, d_full$MR.exponent == "0.68"),
                               aes(x = log(M..kg.), y = E_divesurf_max, color = Group)) +
  geom_point(aes(size =Percent), alpha = 0.3) +  
#  geom_smooth(data = filter(d_full, Group == "Rorqual")) +
#  geom_smooth(data = filter(d_full, Group == "Odontocete")) +
  geom_smooth(data = (M_predictmass)) +
  geom_smooth(data = (O_predictmass)) +
  
  
  annotation_custom(rastOo, ymin = 200, ymax = 275, xmin = -1) +
  annotation_custom(rastBp, ymin = 300, ymax = 375, xmin = 8, xmax = 12.5) +
  theme_bw() +
  labs(x = "Log (Mass in kg)", y = "Energetic Efficiency (max)")


p1_logM_divesurf_max


