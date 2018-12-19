library(readxl)
library(ggplot2)
library(dplyr)
library(mgcv)

scaling_matrix <- read_excel("foragestats_combined_ko.xlsx", sheet = 1)
scaling_matrix_summary <- read_excel("foragestats_combined_ko.xlsx", sheet = 2)
odont_matrix <- read_excel("Odontoceti model output v9.4.xlsx", sheet = 1)
cet_matrix <- read_excel("Cetacea model output v9.6.xlsx", sheet = 1)

#dive depth vs. body size
p <- ggplot(scaling_matrix, aes (Body_mass_kg,Depth_m_max))
p + geom_point(aes(colour = factor(Species)),size = 1) + theme_minimal()

model1 <- lm(Depth_m_max ~ Body_mass_kg, data=scaling_matrix)
summary(model1)

#try to plot by species in a common color framework - http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
p <- ggplot(scaling_matrix, aes (Body_mass_kg,Depth_m_max, colour = factor(taxa))) + geom_point(aes(colour = factor(taxa)),size = 1) + theme_minimal() + geom_smooth(method = "lm") + guides(color=guide_legend(override.aes=list(fill=NA))) #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
p + geom_point(aes(colour = factor(Species)),size = 1)  #+ scale_color_manual(values=c("red", "blue", "green"))


#by taxa, with linear models
p <- ggplot(scaling_matrix, aes (Body_mass_kg,Depth_m_max))
p + geom_point(aes(colour = factor(taxa)),size = 1) + theme_minimal()

model_O <- lm(Depth_m_max ~ Body_mass_kg, data=filter(scaling_matrix, taxa == "O"))
summary(model_O)

model_M <- lm(Depth_m_max ~ Body_mass_kg, data=filter(scaling_matrix, taxa == "M"))
summary(model_M)

p <- ggplot(scaling_matrix, aes (Body_mass_kg,Depth_m_max, colour = factor(taxa))) + geom_point(aes(colour = factor(taxa)),size = 1) + theme_minimal() + geom_smooth(method = "lm")  + theme(legend.position="top")  + guides(color=guide_legend(override.aes=list(fill=NA))) #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
p 

p <- ggplot(scaling_matrix, aes (Body_mass_kg,Depth_m_max, colour = factor(taxa))) + geom_point(aes(colour = factor(Species), shape = factor(taxa)),size = 1) + theme_minimal() + geom_smooth(method = "lm")  + theme(legend.position="top")  + guides(color=guide_legend(override.aes=list(fill=NA))) #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
p 

#energy efficiency vs. body size
###p <- ggplot(scaling_matrix, aes (Species))

#feeding rates per min dive time as a function of TADL-dive time
spps<-unique(scaling_matrix$Species)
#scaling_matrix$TADL<-NA
#for (sp in spps){
#  scaling_matrix$TADL[scaling_matrix$Species==sp] <- scaling_matrix_summary$TADL[scaling_matrix_summary$Species==sp]
#}

scaling_matrix <- scaling_matrix_summary %>% select(Species, TADL) %>% left_join(scaling_matrix,.,by = "Species")
scaling_matrix <- left_join(scaling_matrix,as_tibble(select(scaling_matrix_summary, Species, TADL)), by = "Species")

p <- ggplot(scaling_matrix, aes (foraging_count_per_dive_min_median,Duration_min_max, colour = factor(taxa))) + geom_point(aes(colour = factor(taxa)),size = 1) + theme_minimal() + geom_smooth(method = "lm", formula = y~log(x))  + theme(legend.position="top")  + guides(color=guide_legend(override.aes=list(fill=NA))) #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
p 

p <- ggplot(scaling_matrix, aes (Duration_min_max-TADL.x,foraging_count_max, colour = factor(taxa))) + geom_point(aes(colour = factor(taxa)),size = 1, alpha=0.5) + theme_minimal() + geom_smooth(method = "lm", formula = y~(x))  + theme(legend.position="top")  + guides(color=guide_legend(override.aes=list(fill=NA))) #+ theme(legend.justification=c(1,0), legend.position=c(1,0))
p 
#color by species, shape by taxa
p + geom_point(aes(colour = factor(Species), shape = factor(taxa)) ,size = 1) #+ scale_shape_manual("taxa",values=c("O"=16,"M"=3)) #+ scale_color_manual(values=c("red", "blue", "green")) #,"2005"=4

p + geom_point(aes(colour = factor(Species), shape = factor(taxa)) ,size = 1, alpha=0.5) + scale_shape_manual("taxa",values=c("O"=16,"M"=3)) #+ scale_color_manual(values=c("red", "blue", "green")) #,"2005"=4



#Test whether slopes are significantly different between M & O
TADL_O_gamm<- filter(scaling_matrix, taxa == "O") %>% gamm(foraging_count_max ~ (Duration_min_max-TADL), random=list(Species=~1), data=.)
### $gam to look at gam effects. $lme to look at random effects.
summary(TADL_O_gamm$gam)

TADL_M_gamm<- filter(scaling_matrix, taxa == "M") %>% gamm(foraging_count_max ~ (Duration_min_max-TADL), random=list(Species=~1), data=.)
summary(TADL_M_gamm$gam)

TADL_gamm <- gamm(foraging_count_max ~ Duration_min_max-TADL + taxa, random=list(Species=~1), data=scaling_matrix)
summary(TADL_gamm$gam)


#Odontocente efficiency models
#Odont_eff_gamm <- odont_matrix %>% gamm(E_divesurf_max ~ M_kg, family=poisson(link='log'), random=list(Species=~1), data=.)




