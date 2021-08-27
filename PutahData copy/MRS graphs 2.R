# MRS graphs 2

setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto/PutahData")

library(codyn)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(vegan)
library(data.table)
library(ggpubr)
library(lme4)
library(merTools)
library(jtools)
library(lmerTest)
library(RColorBrewer)
library(wesanderson)
library(ggeffects)

mrs3 <- read.csv("mrs_Table3.csv")

SiteA <- mrs3 %>% 
  filter(mrs3$Site=="A") %>% 
  ggplot() +
  geom_point(aes(x = year_second, y= MRSValue)) +
  geom_smooth(aes(x = year_second, y= MRSValue), method= "lm", fullrange=TRUE) +
  geom_vline(xintercept = 2000,linetype="dotted", size=1.5) +
  theme_classic() +
  scale_x_continuous(breaks=seq(1993,2017,1)) +
  ylim(0,5) 

SiteA <- SiteA + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteA



SiteB <- mrs3 %>% 
  filter(mrs3$Site=="B") %>% 
  ggplot() +
  geom_point(aes(x = year_second, y= MRSValue)) +
  geom_smooth(aes(x = year_second, y= MRSValue), method= "lm", fullrange=TRUE) +
  geom_vline(xintercept = 2000,linetype="dotted", size=1.5) +
  theme_classic() +
  scale_x_continuous(breaks=seq(1993,2017,1))+
  ylim(0,5) 

SiteB <- SiteB + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteB



SiteC <- mrs3 %>% 
  filter(mrs3$Site=="C") %>% 
  ggplot() +
  geom_point(aes(x = year_second, y= MRSValue)) +
  geom_smooth(aes(x = year_second, y= MRSValue), method= "lm", fullrange=TRUE) +
  geom_vline(xintercept = 2000,linetype="dotted", size=1.5) +
  theme_classic() +
  scale_x_continuous(breaks=seq(1993,2017,1))+
  ylim(0,5) 

SiteC <- SiteC + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteC

SiteD <- mrs3 %>% 
  filter(mrs3$Site=="D") %>% 
  ggplot() +
  geom_point(aes(x = year_second, y= MRSValue)) +
  geom_smooth(aes(x = year_second, y= MRSValue), method= "lm", fullrange=TRUE) +
  geom_vline(xintercept = 2000,linetype="dotted", size=1.5) +
  theme_classic() +
  scale_x_continuous(breaks=seq(1993,2017,1))+
  ylim(0,5) 

SiteD <- SiteD + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteD


SiteE <- mrs3 %>% 
  filter(mrs3$Site=="E") %>% 
  ggplot() +
  geom_point(aes(x = year_second, y= MRSValue)) +
  geom_smooth(aes(x = year_second, y= MRSValue), method= "lm", fullrange=TRUE) +
  geom_vline(xintercept = 2000,linetype="dotted", size=1.5) +
  theme_classic() +
  scale_x_continuous(breaks=seq(1993,2017,1))+
  ylim(0,5) 

SiteE <- SiteE + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteE



SiteF <- mrs3 %>% 
  filter(mrs3$Site=="F") %>% 
  ggplot() +
  geom_point(aes(x = year_second, y= MRSValue)) +
  geom_smooth(aes(x = year_second, y= MRSValue), method= "lm", fullrange=TRUE) +
  geom_vline(xintercept = 2000,linetype="dotted", size=1.5) +
  theme_classic() +
  scale_x_continuous(breaks=seq(1993,2017,1))+
  ylim(0,5) 

SiteF <- SiteF + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteF





MRSgraphs2<- ggarrange(SiteA,SiteB,SiteC,SiteD, SiteE, SiteF, 
                         labels = c("Site A", "Site B", "Site C", "Site D","Site E","Site F"),
                         font.label = list(size = 12, color = "black", face = "plain"),
                         label.x = .05,
                         label.y = 1,
                         heights = c(1),
                         widths = c(2.5),
                         ncol = 2, nrow = 3)

MRSgraphs2 <-annotate_figure(MRSgraphs2,
                               bottom = text_grob("Year", size = 14),
                               left = text_grob("MRS", rot = 90, size = 14))

MRSgraphs2

ggsave("MRSgraphs2.pdf")


#model 2

#lme4 provides functions for fitting and analyzing mixed models: linear (lmer), generalized linear (glmer) and nonlinear (nlmer.)

#lmer(formula, data = NULL, REML = TRUE, control = lmerControl(),
#start = NULL, verbose = 0L, subset, weights, na.action,
#offset, contrasts = NULL, devFunOnly = FALSE)

#Dependent variable = MRS, independent variable = year, random variable = site. See if this model is significant.

#lmer(dept~intercept?, REM=FALSE, data=)

#mrs_model0 <- lmer(MRSValue~1+Site+(1|year_pair),data=mrs3,REML=FALSE)
#summary(mrs_model0)
#summ(mrs_model0)
#ranova(mrs_model0)


#mrs_model1 <- lmer(MRSValue~1+(1|year_pair),data=mrs3,REML=FALSE)
#summary(mrs_model1)
#summ(mrs_model1)
#ranova(mrs_model1)


mrs_model2 <- lmer(MRSValue~year_second+(1|Site),data=mrs3,REML=FALSE) #correct one? 
summary(mrs_model2)
summ(mrs_model2) 
ranef(mrs_model2)
resid(mrs_model2)
VarCorr(mrs_model2)
coef(mrs_model2)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

plot(fitted(mrs_model2), resid(mrs_model2, type = "pearson"))
abline(0,0, col="red")

MRSmodel <- ggplot(fortify(mrs_model2), aes(year_second, MRSValue, color=Site, cex.axis=1.5, cex.lab= 3)) +
  stat_summary(fun.data=mean_se, geom="pointrange", alpha=.7) +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line", size=.8, alpha=.7) +
  theme_classic()+ 
  ylab("Mean Rank Shift")+
  scale_colour_manual(values=cbPalette)+
  geom_vline(xintercept = 2000, size=1)+
  scale_x_continuous(breaks=seq(1993,2017,1))+
  theme(axis.text.x = element_text(angle=90), axis.title.x=element_blank(), axis.text = element_text(size = 12), axis.title =element_text(size = 16), legend.title = element_text(size = 14), legend.text = element_text(size=12))+
  geom_smooth(data = cbind(mrs3, y.hat = predict(mrs_model2)), aes(x =year_second, y = y.hat), col ="black", size= 2, alpha=.5)

MRSmodel 

ggsave("MRSmodel.jpeg")

##https://rstudio-pubs-static.s3.amazonaws.com/63556_e35cc7e2dfb54a5bb551f3fa4b3ec4ae.html