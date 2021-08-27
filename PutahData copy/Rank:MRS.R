# MRS
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

Site_A <- read.csv("Rank1.csv")

Collins <- data("collins08")

#RANK SHIFT/MRS
# rank_shift(df, time.var, species.var, abundance.var, replicate.var)


Site_A <- read.csv("Rank1.csv")
myoutputA <- rank_shift(Site_A,
                       time.var = "year",
                       species.var = "spname",
                       abundance.var = "CountOfsp")
myoutputA
SiteA <- myoutputA %>% 
  ggplot(aes(x= myoutputA$year_pair, y= myoutputA$MRS)) +
  geom_point() +
  geom_smooth(aes(x= myoutputA$year_pair, y= myoutputA$MRS, group = 1), method = "lm") +
  geom_line(aes(x= myoutputA$year_pair, y= myoutputA$MRS, group = 1))+
  xlab("Year") +
  ylab("MRS")+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,5) 
SiteA
  
Site_B <- read.csv("SiteB.csv")
myoutputB <- rank_shift(Site_B,
                        time.var = "year",
                        species.var = "spname",
                        abundance.var = "CountOfsp")

myoutputB
SiteB <- myoutputB %>% 
  ggplot(aes(x= myoutputB$year_pair, y= myoutputB$MRS)) +
  geom_point() +
  geom_smooth(aes(x= myoutputB$year_pair, y= myoutputB$MRS, group = 1), method = "lm") +
  geom_line(aes(x= myoutputB$year_pair, y= myoutputB$MRS, group = 1))+
  xlab("Year") +
  ylab("MRS")+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,5) 
SiteB




Site_C <- read.csv("SiteC.csv")
myoutputC <- rank_shift(Site_C,
                        time.var = "year",
                        species.var = "spname",
                        abundance.var = "CountOfsp")
myoutputC
SiteC <- myoutputC %>% 
  ggplot(aes(x= myoutputC$year_pair, y= myoutputC$MRS)) +
  geom_point() +
  geom_smooth(aes(x= myoutputC$year_pair, y= myoutputC$MRS, group = 1), method = "lm")+
  geom_line(aes(x= myoutputC$year_pair, y= myoutputC$MRS), group = 1)+
  xlab("Year") +
  ylab("MRS")+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,5) 
SiteC



Site_D <- read.csv("SiteD.csv")
myoutputD <- rank_shift(Site_D,
                        time.var = "year",
                        species.var = "spname",
                        abundance.var = "CountOfsp")
myoutputD
SiteD <- myoutputD %>% 
  ggplot(aes(x= myoutputD$year_pair, y= myoutputD$MRS)) +
  geom_point() +
  geom_smooth(aes(x= myoutputD$year_pair, y= myoutputD$MRS, group = 1), method = "lm") +
  geom_line(aes(x= myoutputD$year_pair, y= myoutputD$MRS), group = 1) +
  xlab("Year") +
  ylab("MRS")+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,5) 
SiteD


Site_E <- read.csv("SiteE.csv")
myoutputE <- rank_shift(Site_E,
                        time.var = "year",
                        species.var = "spname",
                        abundance.var = "CountOfsp")
myoutputE
SiteE <- myoutputE %>% 
  ggplot(aes(x= myoutputE$year_pair, y= myoutputE$MRS)) +
  geom_point() +
  geom_smooth(aes(x= myoutputE$year_pair, y= myoutputE$MRS, group = 1), method = "lm") +
  geom_line(aes(x= myoutputE$year_pair, y= myoutputE$MRS), group = 1)+
  xlab("Year") +
  ylab("MRS")+
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,5) 
SiteE

Site_F <- read.csv("SiteF.csv")
myoutputF <- rank_shift(Site_F,
                        time.var = "year",
                        species.var = "spname",
                        abundance.var = "CountOfsp")
myoutputF
SiteF <- myoutputF %>% 
  ggplot(aes(x= myoutputF$year_pair, y= myoutputF$MRS)) +
  geom_point() +
  geom_line(aes(x= myoutputF$year_pair, y= myoutputF$MRS), group = 1)+
  geom_smooth(aes(x= myoutputF$year_pair, y= myoutputF$MRS, group = 1), method = "lm") +
  xlab("Year") +
  ylab("MRS") +
  theme(axis.text.x = element_text(angle = 90))+
  ylim(0,5) 
SiteF

MRSgraphs<- ggarrange(SiteA,SiteB,SiteC,SiteD, SiteE, SiteF, 
                        labels = c("A", "B", "C","D","E","F"),
                        ncol = 2, nrow = 3)
MRSgraphs

ggsave("MRSgraphs.pdf")


mrs <- data.table(myoutputA,myoutputB,myoutputC,myoutputD,myoutputE,myoutputF)
print(mrs)
mrs_table<- fwrite(mrs,"mrs_Table.csv")

mrs2 <- read.csv("mrs_Table2.csv")
mrs3 <- read.csv("mrs_Table3.csv")

?lme4
#lme4 provides functions for fitting and analyzing mixed models: linear (lmer), generalized linear (glmer) and nonlinear (nlmer.)

#lmer(formula, data = NULL, REML = TRUE, control = lmerControl(),
#start = NULL, verbose = 0L, subset, weights, na.action,
#offset, contrasts = NULL, devFunOnly = FALSE)

#Dependent variable = MRS, independent variable = year, random variable = site. See if this model is significant.

#lmer(dept~intercept?, REM=FALSE, data=)

mrs_model0 <- lmer(MRSValue~1+Site+(1|year_pair),data=mrs3,REML=FALSE)
summary(mrs_model0)
summ(mrs_model0)
ranova(mrs_model0)


mrs_model1 <- lmer(MRSValue~1+(1|year_pair),data=mrs3,REML=FALSE)
summary(mrs_model1)
summ(mrs_model1)
ranova(mrs_model1)


mrs_model2 <- lmer(MRSValue~year_pair+(1|Site),data=mrs3,REML=FALSE) #correct one? 
summary(mrs_model2)
summ(mrs_model2) #??
ranova(mrs_model2) #??







#TURNOVER/PP?

#turnover (df, time.var, species.var, abundance.var, replicate.var, metric)

myoutputA_T <-turnover(Site_A,
                        time.var = "year",
                        species.var = "spname",
                        abundance.var = "CountOfsp",
                        metric = "total")
myoutputA_T
SiteA_T <- myoutputA_T %>% 
  ggplot() +
  geom_point(aes(x = myoutputA_T$year, y= myoutputA_T$total)) +
  geom_smooth(aes(x= myoutputA_T$year, y= myoutputA_T$total), method="lm") +
  xlab("Year") +
  ylab("Species Turnover") +
  ylim(0,.8)
SiteA_T


myoutputB_T <-turnover(Site_B,
                       time.var = "year",
                       species.var = "spname",
                       abundance.var = "CountOfsp",
                       metric = "total")
myoutputB_T
SiteB_T <- myoutputB_T %>% 
  ggplot() +
  geom_point(aes(x = myoutputB_T$year, y= myoutputB_T$total)) +
  geom_smooth(aes(x= myoutputB_T$year, y= myoutputB_T$total), method="lm") +
  xlab("Year") +
  ylab("Species Turnover")+
  ylim(0,.8)
SiteB_T


myoutputC_T <-turnover(Site_C,
                       time.var = "year",
                       species.var = "spname",
                       abundance.var = "CountOfsp",
                       metric = "total")
myoutputC_T
SiteC_T <- myoutputC_T %>% 
  ggplot() +
  geom_point(aes(x = myoutputC_T$year, y= myoutputC_T$total)) +
  geom_smooth(aes(x= myoutputC_T$year, y= myoutputC_T$total), method="lm") +
  xlab("Year") +
  ylab("Species Turnover")+
  ylim(0,.8)
SiteC_T


myoutputD_T <-turnover(Site_D,
                       time.var = "year",
                       species.var = "spname",
                       abundance.var = "CountOfsp",
                       metric = "total")
myoutputD_T
SiteD_T <- myoutputD_T %>% 
  ggplot() +
  geom_point(aes(x = myoutputD_T$year, y= myoutputD_T$total)) +
  geom_smooth(aes(x= myoutputD_T$year, y= myoutputD_T$total), method="lm") +
  xlab("Year") +
  ylab("Species Turnover")+
  ylim(0,.8)
SiteD_T


myoutputE_T <-turnover(Site_E,
                       time.var = "year",
                       species.var = "spname",
                       abundance.var = "CountOfsp",
                       metric = "total")
myoutputE_T
SiteE_T <- myoutputE_T %>% 
  ggplot() +
  geom_point(aes(x = myoutputE_T$year, y= myoutputE_T$total)) +
  geom_smooth(aes(x= myoutputE_T$year, y= myoutputE_T$total), method="lm") +
  xlab("Year") +
  ylab("Species Turnover")+
  ylim(0,.8)
SiteE_T


myoutputF_T <-turnover(Site_F,
                       time.var = "year",
                       species.var = "spname",
                       abundance.var = "CountOfsp",
                       metric = "total")
myoutputF_T
SiteF_T <- myoutputF_T %>% 
  ggplot() +
  geom_point(aes(x = myoutputF_T$year, y= myoutputF_T$total)) +
  geom_smooth(aes(x= myoutputF_T$year, y= myoutputF_T$total), method="lm") +
  xlab("Year") +
  ylab("Species Turnover")+
  ylim(0,.8)
SiteF_T

Turnovergraphs<- ggarrange(SiteA_T,SiteB_T,SiteC_T,SiteD_T, SiteE_T, SiteF_T, 
                      labels = c("A", "B", "C","D","E","F"),
                      ncol = 2, nrow = 3)
Turnovergraphs

ggsave("Turnovergraphs.pdf")

