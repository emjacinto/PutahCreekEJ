#nvnn graphs 2

setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto/PutahData")
library(ggplot2)
library(tidyverse)
library(vegan)
library(dplyr)
library(data.table)
library(ggpubr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library('compute.es')
library('effects')
library('emmeans')
library('multcomp')
library(mvtnorm)
library(survival)
library(TH.data)

#native vs nonnative, number of species by year, at each site or group of sites

Map_csv <- read.csv("NvsNN_table2.csv")
Map_csv2 <- read.csv("NvsNN_table3.csv")

SiteA <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Year, y= SiteA_N1), color = "blue3", size=3, alpha=.7) +
  geom_point(aes(x = Year, y= SiteA_NN1), color = "orange3", size=3, alpha=.7) +
  geom_smooth(aes(x = Year, y= SiteA_N1), color="blue3", method="lm") +
  geom_smooth(aes(x = Year, y= SiteA_NN1), color="orange3", method= "lm", fullrange=TRUE) +
  geom_vline(xintercept = 2000, size=1) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0), limits=c(-50, 18)) +
  scale_x_continuous(breaks=seq(1993,2017,1), expand=c(0,0), limits=c(1993,2017))+
  coord_cartesian(xlim=c(1993,2017), ylim=c(0,18))

SiteA <- SiteA + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
                  theme(axis.text.x = element_text(angle=90))+
                  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteA <- SiteA + annotate("text", x = 2012 , y = 16, label = "Test of Slopes P = 0.0013", size=3)
SiteA



SiteB <-Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Year, y= SiteB_N),color = "blue3", size=3, alpha=.7) +
  geom_point(aes(x = Year, y= SiteB_NN), color = "orange3", size=3, alpha=.7) +
  geom_smooth(aes(x = Year, y= SiteB_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Year, y= SiteB_NN), color="orange3", method= "lm") +
  geom_vline(xintercept = 2000, size=1) +
  ylim(0,17) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0), limits=c(-50,18)) +
  scale_x_continuous(breaks=seq(1993,2017,1), expand=c(0,0), limits=c(1993,2017))+
  coord_cartesian(xlim=c(1993,2017), ylim=c(0,18))

SiteB <- SiteB + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteB <- SiteB + annotate("text", x = 2012 , y = 16, label = "Test of Slopes P < 0.0001", size=3)
SiteB


SiteC <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Year, y=SiteC_N),color = "blue3", size=3, alpha=.7) +
  geom_point(aes(x = Year, y=SiteC_NN), color = "orange3", size=3, alpha=.7) +
  geom_smooth(aes(x = Year, y= SiteC_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Year, y= SiteC_NN), color="orange3", method= "lm") +
  geom_vline(xintercept = 2000, size=1) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0), limits=c(-50, 18)) +
  scale_x_continuous(breaks=seq(1993,2017,1), expand=c(0,0), limits=c(1993,2017))+
  coord_cartesian(xlim=c(1993,2017), ylim=c(0,18))

SiteC <- SiteC + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteC <- SiteC + annotate("text", x = 2012 , y = 16, label = "Test of Slopes P < 0.0001", size=3)
SiteC



SiteD <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x =Year, y= SiteD_N),color = "blue3",size=3, alpha=.7) +
  geom_point(aes(x = Year, y= SiteD_NN), color = "orange3", size=3, alpha=.7) +
  geom_smooth(aes(x = Year, y= SiteD_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Year, y= SiteD_NN), color="orange3", method= "lm") +
  geom_vline(xintercept = 2000, size=1) +
  ylim(0,17) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0), limits=c(-50, 18)) +
  scale_x_continuous(breaks=seq(1993,2017,1), expand=c(0,0), limits=c(1993,2017))+
  coord_cartesian(xlim=c(1993,2017), ylim=c(0,18))

SiteD <- SiteD + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteD <- SiteD + annotate("text", x = 2012 , y = 16, label = "Test of Slopes P = 0.03", size=3) 
SiteD


SiteE <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Year, y= SiteE_N),color = "blue3", size=3, alpha=.7) +
  geom_point(aes(x = Year, y= SiteE_NN), color = "orange3", size=3, alpha=.7) +
  geom_smooth(aes(x = Year, y= SiteE_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Year, y= SiteE_NN), color="orange3", method= "lm") +
  geom_vline(xintercept = 2000, size=1) +
  ylim(0,17) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0), limits=c(-50, 18)) +
  scale_x_continuous(breaks=seq(1993,2017,1), expand=c(0,0), limits=c(1993,2017))+
  coord_cartesian(xlim=c(1993,2017), ylim=c(0,18))

SiteE <- SiteE + theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteE <- SiteE + annotate("text", x = 2012 , y = 16, label = "Test of Slopes P = 0.73", size=3)  
SiteE


SiteF <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Year, y= SiteF_N),color = "blue3", size=3, alpha=.7) +
  geom_point(aes(x = Year, y= SiteF_NN), color = "orange3", size=3, alpha=.7) +
  geom_smooth(aes(x = Year, y= SiteF_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Year, y= SiteF_NN), color="orange3", method= "lm") +
  geom_vline(xintercept = 2000, size=1) +
  ylim(0,17) +
  theme_classic() +
  scale_y_continuous(expand=c(0,0), limits=c(-50, 18)) +
  scale_x_continuous(breaks=seq(1993,2017,1), expand=c(0,0), limits=c(1993,2017))+
  coord_cartesian(xlim=c(1993,2017), ylim=c(0,18))

SiteF <- SiteF+ theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(axis.text.x = element_text(angle=90))+
  theme(plot.margin = unit(c(1,1,1,1), "lines"))
SiteF <- SiteF + annotate("text", x = 2012 , y = 16, label = "Test of Slopes P = 0.66", size=3)
SiteF


NvsNNgraphs2<- ggarrange(SiteA,SiteB,SiteC,SiteD, SiteE, SiteF, 
                        labels = c("Site A", "Site B", "Site C", "Site D","Site E","Site F"),
                        font.label = list(size = 12, color = "black", face = "plain"),
                        label.x = .05,
                        label.y = 1,
                        heights = c(1),
                        widths = c(2.5),
                        ncol = 2, nrow = 3)

NvsNNgraphs2 <-annotate_figure(NvsNNgraphs2,
                bottom = text_grob("Year", size = 14),
                left = text_grob("Number of Species", rot = 90, size = 14))

NvsNNgraphs2

ggsave("NvsNNgraphs2.jpeg")



#ANCOVA Analysis 2

#The model y ~ x + g would fit a main effects ANCOVA model, 
#while y ~ x * g would fit a model which includes interaction with the covariate.

#Model with interaction between categorical variable and predictor variable
#Create regression model1

Map_csvA <- Map_csv2 %>% 
  mutate(log10(SiteA+1))

mod1A <- aov(`log10(SiteA + 1)`~ Year*SiteA_Type, Map_csvA)
summary(mod1A)

mod2A <- aov(`log10(SiteA + 1)`~ Year*SiteA_Type*Pre.Post, Map_csvA)
summary(mod2A)
 
#test of the slopes of native/non pre accord and then the same test post. 
#This is called a “posthoc test” that you have to activate in the model. 
#You can use a Tukey’s test for this.

#model = aov(dv ~ factor + covariate + factor:covariate, data=df)

PreA <- subset(Map_csvA, Pre.Post==1)
PreA$SiteA_Type <- as.factor(PreA$SiteA_Type)
Mod3A <- aov(`log10(SiteA + 1)`~ Year*SiteA_Type, data=PreA)
summary(Mod3A)
posthA1 <- glht(Mod3A, linfct= mcp(SiteA_Type='Tukey'))
summary(posthA1)

PostA <- subset(Map_csvA, Pre.Post==2)
PostA$SiteA_Type <- as.factor(PostA$SiteA_Type)
Mod4A <- aov(`log10(SiteA + 1)`~ SiteA_Type, data=PostA)
summary(Mod4A)
posthA2 <- glht(Mod4A, linfct= mcp(SiteA_Type='Tukey'))
summary(posthA2)




#B
Map_csvB <- Map_csv2 %>% 
  mutate(log10(SiteB+1))

mod1B <- aov(`log10(SiteB + 1)`~ Year*SiteB_Type, Map_csvB)
summary(mod1B)

mod2B <- aov(`log10(SiteB + 1)`~ Year*SiteB_Type*Pre.Post, Map_csvB)
summary(mod2B)

#C
Map_csvC <- Map_csv2 %>% 
  mutate(log10(SiteC+1))

mod1C <- aov(`log10(SiteC + 1)`~ Year*SiteC_Type, Map_csvC)
summary(mod1C)

mod2C <- aov(`log10(SiteC + 1)`~ Year*SiteC_Type*Pre.Post, Map_csvC)
summary(mod2C)

#D
Map_csvD <- Map_csv2 %>% 
  mutate(log10(SiteD+1))

mod1D <- aov(`log10(SiteD + 1)`~ Year*SiteD_Type, Map_csvD)
summary(mod1D)

mod2D <- aov(`log10(SiteD + 1)`~ Year*SiteD_Type*Pre.Post, Map_csvD)
summary(mod2D)

#E
Map_csvE <- Map_csv2 %>% 
  mutate(log10(SiteE+1))

mod1E <- aov(`log10(SiteE + 1)`~ Year*SiteE_Type, Map_csvE)
summary(mod1E)

mod2E <- aov(`log10(SiteE + 1)`~ Year*SiteE_Type*Pre.Post, Map_csvE)
summary(mod2E)

#F
Map_csvF <- Map_csv2 %>% 
  mutate(log10(SiteF+1))

mod1F <- aov(`log10(SiteF + 1)`~ Year*SiteF_Type, Map_csvF)
summary(mod1F)

mod2F <- aov(`log10(SiteF + 1)`~ Year*SiteF_Type*Pre.Post, Map_csvF)
summary(mod2F)




