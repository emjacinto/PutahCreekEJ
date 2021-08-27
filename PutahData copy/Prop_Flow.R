setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto/PutahData")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(vegan)
library(dplyr)
library(data.table)
library(ggpubr)

SpFlow <- read.csv("Flow_from_dam_spring.csv")
Map_csv <- read.csv("NvsNN_table2.csv")
prop <- read.csv("Proportion_allsites.csv")
#add proportions from abundance, combine all these files

Flow_NN <- merge(SpFlow, prop, by="Year")

SiteA <- Flow_NN %>% #Year
  ggplot() +
  geom_point(aes(x = Flow_NN$Year, y= Flow_NN$SiteA),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$Year, y= Flow_NN$SiteA), color="blue3", method="lm") +
  xlab("Year") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteA

SiteA2 <- Flow_NN %>% #Spring Flow
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteA),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteA), color="blue3", method="lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteA2


# glm- general linear model Y~X
#year
mod1 <- lm(SiteA ~ Year , Flow_NN)
summary(mod1) #p=.0250 *

mod2 <- lm(SiteA ~ log.spring.discharge...1., Flow_NN)
summary(mod2) #p= .0662



SiteB <- Flow_NN %>% #Year
  ggplot() +
  geom_point(aes(x = Flow_NN$Year, y= Flow_NN$SiteB),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$Year, y= Flow_NN$SiteB), color="blue3", method="lm") +
  xlab("Year") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteB

SiteB2 <- Flow_NN %>% #Spring Flow
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteB),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteB), color="blue3", method="lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteB2

# glm- general linear model Y~X
mod1B <- lm(SiteB ~ Year , Flow_NN)
summary(mod1) #p= 0.000363 ***

mod3B <- lm(SiteB ~ log.spring.discharge...1., Flow_NN)
summary(mod3) #p= .66



SiteC <- Flow_NN %>% #Year
  ggplot() +
  geom_point(aes(x = Flow_NN$Year, y= Flow_NN$SiteC),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$Year, y= Flow_NN$SiteC), color="blue3", method="lm") +
  xlab("Year") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteC

SiteC2 <- Flow_NN %>% #Spring Flow
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteC),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteC), color="blue3", method="lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteC2

# glm- general linear model Y~X
mod1C <- lm(SiteC ~ Year , Flow_NN)
summary(mod1C) #p=2.5e-06 ***

mod3C <-lm(SiteC ~ log.spring.discharge...1., Flow_NN)
summary(mod3C) #p=0.383



SiteD <- Flow_NN %>% #Year
  ggplot() +
  geom_point(aes(x = Flow_NN$Year, y= Flow_NN$SiteD),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$Year, y= Flow_NN$SiteD), color="blue3", method="lm") +
  xlab("Year") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteD

SiteD2 <- Flow_NN %>% #Spring Flow
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteD),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteD), color="blue3", method="lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteD2

# glm- general linear model Y~X
mod1D <- lm(SiteD ~ Year , Flow_NN)
summary(mod1D) #p= 0.000152 ***

mod3D <- lm(SiteD ~ log.spring.discharge...1., Flow_NN)
summary(mod3D) #p= 0.638025 



SiteE <- Flow_NN %>% #Year
  ggplot() +
  geom_point(aes(x = Flow_NN$Year, y= Flow_NN$SiteE),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$Year, y= Flow_NN$SiteE), color="blue3", method="lm") +
  xlab("Year") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteE

SiteE2 <- Flow_NN %>% #Spring Flow
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteE),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteE), color="blue3", method="lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteE2

# glm- general linear model Y~X
mod1E <- lm(SiteE ~ Year , Flow_NN)
summary(mod1E) #p=0.245

mod3E <- lm(SiteE ~ log.spring.discharge...1., Flow_NN)
summary(mod3E) #p=0.0889



SiteF <- Flow_NN %>% #Year
  ggplot() +
  geom_point(aes(x = Flow_NN$Year, y= Flow_NN$SiteF),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$Year, y= Flow_NN$SiteF), color="blue3", method="lm") +
  xlab("Year") +
  ylab("Proportion of Native Species")+
  ylim(0,1) 
SiteF

SiteF2 <- Flow_NN %>% #Spring Flow
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteF),color = "blue3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteF), color="blue3", method="lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Proportion of Native Species") +
  ylim(0,1) 
SiteF2

# glm- general linear model Y~X
mod1F <- lm(SiteF ~ Year , Flow_NN)
summary(mod1F) #p= 0.0130 *

mod3F <- lm(SiteF ~ log.spring.discharge...1., Flow_NN)
summary(mod3F) #p= 0.363229




propgraphs1<- ggarrange(SiteA,SiteB,SiteC,SiteD, SiteE, SiteF, 
                         labels = c("A", "B", "C","D","E","F"),
                         ncol = 3, nrow = 2)
propgraphs1
ggsave("propgraphs1.pdf")




propgraphs2<- ggarrange(SiteA2,SiteB2,SiteC2,SiteD2, SiteE2, SiteF2, 
                         labels = c("A", "B", "C","D","E","F"),
                         ncol = 3, nrow = 2)

propgraphs2
ggsave("propgraphs2.pdf")

