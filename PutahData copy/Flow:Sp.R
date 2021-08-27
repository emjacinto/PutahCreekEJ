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

Flow_NN <- merge(SpFlow,Map_csv, by="Year")

SiteA <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteA_N1),color = "blue3") +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteA_NN1), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteA_N1), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteA_NN1), color="orange3", method= "lm") +
  xlab("Mean Spring Flow") +
  ylab("Number of Species") +
  ylim(0,17)

SiteA
#add legend- N and NN
warnings()

SiteA2 <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteA_N1),color = "blue3") +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteA_NN1), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteA_N1), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteA_NN1), color="orange3", method= "lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Number of Species")+
  ylim(0,17)
  
SiteA2

# glm- general linear model Y~X
mod1 <- glm(SiteA_N1 ~Mean.spring.flow..Mar1.May31. , Flow_NN, family = "poisson")
summary(mod1) #p=0.824 

mod2 <-glm(SiteA_NN1 ~ Mean.spring.flow..Mar1.May31., Flow_NN, family = "poisson")
summary(mod2) #p=0.0896 

mod3 <- glm(SiteA_N1 ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod3) #p=0.57

mod4 <-glm(SiteA_NN1 ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod4) #p=0.01427 *

SiteB <-Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteB_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteB_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteB_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteB_NN), color="orange3", method= "lm") +
  xlab("Mean Spring Flow") +
  ylab("Number of Species") +
  ylim(0,17)

SiteB

SiteB2 <-Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteB_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteB_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteB_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteB_NN), color="orange3", method= "lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Number of Species")+
  ylim(0,17)
SiteB2


# glm- general linear model Y~X
mod1 <- glm(SiteB_N ~Mean.spring.flow..Mar1.May31. , Flow_NN, family = "poisson")
summary(mod1) #p= 0.676

mod2 <-glm(SiteB_NN ~ Mean.spring.flow..Mar1.May31., Flow_NN, family = "poisson")
summary(mod2) #p=0.768

mod3 <- glm(SiteB_N ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod3) #p=0.751

mod4 <-glm(SiteB_NN ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod4) #p=0.91 


SiteC <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x =Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteC_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteC_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteC_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteC_NN), color="orange3", method= "lm") +
  xlab("Mean Spring Flow") +
  ylab("Number of Species")+
  ylim(0,17)
SiteC

SiteC2 <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x =Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteC_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteC_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteC_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteC_NN), color="orange3", method= "lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Number of Species")+
  ylim(0,17)
SiteC2

# glm- general linear model Y~X
mod1 <- glm(SiteC_N ~Mean.spring.flow..Mar1.May31. , Flow_NN, family = "poisson")
summary(mod1) #p=0.627

mod2 <-glm(SiteC_NN ~ Mean.spring.flow..Mar1.May31., Flow_NN, family = "poisson")
summary(mod2) #p=0.644

mod3 <- glm(SiteC_N ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod3) #p=0.534

mod4 <-glm(SiteC_NN ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod4) #p=0.967


SiteD <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteD_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteD_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteD_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteD_NN), color="orange3", method= "lm") +
  xlab("Mean Spring Flow") +
  ylab("Number of Species")+
  ylim(0,17)
SiteD

SiteD2 <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteD_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteD_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteD_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteD_NN), color="orange3", method= "lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Number of Species")+
  ylim(0,17)
SiteD2

# glm- general linear model Y~X
mod1 <- glm(SiteD_N ~Mean.spring.flow..Mar1.May31. , Flow_NN, family = "poisson")
summary(mod1) #p=0.9

mod2 <-glm(SiteD_NN ~ Mean.spring.flow..Mar1.May31., Flow_NN, family = "poisson")
summary(mod2) #p=0.884 

mod3 <- glm(SiteD_N ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod3) #p= 0.946  

mod4 <-glm(SiteD_NN ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod4) #p=0.88


SiteE <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteE_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteE_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteE_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteE_NN), color="orange3", method= "lm") +
  xlab("Mean Spring Flow") +
  ylab("Number of Species")+
  ylim(0,17)
SiteE

SiteE2 <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteE_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteE_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteE_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteE_NN), color="orange3", method= "lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Number of Species")+
  ylim(0,17)
SiteE2

# glm- general linear model Y~X
mod1 <- glm(SiteE_N ~Mean.spring.flow..Mar1.May31. , Flow_NN, family = "poisson")
summary(mod1) #p= 0.1493

mod2 <-glm(SiteE_NN ~ Mean.spring.flow..Mar1.May31., Flow_NN, family = "poisson")
summary(mod2) #p=0.347 

mod3 <- glm(SiteE_N ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod3) #p=0.458

mod4 <-glm(SiteE_NN ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod4) #p= 0.349  


SiteF <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteF_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteF_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteF_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$Mean.spring.flow..Mar1.May31., y= Flow_NN$SiteF_NN), color="orange3", method= "lm") +
  xlab("Mean Spring Flow") +
  ylab("Number of Species")+
  ylim(0,17)
SiteF

SiteF2 <- Flow_NN %>% 
  ggplot() +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteF_N),color = "blue3") +
  geom_point(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteF_NN), color = "orange3") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteF_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Flow_NN$log.spring.discharge...1., y= Flow_NN$SiteF_NN), color="orange3", method= "lm") +
  xlab("Log(mean discharge + 1)") +
  ylab("Number of Species")+
  ylim(0,17)
SiteF2


# glm- general linear model Y~X
mod1 <- glm(SiteF_N ~Mean.spring.flow..Mar1.May31. , Flow_NN, family = "poisson")
summary(mod1) #p=0.988

mod2 <-glm(SiteF_NN ~ Mean.spring.flow..Mar1.May31., Flow_NN, family = "poisson")
summary(mod2) #p= 0.952  

mod3 <- glm(SiteF_N ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod3) #p=0.657

mod4 <-glm(SiteF_NN ~ log.spring.discharge...1., Flow_NN, family = "poisson")
summary(mod4) #p=0.848 


NvsNNgraphs2<- ggarrange(SiteA,SiteB,SiteC,SiteD, SiteE, SiteF, 
                        labels = c("A", "B", "C","D","E","F"),
                        ncol = 2, nrow = 3)


NvsNNgraphs2
ggsave("NvsNNgraphs_FLOW1.pdf")


NvsNNgraphs3<- ggarrange(SiteA2,SiteB2,SiteC2,SiteD2, SiteE2, SiteF2, 
                         labels = c("A", "B", "C","D","E","F"),
                         ncol = 2, nrow = 3)


NvsNNgraphs3

ggsave("NvsNNgraphs_FLOW2.pdf")
