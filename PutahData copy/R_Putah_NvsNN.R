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

#native vs nonnative, number of species by year, at each site or group of sites

Fish <- read.csv("ShannonPutah.csv")

obs <- Fish[-c(1:2)]
obs[obs>1] <- 1
obs

Native <- data.frame(obs$CHN, obs$COT, obs$PKM, obs$PLR, obs$RBT, obs$RCH, obs$SAP,obs$SBF,obs$SBK, obs$SKR, obs$TUP)

NonNative <- data.frame(obs$BBH, obs$BCR, obs$BGS, obs$BLP, obs$BRB, obs$CCF, obs$CRP, obs$FHM,obs$GLF, obs$GSF, obs$GSH, obs$ISS, obs$LEP, obs$LMB, obs$MSQ, obs$PMK, obs$RES, obs$RSH, obs$SMB, obs$SPB, obs$STB, obs$WCF, obs$WRM, obs$YFG)

#Site A
SiteA_N1 <- rowSums(Native[1:24,]) 
SiteA_N2 <- rowSums(Putah_nvnn2[1:24,3:13])

SiteA_NN1 <- rowSums(NonNative[1:24,]) 
SiteA_Years <- Fish[1:24,2]



#Site B
SiteB_N <- rowSums(Native[25:48,]) 
SiteB_NN <- rowSums(NonNative[25:48,]) 
SiteB_Years <- Fish[25:48,2]

#Site C
SiteC_N <- rowSums(Native[49:72,]) 
SiteC_NN <- rowSums(NonNative[49:72,]) 
SiteC_Years <- Fish[49:72,2]

#Site D
SiteD_N <- rowSums(Native[73:96,]) 
SiteD_NN <- rowSums(NonNative[73:96,]) 
SiteD_Years <- Fish[73:96,2]


#Site E 
SiteE_N <- rowSums(Native[97:120,]) 
SiteE_NN <- rowSums(NonNative[97:120,]) 
SiteE_Years <- Fish[97:120,2]


#Site F 
SiteF_N <- rowSums(Native[121:144,]) 
SiteF_NN <- rowSums(NonNative[121:144,]) 
SiteF_Years <- Fish[121:144,2]



NvsNN_Table <- data.table(SiteA_N1, SiteA_NN1, SiteB_N, SiteB_NN, SiteC_N, SiteC_NN, SiteD_N, SiteD_NN, SiteE_N, SiteE_NN, SiteF_N, SiteF_NN)

print(NvsNN_Table)

NvsNN_table<- fwrite(NvsNN_Table,"NvsNN_Table.csv")


Map_csv <- read.csv("NvsNN_table2.csv")
Map_csv2 <- read.csv("NvsNN_table3.csv")

SiteA <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_N1),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_NN1), color = "orange3") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteA_N1), color="blue3", method="lm") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteA_NN1), color="orange3", method= "lm") +
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,17) +
  xlim(1993,2017)

SiteA


#glm- general linear model Y~X
mod1 <- glm(SiteA_N1 ~ Year, Map_csv, family = "poisson")
summary(mod1) #not sig = .999
mod2 <-glm(SiteA_NN1 ~ Year, Map_csv, family = "poisson")
summary(mod2) #p=.000465***

## lm- general linear model Y~X
mod1lmA<- lm(SiteA_N1 ~ Year, Map_csv)
summary(mod1lmA) #not sig = .998
mod2lmB <-lm(SiteA_NN1 ~ Year, Map_csv)
summary(mod2lmB) #p=.000482***

##ANCOVA 
#Dependent variable = log10 species richness, 
#Independent variable = year, 
#Categorical Variable = native/non
#aov(dependent~independent*factor, data= dataframe) interaction between the two (:) stop here if not sig
#aov(dependent~independent+factor, data= dataframe) factor sign, intercept is sig different, from factor line

#test normality- The null hypothesis of these tests is that “sample distribution is normal”. If the test is significant, the distribution is non-normal

Map_csv2 %>% 
  group_by(SiteA_Type) %>% 
  shapiro_test(SiteA) #nonnormal
##levene_test(SiteA ~ year, Map_csv2)  


#LOG
Map_csvA <- Map_csv2 %>% 
  mutate(log10(SiteA+1))



#ANCOVA Analysis

#The model y ~ x + g would fit a main effects ANCOVA model, 
#while y ~ x * g would fit a model which includes interaction with the covariate.

#Model with interaction between categorical variable and predictor variable
#Create regression model1

mod1A <- aov(`log10(SiteA + 1)`~ Year*SiteA_Type, Map_csvA)
summary(mod1A)


#This result shows that both horse power and transmission type has significant effect on miles per gallon as the p-value in both cases is less than 0.05 (top 2 values in example). But the interaction between these two variables is not significant as the p-value is more than 0.05.


#Model without interaction between categorical variable and predictor variable
#Create the regression model2
mod2A <- aov(`log10(SiteA + 1)`~ Year+SiteA_Type, Map_csvA)
summary(mod2A)


#ancova<-lm(Response variable~Explanatory variable 1*Explanatory variable 2, data=DATA), 2 way, same as first 
#mod <- lm(`log10(SiteA + 1)`~ Year*SiteA_Type, Map_csvA)
#summary(mod)
#anova(mod)

#compare models
anova(mod1A,mod2A)

#As the p-value is greater than 0.05 we conclude that the interaction between horse power and transmission type is not significant.


##3
#however, if we are trying to run an ANCOVA, type I errors will lead to wrong results and we instead need to use type III errors. 
#mod4A <- aov(`log10(SiteA + 1)`~ Year+SiteA_Type, Map_csvA)
#Anova(mod4A, type="III")


##4
##Analysis of covariance example and type II sum of squares##
#model.1 <- lm(`log10(SiteA + 1)`~ Year+SiteA_Type + Year:SiteA_Type, data = Map_csvA)
#model.1 <- Anova(model.1, type="II")
#summary(model.1)

#model.2 <- lm(`log10(SiteA + 1)`~ Year+SiteA_Type , data = Map_csvA)
#model.2 <- Anova(model.2, type="II")
#summary(model.2)






SiteB <-Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteB_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteB_NN), color = "orange3") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteB_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteB_NN), color="orange3", method= "lm") +
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,17) +
  xlim(1993,2017)
SiteB


####MODELS
# glm- general linear model Y~X
mod1B <- glm(SiteB_N ~ Year, Map_csv, family = "poisson")
summary(mod1B) #not sig = .203
mod2B <-glm(SiteB_NN ~ Year, Map_csv, family = "poisson")
summary(mod2B) #p=1.67e-8 ***

## lm- general linear model Y~X
mod1lmB<- lm(SiteB_N ~ Year, Map_csv)
summary(mod1lmB) #p=0.0142
mod2lmB <-lm(SiteB_NN ~ Year, Map_csv)
summary(mod2lmB) #p=9.53e-06

#LOG
Map_csvB <- Map_csv2 %>% 
  mutate(log10(SiteB+1))


#ANCOVA
#Model with interaction between categorical variable and predictor variable
#Create regression model1

mod1B <- aov(`log10(SiteB + 1)`~ Year*SiteB_Type, Map_csvB)
summary(mod1B)

#Model without interaction between categorical variable and predictor variable
#Create the regression model2
mod2B <- aov(`log10(SiteB + 1)`~ Year+SiteB_Type, Map_csvB)
summary(mod2B)

#compare models
anova(mod1B,mod2B)


SiteC <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteC_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteC_NN), color = "orange3") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteC_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteC_NN), color="orange3", method= "lm") +
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,17) +
  xlim(1993,2017)
SiteC



###MODELS
# glm- general linear model Y~X
mod1C <- glm(SiteC_N ~ Year, Map_csv, family = "poisson")
summary(mod1C) #not sig = .817
mod2C <-glm(SiteC_NN ~ Year, Map_csv, family = "poisson")
summary(mod2C) #p=6.16e-6***

## lm- general linear model Y~X
mod1lmC<- lm(SiteC_N ~ Year, Map_csv)
summary(mod1lmC) #p=0.641
mod2lmC <-lm(SiteC_NN ~ Year, Map_csv)
summary(mod2lmC) #p= 5.99e-08

#LOG
Map_csvC <- Map_csv2 %>% 
  mutate(log10(SiteC+1))


#ANCOVA
#Model with interaction between categorical variable and predictor variable
#Create regression model1

mod1C <- aov(`log10(SiteC + 1)`~ Year*SiteC_Type, Map_csvC)
summary(mod1C)

#Model without interaction between categorical variable and predictor variable
#Create the regression model2
mod2C <- aov(`log10(SiteC + 1)`~ Year+SiteC_Type, Map_csvC)
summary(mod2C)

#compare models
anova(mod1C,mod2C)




SiteD <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteD_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteD_NN), color = "orange3") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteD_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteD_NN), color="orange3", method= "lm") +
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,17) +
  xlim(1993,2017)
SiteD

###MODELS
mod1D <- glm(SiteD_N ~ Year, Map_csv, family = "poisson")
summary(mod1D) #not sig = .749
mod2D <-glm(SiteD_NN ~ Year, Map_csv, family = "poisson")
summary(mod2D) #p=.137
## lm- general linear model Y~X
mod1lmD<- lm(SiteD_N ~ Year, Map_csv)
summary(mod1lmD) #p= 0.358
mod2lmD <-lm(SiteD_NN ~ Year, Map_csv)
summary(mod2lmD ) #p=0.0336

#LOG
Map_csvD <- Map_csv2 %>% 
  mutate(log10(SiteD+1))


#ANCOVA
#Model with interaction between categorical variable and predictor variable
#Create regression model1

mod1D <- aov(`log10(SiteD + 1)`~ Year*SiteD_Type, Map_csvD)
summary(mod1D)

#Model without interaction between categorical variable and predictor variable
#Create the regression model2
mod2D <- aov(`log10(SiteD + 1)`~ Year+SiteD_Type, Map_csvD)
summary(mod2D)

#compare models
anova(mod1D,mod2D)












SiteE <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteE_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteE_NN), color = "orange3") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteE_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteE_NN), color="orange3", method= "lm") +
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,17) +
  xlim(1993,2017)
SiteE


###MODELS
mod1E <- glm(SiteE_N ~ Year, Map_csv, family = "poisson")
summary(mod1E) #not sig = .385
mod2E <-glm(SiteE_NN ~ Year, Map_csv, family = "poisson")
summary(mod2E) #p=.379

## lm- general linear model Y~X
mod1lmE<- lm(SiteE_N ~ Year, Map_csv)
summary(mod1lmE) #p=0.307
mod2lmE <-lm(SiteE_NN ~ Year, Map_csv)
summary(mod2lmE) #p= 0.138

#LOG
Map_csvE <- Map_csv2 %>% 
  mutate(log10(SiteE+1))


#ANCOVA
#Model with interaction between categorical variable and predictor variable
#Create regression model1

mod1E <- aov(`log10(SiteE + 1)`~ Year*SiteE_Type, Map_csvE)
summary(mod1E)

#Model without interaction between categorical variable and predictor variable
#Create the regression model2
mod2E <- aov(`log10(SiteE + 1)`~ Year+SiteE_Type, Map_csvE)
summary(mod2E)

#compare models
anova(mod1E,mod2E)









SiteF <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteF_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteF_NN), color = "orange3") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteF_N), color="blue3", method="lm") +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteF_NN), color="orange3", method= "lm") +
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,17) +
  xlim(1993,2017)
SiteF

##MODELS
mod1F <- glm(SiteF_N ~ Year, Map_csv, family = "poisson")
summary(mod1F) #not sig = .969
mod2F <-glm(SiteF_NN ~ Year, Map_csv, family = "poisson")
summary(mod2F) #p=.347

## lm- general linear model Y~X
mod1lmF<- lm(SiteF_N ~ Year, Map_csv)
summary(mod1lmF) #p=0.960
mod2lmF <-lm(SiteF_NN ~ Year, Map_csv)
summary(mod2lmF ) #p=0.211

#LOG
Map_csvF <- Map_csv2 %>% 
  mutate(log10(SiteF+1))


#ANCOVA
#Model with interaction between categorical variable and predictor variable
#Create regression model1

mod1F <- aov(`log10(SiteF + 1)`~ Year*SiteF_Type, Map_csvF)
summary(mod1F)

#Model without interaction between categorical variable and predictor variable
#Create the regression model2
mod2F <- aov(`log10(SiteF + 1)`~ Year+SiteF_Type, Map_csvF)
summary(mod2F)

#compare models
anova(mod1F,mod2F)









NvsNNgraphs<- ggarrange(SiteA,SiteB,SiteC,SiteD, SiteE, SiteF, 
                      labels = c("A", "B", "C","D","E","F"),
                      ncol = 2, nrow = 3)
NvsNNgraphs

ggsave("NvsNNgraphs1.pdf")







 
#Species Richness

SiteA2 <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_N1 + SiteA_NN1)) +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteA_N1 + SiteA_NN1), method="lm") +
  xlab("Year") +
  ylab("Number of Species")+
  xlim(1993,2017) +
  ylim(0,20)
SiteA2

SiteB2 <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteB_N + SiteB_NN)) +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteB_N + SiteB_NN), method="lm") +
  xlab("Year") +
  ylab("Number of Species")+
  xlim(1993,2017)+
  ylim(0,20)
SiteB2

SiteC2 <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteC_N + SiteC_NN)) +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteC_N + SiteC_NN), method="lm") +
  xlab("Year") +
  ylab("Number of Species")+
  xlim(1993,2017)+
  ylim(0,20)
SiteC2

SiteD2 <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteD_N + SiteD_NN)) +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteD_N + SiteD_NN), method="lm") +
  xlab("Year") +
  ylab("Number of Species")+
  xlim(1993,2017)+
  ylim(0,20)
SiteD2

SiteE2 <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteE_N + SiteE_NN)) +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteE_N + SiteE_NN), method="lm") +
  xlab("Year") +
  ylab("Number of Species")+
  xlim(1993,2017)+
  ylim(0,20)
SiteE2

SiteF2 <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteF_N + SiteF_NN)) +
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteF_N + SiteF_NN), method="lm") +
  xlab("Year") +
  ylab("Number of Species")+
  xlim(1993,2017)+
  ylim(0,20)
SiteF2



SPECIESRICH<- ggarrange(SiteA2,SiteB2,SiteC2,SiteD2, SiteE2, SiteF2, 
                        labels = c("A", "B", "C","D","E","F"),
                        ncol = 2, nrow = 3)

SPECIESRICH

ggsave("SPRICH.pdf")



#Collecting Sites into categories


#up and down, these just add the species
UpStreamB <-Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_N1+ Map_csv$SiteB_N+ Map_csv$SiteC_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_NN1+ Map_csv$SiteB_NN+Map_csv$SiteC_NN), color = "orange3") +
  geom_smooth(aes(Map_csv$Year, y= Map_csv$SiteA_N1+ Map_csv$SiteB_N+ Map_csv$SiteC_N), color="blue3", method="lm")+
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteA_NN1+ Map_csv$SiteB_NN+ Map_csv$SiteC_NN), color="orange3", method= "lm")+
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,35) +
  xlim(1993,2017)
UpStreamB  


#upstream mean
UpStream <-Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_N1),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_NN1), color = "orange3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteB_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteB_NN), color = "orange3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteC_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteC_NN), color = "orange3") +
  geom_smooth(aes(Map_csv$Year, y= Map_csv$Meanup_N), color="blue3", method="lm")+
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$Meanup_NN), color="orange3", method= "lm")+
  xlab("Year") +
  ylab("Number of Species")+
  xlim(1993,2017)+
  ylim(0,18)
UpStream


#Downstream mean
DownStream <-Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteD_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteD_NN), color = "orange3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteE_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteE_NN), color = "orange3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteF_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteF_NN), color = "orange3") +
  geom_smooth(aes(Map_csv$Year, y= Map_csv$Meandown_N), color="blue3", method="lm")+
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$Meandown_NN), color="orange3", method= "lm")+
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,18) +
  xlim(1993,2017)

DownStream


NvsNNgraphs2<- ggarrange(UpStream,DownStream,
                        labels = c("Upstream", "Downstream"),
                        ncol = 1, nrow = 2)
NvsNNgraphs2

ggsave("NvsNNgraphs2.pdf")








#up/mid/down - adding species (not using for now)

UpStream2 <- Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_N1+ Map_csv$SiteB_N), color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteA_NN1+ Map_csv$SiteB_NN), color = "orange3") +
  geom_smooth(aes(Map_csv$Year, y= Map_csv$SiteA_N1+ Map_csv$SiteB_N), color="blue3", method="lm")+
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteA_NN1+ Map_csv$SiteB_NN), color="orange3", method= "lm")+
  xlab("Year") +
  ylab("Number of Species") + 
  ylim(0,25) +
  xlim(1993,2017)
UpStream2

MidStream <-Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteC_N + Map_csv$SiteD_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteC_NN+ Map_csv$SiteD_NN), color = "orange3") +
  geom_smooth(aes(Map_csv$Year, y= Map_csv$SiteC_N+ Map_csv$SiteD_N), color="blue3", method="lm")+
  geom_smooth(aes(x = Map_csv$Year, y=Map_csv$SiteC_NN+Map_csv$SiteD_NN), color="orange3", method= "lm")+
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,25) +
  xlim(1993,2017)
MidStream

DownStream2 <-Map_csv %>% 
  ggplot() +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteE_N+ Map_csv$SiteF_N),color = "blue3") +
  geom_point(aes(x = Map_csv$Year, y= Map_csv$SiteE_NN+Map_csv$SiteF_NN), color = "orange3") +
  geom_smooth(aes(Map_csv$Year, y= Map_csv$SiteE_N+ Map_csv$SiteF_N), color="blue3", method="lm")+
  geom_smooth(aes(x = Map_csv$Year, y= Map_csv$SiteE_NN+ Map_csv$SiteF_NN), color="orange3", method= "lm")+
  xlab("Year") +
  ylab("Number of Species")+
  ylim(0,25) +
  xlim(1993,2017)
DownStream2


NvsNNgraphs3 <- ggarrange(UpStream2,MidStream,DownStream2,labels = c("Upstream","Midstream", "Downstream"),
ncol = 1, nrow = 3)

NvsNNgraphs3

ggsave("NvsNNgraphs3.pdf")


SpFlow <- read.csv("Flow_from_dam_spring.csv")
