setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto/PutahData")
library(ggplot2)
library(tidyverse)
library(vegan)

Putah_abund <- read.csv("Putah_Vegan_abund.csv")
Putah_nvnn <- read.csv("Putah_Vegan_nvsnn.csv")
Putah_pres <- read.csv("Putah_Vegan_presab.csv")
Putah_nvnn2 <- read.csv("Putah_nvnn2.csv")

#native vs nonnative, number of species by year, at each site or group of sites

Fish <- read.csv("ShannonPutah.csv")

obs <- Fish[-c(1:2)]
obs[obs>1] <- 1
obs

Native <- Fish[Fish$]

NonNative <- Fish[Fish$]








head(Putah_nvnn2)
#Site A
SiteA_N <- rowSums(Putah_nvnn2[1:24,3:13]) 
SiteA_NN <- rowSums(Putah_nvnn2[1:24,14:37]) 
SiteA_Years <- Putah_nvnn2[1:24,2]



#Site B
SiteB_N <- rowSums(Putah_nvnn2[1:24,3:13]) 
SiteB_NN <- rowSums(Putah_nvnn2[1:24,14:37]) 
SiteB_Years <- Putah_nvnn2[1:24,2]

#Site C
SiteC_N <- rowSums(Putah_nvnn2[1:24,3:13]) 
SiteC_NN <- rowSums(Putah_nvnn2[1:24,14:37]) 
SiteC_Years <- Putah_nvnn2[1:24,2]

#Site D
head(Putah_nvnn2)
SiteD_N <- rowSums(Putah_nvnn2[1:24,3:13]) 
SiteD_NN <- rowSums(Putah_nvnn2[1:24,14:37]) 
SiteD_Years <- Putah_nvnn2[1:24,2]

#Site E 
SiteE_N <- rowSums(Putah_nvnn2[1:24,3:13]) 
SiteE_NN <- rowSums(Putah_nvnn2[1:24,14:37]) 
SiteE_Years <- Putah_nvnn2[1:24,2]

#Site F
SiteF_N <- rowSums(Putah_nvnn2[1:24,3:13]) 
SiteF_NN <- rowSums(Putah_nvnn2[1:24,14:37]) 
SiteF_Years <- Putah_nvnn2[1:24,2]
















#Putah2<- Putah %>% 
  #filter(SiteID %in% c("1","3","5")) %>% 
  #group_by(FishTypeID,Status) %>% 
  #count(Status,Year)

#ggplot(data = Putah, mapping = aes(x= Year, y= Putah1)) 

#ggplot(aes(x=Year, y=FishTypeID, color= Status) + geom_point()


#Putah_NvsNN_Upstream <- Putah_NvsNN %>% 
  #filter(SiteID %in% c("1","3","5")) %>% 
  #group_by(FishTypeID,Status,Year,SiteID) %>% 
  #count(FishID,Status,Year) %>% 
  #ggplot(aes(x=Year, y=FishTypeID) +
  #geom_point()
         
         
#Putah_NvsNN_Upstream <- Putah_NvsNN %>% 
#filter(SiteID %in% c("1","3","5"))
#summary(Putah_NvsNN_Upstream$FishTypeID)


