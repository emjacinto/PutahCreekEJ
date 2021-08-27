# Correlation Coefficients table 2- Abundance 

setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto")
library(ggplot2)
library(tidyverse)
library(vegan)
library(dplyr)
library(data.table)
library(ggpubr)
library(data.table)


#Test at P<0.05 = significant. Then fill in this table with the correlation coefficients. Bold any values that are significant.

#cor(x, y, method = c("pearson", "kendall", "spearman")), value
#cor.test(x, y, method=c("pearson", "kendall", "spearman")), more values and stats 
#cor(x, y,  method = "pearson", use = "complete.obs"),If your data contain missing values, use the following R code to handle missing values by case-wise deletion.


#Abundance-Pearson’s correlation coefficients between log-abundance (catch and prop abundance) and year  

#Files
ShannonFile2 <- read.csv("PutahData/Abund_Corr.csv", strip.white = TRUE)
str(ShannonFile2)

##SITE A## 
SiteA_Abund<- ShannonFile2 %>% 
  filter(SITE=="1")
#cor.test(x, y, method=c("pearson")), more values and stats 


##BBH
#Catch
BBH_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$BBH_LOG, method=c("pearson"), use = "complete.obs")
BBH_Catch_A
#cor=-0.1189203 p=0.5981

#Prop abundance 
BBH_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$BBH_PROP, method=c("pearson"), use = "complete.obs")
BBH_prop_A
#cor=-0.1721948  p= 0.4435

##BCR
#Catch
BCR_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$BCR_LOG, method=c("pearson"), use = "complete.obs")
BCR_Catch_A
#cor=-0.005442283 p= 0.9808

#Prop abundance 
BCR_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$BCR_PROP, method=c("pearson"), use = "complete.obs")
BCR_prop_A
#cor=-0.005442283 p=0.9808

##BGS
#Catch
BGS_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$BGS_LOG, method=c("pearson"), use = "complete.obs")
BGS_Catch_A
#cor= -0.5462701 p=0.008531

#Prop abundance 
BGS_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$BGS_PROP, method=c("pearson"), use = "complete.obs")
BGS_prop_A
#cor=-0.4306723 p= 0.0454

##BLP
#Catch
BLP_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$BLP_LOG, method=c("pearson"), use = "complete.obs")
BLP_Catch_A
#cor= -0.3898746  p= 0.07286

#Prop abundance 
BLP_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$BLP_PROP, method=c("pearson"), use = "complete.obs")
BLP_prop_A
#cor= -0.3588523 p= 0.101

##BRB
#Catch
BRB_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$BRB_LOG, method=c("pearson"), use = "complete.obs")
BRB_Catch_A
#cor=NA p=NA

#Prop abundance 
BRB_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$BRB_PROP, method=c("pearson"), use = "complete.obs")
BRB_prop_A
#cor=NA p=NA

##CCF
#Catch
CCF_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$CCF_LOG, method=c("pearson"), use = "complete.obs")
CCF_Catch_A
#cor=NA p=NA

#Prop abundance 
CCF_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$CCF_PROP, method=c("pearson"), use = "complete.obs")
CCF_prop_A
#cor=NA p=NA

##CHN
#Catch
CHN_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$CHN_LOG, method=c("pearson"), use = "complete.obs")
CHN_Catch_A
#cor= 0.286527 p=0.1961

#Prop abundance 
CHN_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$CHN_PROP, method=c("pearson"), use = "complete.obs")
CHN_prop_A
#cor=0.3076365 p=0.1637

##COT
#Catch
COT_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$COT_LOG, method=c("pearson"), use = "complete.obs")
COT_Catch_A
#cor= 0.1681461 p=0.4545

#Prop abundance
COT_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$COT_PROP, method=c("pearson"), use = "complete.obs")
COT_prop_A
#cor= 0.2055545 p= 0.3588

##CRP
#Catch
CRP_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$CRP_LOG, method=c("pearson"), use = "complete.obs")
CRP_Catch_A
#cor= -0.4845723  p=0.02228

#Prop abundance 
CRP_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$CRP_PROP, method=c("pearson"), use = "complete.obs")
CRP_prop_A
#cor= -0.4544918 p= 0.03359

##FHM
#Catch
FHM_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$FHM_LOG, method=c("pearson"), use = "complete.obs")
FHM_Catch_A
#cor= NA p= NA

#Prop abundance 
FHM_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$FHM_PROP, method=c("pearson"), use = "complete.obs")
FHM_prop_A
#cor=NA p=NA#check nas that no fish

##GLF
#Catch
GLF_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$GLF_LOG, method=c("pearson"), use = "complete.obs")
GLF_Catch_A
#cor=  -0.4947976 p= 0.01923

#Prop abundance
GLF_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$GLF_PROP, method=c("pearson"), use = "complete.obs")
GLF_prop_A
#cor=-0.4649579 p=0.02923

##GSF
#Catch
GSF_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$GSF_LOG, method=c("pearson"), use = "complete.obs")
GSF_Catch_A
#cor= -0.5432623 p= 0.008977

#Prop abundance 
GSF_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$GSF_PROP, method=c("pearson"), use = "complete.obs")
GSF_prop_A
#cor= -0.3090364 p= 0.1617

##GSH
#Catch
GSH_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$GSH_LOG, method=c("pearson"), use = "complete.obs")
GSH_Catch_A
#cor=NA p=NA

#Prop abundance 
GSH_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$GSH_PROP, method=c("pearson"), use = "complete.obs")
GSH_prop_A
#cor=NA p=NA

##ISS
#Catch
ISS_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$ISS_LOG, method=c("pearson"), use = "complete.obs")
ISS_Catch_A
#cor= 0.03395362  p=0.8808

#Prop abundance 
ISS_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$ISS_PROP, method=c("pearson"), use = "complete.obs")
ISS_prop_A
#cor= 0.02983489 p= 0.8951

##LEP
#Catch
LEP_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$LEP_LOG, method=c("pearson"), use = "complete.obs")
LEP_Catch_A
#cor= -0.159704 p=0.4777

#Prop abundance 
LEP_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$LEP_PROP, method=c("pearson"), use = "complete.obs")
LEP_prop_A
#cor= -0.1883678  p= 0.4012

##LMB
#Catch
LMB_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$LMB_LOG, method=c("pearson"), use = "complete.obs")
LMB_Catch_A
#cor= -0.2331017 p=0.2965

#Prop abundance 
LMB_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$LMB_PROP, method=c("pearson"), use = "complete.obs")
LMB_prop_A
#cor= -0.3999443  p=0.06515

##MSQ
#Catch
MSQ_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$MSQ_LOG, method=c("pearson"), use = "complete.obs")
MSQ_Catch_A
#cor= -0.2226953 p=0.3192

#Prop abundance 
MSQ_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$MSQ_PROP, method=c("pearson"), use = "complete.obs")
MSQ_prop_A
#cor=  -0.2579976 p=0.2464

##PKM
#Catch
PKM_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$PKM_LOG, method=c("pearson"), use = "complete.obs")
PKM_Catch_A
#cor= 0.2342833 p=0.294

#Prop abundance
PKM_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$PKM_PROP, method=c("pearson"), use = "complete.obs")
PKM_prop_A
#cor=0.161199  p=0.4736

##PLR
#Catch
PLR_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$PLR_LOG, method=c("pearson"), use = "complete.obs")
PLR_Catch_A
#cor=0.07139686 p=0.7522

#Prop abundance
PLR_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$PLR_PROP, method=c("pearson"), use = "complete.obs")
PLR_prop_A
#cor= 0.03668583 p=0.8712

##PMK
#Catch
PMK_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$PMK_LOG, method=c("pearson"), use = "complete.obs")
PMK_Catch_A
#cor=NA p=NA

#Prop abundance
PMK_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$PMK_PROP, method=c("pearson"), use = "complete.obs")
PMK_prop_A
#cor=NA p=NA

##RBT
#Catch
RBT_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$RBT_LOG, method=c("pearson"), use = "complete.obs")
RBT_Catch_A
#cor= 0.5947278 p= 0.003508

#Prop abundance 
RBT_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$RBT_PROP, method=c("pearson"), use = "complete.obs")
RBT_prop_A
#cor= 0.6457213 p= 0.001171

##RCH
#Catch
RCH_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$RCH_LOG, method=c("pearson"), use = "complete.obs")
RCH_Catch_A
#cor= -0.3096077  p=  0.1609

#Prop abundance
RCH_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$RCH_PROP, method=c("pearson"), use = "complete.obs")
RCH_prop_A
#cor=-0.3139069  p=0.1548

##RES
#Catch
RES_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$RES_LOG, method=c("pearson"), use = "complete.obs")
RES_Catch_A
#cor=NA p=NA

#Prop abundance
RES_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$RES_PROP, method=c("pearson"), use = "complete.obs")
RES_prop_A
#cor=NA p=NA

##RSH
#Catch
RSH_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$RSH_LOG, method=c("pearson"), use = "complete.obs")
RSH_Catch_A
#cor=NA p=NA

#Prop abundance
RSH_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$RSH_PROP, method=c("pearson"), use = "complete.obs")
RSH_prop_A
#cor= NA p= NA

##SAP
#Catch
SAP_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$SAP_LOG, method=c("pearson"), use = "complete.obs")
SAP_Catch_A
#cor=NA p=NA

#Prop abundance 
SAP_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$SAP_PROP, method=c("pearson"), use = "complete.obs")
SAP_prop_A
#cor=NA p=NA

##SBF
#Catch
SBF_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$SBF_LOG, method=c("pearson"), use = "complete.obs")
SBF_Catch_A
#cor=NA p=NA

#Prop abundance 
SBF_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$SBF_PROP, method=c("pearson"), use = "complete.obs")
SBF_prop_A
#cor=NA p=NA

##SBK
#Catch
SBK_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$SBK_LOG, method=c("pearson"), use = "complete.obs")
SBK_Catch_A
#cor= -0.01650176  p= 0.9419

#Prop abundance 
SBK_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$SBK_PROP, method=c("pearson"), use = "complete.obs")
SBK_prop_A
#cor= 0.119239 p= 0.5971

##SKR
#Catch
SKR_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$SKR_LOG, method=c("pearson"), use = "complete.obs")
SKR_Catch_A
#cor= -0.3459148 p= 0.1148

#Prop abundance 
SKR_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$SKR_PROP, method=c("pearson"), use = "complete.obs")
SKR_prop_A
#cor= -0.348913 p= 0.1115

##SMB
#Catch
SMB_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$SMB_LOG, method=c("pearson"), use = "complete.obs")
SMB_Catch_A
#cor= -0.1888143  p= 0.4001

#Prop abundance 
SMB_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$SMB_PROP, method=c("pearson"), use = "complete.obs")
SMB_prop_A
#cor= -0.2122417  p= 0.343

##SPB
#Catch
SPB_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$SPB_LOG, method=c("pearson"), use = "complete.obs")
SPB_Catch_A
#cor= 0.3537484 p= 0.1063

#Prop abundance 
SPB_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$SPB_PROP, method=c("pearson"), use = "complete.obs")
SPB_prop_A
#cor= 0.3537484 p= 0.1063

##STB
#Catch
STB_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$STB_LOG, method=c("pearson"), use = "complete.obs")
STB_Catch_A
#cor=NA p=NA

#Prop abundance 
STB_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$STB_PROP, method=c("pearson"), use = "complete.obs")
STB_prop_A
#cor=NA p=NA

##TUP
#Catch
TUP_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$TUP_LOG, method=c("pearson"), use = "complete.obs")
TUP_Catch_A
#cor= -0.4093961 p= 0.05849

#Prop abundance 
TUP_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$TUP_PROP, method=c("pearson"), use = "complete.obs")
TUP_prop_A
#cor= -0.458892 p= 0.0317

##WCF
#Catch
WCF_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$WCF_LOG, method=c("pearson"), use = "complete.obs")
WCF_Catch_A
#cor=NA p=NA

#Prop abundance 
WCF_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$WCF_PROP, method=c("pearson"), use = "complete.obs")
WCF_prop_A
#cor=NA p=NA

##WRM
#Catch
WRM_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$WRM_LOG, method=c("pearson"), use = "complete.obs")
WRM_Catch_A
#cor=NA p=NA

#Prop abundance 
WRM_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$WRM_PROP, method=c("pearson"), use = "complete.obs")
WRM_prop_A
#cor=NA p=NA

##YFG
#Catch
YFG_Catch_A<- cor.test(SiteA_Abund$YEAR, SiteA_Abund$YFG_LOG, method=c("pearson"), use = "complete.obs")
YFG_Catch_A
#cor=NA p=NA

#Prop abundance 
YFG_prop_A <-cor.test(SiteA_Abund$YEAR, SiteA_Abund$YFG_PROP, method=c("pearson"), use = "complete.obs")
YFG_prop_A
#cor=NA p=NA






##SITE B##
SiteB_Abund<- ShannonFile2 %>% 
  filter(SITE=="3")

##BBH
#Catch
BBH_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$BBH_LOG, method=c("pearson"), use = "complete.obs")
BBH_Catch_B
#cor=NA p=NA

#Prop abundance 
BBH_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$BBH_PROP, method=c("pearson"), use = "complete.obs")
BBH_prop_B
#cor=NA  p=NA

##BCR
#Catch
BCR_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$BCR_LOG, method=c("pearson"), use = "complete.obs")
BCR_Catch_B
#cor=NA p=NA

#Prop abundance 
BCR_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$BCR_PROP, method=c("pearson"), use = "complete.obs")
BCR_prop_B
#cor=NA p=NA

##BGS
#Catch
BGS_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$BGS_LOG, method=c("pearson"), use = "complete.obs")
BGS_Catch_B
#cor= -0.4683077 p=0.02794

#Prop abundance 
BGS_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$BGS_PROP, method=c("pearson"), use = "complete.obs")
BGS_prop_B
#cor= -0.4233483 p=0.04962

##BLP
#Catch
BLP_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$BLP_LOG, method=c("pearson"), use = "complete.obs")
BLP_Catch_B
#cor= -0.7060202 p=0.000241

#Prop abundance 
BLP_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$BLP_PROP, method=c("pearson"), use = "complete.obs")
BLP_prop_B
#cor= -0.6602917 p=0.0008249

##BRB
#Catch
BRB_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$BRB_LOG, method=c("pearson"), use = "complete.obs")
BRB_Catch_B
#cor=NA p=NA

#Prop abundance 
BRB_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$BRB_PROP, method=c("pearson"), use = "complete.obs")
BRB_prop_B
#cor=NA p=NA

##CCF
#Catch
CCF_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$CCF_LOG, method=c("pearson"), use = "complete.obs")
CCF_Catch_B
#cor=NA p=NA

#Prop abundance 
CCF_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$CCF_PROP, method=c("pearson"), use = "complete.obs")
CCF_prop_B
#cor=NA p=NA

##CHN
#Catch
CHN_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$CHN_LOG, method=c("pearson"), use = "complete.obs")
CHN_Catch_B
#cor=NA p=NA

#Prop abundance 
CHN_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$CHN_PROP, method=c("pearson"), use = "complete.obs")
CHN_prop_B
#cor=NA p=NA

##COT
#Catch
COT_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$COT_LOG, method=c("pearson"), use = "complete.obs")
COT_Catch_B
#cor= 0.3290932 p= 0.1348

#Prop abundance
COT_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$COT_PROP, method=c("pearson"), use = "complete.obs")
COT_prop_B
#cor= 0.5054302 p= 0.01642

##CRP
#Catch
CRP_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$CRP_LOG, method=c("pearson"), use = "complete.obs")
CRP_Catch_B
#cor= -0.5288327 p= 0.01139

#Prop abundance 
CRP_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$CRP_PROP, method=c("pearson"), use = "complete.obs")
CRP_prop_B
#cor= -0.4516147 p=  0.03487

##FHM
#Catch
FHM_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$FHM_LOG, method=c("pearson"), use = "complete.obs")
FHM_Catch_B
#cor=NA p=NA

#Prop abundance 
FHM_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$FHM_PROP, method=c("pearson"), use = "complete.obs")
FHM_prop_B
#cor=NA p=NA

##GLF
#Catch
GLF_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$GLF_LOG, method=c("pearson"), use = "complete.obs")
GLF_Catch_B
#cor= -0.439844 p= 0.04052

#Prop abundance
GLF_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$GLF_PROP, method=c("pearson"), use = "complete.obs")
GLF_prop_B
#cor= -0.4043425 p= 0.06198

##GSF
#Catch
GSF_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$GSF_LOG, method=c("pearson"), use = "complete.obs")
GSF_Catch_B
#cor= -0.5656629 p= 0.006074

#Prop abundance 
GSF_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$GSF_PROP, method=c("pearson"), use = "complete.obs")
GSF_prop_B
#cor= -0.3859214 p= 0.07607

##GSH
#Catch
GSH_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$GSH_LOG, method=c("pearson"), use = "complete.obs")
GSH_Catch_B
#cor=NA p=NA

#Prop abundance 
GSH_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$GSH_PROP, method=c("pearson"), use = "complete.obs")
GSH_prop_B
#cor=NA p=NA

##ISS
#Catch
ISS_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$ISS_LOG, method=c("pearson"), use = "complete.obs")
ISS_Catch_B
#cor=NA p=NA

#Prop abundance 
ISS_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$ISS_PROP, method=c("pearson"), use = "complete.obs")
ISS_prop_B
#cor=NA p=NA

##LEP
#Catch
LEP_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$LEP_LOG, method=c("pearson"), use = "complete.obs")
LEP_Catch_B
#cor= -0.2178142  p= 0.3302

#Prop abundance 
LEP_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$LEP_PROP, method=c("pearson"), use = "complete.obs")
LEP_prop_B
#cor= -0.2178142 p= 0.3302

##LMB
#Catch
LMB_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$LMB_LOG, method=c("pearson"), use = "complete.obs")
LMB_Catch_B
#cor= -0.6307002 p= 0.00165

#Prop abundance 
LMB_prop_B<-cor.test(SiteB_Abund$YEAR, SiteB_Abund$LMB_PROP, method=c("pearson"), use = "complete.obs")
LMB_prop_B
#cor= -0.5750526 p= 0.005114

##MSQ
#Catch
MSQ_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$MSQ_LOG, method=c("pearson"), use = "complete.obs")
MSQ_Catch_B
#cor= -0.4257814  p= 0.04819

#Prop abundance 
MSQ_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$MSQ_PROP, method=c("pearson"), use = "complete.obs")
MSQ_prop_B
#cor= -0.359058  p= 0.1008

##PKM
#Catch
PKM_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$PKM_LOG, method=c("pearson"), use = "complete.obs")
PKM_Catch_B
#cor= 0.05995921  p=0.791

#Prop abundance
PKM_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$PKM_PROP, method=c("pearson"), use = "complete.obs")
PKM_prop_B
#cor= -0.06978422 p= 0.7576

##PLR
#Catch
PLR_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$PLR_LOG, method=c("pearson"), use = "complete.obs")
PLR_Catch_B
#cor= -0.2886849 p= 0.1926

#Prop abundance
PLR_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$PLR_PROP, method=c("pearson"), use = "complete.obs")
PLR_prop_B
#cor= -0.2425333 p= 0.2768

##PMK
#Catch
PMK_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$PMK_LOG, method=c("pearson"), use = "complete.obs")
PMK_Catch_B
#cor= NA p= NA

#Prop abundance
PMK_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$PMK_PROP, method=c("pearson"), use = "complete.obs")
PMK_prop_B
#cor= NA p=NA

##RBT
#Catch
RBT_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$RBT_LOG, method=c("pearson"), use = "complete.obs")
RBT_Catch_B
#cor= 0.8103684  p= 4.834e-06

#Prop abundance 
RBT_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$RBT_PROP, method=c("pearson"), use = "complete.obs")
RBT_prop_B
#cor=0.629871 p= 0.001681

##RCH
#Catch
RCH_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$RCH_LOG, method=c("pearson"), use = "complete.obs")
RCH_Catch_B
#cor=NA p=NA 

#Prop abundance
RCH_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$RCH_PROP, method=c("pearson"), use = "complete.obs")
RCH_prop_B
#cor=NA p=NA

##RES
#Catch
RES_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$RES_LOG, method=c("pearson"), use = "complete.obs")
RES_Catch_B
#cor= -0.5529559 p= 0.007605

#Prop abundance
RES_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$RES_PROP, method=c("pearson"), use = "complete.obs")
RES_prop_B
#cor= -0.5087677 p= 0.01561

##RSH
#Catch
RSH_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$RSH_LOG, method=c("pearson"), use = "complete.obs")
RSH_Catch_B
#cor=NA p=NA

#Prop abundance
RSH_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$RSH_PROP, method=c("pearson"), use = "complete.obs")
RSH_prop_B
#cor=NA p=NA

##SAP
#Catch
SAP_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$SAP_LOG, method=c("pearson"), use = "complete.obs")
SAP_Catch_B
#cor=NA p=NA

#Prop abundance 
SAP_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$SAP_PROP, method=c("pearson"), use = "complete.obs")
SAP_prop_B
#cor=NA p=NA

##SBF
#Catch
SBF_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$SBF_LOG, method=c("pearson"), use = "complete.obs")
SBF_Catch_B
#cor=NA p=NA

#Prop abundance 
SBF_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$SBF_PROP, method=c("pearson"), use = "complete.obs")
SBF_prop_B
#cor=NA p=NA

##SBK
#Catch
SBK_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$SBK_LOG, method=c("pearson"), use = "complete.obs")
SBK_Catch_B
#cor= 0.5372738 p= 0.009922

#Prop abundance 
SBK_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$SBK_PROP, method=c("pearson"), use = "complete.obs")
SBK_prop_B
#cor= 0.4904348 p= 0.02049

##SKR
#Catch
SKR_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$SKR_LOG, method=c("pearson"), use = "complete.obs")
SKR_Catch_B
#cor= 0.2363558 p= 0.2896

#Prop abundance 
SKR_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$SKR_PROP, method=c("pearson"), use = "complete.obs")
SKR_prop_B
#cor= 0.2154771 p= 0.3355

##SMB
#Catch
SMB_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$SMB_LOG, method=c("pearson"), use = "complete.obs")
SMB_Catch_B
#cor= -0.7442226 p= 7.148e-05

#Prop abundance 
SMB_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$SMB_PROP, method=c("pearson"), use = "complete.obs")
SMB_prop_B
#cor= -0.6136649 p= 0.002385

##SPB
#Catch
SPB_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$SPB_LOG, method=c("pearson"), use = "complete.obs")
SPB_Catch_B
#cor=NA p=NA

#Prop abundance 
SPB_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$SPB_PROP, method=c("pearson"), use = "complete.obs")
SPB_prop_B
#cor=NA p=NA

##STB
#Catch
STB_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$STB_LOG, method=c("pearson"), use = "complete.obs")
STB_Catch_B
#cor=NA p=NA

#Prop abundance 
STB_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$STB_PROP, method=c("pearson"), use = "complete.obs")
STB_prop_B
#cor=NA p=NA

##TUP
#Catch
TUP_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$TUP_LOG, method=c("pearson"), use = "complete.obs")
TUP_Catch_B
#cor= 0.3733368 p=0.08701

#Prop abundance 
TUP_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$TUP_PROP, method=c("pearson"), use = "complete.obs")
TUP_prop_B
#cor= 0.7015586  p= 0.0002744

##WCF
#Catch
WCF_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$WCF_LOG, method=c("pearson"), use = "complete.obs")
WCF_Catch_B
#cor=-0.2489305 p=0.2639

#Prop abundance 
WCF_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$WCF_PROP, method=c("pearson"), use = "complete.obs")
WCF_prop_B
#cor= -0.2489305 p= 0.2639

##WRM
#Catch
WRM_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$WRM_LOG, method=c("pearson"), use = "complete.obs")
WRM_Catch_B
#cor=NA p=NA

#Prop abundance 
WRM_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$WRM_PROP, method=c("pearson"), use = "complete.obs")
WRM_prop_B
#cor=NA p=NA

##YFG
#Catch
YFG_Catch_B<- cor.test(SiteB_Abund$YEAR, SiteB_Abund$YFG_LOG, method=c("pearson"), use = "complete.obs")
YFG_Catch_B
#cor=NA p=NA

#Prop abundance 
YFG_prop_B <-cor.test(SiteB_Abund$YEAR, SiteB_Abund$YFG_PROP, method=c("pearson"), use = "complete.obs")
YFG_prop_B
#cor=NA p=NA






##SITE C##
SiteC_Abund<- ShannonFile2 %>% 
  filter(SITE=="5")

##BBH
#Catch
BBH_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$BBH_LOG, method=c("pearson"), use = "complete.obs")
BBH_Catch_C
#cor= -0.335318 p=  0.1178

#Prop abundance 
BBH_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$BBH_PROP, method=c("pearson"), use = "complete.obs")
BBH_prop_C
#cor=  -0.335318 p= 0.1178

##BCR
#Catch
BCR_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$BCR_LOG, method=c("pearson"), use = "complete.obs")
BCR_Catch_C
#cor= -0.248337 p= 0.2532

#Prop abundance 
BCR_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$BCR_PROP, method=c("pearson"), use = "complete.obs")
BCR_prop_C
#cor=  -0.248337 p= 0.2532

##BGS
#Catch
BGS_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$BGS_LOG, method=c("pearson"), use = "complete.obs")
BGS_Catch_C
#cor= -0.7771615 p= 1.287e-05

#Prop abundance 
BGS_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$BGS_PROP, method=c("pearson"), use = "complete.obs")
BGS_prop_C
#cor= -0.5314864  p= 0.009057

##BLP
#Catch
BLP_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$BLP_LOG, method=c("pearson"), use = "complete.obs")
BLP_Catch_C
#cor= -0.5181992  p= 0.01131

#Prop abundance 
BLP_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$BLP_PROP, method=c("pearson"), use = "complete.obs")
BLP_prop_C
#cor= -0.525127 p= 0.01008

##BRB
#Catch
BRB_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$BRB_LOG, method=c("pearson"), use = "complete.obs")
BRB_Catch_C
#cor= -0.457882 p= 0.02802

#Prop abundance 
BRB_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$BRB_PROP, method=c("pearson"), use = "complete.obs")
BRB_prop_C
#cor= -0.4624698 p= 0.02629

##CCF
#Catch
CCF_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$CCF_LOG, method=c("pearson"), use = "complete.obs")
CCF_Catch_C
#cor= -0.4308199 p= 0.04014

#Prop abundance 
CCF_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$CCF_PROP, method=c("pearson"), use = "complete.obs")
CCF_prop_C
#cor= -0.4199276 p= 0.04605

##CHN
#Catch
CHN_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$CHN_LOG, method=c("pearson"), use = "complete.obs")
CHN_Catch_C
#cor=NA p=NA

#Prop abundance 
CHN_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$CHN_PROP, method=c("pearson"), use = "complete.obs")
CHN_prop_C
#cor=NA p=NA

##COT
#Catch
COT_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$COT_LOG, method=c("pearson"), use = "complete.obs")
COT_Catch_C
#cor= 0.6497842 p= 0.0007914

#Prop abundance
COT_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$COT_PROP, method=c("pearson"), use = "complete.obs")
COT_prop_C
#cor=0.4362289 p= 0.03744

##CRP
#Catch
CRP_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$CRP_LOG, method=c("pearson"), use = "complete.obs")
CRP_Catch_C
#cor= -0.505377 p= 0.0139

#Prop abundance 
CRP_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$CRP_PROP, method=c("pearson"), use = "complete.obs")
CRP_prop_C
#cor= -0.3953519 p= 0.06187

##FHM
#Catch
FHM_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$FHM_LOG, method=c("pearson"), use = "complete.obs")
FHM_Catch_C
#cor= -0.1903497 p= 0.3843

#Prop abundance 
FHM_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$FHM_PROP, method=c("pearson"), use = "complete.obs")
FHM_prop_C
#cor= -0.1903497 p=0.3843

##GLF
#Catch
GLF_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$GLF_LOG, method=c("pearson"), use = "complete.obs")
GLF_Catch_C
#cor= -0.352616 p= 0.09889

#Prop abundance
GLF_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$GLF_PROP, method=c("pearson"), use = "complete.obs")
GLF_prop_C
#cor= -0.3556995  p= 0.09577

##GSF
#Catch
GSF_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$GSF_LOG, method=c("pearson"), use = "complete.obs")
GSF_Catch_C
#cor= -0.7825213 p= 1.022e-05

#Prop abundance 
GSF_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$GSF_PROP, method=c("pearson"), use = "complete.obs")
GSF_prop_C
#cor= -0.6713892 p=0.0004523

##GSH
#Catch
GSH_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$GSH_LOG, method=c("pearson"), use = "complete.obs")
GSH_Catch_C
#cor=NA p=NA

#Prop abundance 
GSH_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$GSH_PROP, method=c("pearson"), use = "complete.obs")
GSH_prop_C
#cor=NA p=NA


##ISS
#Catch
ISS_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$ISS_LOG, method=c("pearson"), use = "complete.obs")
ISS_Catch_C
#cor= -0.07437505 p= 0.7359

#Prop abundance 
ISS_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$ISS_PROP, method=c("pearson"), use = "complete.obs")
ISS_prop_C
#cor= -0.07437505 p=  0.7359

##LEP
#Catch
LEP_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$LEP_LOG, method=c("pearson"), use = "complete.obs")
LEP_Catch_C
#cor= -0.5239485 p= 0.01028

#Prop abundance 
LEP_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$LEP_PROP, method=c("pearson"), use = "complete.obs")
LEP_prop_C
#cor= -0.3920312 p= 0.0643

##LMB
#Catch
LMB_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$LMB_LOG, method=c("pearson"), use = "complete.obs")
LMB_Catch_C
#cor= -0.4642523 p= 0.02564

#Prop abundance 
LMB_prop_C<-cor.test(SiteC_Abund$YEAR, SiteC_Abund$LMB_PROP, method=c("pearson"), use = "complete.obs")
LMB_prop_C
#cor= -0.6106088 p= 0.001971

##MSQ
#Catch
MSQ_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$MSQ_LOG, method=c("pearson"), use = "complete.obs")
MSQ_Catch_C
#cor= -0.377983 p= 0.07535

#Prop abundance 
MSQ_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$MSQ_PROP, method=c("pearson"), use = "complete.obs")
MSQ_prop_C
#cor= -0.2872064 p= 0.1839

##PKM
#Catch
PKM_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$PKM_LOG, method=c("pearson"), use = "complete.obs")
PKM_Catch_C
#cor= 0.7388024 p= 5.667e-05

#Prop abundance
PKM_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$PKM_PROP, method=c("pearson"), use = "complete.obs")
PKM_prop_C
#cor= 0.6203229 p=0.00159

##PLR
#Catch
PLR_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$PLR_LOG, method=c("pearson"), use = "complete.obs")
PLR_Catch_C
#cor= -0.3639725 p= 0.08776

#Prop abundance
PLR_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$PLR_PROP, method=c("pearson"), use = "complete.obs")
PLR_prop_C
#cor= -0.3243389 p= 0.1311

##PMK
#Catch
PMK_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$PMK_LOG, method=c("pearson"), use = "complete.obs")
PMK_Catch_C
#cor= 0.01260594 p= 0.9545

#Prop abundance
PMK_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$PMK_PROP, method=c("pearson"), use = "complete.obs")
PMK_prop_C
#cor= 0.01260594 p= 0.9545

##RBT
#Catch
RBT_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$RBT_LOG, method=c("pearson"), use = "complete.obs")
RBT_Catch_C
#cor= -0.161356 p= 0.462

#Prop abundance 
RBT_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$RBT_PROP, method=c("pearson"), use = "complete.obs")
RBT_prop_C
#cor= -0.161356 p= 0.462

##RCH
#Catch
RCH_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$RCH_LOG, method=c("pearson"), use = "complete.obs")
RCH_Catch_C
#cor=NA p=NA

#Prop abundance
RCH_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$RCH_PROP, method=c("pearson"), use = "complete.obs")
RCH_prop_C
#cor=NA p=NA

##RES
#Catch
RES_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$RES_LOG, method=c("pearson"), use = "complete.obs")
RES_Catch_C
#cor= -0.2773307 p= 0.2001

#Prop abundance
RES_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$RES_PROP, method=c("pearson"), use = "complete.obs")
RES_prop_C
#cor= -0.2773307 p= 0.2001

##RSH
#Catch
RSH_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$RSH_LOG, method=c("pearson"), use = "complete.obs")
RSH_Catch_C
#cor= -0.1262457 p=0.566

#Prop abundance
RSH_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$RSH_PROP, method=c("pearson"), use = "complete.obs")
RSH_prop_C
#cor= -0.06306826 p=0.775

##SAP
#Catch
SAP_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$SAP_LOG, method=c("pearson"), use = "complete.obs")
SAP_Catch_C
#cor= -0.248337 p= 0.2532

#Prop abundance 
SAP_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$SAP_PROP, method=c("pearson"), use = "complete.obs")
SAP_prop_C
#cor= -0.248337 p= 0.2532

##SBF
#Catch
SBF_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$SBF_LOG, method=c("pearson"), use = "complete.obs")
SBF_Catch_C
#cor= -0.4629628 p= 0.02611

#Prop abundance 
SBF_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$SBF_PROP, method=c("pearson"), use = "complete.obs")
SBF_prop_C
#cor= -0.424497 p= 0.0435

##SBK
#Catch
SBK_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$SBK_LOG, method=c("pearson"), use = "complete.obs")
SBK_Catch_C
#cor=NA p=NA

#Prop abundance 
SBK_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$SBK_PROP, method=c("pearson"), use = "complete.obs")
SBK_prop_C
#cor=NA p=NA

##SKR
#Catch
SKR_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$SKR_LOG, method=c("pearson"), use = "complete.obs")
SKR_Catch_C
#cor= 0.4331185 p= 0.03897

#Prop abundance 
SKR_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$SKR_PROP, method=c("pearson"), use = "complete.obs")
SKR_prop_C
#cor= 0.3535606  p= 0.09792

##SMB
#Catch
SMB_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$SMB_LOG, method=c("pearson"), use = "complete.obs")
SMB_Catch_C
#cor= -0.6258233 p= 0.001403

#Prop abundance 
SMB_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$SMB_PROP, method=c("pearson"), use = "complete.obs")
SMB_prop_C
#cor= -0.3393929 p= 0.1131

##SPB
#Catch
SPB_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$SPB_LOG, method=c("pearson"), use = "complete.obs")
SPB_Catch_C
#cor= 0.2071037 p= 0.343

#Prop abundance 
SPB_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$SPB_PROP, method=c("pearson"), use = "complete.obs")
SPB_prop_C
#cor= 0.09597992  p= 0.6631

##STB
#Catch
STB_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$STB_LOG, method=c("pearson"), use = "complete.obs")
STB_Catch_C
#cor=NA p=NA

#Prop abundance 
STB_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$STB_PROP, method=c("pearson"), use = "complete.obs")
STB_prop_C
#cor= NA p=NA

##TUP
#Catch
TUP_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$TUP_LOG, method=c("pearson"), use = "complete.obs")
TUP_Catch_C
#cor= 0.8067174 p= 3.324e-06

#Prop abundance 
TUP_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$TUP_PROP, method=c("pearson"), use = "complete.obs")
TUP_prop_C
#cor= 0.7630668 p= 2.29e-05

##WCF
#Catch
WCF_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$WCF_LOG, method=c("pearson"), use = "complete.obs")
WCF_Catch_C
#cor= -0.2371231 p= 0.276

#Prop abundance 
WCF_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$WCF_PROP, method=c("pearson"), use = "complete.obs")
WCF_prop_C
#cor= -0.2073918 p= 0.3424

##WRM
#Catch
WRM_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$WRM_LOG, method=c("pearson"), use = "complete.obs")
WRM_Catch_C
#cor=NA p=NA

#Prop abundance 
WRM_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$WRM_PROP, method=c("pearson"), use = "complete.obs")
WRM_prop_C
#cor=NA p= NA

##YFG
#Catch
YFG_Catch_C<- cor.test(SiteC_Abund$YEAR, SiteC_Abund$YFG_LOG, method=c("pearson"), use = "complete.obs")
YFG_Catch_C
#cor= NA p= NA

#Prop abundance 
YFG_prop_C <-cor.test(SiteC_Abund$YEAR, SiteC_Abund$YFG_PROP, method=c("pearson"), use = "complete.obs")
YFG_prop_C
#cor=NA p=NA







##SITE D##
SiteD_Abund<- ShannonFile2 %>% 
  filter(SITE=="6")

##BBH
#Catch
BBH_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$BBH_LOG, method=c("pearson"), use = "complete.obs")
BBH_Catch_D
#cor= -0.5581337 p= 0.005646

#Prop abundance 
BBH_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$BBH_PROP, method=c("pearson"), use = "complete.obs")
BBH_prop_D
#cor= -0.4262398   p= 0.04255

##BCR
#Catch
BCR_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$BCR_LOG, method=c("pearson"), use = "complete.obs")
BCR_Catch_D
#cor= -0.2965141  p= 0.1695

#Prop abundance 
BCR_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$BCR_PROP, method=c("pearson"), use = "complete.obs")
BCR_prop_D
#cor= -0.3021643 p= 0.1611

##BGS
#Catch
BGS_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$BGS_LOG, method=c("pearson"), use = "complete.obs")
BGS_Catch_D
#cor= -0.6107056 p= 0.001967

#Prop abundance 
BGS_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$BGS_PROP, method=c("pearson"), use = "complete.obs")
BGS_prop_D
#cor= -0.6757712 p= 0.0004016

##BLP
#Catch
BLP_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$BLP_LOG, method=c("pearson"), use = "complete.obs")
BLP_Catch_D
#cor= -0.2168321 p= 0.3203

#Prop abundance 
BLP_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$BLP_PROP, method=c("pearson"), use = "complete.obs")
BLP_prop_D
#cor= -0.4135747 p= 0.0498

##BRB
#Catch
BRB_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$BRB_LOG, method=c("pearson"), use = "complete.obs")
BRB_Catch_D
#cor= -0.3063244  p= 0.1551

#Prop abundance 
BRB_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$BRB_PROP, method=c("pearson"), use = "complete.obs")
BRB_prop_D
#cor= -0.3063244 p= 0.1551

##CCF
#Catch
CCF_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$CCF_LOG, method=c("pearson"), use = "complete.obs")
CCF_Catch_D
#cor= -0.3283814 p= 0.1261

#Prop abundance 
CCF_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$CCF_PROP, method=c("pearson"), use = "complete.obs")
CCF_prop_D
#cor= -0.2840203 p= 0.189

##CHN
#Catch
CHN_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$CHN_LOG, method=c("pearson"), use = "complete.obs")
CHN_Catch_D
#cor=NA p=NA

#Prop abundance 
CHN_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$CHN_PROP, method=c("pearson"), use = "complete.obs")
CHN_prop_D
#cor=NA p=NA

##COT
#Catch
COT_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$COT_LOG, method=c("pearson"), use = "complete.obs")
COT_Catch_D
#cor= 0.5764406 p= 0.003989

#Prop abundance
COT_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$COT_PROP, method=c("pearson"), use = "complete.obs")
COT_prop_D
#cor= 0.4024052 p= 0.05696

##CRP
#Catch
CRP_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$CRP_LOG, method=c("pearson"), use = "complete.obs")
CRP_Catch_D
#cor= -0.2545194  p= 0.2412

#Prop abundance 
CRP_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$CRP_PROP, method=c("pearson"), use = "complete.obs")
CRP_prop_D
#cor= -0.2854725 p= 0.1867

##FHM
#Catch
FHM_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$FHM_LOG, method=c("pearson"), use = "complete.obs")
FHM_Catch_D
#cor= 0.3605299 p= 0.09103

#Prop abundance 
FHM_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$FHM_PROP, method=c("pearson"), use = "complete.obs")
FHM_prop_D
#cor= 0.3605299  p= 0.09103

##GLF
#Catch
GLF_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$GLF_LOG, method=c("pearson"), use = "complete.obs")
GLF_Catch_D
#cor= -0.1200302 p= 0.5854

#Prop abundance
GLF_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$GLF_PROP, method=c("pearson"), use = "complete.obs")
GLF_prop_D
#cor= -0.2939269 p= 0.1734

##GSF
#Catch
GSF_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$GSF_LOG, method=c("pearson"), use = "complete.obs")
GSF_Catch_D
#cor= -0.4166573  p= 0.04795

#Prop abundance 
GSF_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$GSF_PROP, method=c("pearson"), use = "complete.obs")
GSF_prop_D
#cor= -0.4722609  p= 0.02288

##GSH
#Catch
GSH_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$GSH_LOG, method=c("pearson"), use = "complete.obs")
GSH_Catch_D
#cor=NA p=NA

#Prop abundance 
GSH_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$GSH_PROP, method=c("pearson"), use = "complete.obs")
GSH_prop_D
#cor=NA p=NA

##ISS
#Catch
ISS_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$ISS_LOG, method=c("pearson"), use = "complete.obs")
ISS_Catch_D
#cor= 0.07599557 p= 0.7304

#Prop abundance 
ISS_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$ISS_PROP, method=c("pearson"), use = "complete.obs")
ISS_prop_D
#cor= -0.07217286 p= 0.7435

##LEP
#Catch
LEP_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$LEP_LOG, method=c("pearson"), use = "complete.obs")
LEP_Catch_D
#cor= -0.6196954 p=0.001612

#Prop abundance 
LEP_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$LEP_PROP, method=c("pearson"), use = "complete.obs")
LEP_prop_D
#cor= -0.5467193 p= 0.006945

##LMB
#Catch
LMB_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$LMB_LOG, method=c("pearson"), use = "complete.obs")
LMB_Catch_D
#cor= 0.0008902886  p= 0.9968

#Prop abundance 
LMB_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$LMB_PROP, method=c("pearson"), use = "complete.obs")
LMB_prop_D
#cor= -0.330151 p=0.1239

##MSQ
#Catch
MSQ_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$MSQ_LOG, method=c("pearson"), use = "complete.obs")
MSQ_Catch_D
#cor= -0.4420847  p= 0.03467

#Prop abundance 
MSQ_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$MSQ_PROP, method=c("pearson"), use = "complete.obs")
MSQ_prop_D
#cor= -0.4107896 p= 0.05152

##PKM
#Catch
PKM_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$PKM_LOG, method=c("pearson"), use = "complete.obs")
PKM_Catch_D
#cor= 0.6846846  p= 0.0003134

#Prop abundance
PKM_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$PKM_PROP, method=c("pearson"), use = "complete.obs")
PKM_prop_D
#cor= 0.6167566 p= 0.001722

##PLR
#Catch
PLR_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$PLR_LOG, method=c("pearson"), use = "complete.obs")
PLR_Catch_D
#cor= -0.2320102 p= 0.2868

#Prop abundance
PLR_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$PLR_PROP, method=c("pearson"), use = "complete.obs")
PLR_prop_D
#cor= -0.215166 p= 0.3241

##PMK
#Catch
PMK_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$PMK_LOG, method=c("pearson"), use = "complete.obs")
PMK_Catch_D
#cor= -0.1323624 p= 0.5471

#Prop abundance
PMK_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$PMK_PROP, method=c("pearson"), use = "complete.obs")
PMK_prop_D
#cor= -0.1323624 p=0.5471

##RBT
#Catch
RBT_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$RBT_LOG, method=c("pearson"), use = "complete.obs")
RBT_Catch_D
#cor=NA p=NA

#Prop abundance 
RBT_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$RBT_PROP, method=c("pearson"), use = "complete.obs")
RBT_prop_D
#cor=NA p=NA

##RCH
#Catch
RCH_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$RCH_LOG, method=c("pearson"), use = "complete.obs")
RCH_Catch_D
#cor=NA p=NA

#Prop abundance
RCH_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$RCH_PROP, method=c("pearson"), use = "complete.obs")
RCH_prop_D
#cor=NA p=NA

##RES
#Catch
RES_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$RES_LOG, method=c("pearson"), use = "complete.obs")
RES_Catch_D
#cor= 0.04086113 p= 0.8531

#Prop abundance
RES_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$RES_PROP, method=c("pearson"), use = "complete.obs")
RES_prop_D
#cor= -0.0586126  p= 0.7905

##RSH
#Catch
RSH_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$RSH_LOG, method=c("pearson"), use = "complete.obs")
RSH_Catch_D
#cor= -0.2196819 p= 0.3138

#Prop abundance
RSH_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$RSH_PROP, method=c("pearson"), use = "complete.obs")
RSH_prop_D
#cor= -0.2679099  p= 0.2165

##SAP
#Catch
SAP_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$SAP_LOG, method=c("pearson"), use = "complete.obs")
SAP_Catch_D
#cor= -0.2827789 p= 0.1911

#Prop abundance 
SAP_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$SAP_PROP, method=c("pearson"), use = "complete.obs")
SAP_prop_D
#cor= -0.2325496 p= 0.2856

##SBF
#Catch
SBF_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$SBF_LOG, method=c("pearson"), use = "complete.obs")
SBF_Catch_D
#cor= -0.2773307 p= 0.2001

#Prop abundance 
SBF_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$SBF_PROP, method=c("pearson"), use = "complete.obs")
SBF_prop_D
#cor= -0.2773307 p= 0.2001

##SBK
#Catch
SBK_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$SBK_LOG, method=c("pearson"), use = "complete.obs")
SBK_Catch_D
#cor=NA p=NA

#Prop abundance 
SBK_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$SBK_PROP, method=c("pearson"), use = "complete.obs")
SBK_prop_D
#cor=NA p=NA

##SKR
#Catch
SKR_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$SKR_LOG, method=c("pearson"), use = "complete.obs")
SKR_Catch_D
#cor= 0.564193 p= 0.005044

#Prop abundance 
SKR_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$SKR_PROP, method=c("pearson"), use = "complete.obs")
SKR_prop_D
#cor= 0.5071856  p= 0.0135

##SMB
#Catch
SMB_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$SMB_LOG, method=c("pearson"), use = "complete.obs")
SMB_Catch_D
#cor= 0.2574152  p= 0.2357

#Prop abundance 
SMB_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$SMB_PROP, method=c("pearson"), use = "complete.obs")
SMB_prop_D
#cor= 0.2887141 p= 0.1815

##SPB
#Catch
SPB_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$SPB_LOG, method=c("pearson"), use = "complete.obs")
SPB_Catch_D
#cor= 0.2862479  p= 0.1855

#Prop abundance 
SPB_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$SPB_PROP, method=c("pearson"), use = "complete.obs")
SPB_prop_D
#cor= 0.3241812 p= 0.1313

##STB
#Catch
STB_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$STB_LOG, method=c("pearson"), use = "complete.obs")
STB_Catch_D
#cor=NA p=NA

#Prop abundance 
STB_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$STB_PROP, method=c("pearson"), use = "complete.obs")
STB_prop_D
#cor=NA p=NA

##TUP
#Catch
TUP_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$TUP_LOG, method=c("pearson"), use = "complete.obs")
TUP_Catch_D
#cor= 0.6499012 p= 0.0007891

#Prop abundance 
TUP_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$TUP_PROP, method=c("pearson"), use = "complete.obs")
TUP_prop_D
#cor= 0.5612374 p=  0.00533

##WCF
#Catch
WCF_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$WCF_LOG, method=c("pearson"), use = "complete.obs")
WCF_Catch_D
#cor= -0.01638772 p= 0.9408

#Prop abundance 
WCF_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$WCF_PROP, method=c("pearson"), use = "complete.obs")
WCF_prop_D
#cor=-0.01638772  p=0.9408

##WRM
#Catch
WRM_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$WRM_LOG, method=c("pearson"), use = "complete.obs")
WRM_Catch_D
#cor=NA p=NA

#Prop abundance 
WRM_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$WRM_PROP, method=c("pearson"), use = "complete.obs")
WRM_prop_D
#cor=NA p=NA

##YFG
#Catch
YFG_Catch_D<- cor.test(SiteD_Abund$YEAR, SiteD_Abund$YFG_LOG, method=c("pearson"), use = "complete.obs")
YFG_Catch_D
#cor=NA p=NA

#Prop abundance 
YFG_prop_D <-cor.test(SiteD_Abund$YEAR, SiteD_Abund$YFG_PROP, method=c("pearson"), use = "complete.obs")
YFG_prop_D
#cor=NA p=NA





##SITE E##
SiteE_Abund<- ShannonFile2 %>% 
  filter(SITE=="9")

##BBH
#Catch
BBH_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$BBH_LOG, method=c("pearson"), use = "complete.obs")
BBH_Catch_E
#cor= -0.2032193 p= 0.3644

#Prop abundance 
BBH_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$BBH_PROP, method=c("pearson"), use = "complete.obs")
BBH_prop_E
#cor= -0.2393511  p= 0.2834

##BCR
#Catch
BCR_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$BCR_LOG, method=c("pearson"), use = "complete.obs")
BCR_Catch_E
#cor= -0.5592859 p= 0.006807

#Prop abundance 
BCR_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$BCR_PROP, method=c("pearson"), use = "complete.obs")
BCR_prop_E
#cor= -0.5145114 p= 0.01429

##BGS
#Catch
BGS_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$BGS_LOG, method=c("pearson"), use = "complete.obs")
BGS_Catch_E
#cor= -0.474344 p= 0.02572

#Prop abundance 
BGS_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$BGS_PROP, method=c("pearson"), use = "complete.obs")
BGS_prop_E
#cor= -0.6699642 p= 0.000647

##BLP
#Catch
BLP_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$BLP_LOG, method=c("pearson"), use = "complete.obs")
BLP_Catch_E
#cor= 0.3050165 p= 0.1675

#Prop abundance 
BLP_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$BLP_PROP, method=c("pearson"), use = "complete.obs")
BLP_prop_E
#cor= 0.2588665  p= 0.2447

##BRB
#Catch
BRB_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$BRB_LOG, method=c("pearson"), use = "complete.obs")
BRB_Catch_E
#cor=NA p=NA

#Prop abundance 
BRB_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$BRB_PROP, method=c("pearson"), use = "complete.obs")
BRB_prop_E
#cor=NA p=NA

##CCF
#Catch
CCF_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$CCF_LOG, method=c("pearson"), use = "complete.obs")
CCF_Catch_E
#cor= 0.1386217  p= 0.5384

#Prop abundance 
CCF_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$CCF_PROP, method=c("pearson"), use = "complete.obs")
CCF_prop_E
#cor= 0.1386217 p= 0.5384

##CHN
#Catch
CHN_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$CHN_LOG, method=c("pearson"), use = "complete.obs")
CHN_Catch_E
#cor=NA p=NA

#Prop abundance 
CHN_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$CHN_PROP, method=c("pearson"), use = "complete.obs")
CHN_prop_E
#cor=NA p=NA

##COT
#Catch
COT_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$COT_LOG, method=c("pearson"), use = "complete.obs")
COT_Catch_E
#cor= 0.02778682 p= 0.9023

#Prop abundance
COT_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$COT_PROP, method=c("pearson"), use = "complete.obs")
COT_prop_E
#cor= 0.1147425  p= 0.6111

##CRP
#Catch
CRP_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$CRP_LOG, method=c("pearson"), use = "complete.obs")
CRP_Catch_E
#cor= -0.5710262 p= 0.005509

#Prop abundance 
CRP_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$CRP_PROP, method=c("pearson"), use = "complete.obs")
CRP_prop_E
#cor= -0.5296866  p= 0.01123

##FHM
#Catch
FHM_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$FHM_LOG, method=c("pearson"), use = "complete.obs")
FHM_Catch_E
#cor= -0.4449617 p= 0.03798

#Prop abundance 
FHM_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$FHM_PROP, method=c("pearson"), use = "complete.obs")
FHM_prop_E
#cor= -0.417987 p= 0.05289

##GLF
#Catch
GLF_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$GLF_LOG, method=c("pearson"), use = "complete.obs")
GLF_Catch_E
#cor= -0.3912725 p= 0.07175

#Prop abundance
GLF_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$GLF_PROP, method=c("pearson"), use = "complete.obs")
GLF_prop_E
#cor= -0.3324234 p= 0.1306

##GSF
#Catch
GSF_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$GSF_LOG, method=c("pearson"), use = "complete.obs")
GSF_Catch_E
#cor= 0.1527558 p= 0.4973

#Prop abundance 
GSF_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$GSF_PROP, method=c("pearson"), use = "complete.obs")
GSF_prop_E
#cor= 0.005487225 p= 0.9807

##GSH
#Catch
GSH_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$GSH_LOG, method=c("pearson"), use = "complete.obs")
GSH_Catch_E
#cor=NA p=NA

#Prop abundance 
GSH_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$GSH_PROP, method=c("pearson"), use = "complete.obs")
GSH_prop_E
#cor=NA p=NA

##ISS
#Catch
ISS_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$ISS_LOG, method=c("pearson"), use = "complete.obs")
ISS_Catch_E
#cor= 0.3460171 p= 0.1147

#Prop abundance 
ISS_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$ISS_PROP, method=c("pearson"), use = "complete.obs")
ISS_prop_E
#cor= 0.3478431 p= 0.1127

##LEP
#Catch
LEP_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$LEP_LOG, method=c("pearson"), use = "complete.obs")
LEP_Catch_E
#cor= -0.6300355 p= 0.001675

#Prop abundance 
LEP_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$LEP_PROP, method=c("pearson"), use = "complete.obs")
LEP_prop_E
#cor= -0.5902829 p= 0.003828

##LMB
#Catch
LMB_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$LMB_LOG, method=c("pearson"), use = "complete.obs")
LMB_Catch_E
#cor= 0.7695973 p= 2.822e-05

#Prop abundance 
LMB_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$LMB_PROP, method=c("pearson"), use = "complete.obs")
LMB_prop_E
#cor= 0.7050097 p= 0.0002483

##MSQ
#Catch
MSQ_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$MSQ_LOG, method=c("pearson"), use = "complete.obs")
MSQ_Catch_E
#cor= -0.5103761 p= 0.01523

#Prop abundance 
MSQ_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$MSQ_PROP, method=c("pearson"), use = "complete.obs")
MSQ_prop_E
#cor= -0.4559264 p= 0.03296

##PKM
#Catch
PKM_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$PKM_LOG, method=c("pearson"), use = "complete.obs")
PKM_Catch_E
#cor= 0.2724397 p= 0.22

#Prop abundance
PKM_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$PKM_PROP, method=c("pearson"), use = "complete.obs")
PKM_prop_E
#cor= 0.1379958 p= 0.5403

##PLR
#Catch
PLR_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$PLR_LOG, method=c("pearson"), use = "complete.obs")
PLR_Catch_E
#cor= -0.0937347 p= 0.6782

#Prop abundance
PLR_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$PLR_PROP, method=c("pearson"), use = "complete.obs")
PLR_prop_E
#cor= -0.0937347  p= 0.6782

##PMK
#Catch
PMK_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$PMK_LOG, method=c("pearson"), use = "complete.obs")
PMK_Catch_E
#cor= -0.006601035 p= 0.9767

#Prop abundance
PMK_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$PMK_PROP, method=c("pearson"), use = "complete.obs")
PMK_prop_E
#cor= -0.006601035 p= 0.9767

##RBT
#Catch
RBT_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$RBT_LOG, method=c("pearson"), use = "complete.obs")
RBT_Catch_E
#cor= NA p=NA

#Prop abundance 
RBT_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$RBT_PROP, method=c("pearson"), use = "complete.obs")
RBT_prop_E
#cor=NA p=NA

##RCH
#Catch
RCH_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$RCH_LOG, method=c("pearson"), use = "complete.obs")
RCH_Catch_E
#cor= -0.2970466 p= 0.1794

#Prop abundance
RCH_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$RCH_PROP, method=c("pearson"), use = "complete.obs")
RCH_prop_E
#cor= -0.2970466 p= 0.1794

##RES
#Catch
RES_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$RES_LOG, method=c("pearson"), use = "complete.obs")
RES_Catch_E
#cor= 0.09864522 p= 0.6623

#Prop abundance
RES_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$RES_PROP, method=c("pearson"), use = "complete.obs")
RES_prop_E
#cor= 0.06641095  p= 0.769

##RSH
#Catch
RSH_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$RSH_LOG, method=c("pearson"), use = "complete.obs")
RSH_Catch_E
#cor= -0.2210546 p= 0.3229

#Prop abundance
RSH_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$RSH_PROP, method=c("pearson"), use = "complete.obs")
RSH_prop_E
#cor= -0.3220789  p= 0.1438

##SAP
#Catch
SAP_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$SAP_LOG, method=c("pearson"), use = "complete.obs")
SAP_Catch_E
#cor=NA p=NA

#Prop abundance 
SAP_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$SAP_PROP, method=c("pearson"), use = "complete.obs")
SAP_prop_E
#cor=NA p=NA

##SBF
#Catch
SBF_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$SBF_LOG, method=c("pearson"), use = "complete.obs")
SBF_Catch_E
#cor= -0.5610454  p= 0.006597

#Prop abundance 
SBF_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$SBF_PROP, method=c("pearson"), use = "complete.obs")
SBF_prop_E
#cor= -0.5099176 p= 0.01533

##SBK
#Catch
SBK_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$SBK_LOG, method=c("pearson"), use = "complete.obs")
SBK_Catch_E
#cor=NA p=NA

#Prop abundance 
SBK_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$SBK_PROP, method=c("pearson"), use = "complete.obs")
SBK_prop_E
#cor=NA p=NA

##SKR
#Catch
SKR_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$SKR_LOG, method=c("pearson"), use = "complete.obs")
SKR_Catch_E
#cor=-0.3538588 p=0.1062

#Prop abundance 
SKR_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$SKR_PROP, method=c("pearson"), use = "complete.obs")
SKR_prop_E
#cor= -0.2793393 p= 0.208

##SMB
#Catch
SMB_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$SMB_LOG, method=c("pearson"), use = "complete.obs")
SMB_Catch_E
#cor= 0.4429669 p= 0.03896

#Prop abundance 
SMB_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$SMB_PROP, method=c("pearson"), use = "complete.obs")
SMB_prop_E
#cor= 0.3836129 p= 0.078

##SPB
#Catch
SPB_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$SPB_LOG, method=c("pearson"), use = "complete.obs")
SPB_Catch_E
#cor= 0.6326154  p= 0.001581

#Prop abundance 
SPB_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$SPB_PROP, method=c("pearson"), use = "complete.obs")
SPB_prop_E
#cor= 0.5480851  p=  0.008271

##STB
#Catch
STB_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$STB_LOG, method=c("pearson"), use = "complete.obs")
STB_Catch_E
#cor= 0.2548 p= 0.2525

#Prop abundance 
STB_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$STB_PROP, method=c("pearson"), use = "complete.obs")
STB_prop_E
#cor=0.2548 p=  0.2525

##TUP
#Catch
TUP_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$TUP_LOG, method=c("pearson"), use = "complete.obs")
TUP_Catch_E
#cor= 0.01150196 p= 0.9595

#Prop abundance 
TUP_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$TUP_PROP, method=c("pearson"), use = "complete.obs")
TUP_prop_E
#cor= -0.1511485  p= 0.5019

##WCF
#Catch
WCF_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$WCF_LOG, method=c("pearson"), use = "complete.obs")
WCF_Catch_E
#cor= 0.218974 p= 0.3275

#Prop abundance 
WCF_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$WCF_PROP, method=c("pearson"), use = "complete.obs")
WCF_prop_E
#cor= 0.2679048 p= 0.228

##WRM
#Catch
WRM_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$WRM_LOG, method=c("pearson"), use = "complete.obs")
WRM_Catch_E
#cor= -0.4216976  p= 0.05061

#Prop abundance 
WRM_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$WRM_PROP, method=c("pearson"), use = "complete.obs")
WRM_prop_E
#cor=NA p=NA????

##YFG
#Catch
YFG_Catch_E<- cor.test(SiteE_Abund$YEAR, SiteE_Abund$YFG_LOG, method=c("pearson"), use = "complete.obs")
YFG_Catch_E
#cor=NA p=NA

#Prop abundance 
YFG_prop_E <-cor.test(SiteE_Abund$YEAR, SiteE_Abund$YFG_PROP, method=c("pearson"), use = "complete.obs")
YFG_prop_E
#cor=NA p=NA






##SITE F##
SiteF_Abund<- ShannonFile2 %>% 
  filter(SITE=="10")

##BBH
#Catch
BBH_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$BBH_LOG, method=c("pearson"), use = "complete.obs")
BBH_Catch_F
#cor= 0.05061 p= 0.008128

#Prop abundance 
BBH_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$BBH_PROP, method=c("pearson"), use = "complete.obs")
BBH_prop_F
#cor= -0.452261 p= 0.02649

##BCR
#Catch
BCR_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$BCR_LOG, method=c("pearson"), use = "complete.obs")
BCR_Catch_F
#cor= -0.5924572 p= 0.002285

#Prop abundance 
BCR_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$BCR_PROP, method=c("pearson"), use = "complete.obs")
BCR_prop_F
#cor= -0.5094391 p= 0.011

##BGS
#Catch
BGS_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$BGS_LOG, method=c("pearson"), use = "complete.obs")
BGS_Catch_F
#cor= -0.436271 p= 0.03307

#Prop abundance 
BGS_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$BGS_PROP, method=c("pearson"), use = "complete.obs")
BGS_prop_F
#cor= -0.4985987 p= 0.01314

##BLP
#Catch
BLP_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$BLP_LOG, method=c("pearson"), use = "complete.obs")
BLP_Catch_F
#cor= 0.4035772 p= 0.0505

#Prop abundance 
BLP_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$BLP_PROP, method=c("pearson"), use = "complete.obs")
BLP_prop_F
#cor= 0.3268486 p= 0.119

##BRB
#Catch
BRB_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$BRB_LOG, method=c("pearson"), use = "complete.obs")
BRB_Catch_F
#cor= NA p=NA

#Prop abundance 
BRB_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$BRB_PROP, method=c("pearson"), use = "complete.obs")
BRB_prop_F
#cor=NA p=NA

##CCF
#Catch
CCF_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$CCF_LOG, method=c("pearson"), use = "complete.obs")
CCF_Catch_F
#cor= -0.2079825 p= 0.3294

#Prop abundance 
CCF_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$CCF_PROP, method=c("pearson"), use = "complete.obs")
CCF_prop_F
#cor= -0.1764855 p= 0.4094

##CHN
#Catch
CHN_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$CHN_LOG, method=c("pearson"), use = "complete.obs")
CHN_Catch_F
#cor=NA p=NA

#Prop abundance 
CHN_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$CHN_PROP, method=c("pearson"), use = "complete.obs")
CHN_prop_F
#Cor=NA p=NA

##COT
#Catch
COT_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$COT_LOG, method=c("pearson"), use = "complete.obs")
COT_Catch_F
#cor= -0.05227405 p= 0.8083

#Prop abundance
COT_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$COT_PROP, method=c("pearson"), use = "complete.obs")
COT_prop_F
#cor= 0.2809049 p= 0.1836

##CRP
#Catch
CRP_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$CRP_LOG, method=c("pearson"), use = "complete.obs")
CRP_Catch_F
#cor= -0.3876781 p= 0.06123

#Prop abundance 
CRP_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$CRP_PROP, method=c("pearson"), use = "complete.obs")
CRP_prop_F
#cor= -0.2259518 p= 0.2884

##FHM
#Catch
FHM_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$FHM_LOG, method=c("pearson"), use = "complete.obs")
FHM_Catch_F
#cor= -0.7092619  p= 0.0001042

#Prop abundance 
FHM_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$FHM_PROP, method=c("pearson"), use = "complete.obs")
FHM_prop_F
#cor= -0.5622824  p= 0.004238

##GLF
#Catch
GLF_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$GLF_LOG, method=c("pearson"), use = "complete.obs")
GLF_Catch_F
#cor= -0.3209819  p=  0.1262

#Prop abundance
GLF_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$GLF_PROP, method=c("pearson"), use = "complete.obs")
GLF_prop_F
#cor= -0.1709257  p= 0.4245

##GSF
#Catch
GSF_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$GSF_LOG, method=c("pearson"), use = "complete.obs")
GSF_Catch_F
#cor= -0.6638756 p= 0.0004044

#Prop abundance 
GSF_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$GSF_PROP, method=c("pearson"), use = "complete.obs")
GSF_prop_F
#cor= -0.4073554  p= 0.04818

##GSH
#Catch
GSH_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$GSH_LOG, method=c("pearson"), use = "complete.obs")
GSH_Catch_F
#cor= 0.3343676  p= 0.1103

#Prop abundance 
GSH_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$GSH_PROP, method=c("pearson"), use = "complete.obs")
GSH_prop_F
#cor= 0.2896682 p= 0.1698

##ISS
#Catch
ISS_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$ISS_LOG, method=c("pearson"), use = "complete.obs")
ISS_Catch_F
#cor= 0.4141709  p= 0.04421

#Prop abundance 
ISS_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$ISS_PROP, method=c("pearson"), use = "complete.obs")
ISS_prop_F
#cor= 0.5364999 p= 0.006877

##LEP
#Catch
LEP_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$LEP_LOG, method=c("pearson"), use = "complete.obs")
LEP_Catch_F
#cor= -0.2423962  p= 0.2538

#Prop abundance 
LEP_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$LEP_PROP, method=c("pearson"), use = "complete.obs")
LEP_prop_F
#cor= 0.01028424 p=0.962

##LMB
#Catch
LMB_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$LMB_LOG, method=c("pearson"), use = "complete.obs")
LMB_Catch_F
#cor= 0.6416328 p=0.0007263

#Prop abundance 
LMB_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$LMB_PROP, method=c("pearson"), use = "complete.obs")
LMB_prop_F
#cor= 0.7681563 p=1.171e-05

##MSQ
#Catch
MSQ_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$MSQ_LOG, method=c("pearson"), use = "complete.obs")
MSQ_Catch_F
#cor= -0.3372909 p= 0.107

#Prop abundance 
MSQ_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$MSQ_PROP, method=c("pearson"), use = "complete.obs")
MSQ_prop_F
#cor= -0.3066879  p=0.1449

##PKM
#Catch
PKM_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$PKM_LOG, method=c("pearson"), use = "complete.obs")
PKM_Catch_F
#cor= 0.526762  p= 0.008179

#Prop abundance
PKM_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$PKM_PROP, method=c("pearson"), use = "complete.obs")
PKM_prop_F
#cor= 0.5829574 p= 0.002793

##PLR
#Catch
PLR_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$PLR_LOG, method=c("pearson"), use = "complete.obs")
PLR_Catch_F
#cor= -0.1093071 p= 0.6111

#Prop abundance
PLR_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$PLR_PROP, method=c("pearson"), use = "complete.obs")
PLR_prop_F
#cor= -0.1093071 p=0.6111

##PMK
#Catch
PMK_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$PMK_LOG, method=c("pearson"), use = "complete.obs")
PMK_Catch_F
#cor= 0.01946999 p=0.928

#Prop abundance
PMK_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$PMK_PROP, method=c("pearson"), use = "complete.obs")
PMK_prop_F
#cor= 0.01438166 p= 0.9468

##RBT
#Catch
RBT_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$RBT_LOG, method=c("pearson"), use = "complete.obs")
RBT_Catch_F
#cor=NA p=NA

#Prop abundance 
RBT_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$RBT_PROP, method=c("pearson"), use = "complete.obs")
RBT_prop_F
#cor=NA p=NA

##RCH
#Catch
RCH_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$RCH_LOG, method=c("pearson"), use = "complete.obs")
RCH_Catch_F
#cor=0.2803966 p=0.1845

#Prop abundance
RCH_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$RCH_PROP, method=c("pearson"), use = "complete.obs")
RCH_prop_F
#cor=-0.2803966  p=0.1845

##RES
#Catch
RES_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$RES_LOG, method=c("pearson"), use = "complete.obs")
RES_Catch_F
#cor= 0.6173998 p= 0.001308

#Prop abundance
RES_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$RES_PROP, method=c("pearson"), use = "complete.obs")
RES_prop_F
#cor= 0.5255984 p= 0.008347

##RSH
#Catch
RSH_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$RSH_LOG, method=c("pearson"), use = "complete.obs")
RSH_Catch_F
#cor= -0.07753533 p= 0.7188

#Prop abundance
RSH_prop_F<-cor.test(SiteF_Abund$YEAR, SiteF_Abund$RSH_PROP, method=c("pearson"), use = "complete.obs")

RSH_prop_F
#cor= -0.1871353 p= 0.3812

##SAP
#Catch
SAP_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$SAP_LOG, method=c("pearson"), use = "complete.obs")
SAP_Catch_F
#cor=NA p=NA

#Prop abundance 
SAP_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$SAP_PROP, method=c("pearson"), use = "complete.obs")
SAP_prop_F
#cor=NA p=NA

##SBF
#Catch
SBF_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$SBF_LOG, method=c("pearson"), use = "complete.obs")
SBF_Catch_F
#cor= -0.593802 p= 0.00222

#Prop abundance 
SBF_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$SBF_PROP, method=c("pearson"), use = "complete.obs")
SBF_prop_F
#cor= -0.3449143 p= 0.09881

##SBK
#Catch
SBK_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$SBK_LOG, method=c("pearson"), use = "complete.obs")
SBK_Catch_F
#cor=NA p=NA

#Prop abundance 
SBK_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$SBK_PROP, method=c("pearson"), use = "complete.obs")
SBK_prop_F
#cor=NA p=NA

##SKR
#Catch
SKR_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$SKR_LOG, method=c("pearson"), use = "complete.obs")
SKR_Catch_F
#cor= 0.3009251  p= 0.153

#Prop abundance 
SKR_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$SKR_PROP, method=c("pearson"), use = "complete.obs")
SKR_prop_F
#cor= 0.2975015 p= 0.158

##SMB
#Catch
SMB_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$SMB_LOG, method=c("pearson"), use = "complete.obs")
SMB_Catch_F
#cor= 0.1140756 p=0.5956

#Prop abundance 
SMB_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$SMB_PROP, method=c("pearson"), use = "complete.obs")
SMB_prop_F
#cor= 0.1182638  p= 0.5821

##SPB
#Catch
SPB_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$SPB_LOG, method=c("pearson"), use = "complete.obs")
SPB_Catch_F
#cor= 0.4942729 p= 0.01409

#Prop abundance 
SPB_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$SPB_PROP, method=c("pearson"), use = "complete.obs")
SPB_prop_F
#cor=  0.5057895 p= 0.01168

##STB
#Catch
STB_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$STB_LOG, method=c("pearson"), use = "complete.obs")
STB_Catch_F
#cor= -0.2705487 p=0.201

#Prop abundance 
STB_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$STB_PROP, method=c("pearson"), use = "complete.obs")
STB_prop_F
#cor= -0.289065 p= 0.1707

##TUP
#Catch
TUP_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$TUP_LOG, method=c("pearson"), use = "complete.obs")
TUP_Catch_F
#cor= 0.2004264 p= 0.3477

#Prop abundance 
TUP_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$TUP_PROP, method=c("pearson"), use = "complete.obs")
TUP_prop_F
#cor= 0.2063327 p= 0.3334

##WCF
#Catch
WCF_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$WCF_LOG, method=c("pearson"), use = "complete.obs")
WCF_Catch_F
#cor= 0.4392776 p= 0.03174

#Prop abundance 
WCF_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$WCF_PROP, method=c("pearson"), use = "complete.obs")
WCF_prop_F
#cor= 0.4686215 p= 0.0209

##WRM
#Catch
WRM_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$WRM_LOG, method=c("pearson"), use = "complete.obs")
WRM_Catch_F
#cor= -0.07166495 p= 0.7393

#Prop abundance 
WRM_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$WRM_PROP, method=c("pearson"), use = "complete.obs")
WRM_prop_F
#cor=NA p=NA???

##YFG
#Catch
YFG_Catch_F<- cor.test(SiteF_Abund$YEAR, SiteF_Abund$YFG_LOG, method=c("pearson"), use = "complete.obs")
YFG_Catch_F
#cor= -0.1093071  p= 0.6111

#Prop abundance 
YFG_prop_F <-cor.test(SiteF_Abund$YEAR, SiteF_Abund$YFG_PROP, method=c("pearson"), use = "complete.obs")
YFG_prop_F
#cor= -0.1093071 p= 0.6111

