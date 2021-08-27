# Correlation Coefficients table 1 
setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto")
library(ggplot2)
library(tidyverse)
library(vegan)
library(dplyr)
library(data.table)
library(ggpubr)
library(data.table)

#correlate index values at each site to year
#Do a “Spearman correlation” for Shannon’s and Pielou’s 
#and a Pearson correlation for Richness

#Test at P<0.05 = significant. Then fill in this table with the correlation coefficients. Bold any values that are significant.

#cor(x, y, method = c("pearson", "kendall", "spearman")), value
#cor.test(x, y, method=c("pearson", "kendall", "spearman")), more values and stats 
#cor(x, y,  method = "pearson", use = "complete.obs"),If your data contain missing values, use the following R code to handle missing values by case-wise deletion.

# http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
# Shannon - Spearman

Shannon <- read.csv("PutahData/Shannon_Table2.csv")
str(Shannon)
SiteA_SP <- cor.test(Shannon$X, Shannon$A, method=c("spearman"),use = "complete.obs")
SiteA_SP
#Rho= -0.3224167  p=.1434
SiteB_SP <- cor.test(Shannon$X, Shannon$B, method=c("spearman"), use = "complete.obs", exact=F)
SiteB_SP
#Cannot compute exact p-value with ties
#Rho= -0.3563965 p= 0.1035
SiteC_SP <- cor.test(Shannon$X, Shannon$C, method=c("spearman"), use = "complete.obs", exact=F)
SiteC_SP
#Cannot compute exact p-value with ties
#Rho= -0.7002718   p= 0.0001989
SiteD_SP <- cor.test(Shannon$X, Shannon$D, method=c("spearman"), use = "complete.obs")
SiteD_SP
#Rho= -0.5266798 p=0.01081
SiteE_SP <- cor.test(Shannon$X, Shannon$E, method=c("spearman"), use = "complete.obs")
SiteE_SP
#Rho= 0.1112366   p=0.621
SiteF_SP <- cor.test(Shannon$X, Shannon$F, method=c("spearman"), use = "complete.obs")
SiteF_SP
#Rho=0.3104348 p=0.1398



# “Spearman correlation” for Pielou’s 

Pielous <- read.csv("PutahData/PielousTable2.csv")

SiteA_SPP <- cor.test(Pielous$X, Pielous$A, method=c("spearman"), use = "complete.obs")
SiteA_SPP
#Cannot compute exact p-value with ties
#Rho= 0.1039254  p= 0.6453
SiteB_SPP <- cor.test(Pielous$X, Pielous$B, method=c("spearman"), use = "complete.obs")
SiteB_SPP
#Rho=0.1473744  p=0.5112
SiteC_SPP <- cor.test(Pielous$X, Pielous$C, method=c("spearman"), use = "complete.obs")
SiteC_SPP
#Rho= -0.3636364 p=0.08876
SiteD_SPP <- cor.test(Pielous$X, Pielous$D, method=c("spearman"), use = "complete.obs")
SiteD_SPP
#Cannot compute exact p-value with ties
#Rho=-0.3943662   p=0.06258
SiteE_SPP <- cor.test(Pielous$X, Pielous$E, method=c("spearman"), use = "complete.obs")
SiteE_SPP
#Cannot compute exact p-value with ties
#Rho=0.2778876   p= 0.2105
SiteF_SPP <- cor.test(Pielous$X, Pielous$F, method=c("spearman"), use = "complete.obs")
SiteF_SPP
#Cannot compute exact p-value with ties
#Rho=0.5022831  p= 0.01238

#Pearson correlation for Richness

Richness <- read.csv("PutahData/NvsNN_table2.csv")
# add native and nonnative into dataframe or maybe add there

SiteA_R <- cor.test(Richness$SiteA_N1+Richness$SiteA_NN1, Richness$Year, method=c("pearson"),use = "complete.obs")
SiteA_R
#cor= -0.4834499  p= 0.02264

SiteB_R <- cor.test(Richness$SiteB_N+Richness$SiteB_NN, Richness$Year, method=c("pearson"),use = "complete.obs")
SiteB_R
#cor= -0.7221768  p=0.0001476

SiteC_R <- cor.test(Richness$SiteC_N+Richness$SiteC_NN, Richness$Year, method=c("pearson"),use = "complete.obs")
SiteC_R 
#cor= -0.8359566  p=6.812e-07

SiteD_R <- cor.test(Richness$SiteD_N+Richness$SiteD_NN, Richness$Year, method=c("pearson"),use = "complete.obs")
SiteD_R
#cor=-0.3457095  p=0.1061

SiteE_R <- cor.test(Richness$SiteE_N+Richness$SiteE_NN, Richness$Year, method=c("pearson"),use = "complete.obs")
SiteE_R
#cor=-0.3583015  p=0.1016

SiteF_R <- cor.test(Richness$SiteF_N+Richness$SiteF_NN, Richness$Year, method=c("pearson"),use = "complete.obs")
SiteF_R
#cor= -0.2447128 p=0.2491


