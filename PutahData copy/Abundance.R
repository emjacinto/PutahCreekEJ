
setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto/PutahData")
library(vegan)
Putah_abund <- read.csv("Putah_Vegan_abund.csv")

?radfit
rad <- radfit(Putah_abund)

summary(rad)

#plot(Putah_abund, xlab = "Rank", ylab = "Abundance", type = "b", ...)
radlattice(x, BIC = FALSE, ...)