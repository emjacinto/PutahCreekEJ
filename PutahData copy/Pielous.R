#Pielou's Index

setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto")

library(tidyverse)
library(vegan)
library(data.table)
library(ggpubr)

#cite diversity-vegan and vegan in R
citation("vegan")

Shannon <- read.csv("PutahData/ShannonPutah.csv")


#isolate date, remove site and year columns
Pielou1993 <- Shannon[Shannon$YEAR == '1993',]
Pielou1993 <- Pielou1993[-c(1:2)]
H93 <- diversity(Pielou1993) #Pielou's Index
J93 <- H93/log(specnumber(Pielou1993))
print(J93)


Pielou1994 <- Shannon[Shannon$YEAR == '1994',]
Pielou1994 <- Pielou1994[-c(1:2)]
H94 <- diversity(Pielou1994) #Pielou's Index
J94 <- H94/log(specnumber(Pielou1994))
print(J94)


Pielou1995 <- Shannon[Shannon$YEAR == '1995',]
Pielou1995 <- Pielou1995[-c(1:2)]
H95 <- diversity(Pielou1995) #Pielou's Index
J95 <- H95/log(specnumber(Pielou1995))
print(J95)


Pielou1996 <- Shannon[Shannon$YEAR == '1996',]
Pielou1996 <- Pielou1996[-c(1:2)]
H96 <- diversity(Pielou1996) #Pielou's Index
J96 <- H96/log(specnumber(Pielou1996))
print(J96)


Pielou1997 <- Shannon[Shannon$YEAR == '1997',]
Pielou1997 <- Pielou1997[-c(1:2)]
H97 <- diversity(Pielou1997) #Pielou's Index
J97 <- H97/log(specnumber(Pielou1997))
print(J97)


Pielou1998 <- Shannon[Shannon$YEAR == '1998',]
Pielou1998 <- Pielou1998[-c(1:2)]
H98 <- diversity(Pielou1998) #Pielou's Index
J98 <- H98/log(specnumber(Pielou1998))
print(J98)


Pielou1999 <- Shannon[Shannon$YEAR == '1999',]
Pielou1999 <- Pielou1999[-c(1:2)]
H99 <- diversity(Pielou1999) #Pielou's Index
J99 <- H99/log(specnumber(Pielou1999))
print(J99)


Pielou2000 <- Shannon[Shannon$YEAR == '2000',]
Pielou2000 <- Pielou2000[-c(1:2)]
H00 <- diversity(Pielou2000) #Pielou's Index
J00 <- H00/log(specnumber(Pielou2000))
print(J00)

Pielou2001 <- Shannon[Shannon$YEAR == '2001',]
Pielou2001 <- Pielou2001[-c(1:2)]
H01 <- diversity(Pielou2001) #Pielou's Index
J01 <- H01/log(specnumber(Pielou2001))
print(J01)

Pielou2002 <- Shannon[Shannon$YEAR == '2002',]
Pielou2002 <- Pielou2002[-c(1:2)]
H02 <- diversity(Pielou2002) #Pielou's Index
J02 <- H02/log(specnumber(Pielou2002))
print(J02)


Pielou2003 <- Shannon[Shannon$YEAR == '2003',]
Pielou2003 <- Pielou2003[-c(1:2)]
H03 <- diversity(Pielou2003) #Pielou's Index
J03 <- H03/log(specnumber(Pielou2003))
print(J03)


Pielou2004 <- Shannon[Shannon$YEAR == '2004',]
Pielou2004 <- Pielou2004[-c(1:2)]
H04 <- diversity(Pielou2004) #Pielou's Index
J04 <- H04/log(specnumber(Pielou2004))
print(J04)

Pielou2005 <- Shannon[Shannon$YEAR == '2005',]
Pielou2005 <- Pielou2005[-c(1:2)]
H05 <- diversity(Pielou2005) #Pielou's Index
J05 <- H05/log(specnumber(Pielou2005))
print(J05)

Pielou2006 <- Shannon[Shannon$YEAR == '2006',]
Pielou2006 <- Pielou2006[-c(1:2)]
H06 <- diversity(Pielou2006) #Pielou's Index
J06 <- H06/log(specnumber(Pielou2006))
print(J06)

Pielou2007 <- Shannon[Shannon$YEAR == '2007',]
Pielou2007 <- Pielou2007[-c(1:2)]
H07 <- diversity(Pielou2007) #Pielou's Index
J07 <- H07/log(specnumber(Pielou2007))
print(J07)

Pielou2008 <- Shannon[Shannon$YEAR == '2008',]
Pielou2008 <- Pielou2008[-c(1:2)]
H08 <- diversity(Pielou2008) #Pielou's Index
J08 <- H08/log(specnumber(Pielou2008))
print(J08)

#No data for 2009 

Pielou2010 <- Shannon[Shannon$YEAR == '2010',]
Pielou2010 <- Pielou2010[-c(1:2)]
H10 <- diversity(Pielou2010) #Pielou's Index
J10 <- H10/log(specnumber(Pielou2010))
print(J10)

Pielou2011 <- Shannon[Shannon$YEAR == '2011',]
Pielou2011 <- Pielou2011[-c(1:2)]
H11 <- diversity(Pielou2011) #Pielou's Index
J11 <- H11/log(specnumber(Pielou2011))
print(J11)

Pielou2012 <- Shannon[Shannon$YEAR == '2012',]
Pielou2012 <- Pielou2012[-c(1:2)]
H12 <- diversity(Pielou2012) #Pielou's Index
J12 <- H12/log(specnumber(Pielou2012))
print(J12)


Pielou2013 <- Shannon[Shannon$YEAR == '2013',]
Pielou2013 <- Pielou2013[-c(1:2)]
H13 <- diversity(Pielou2013) #Pielou's Index
J13 <- H13/log(specnumber(Pielou2013))
print(J13)

Pielou2014 <- Shannon[Shannon$YEAR == '2014',]
Pielou2014 <- Pielou2014[-c(1:2)]
H14 <- diversity(Pielou2014) #Pielou's Index
J14 <- H14/log(specnumber(Pielou2014))
print(J14)


Pielou2015 <- Shannon[Shannon$YEAR == '2015',]
Pielou2015 <- Pielou2015[-c(1:2)]
H15 <- diversity(Pielou2015) #Pielou's Index
J15 <- H15/log(specnumber(Pielou2015))
print(J15)


Pielou2016 <- Shannon[Shannon$YEAR == '2016',]
Pielou2016 <- Pielou2016[-c(1:2)]
H16 <- diversity(Pielou2016) #Pielou's Index
J16 <- H16/log(specnumber(Pielou2016))
print(J16)


Pielou2017 <- Shannon[Shannon$YEAR == '2017',]
Pielou2017 <- Pielou2017[-c(1:2)]
H17 <- diversity(Pielou2017) #Pielou's Index
J17 <- H17/log(specnumber(Pielou2017))
print(J17)


Pielous_Table <-data.table(J93, J94, J95, J96, J97, J98, J99, J00, J01, J02, J03, J04, J05, J06, J07, J08, J10, J11, J12, J13, J14, J15, J16, J17)# list all the year dataframes
print(Pielous_Table)


Pielous_table<- fwrite(Pielous_Table,"PutahData/Pielous_Table.csv")


Map_csv <- read.csv("PutahData/PielousTable2.csv")
Map_csv

SiteA<- Map_csv %>% # Site A
  ggplot(aes(x = Map_csv$X, y= Map_csv$A)) +
  geom_point() + 
  geom_smooth(method="lm") +
  xlab("Year")+
  ylab("Pielou's Evenness")+
  ylim(0,1)
SiteA

#generating a linear regression model
# lm- Fitting Linear Model, formula Y~X
mod1 <- lm(Map_csv$A ~ Map_csv$X)

SiteB <-Map_csv %>% # Site B
  ggplot(aes(x = Map_csv$X, y= Map_csv$B)) +
  geom_point() + 
  geom_smooth(method="lm")+
  xlab("Year")+
  ylab("Pielou's Evenness")+
  ylim(0,1)

SiteC <- Map_csv %>% # Site C
  ggplot(aes(x = Map_csv$X, y= Map_csv$C)) +
  geom_point() + 
  geom_smooth(method="lm")+
  xlab("Year")+
  ylab("Pielou's Evenness")+
  ylim(0,1)

SiteD <- Map_csv %>% # Site D
  ggplot(aes(x = Map_csv$X, y= Map_csv$D)) +
  geom_point() + 
  geom_smooth(method="lm") +
  xlab("Year")+
  ylab("Pielou's Evenness")+
  ylim(0,1)


SiteE <- Map_csv %>% # Site E
  ggplot(aes(x = Map_csv$X, y= Map_csv$E)) +
  geom_point() + 
  geom_smooth(method="lm")+
  xlab("Year") +
  ylab("Pielou's Evenness")+
  ylim(0,1)


SiteF <- Map_csv %>% # Site F
  ggplot(aes(x = Map_csv$X, y= Map_csv$F)) +
  geom_point() + 
  geom_smooth(method="lm")+
  xlab("Year") +
  ylab("Pielou's Evenness")+
  ylim(0,1)
SiteF

ALLSITESP <- ggarrange(SiteA,SiteB,SiteC,SiteD, SiteE, SiteF, 
                      labels = c("A", "B", "C","D","E","F"),
                      ncol = 2, nrow = 3)
ALLSITESP

ggsave("ALLSITE_pielous.pdf")

