
#Putah Shannon Diversity Index 

setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto")

library(tidyverse)
library(dplyr)

library(vegan)
library(data.table)
library(ggpubr)

Shannon <- read.csv("PutahData/ShannonPutah.csv")

#isolate date, remove site and year columns
Shannon1993 <- Shannon[Shannon$YEAR == '1993',]
Shannon1993 <- Shannon1993[-c(1:2)]
Shannon1993 <- diversity(Shannon1993, index = "shannon")
print(Shannon1993)


Shannon1994 <- Shannon[Shannon$YEAR == '1994',]
Shannon1994 <- Shannon1994[-c(1:2)]
Shannon1994 <- diversity(Shannon1994, index = "shannon")
print(Shannon1994)

Shannon1995 <- Shannon[Shannon$YEAR == '1995',]
Shannon1995 <- Shannon1995[-c(1:2)]
Shannon1995 <- diversity(Shannon1995, index = "shannon")
print(Shannon1995)

Shannon1996 <- Shannon[Shannon$YEAR == '1996',]
Shannon1996 <- Shannon1996[-c(1:2)]
Shannon1996 <- diversity(Shannon1996, index = "shannon")
print(Shannon1996)

Shannon1997 <- Shannon[Shannon$YEAR == '1997',]
Shannon1997 <- Shannon1997[-c(1:2)]
Shannon1997 <- diversity(Shannon1997, index = "shannon")
print(Shannon1997)

Shannon1998 <- Shannon[Shannon$YEAR == '1998',]
Shannon1998 <- Shannon1998[-c(1:2)]
Shannon1998 <- diversity(Shannon1998, index = "shannon")
print(Shannon1998)


Shannon1999 <- Shannon[Shannon$YEAR == '1999',]
Shannon1999 <- Shannon1999[-c(1:2)]
Shannon1999 <- diversity(Shannon1999, index = "shannon")
print(Shannon1999)

Shannon2000 <- Shannon[Shannon$YEAR == '2000',]
Shannon2000 <- Shannon2000[-c(1:2)]
Shannon2000 <- diversity(Shannon2000, index = "shannon")
print(Shannon2000)

Shannon2001 <- Shannon[Shannon$YEAR == '2001',]
Shannon2001 <- Shannon2001[-c(1:2)]
Shannon2001 <- diversity(Shannon2001, index = "shannon")
print(Shannon2001)

Shannon2002 <- Shannon[Shannon$YEAR == '2002',]
Shannon2002 <- Shannon2002[-c(1:2)]
Shannon2002 <- diversity(Shannon2002, index = "shannon")
print(Shannon2002)

Shannon2003 <- Shannon[Shannon$YEAR == '2003',]
Shannon2003 <- Shannon2003[-c(1:2)]
Shannon2003 <- diversity(Shannon2003, index = "shannon")
print(Shannon2003)

Shannon2004 <- Shannon[Shannon$YEAR == '2004',]
Shannon2004 <- Shannon2004[-c(1:2)]
Shannon2004 <- diversity(Shannon2004, index = "shannon")
print(Shannon2004)

Shannon2005 <- Shannon[Shannon$YEAR == '2005',]
Shannon2005 <- Shannon2005[-c(1:2)]
Shannon2005 <- diversity(Shannon2005, index = "shannon")
print(Shannon2005)

Shannon2006 <- Shannon[Shannon$YEAR == '2006',]
Shannon2006 <- Shannon2006[-c(1:2)]
Shannon2006 <- diversity(Shannon2006, index = "shannon")
print(Shannon2006)

Shannon2007 <- Shannon[Shannon$YEAR == '2007',]
Shannon2007 <- Shannon2007[-c(1:2)]
Shannon2007 <- diversity(Shannon2007, index = "shannon")
print(Shannon2007)

Shannon2008 <- Shannon[Shannon$YEAR == '2008',]
Shannon2008 <- Shannon2008[-c(1:2)]
Shannon2008 <- diversity(Shannon2008, index = "shannon")
print(Shannon2008)

#No data for 2009 

Shannon2010 <- Shannon[Shannon$YEAR == '2010',]
Shannon2010 <- Shannon2010[-c(1:2)]
Shannon2010 <- diversity(Shannon2010, index = "shannon")
print(Shannon2010)

Shannon2011 <- Shannon[Shannon$YEAR == '2011',]
Shannon2011 <- Shannon2011[-c(1:2)]
Shannon2011 <- diversity(Shannon2011, index = "shannon")
print(Shannon2011)

Shannon2012 <- Shannon[Shannon$YEAR == '2012',]
Shannon2012 <- Shannon2012[-c(1:2)]
Shannon2012 <- diversity(Shannon2012, index = "shannon")
print(Shannon2012)

Shannon2013 <- Shannon[Shannon$YEAR == '2013',]
Shannon2013 <- Shannon2013[-c(1:2)]
Shannon2013 <- diversity(Shannon2013, index = "shannon")
print(Shannon2013)

Shannon2014 <- Shannon[Shannon$YEAR == '2014',]
Shannon2014 <- Shannon2014[-c(1:2)]
Shannon2014 <- diversity(Shannon2014, index = "shannon")
print(Shannon2014)

Shannon2015 <- Shannon[Shannon$YEAR == '2015',]
Shannon2015 <- Shannon2015[-c(1:2)]
Shannon2015 <- diversity(Shannon2015, index = "shannon")
print(Shannon2015)

Shannon2016 <- Shannon[Shannon$YEAR == '2016',]
Shannon2016 <- Shannon2016[-c(1:2)]
Shannon2016 <- diversity(Shannon2016, index = "shannon")
print(Shannon2016)

Shannon2017 <- Shannon[Shannon$YEAR == '2017',]
Shannon2017 <- Shannon2017[-c(1:2)]
Shannon2017 <- diversity(Shannon2017, index = "shannon")
print(Shannon2017)

Shannon_Table <-data.table(Shannon1993, Shannon1994, Shannon1995, Shannon1996, Shannon1997, Shannon1998, Shannon1999, Shannon2000, Shannon2001, Shannon2002, Shannon2003, Shannon2004, Shannon2005, Shannon2006, Shannon2007,Shannon2008, Shannon2010, Shannon2011, Shannon2012, Shannon2013, Shannon2014, Shannon2015, Shannon2016, Shannon2017)

print(Shannon_Table)

#cite vegan- in vegan notes 

Shannon_table<- fwrite(Shannon_Table,"PutahData/Shannon_Table.csv")


Map_csv <- read.csv("PutahData/Shannon_Table2.csv")


SiteA<- Map_csv %>% # Site A
  ggplot(aes(x = Map_csv$X, y= Map_csv$A)) +
  geom_point() + 
  geom_smooth(method="lm") +
  xlab("Year")+
  ylab("Shannon Index")+
  ylim(0,2.25)
SiteA

SiteB <-Map_csv %>% # Site B
  ggplot(aes(x = Map_csv$X, y= Map_csv$B)) +
  geom_point() + 
  geom_smooth(method="lm")+
  xlab("Year")+
  ylab("Shannon Index")+
  ylim(0,2.25)

SiteC <- Map_csv %>% # Site C
  ggplot(aes(x = Map_csv$X, y= Map_csv$C)) +
  geom_point() + 
  geom_smooth(method="lm")+
  xlab("Year")+
  ylab("Shannon Index")+
  ylim(0,2.25)

SiteD <- Map_csv %>% # Site D
  ggplot(aes(x = Map_csv$X, y= Map_csv$D)) +
  geom_point() + 
  geom_smooth(method="lm") +
  xlab("Year")+
  ylab("Shannon Index")+
  ylim(0,2.25)


SiteE <- Map_csv %>% # Site E
  ggplot(aes(x = Map_csv$X, y= Map_csv$E)) +
  geom_point() + 
  geom_smooth(method="lm")+
  xlab("Year")+
  ylab("Shannon Index")+
  ylim(0,2.25)


SiteF <- Map_csv %>% # Site F
  ggplot(aes(x = Map_csv$X, y= Map_csv$F)) +
  geom_point() + 
  geom_smooth(method="lm")+
  xlab("Year") +
  ylab("Shannon Index")+
  ylim(0,2.25)
SiteF


ALLSITES <- ggarrange(SiteA,SiteB,SiteC,SiteD, SiteE, SiteF, 
          labels = c("A", "B", "C","D","E","F"),
          ncol = 2, nrow = 3)
ALLSITES

ggsave("ALLSITE_Shannon.pdf")

