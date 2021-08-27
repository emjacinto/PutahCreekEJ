setwd("~/Desktop/R_DAVIS_2020/r-davis-in-class-project-emjacinto/PutahData")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plot3D)

options(stringsAsFactors = FALSE)

Flow <- read_csv("Flow_AllSites_2.csv")
Flow2 <- read_csv("Flow_From_dam_78-17.csv")


Mean_Flow <-Flow %>% 
  group_by(Month,Year) %>% #mean monthly
  summarise(mean_flow= mean(`Converted to Released CMS`))
  
  
  
  ggplot(aes(x = Month, y = mean_flow))+
  geom_line()+
  facet_wrap(~Year)


Flow %>%  #sum monthly
  group_by(Month,Year) %>% 
  summarise(sum_flow = sum(`Converted to Released CMS`)) %>% 
  ggplot(aes(x = Month, y = sum_flow)) +
  geom_line() +
  facet_wrap(~Year)

#group_by(Day,Month,Year) %>% 


Flow_all <- Flow %>%
  ggplot(aes(x = Year, y = Flow$`Converted to Released CMS`))+
  geom_line()+
  xlim(1993,2017)+
  ylab("Daily Release (m^3/s)")
Flow_all
ggsave("Flow_all.pdf")


SpFlow <- read.csv("Flow_from_dam_spring.csv")

Flow_sp <- SpFlow %>%
  ggplot(aes(x =SpFlow$Year, y=SpFlow$Mean.spring.flow..Mar1.May31., group=1)) +
  geom_point()+
  geom_smooth()+
  ylab("Mean Spring Release (m^3/s)") +
  xlab("Year")
 
Flow_sp 
ggsave("Flow_sp.pdf")



  
## 3D graph 
# x, y and z coordinates
#x <- Month <- Flow$Month
#y <- year <- Flow$Year
#z <- CMS <- Flow$`Converted to Released CMS`

#Flow3d1 <- scatter3D(x, y, z, clab = c("Converted to Released CMS"), pch = 19, cex = 0.5,
          #main = "Flow data", xlab = "Month",
         # ylab ="Year", zlab = "Discharge(m^3/s)",ticktype = "detailed", bty ="g", phi = 20)

#Flow3d2 <- scatter3D(x, y, z, clab = c("Converted to Released CMS"), pch = 19, cex = 0.5,
         # main = "Flow data", xlab = "Month",
          #ylab ="Year", zlab = "Discharge(m^3/s)",ticktype = "detailed", type="l",theta = 60, phi = 20)
          


#Karline Soetaert. plot3D: Tools for plotting 3-D and 2-D data. http://cran.r-project.org/web/packages/plot3D/vignettes/plot3D.pdf


#Cite above
# x, y and z coordinates
#CFS


y <- Day <- Flow2$`Day of the Year`
x <- year <- Flow2$Year
z <- CFS <- Flow2$`Released CMS`
Flow3d3 <- scatter3D(x, y, z, clab = c("Released CMS"), pch = 20, cex = .5, type ="h", ylab = "Day of the Year", zlab= "CMS" ,cex.axis=0.75, xlab ="Year", ticktype = "detailed", tck=0, type="l", theta = 30, phi = 30, bty = "b", col = ramp.col(c("red2","orange","yellow2","green4","dodgerblue","blue3")) )



#July-Oct, CMS
Flow5 <- Flow2 %>% 
  filter( 181 <`Day of the Year`) %>% 
  filter(`Day of the Year`<305)
y3 <- Day <- Flow5$`Day of the Year`
x3 <- year <- Flow5$Year
z3 <- CMS <- Flow5$`Released CMS`
Flow3d6 <- scatter3D(x3, y3, z3, clab = c("Released CMS"), pch = 18, cex = .5, type ="h", ylab = "Day of the Year", zlab= "CMS" ,cex.axis=0.5, cex.names=1, xlab ="Year", ticktype = "detailed", bty = "b",  tck=0, type="l", theta = 30, phi = 50, col = ramp.col(c("red2","orange","yellow2","green4","dodgerblue","blue3")))

