#Problem1
library(tidyr)
library(dplyr)
library(ggplot2)
Water_data <- read.csv(file="marine_water_quality.csv",encoding="UTF-8",header = T)
Water_data2 <- as_tibble(Water_data)
#Boxplot
Water_data2 %>%
  filter(Depth=="Surface Water") %>%
  mutate(date=substr(Dates,1,3),date2=as.numeric(date),Temperature=as.numeric(Temperature...C.)) %>%
  mutate(Decades=ifelse(date2==198,"1980s",ifelse(date2==199,"1990s",ifelse(date2==200,"2000s","2010s")))) %>%
  ggplot(aes(x=Decades,y=Temperature,fill=Decades)) +
  geom_boxplot() +
  theme_classic() +
  labs(title="Temperature of each decade", x="Decades", y="Temperature") +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))

#Time series
Water_data2 %>%
  filter(Depth=="Surface Water") %>%
  select(Dates,Total.Nitrogen..mg.L.) %>%
  mutate(date3=as.Date(Dates),Total_Nitrogen=as.numeric(Total.Nitrogen..mg.L.)) %>%
  ggplot(aes(x=date3,y=Total_Nitrogen)) +
  geom_line() +
  theme_classic() +
  labs(title="Total Nitrogen of surface", x="date", y="Total Nitrogen(mg/L)") +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))

#Histogram
Water_data2 %>%
  filter(Depth=="Surface Water") %>%
  select(pH) %>%
  mutate(ph=as.numeric(pH)) %>%
  ggplot(aes(x=ph)) +
  geom_histogram(color="black",bins=10,alpha=0.5) +
  theme_classic() +
  labs(title="ph of water", x="ph", y="count") +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))

#Scatter plot
Water_data2 %>%
  filter(Depth=="Surface Water") %>%
  select(Dates,Temperature...C.) %>%
  mutate(date=substr(Dates,1,4),Temperature2=as.numeric(Temperature...C.)) %>%
  mutate(date4=as.numeric(date)) %>%
  group_by(date4) %>%
  summarise(annual_temperature=mean(Temperature2,na.rm=T)) %>%
  ggplot(aes(x=date4,y=annual_temperature)) +
  geom_point() +
  geom_smooth() +
  theme_classic() +
  labs(title="annual temperature of water", x="date", y="annual_temperature") +
  theme(plot.title=element_text(size=20, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15))
  
#Image plot
library(fields)
library(maps)
library(RNetCDF)
ex.nc <- open.nc("air.mon.ltm.nc")
print.nc(ex.nc)
Lat       <- var.get.nc(ex.nc, "lat")
Lon       <- var.get.nc(ex.nc, "lon")
Air_T     <- var.get.nc(ex.nc, "air")
close.nc(ex.nc)
Lat <- rev(Lat)
Air_T_Jul <- array(NA,dim=c(length(Lon), length(Lat)))
for(row in 1:length(Lat)){
  Air_T_Jul[,row] <- Air_T[, (length(Lat)+1-row),7 ]
}
par(mar=c(4.5,3,2,1))
image.plot(Lon, Lat, Air_T_Jul,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="Surface Temperature [degC]",cex=1.25),           
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(Lon),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(Lat),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("Long term (1800-2020) mean surface temperature in Jul."),
      cex.main=1,font.main=2)
map('world',add=T,lwd=0.75,col="black")
box(lwd=2)