library(tidyr)
library(dplyr)
library(ggplot2)
library(fields)
library(maps)
library(RNetCDF)
library(sp)
library(rgdal)
library(sf)
library(raster)
library(mapdata)
#处理NO2数据
#设置工作目录
setwd("G:/TROPOMI_NO2")
#读取文件
temp_07 <- list.files(pattern="TROPOMI_NO2_VCD_2019-07.*")
#设置经纬度网格
latt <- c(151:550)
longg <- c(701:1400)
latt2 <- latt/10
longg2 <- longg/10
#读取每个文件的NO2值，并且设置经纬度分辨率为0.1，并求平均值
no2_july <- array(0,dim=c(31,700,400))
for (k in 1:31) {
  ex.nc <- open.nc(temp_07[k])
  lat <- var.get.nc(ex.nc,"lat")
  lon <- var.get.nc(ex.nc,"lon")
  vcd <- var.get.nc(ex.nc,"tropo_vcd")
  close.nc(ex.nc)
  for (i in 1:700 ){
    for (j in 1:400){
      no2_july[k,i,j] <- mean(vcd[which(lon>=70+(i-1)/10 & lon<70+i/10),
                                  which(lat>=15+(j-1)/10 & lat<15+j/10)],na.rm=T)
    }
  }
}
no2_mean <-matrix(0,700,400)
for (i in 1:700){
  for(j in 1:400){
    no2_mean[i,j]=mean(no2_july[,i,j],na.rm=T)
  }
}
#画图
png('NO2 of China in 2019_07.png', width=8.5, height=6, units="in", res=400)
image.plot(longg2,latt2,no2_mean,
           horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="molec · cm−2",cex=1.25),         
           xlab='',ylab='',midpoint=T, axes=F, ann=F
           )
           title(xlab="lon",cex.lab=1.25,font.lab=2)
           axis(1,at=pretty(longg2),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
           title(ylab="lat",cex.lab=1.25,font.lab=2)
           axis(2,at=pretty(latt2),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
           title(main=paste("NO2 of China in 2019_07"),
           cex.main=1,font.main=3)
map('china',add=T,lwd=0.75,col="black")
map('world',add=T,lwd=0.75,col="black")
box(lwd=2)
dev.off()

#处理HCHO数据
#设置工作目录
setwd("G:/TROPOMI_HCHO")
#读取文件
tempHCHO_07 <- list.files(pattern="TRHCHO_201907.*")
#读取每个文件的HCHO值，并且设置经纬度分辨率为0.1，并求平均值
hcho_july <- array(0,dim=c(700,400))
hcho_n <- matrix(0,700,400)
hcho_mean <- matrix(0,700,400)
for (k in 1:125) {
  ex.nc <- open.nc(tempHCHO_07[k])
  lati <- var.get.nc(ex.nc,"latitude")
  long <- var.get.nc(ex.nc,"longitude")
  Tvcd <- var.get.nc(ex.nc,"TropoVCD")
  close.nc(ex.nc)
  for (i in 1:nrow(Tvcd)){
    for (j in 1:ncol(Tvcd)){
        if(lati[i,j]>=15 & lati[i,j]<55 & long[i,j] >=70 & 
           long[i,j]<140 & !is.na(Tvcd[i,j])){
        c=ceiling(lati[i,j]*10)-150
        d=ceiling(long[i,j]*10)-700
        hcho_july[d,c]<- hcho_july[d,c]+Tvcd[i,j]
        hcho_n[d,c]<- hcho_n[d,c]+1
      }
    }
  }
}
for (i in 1:700){
  for (j in 1:400){
    hcho_mean[i,j]<-hcho_july[i,j]/hcho_n[i,j]
  }
}
#画图
png('HCHO of China in 2019_07.png', width=8.5, height=6, units="in", res=400)
image.plot(longg2,latt2,hcho_mean,horizontal=T, useRaster=T,
           legend.shrink=0.75, axis.args=list(cex.axis = 1.25), 
           legend.width=1, legend.mar=2,
           legend.args=list(text="molec · cm−2",cex=1.25),         
           xlab='',ylab='',midpoint=T, axes=F, ann=F
)
title(xlab="lon",cex.lab=1.25,font.lab=2)
axis(1,at=pretty(longg2),tck=-0.015,lwd=2,cex.axis=1.25,font=1)
title(ylab="lat",cex.lab=1.25,font.lab=2)
axis(2,at=pretty(latt2),tck=-0.015,lwd=2,cex.axis=1.25,font=1,las=1)
title(main=paste("HCHO of China in 2019_07"),
      cex.main=1,font.main=3)
map('china',add=T,lwd=0.75,col="black")
map('world',add=T,lwd=0.75,col="black")
box(lwd=2)
dev.off()

#处理臭氧数据
setwd("G:/O3")
install.packages("openxlsx")
install.packages("maptools")
library(openxlsx)
library(maptools)
O3_Data <- read.xlsx("O3_2019_07.xlsx",sheet=1)
O3_data <- as_tibble(O3_Data)
#去除数据缺失的站点并画图
O3_data <- O3_data %>%
           filter(O3!=0)
china_map <-readShapePoly("bou2_4p.shp")
china_map1<- fortify(china_map)
png('O3 of China in 2019_07.png', width=8.5, height=6, units="in", res=400)
     ggplot()+
     geom_polygon(data=china_map1,aes(x=long,y=lat,group=group),
                  fill="#FCFCFC",col="grey")+
     geom_point(data=O3_data,aes(x=lon, y=lat, color=O3))+
     scale_color_gradient(low = "white", high = "red")+
     labs(title="O3 of China in 2019_07(μg/m^3)", x="long", y="lat")+
       theme_bw() +
       theme(plot.title=element_text(size=20, face="bold"),
             axis.text.x=element_text(size=10), 
             axis.text.y=element_text(size=10),
             axis.title.x=element_text(size=15),
             axis.title.y=element_text(size=15))
     box(lwd=2)
     dev.off()


#筛选出地面站点所对应的HCHO、CO2柱浓度
O3_data <- O3_data %>%
    filter(lat<50)
NO2 <- c()
HCHO <- c()
for (i in 1:nrow(O3_data)){
  c=ceiling(O3_data$lon[i]*10)-700
  d=ceiling(O3_data$lat[i]*10)-150
  NO2[i] <- no2_mean[c,d]
  HCHO[i]<- hcho_mean[c,d]
}
total_data <- cbind(lon=O3_data$lon,lat=O3_data$lat,O3=O3_data$O3,HCHO,NO2)
total_data <- as_tibble(total_data)
     
#污染物关系分析
plot(total_data$O3 ~ total_data$HCHO,
     xlab = "HCHO(molec · cm−2)",
     ylab = "O3(μg/m^3)",
     pch = 20,
     cex = 1,
     main = "HCHO VS O3")
t.test(total_data$O3 , total_data$HCHO)
fit1 <- lm(total_data$O3 ~ total_data$HCHO)
summary(fit1)
abline(fit1, lwd = 5, col = "gray")

plot(total_data$O3 ~ total_data$NO2,
     xlab = "NO2(molec · cm−2)",
     ylab = "O3(μg/m^3)",
     pch = 20,
     cex = 1,
     main = "NO2 VS O3")
t.test(total_data$O3 , total_data$NO2)
fit2 <- lm(total_data$O3 ~ total_data$NO2)
summary(fit2)
abline(fit2, lwd = 5, col = "gray")

#挑选出华北、华南地区的污染物浓度做线性分析
huabei <-total_data %>%
         filter(lon>113 & lon <120 & lat >33 & lat <43)
anova_two_way1 <- aov(O3 ~ HCHO + NO2, data = huabei)
summary(anova_two_way1)
model1 <- lm(O3 ~ HCHO+NO2, data = huabei)
summary(model1)
huanan <-total_data %>%
         filter(lon>105 & lon <120 & lat >18 & lat <27)
anova_two_way2 <- aov(O3 ~ HCHO + NO2, data = huanan)
summary(anova_two_way2)  
model2 <- lm(O3 ~ HCHO+NO2, data = huanan)
summary(model2)
