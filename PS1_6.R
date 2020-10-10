#Problem6.1
Whole_Data <- read.csv(file="2281305.csv",header=T)
Visibility1 <- Whole_Data$VIS
Obs_Time <- Whole_Data$DATE
Visibility2 <- substr(Visibility1,1,6)
Visibility3 <- as.numeric(Visibility2)
VisibilityQuality <- substr(Visibility1,8,12)
unusable <- which(Visibility3<0)
Visibility3[unusable] <- NA
unusable1 <- which(Visibility3>160000)
Visibility3[unusable1] <- NA
unusable2 <- which(VisibilityQuality!="1,N,1")
Visibility3[unusable2] <- NA
Obs_Time2 <- as.Date(Obs_Time)
plot(Obs_Time2,Visibility3,c)

#Problem6.2
Daily <- unique(Obs_Time2)
Daily_Vis <- c()
for (day in Daily){
  iday <- which(Obs_Time2==day)
  Daily_Max <- max(Visibility3[iday],na.rm = T)
  Daily_Vis <- c(Daily_Vis,Daily_Max)
}
Years <- substr(Daily,1,4)
Years2 <-as.numeric(Years)
Year <- unique(Years2)
for (iyear in Year){
  daynumbers <- c(iyear,0,0,0,0,0,0,0)
  thisyear <- which(Years2==iyear)
  for(days2 in thisyear){
    if(Daily_Vis[days2]>=0 && Daily_Vis[days2]<5000){
      daynumbers[2]<-daynumbers[2]+1}
    else if(Daily_Vis[days2]>=5000 && Daily_Vis[days2]<10000){
      daynumbers[3]<-daynumbers[3]+1}
    else if(Daily_Vis[days2]>=10000 && Daily_Vis[days2]<15000){
      daynumbers[4]<-daynumbers[4]+1}
    else if(Daily_Vis[days2]>=15000 && Daily_Vis[days2]<20000){
      daynumbers[5]<-daynumbers[5]+1}
    else if(Daily_Vis[days2]>=20000 && Daily_Vis[days2]<25000){
      daynumbers[6]<-daynumbers[6]+1}
    else if(Daily_Vis[days2]>=25000 && Daily_Vis[days2]<30000){
      daynumbers[7]<-daynumbers[7]+1}
    else if(Daily_Vis[days2]>=30000){
      daynumbers[8]<-daynumbers[8]+1}
  }
  print(daynumbers)
  Daily_Vis2 <- Daily_Vis[thisyear]
  hist(Daily_Vis2,breaks = c(0,5000,10000,15000,20000,25000,30000))
}
  
hist(Daily_Max)0
