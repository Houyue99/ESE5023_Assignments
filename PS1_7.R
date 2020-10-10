#Problem7.1
Data7 <- read.csv(file="marine_water_quality.csv",encoding="UTF-8",header = T)
Chlorophyll <- Data7$Chlorophyll.a..μg.L
Depth <- Data7$Depth
underwater <- which(Depth!="Surface Water")
Chlorophyll1 <- as.numeric(Chlorophyll)
Chlorophyll1[underwater] <- NA

#Problem7.2
Res_Date <- Data7$Dates
Res_Date2 <- as.Date(Res_Date)
plot(Res_Date2,Chlorophyll1,lwd=0.5, type="l", col="blue")

#Problem7.3
min(Chlorophyll1,na.rm = T)
max(Chlorophyll1,na.rm = T)
mean(Chlorophyll1,na.rm = T)
median(Chlorophyll1,na.rm = T)
range(Chlorophyll1,na.rm = T)