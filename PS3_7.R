#Problem7.1（数据由蒋浩同学提供）
library(tidyr)
library(dplyr)
library(ggplot2)
Data7 <- read.csv(file="marine_water_quality.csv",encoding="UTF-8",header = T)
#通过t-test探究水面叶绿素浓度在2000年之前与2000年之后是否有显著差异
Data <- as_tibble(Data7)
Data<-Data %>%
   mutate(date=substr(Dates,1,4),date2=as.numeric(date),Chlorophyll=as.numeric(Chlorophyll.a..μg.L.))
before <- Data %>%
  filter(Depth=="Surface Water") %>%
  select(date2,Chlorophyll) %>%
  filter(date2<2000) %>%
  pull(Chlorophyll)
after <- Data %>%
  filter(Depth=="Surface Water") %>%
  select(date2,Chlorophyll) %>%
  filter(date2>=2000) %>%
  pull(Chlorophyll)
t.test(before,after)

#Problem7.2:通过ANOVA探究不同季节(选取1，4，7，10月份）水面叶绿素浓度是否有显著差异
Data <- Data %>%
  mutate(date3=substr(Dates,6,7),date4=as.numeric(date3))
Data1 <- Data %>%
  select(date4,Chlorophyll) %>%
  filter(date4==1 | date4==4 | date4==7 | date4==10) %>%
  mutate(date4 = factor(date4, ordered = TRUE)) 
anova_one_way <- aov(Chlorophyll ~ date4, data = Data1)
summary(anova_one_way)

#Problem7.3:线性拟合三十年来水面叶绿素浓度的值，观察水面叶绿素浓度的变化
model_Chlorophyll <- lm( Chlorophyll ~ date2, data=Data )
summary(model_Chlorophyll)

  # good work
