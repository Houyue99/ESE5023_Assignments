#Problem2
library(tidyr)
library(dplyr)
library(ggplot2)
Whole_Data <- read.csv(file="2281305.csv",header = T)
Whole_Data1 <- as_tibble(Whole_Data)
Whole_Data1 %>%
  select(DATE,WND) %>%
  mutate(date=as.Date(DATE),
         month=substr(DATE,1,7),
         speed=as.numeric(substr(WND,9,12)),
         speed1=0.1*speed,
         #good work for handling the scaling factor 10 
         speed_code=as.numeric(substr(WND,14,14))) %>%
  filter(speed!=9999 & speed_code==1) %>%
  group_by(month) %>%
  summarise(date1=date[1],mean_speed=mean(speed1)) %>%
  ggplot(aes(x=date1, y=mean_speed)) +
  geom_line()
# good work
