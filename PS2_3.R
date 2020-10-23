#Problem3
library(tidyr)
library(dplyr)
library(ggplot2)
Data7 <- read.csv(file="marine_water_quality.csv",encoding="UTF-8",header = T)
Data7_1 <-as_tibble(Data7)
Data7_1 %>%
  select(Dates,Depth,Chlorophyll.a..μg.L.) %>%
  filter(Depth=="Surface Water") %>%
  mutate(dates=as.Date(Dates),
         Chl1=as.numeric(Chlorophyll.a..μg.L.)) %>%
  ggplot(aes(x=dates,y=Chl1)) +
  geom_line()
  
