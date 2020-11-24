#Problem3
library(tidyr)
library(dplyr)
library(ggplot2)
nonvegetarians <- c(185,189,187,181,150,176)
vegetarians <- c(171,174,202,171,207,125,189,179,163,174,184,186)
zinc <- c(nonvegetarians,vegetarians)
vegetarians_or_non <- vector(length = 18)
for (i in 1:18){
  if (i<=6){vegetarians_or_non[i]="nonvegetarians"}
  else{vegetarians_or_non[i]="vegetarians"}
}
zinclevel <- list(vegetarians_or_non=vegetarians_or_non,zinc=zinc)
zinclevel2 <- as_tibble(zinclevel)
zinclevel2 %>%
  group_by(vegetarians_or_non) %>%
  summarise(mean_zinclevel=mean(zinc))
anova_one_way <- aov(zinc ~ vegetarians_or_non, data = zinclevel2)
summary(anova_one_way)

# MingYANG recommended：
# try my code:
preveg<-"initiate"
preveg[1:6]<-"pregnveg"
preveg[7:18]<-"pregveg"
preveg[19:23]<-"uprgveg"
zinc <- c(185,189,187,181,150,176,171,174,202,171,207,125,189,179,163,174,184,186,210,139,172,198,177)
prevegdata<-cbind(preveg,zinc)
data_tbl <- as_tibble(prevegdata)
datatbl <- data_tbl %>% 
  filter(preveg != "uprgveg")

anova_one_way <- aov(zinc ~ preveg, data = datatbl)
summary(anova_one_way)
# the end
