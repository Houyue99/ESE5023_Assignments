#Problem1.1
library(tidyr)
library(dplyr)
library(ggplot2)
Unseeded <- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 
             87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4,
             21.4, 17.3, 11.5, 4.9, 4.9, 1.0)
Seeded <- c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 
            274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 
            115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
Precipitation <- c(Unseeded,Seeded)
Unseeded_or_Seeded <- vector(length = 52)
for (i in 1:52){
  if (i<=26){Unseeded_or_Seeded[i]="Unseeded"}
  else{Unseeded_or_Seeded[i]="Seeded"}
}
Precipitation1 <-list(Unseeded_or_Seeded=Unseeded_or_Seeded,Precipitation=Precipitation)
Precipitation2 <- as_tibble(Precipitation1)
boxplot(Precipitation ~ Unseeded_or_Seeded, data=Precipitation2)

#Problem1.2
t.test(Unseeded,Seeded)

# @MingYANG recommended：
# share my code with you blow ↓↓↓
# Unseeded <- c(1202.6, 830.1, 372.4, 345.5, 321.2, 244.3, 163.0, 147.8, 95.0, 87.0, 81.2, 68.5, 47.3, 41.1, 36.6, 29.0, 28.6, 26.3, 26.0, 24.4, 21.4, 17.3, 11.5, 4.9, 4.9, 1.0)
# Seeded <- c(2745.6, 1697.1, 1656.4, 978.0, 703.4, 489.1, 430.0, 334.1, 302.8, 274.7, 274.7, 255.0, 242.5, 200.7, 198.6, 129.6, 119.0, 118.3, 115.3, 92.4, 40.6, 32.7, 31.4, 17.5, 7.7, 4.1)
# rainfall <- cbind(Unseeded,Seeded)      # you may find "cbind" and "rbind" useful!
# data <- data.frame(rainfall)
# boxplot(rainfall,width=c(1,2),col=c(2,7),border=c("purple","black"))
# t.test(Unseeded,Seeded)             # returns the same value of p-value with anova analysis
# the end
