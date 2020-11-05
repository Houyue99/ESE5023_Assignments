#Problem5.1
library(tidyr)
library(dplyr)
library(ggplot2)
Velocity <- c(170,290,-130,-70,-185,-220,200,290,270,200,300,-30,650,150,
              500,920,450,500,500,960,500,850,800,1090)
Distance <- c(0.032,0.034,0.214,0.263,0.275,0.275,0.450,0.500,0.500,0.630,
              0.800,0.900,0.900,0.900,0.900,1.000,1.100,1.100,1.400,1.700,
              2.000,2.000,2.000,2.000)
plot(Distance~Velocity,
     main = "Distance vs Velocity",
     pch = 20,
     cex = 2,
     col = "grey")

#Problem5.2
fit <- lm(Distance ~ Velocity)
abline(fit, lwd = 5, col = "red")

#Problem5.3
Velocity2 <- Velocity*3.15e+7
Distance2 <- Distance*30.9e+18
fit2 <- lm(Distance2 ~ Velocity2)
summary(fit2)