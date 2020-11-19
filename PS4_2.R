#Problem2.1
library(tidyr)
library(dplyr)
library(ggplot2)
library(forecast)
Met_Data <- read.csv(file = "2281305.csv", header = T)
Met_Data <- as_tibble(Met_Data)
monthly_temperature <- Met_Data %>%
  select(DATE,TMP) %>%
  mutate(temperature=ifelse(substr(TMP,7,7)=="1" & substr(TMP,1,5)!="+9999",
                            as.numeric(substr(TMP,1,5))/10,NA)) %>%
  mutate(month=as.numeric(paste(substr(DATE,1,4),substr(DATE,6,7),sep = ""))) %>%
  filter(month<=202008) %>%
  group_by(month) %>%
  summarise(monthly_temperature=mean(temperature,na.rm = T)) %>%
  pull(monthly_temperature)
monthly_temperature <- ts(monthly_temperature, start=c(2010,1), frequency=12)
plot(monthly_temperature, type="l")

#Problem2.2
monthly_temperature_components <- decompose(monthly_temperature)
plot(monthly_temperature_components)
hist(monthly_temperature_components$random, prob=TRUE)
curve(dnorm(x, mean=mean(monthly_temperature_components$random,na.rm=T),
            sd=sd(monthly_temperature_components$random,na.rm=T)),
      add=TRUE, col="red")

#Problem2.3
monthly_temperature_d1 <- diff(monthly_temperature)
plot(monthly_temperature_d1)
acf(monthly_temperature_d1)
pacf(monthly_temperature_d1)
model <- auto.arima(monthly_temperature)

#Problem2.4
months_forecast  <- 2
months_in_plot   <- 10
forecast_15days <- forecast(model, months_forecast)
plot(forecast(model, months_forecast), include = months_in_plot, xlab="Time", 
     ylab="monthly_temperature",type="o",lwd=2) 