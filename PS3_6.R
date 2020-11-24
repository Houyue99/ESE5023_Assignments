#Problem6.1
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
data(cpus)
cpus <- as_tibble(cpus)
sample_index <- sample(nrow(cpus),nrow(cpus)*0.80)
cpus_train <- cpus[sample_index,]
cpus_test  <- cpus[-sample_index,]
library(leaps)
subset_result <- regsubsets(perf~syct+mmin+mmax+cach+chmin+chmax , data=cpus_train, nbest=2, nvmax = 7)
plot(subset_result, scale="bic")

#Problem6.2
model_perf <- lm(perf ~ syct+mmin+mmax+cach+chmax, data=cpus_train)
perf_predict <- predict(model_perf,cpus_test)
plot(cpus_test$perf,perf_predict,
     pch = 20,
     cex = 2,
     col = "grey")
mean_bias <- (mean(perf_predict) - mean(cpus_test$perf))/
  mean(cpus_test$perf)*100
print(mean_bias)

# MingYANG noticed:
# if you used percentage bias you should explain it in your report
# and I think "mean(perf_predict - cpus_test$perf)" is more suitable for this problem
# the end
