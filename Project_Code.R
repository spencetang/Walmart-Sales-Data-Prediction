library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(forecast)
library(tidyverse)
library(car)
library(performance)
library(randomForest)
library(vip)
library(knitr)
library(MASS)
library(car)
library(randomForest)
library(vip)
library(fpp3)
library(fable)


dataset <- read_csv("Walmart.csv")
dim(dataset)

# Initial Data Exploration Graphics
ggplot(dataset, aes(x = reorder(as.factor(Store), 
                                Weekly_Sales/ 1000), Weekly_Sales / 1000)) +
  geom_boxplot() + 
  coord_flip() +
  xlab("Store") +
  ylab("Weekly Sales in Thousands")

dataset$Date <- dmy(dataset$Date)

ggplot(aes(x = Date, y = Weekly_Sales / 1000), data = dataset) +
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y") +
  ylab("Weekly Sales in Thousands")

summary(dataset$Weekly_Sales)

# Data Wrangling and Date Conversion
dataset$Store <- as.factor(dataset$Store)
dataset$Holiday_Flag <- as.factor(dataset$Holiday_Flag)

dataset <- dataset %>% 
  mutate(Week_Number = week(Date))

dataset <- dataset %>% 
  mutate(Year = year(Date))

dataset <- dataset %>% 
  mutate(Month = month(Date))

dataset$Month <- as.factor(dataset$Month)
dataset$Week_Number <- as.factor(dataset$Week_Number)

set.seed(222)

training_id <- which(dataset$Year <= 2011)

dataset_train <- dataset[training_id, ]
dataset_test <- dataset[-training_id, ]

dim(dataset_train)
dim(dataset_test)
dim(dataset)

dataset$Year <- as.factor(dataset$Year)

# Linear Model Diagnostics and Tuning
lm3 <- lm(Weekly_Sales ~ Holiday_Flag + Temperature + Fuel_Price + CPI + 
            Unemployment + Store + Week_Number + Month, data = dataset_train)
summary(lm3)

check_model(lm3, check = c("qq", "pp_check", "ncv", "homogeneity", "outliers"))

check_collinearity(lm3)

tab <- matrix(c(5.71, 18.30, 1723.53, 43.18), ncol=4, byrow=TRUE)
colnames(tab) <- c('Fuel_Price ','Temperature','CPI', 'Unemployment')
rownames(tab) <- c('VIF')
tab <- as.table(tab)
kable(tab)

lm3_reduced <- lm(Weekly_Sales ~ Holiday_Flag + 
                    Store + Week_Number + Month, data = dataset_train)
summary(lm3_reduced)

boxcox(lm3_reduced)
summary(powerTransform(lm3_reduced))

# Final Model
lm3_trans <- lm(log(Weekly_Sales) ~ Holiday_Flag + 
                  Store + Week_Number + Month, data = dataset_train)
summary(lm3_trans)

subset(dataset$Weekly_Sales, dataset$Holiday_Flag == 1) %>% summary()
subset(dataset$Weekly_Sales, dataset$Holiday_Flag == 0) %>% summary()

par(mfrow = c(1, 2), mar=c(5,4,4,2))
acf(resid(lm3_trans), main="acf(resid(lm3_trans))")
pacf(resid(lm3_trans), main="pacf(resid(lm3_trans))")

par(mfrow = c(1,2))
plot(lm3_trans, 1:2)

# Random Forest
set.seed(222)

rf1 <- randomForest(Weekly_Sales ~ Holiday_Flag + Temperature + Fuel_Price + 
                      CPI + Unemployment + Store + Week_Number + Month + Year, 
                    data = dataset_train, importance = TRUE)
rf1

set.seed(222)

rf2 <- randomForest(Weekly_Sales ~ . - Date - Week_Number, ntree = 500, 
                    mtry = 2, data = dataset_train, importance = TRUE)
rf2

vip(rf2)

# Cross-Validation Procedure
RMSE <- function(y, y_hat) {
  sqrt(mean((y - y_hat)^2))
}

pred_lm3 <- predict(lm3, newdata = dataset_test)
pred_lm3_reduced <- predict(lm3_reduced, newdata = dataset_test)
pred_lm3_trans <- predict(lm3_trans, newdata = dataset_test)
pred_rf2 <- predict(rf2, newdata = dataset_test)

RMSE_res <- matrix(c(104821.67, 104980.35, 97726.79, 433795.92),
                   ncol = 4, byrow= TRUE)
colnames(RMSE_res) <- c('Full Model', 'Reduced Model', 'Full Reduced and Transformed Model', 'Random Forest Model')
rownames(RMSE_res) <- "RMSE Values"

kable(RMSE_res)

RMSE(dataset_test$Weekly_Sales, pred_lm3)
RMSE(dataset_test$Weekly_Sales, pred_lm3_reduced)
RMSE(dataset_test$Weekly_Sales, exp(pred_lm3_trans))
RMSE(dataset_test$Weekly_Sales, pred_rf2)

# ARIMA 
dataset2 <- read_csv("Walmart.csv")

ts <- dataset2 %>% 
  group_by(Date)  %>%  
  summarize("Total_Sales_Per_Week" = sum(Weekly_Sales))

ts_sales <- ts %>%  
  mutate(Date = yearweek(dmy(Date))) %>% 
  as_tsibble(index = "Date")

ts_sales %>%
  features(Total_Sales_Per_Week, unitroot_kpss)

ts_sales %>% 
  ACF(Total_Sales_Per_Week) %>% 
  autoplot()

ts_sales %>% 
  PACF(Total_Sales_Per_Week) %>% 
  autoplot()

fit <- ts_sales %>% 
  model(autoarima = ARIMA(Total_Sales_Per_Week, stepwise=FALSE, approximation=FALSE))
report(fit)

fit %>% gg_tsresiduals()

fc <- fit %>%
  forecast(h=25) %>% 
  autoplot(ts_sales)
fc

fit %>% accuracy()