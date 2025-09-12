
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
rawData <- read.csv("co2_mm_mlo.csv", 
                    header = TRUE, 
                    comment.char = "#")

# Check the first rows and column names
head(rawData)
colnames(rawData)

rawData <- rawData %>%
  select(year, month, average)

rawData$Date <- as.Date(paste(rawData$year, rawData$month, "01", sep = "-"))

start_year <- 2010
start_month <- 1

co2_ts <- ts(rawData$average,
             start = c(1958, 3),   
             frequency = 12)

co2_ts_2010 <- window(co2_ts, start = c(start_year, start_month))
plot(co2_ts_2010,
     ylab = "CO₂ (ppm)",
     xlab = "Year",
     main = "Monthly CO₂ Concentration at Mauna Loa (2010 onwards)")


train <- head(co2_ts_2010, round(length(co2_ts_2010) * 0.7))
h <- length(co2_ts_2010) - length(train)
test <- tail(co2_ts_2010, h)


train
test


# Plot train vs test
autoplot(train) + 
  autolayer(test, series = "Test", color = "red") +
  labs(title = "Train/Test Split of Electricity Consumption",
       y = "Electricity Consumption (TWh)", x = "Year")





----------------------------------------------------------------------------------------------------
  
  
  
  
  library(tseries)


# Check number of differencing
#how many diference need to do
ndiffs(train)
#how many seasonal difference nddeed to do
nsdiffs(train)


# 2row and 2column, fill matrix row by row (c(1,1,2,3) is location)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(co2_ts_2010,xlab = "Year", ylab="Change in CO₂ (ppm)", main="Monthly Change in CO₂ (ppm)")
acf(co2_ts_2010,lag.max=40)
pacf(co2_ts_2010,lag.max=40)

# want p < 0.05 only for non-seasonal
adf.test(train)
# p-value = 0.01

# first differencing (seasonal)
z <- ts(diff(train, lag=12))
#plot time series
ts.plot(z,gpars=list(main= "First Differences (Seasonal)", xlab="Year",
                     ylab="Change in CO₂ (ppm)", lty=1))
acf(z, main="z",lag.max=40)
pacf(z, main="z",lag.max=40)
adf.test(z)
#p-value = 0.1437


#second differencing
z2 <- ts(diff(z, lag=1))  
ts.plot(z2,gpars=list(main= "Second Differences (Non-Seasonal)", xlab="Year"
                      ,ylab="Change in CO₂ (ppm)",lty=1))
acf(z2, main="z2",lag.max=40)
pacf(z2, main="z2",lag.max=40)
adf.test(z2)
#p-value = 0.01



#arima  (0,1,1) (0) PACF (1) differencing one time       (1) ACF lag 1 signifincance
#s      (0,1,1) (0 PACF) (seaonal differencing one time) (1 seasonal at acf)
fit_co2 <- arima(train, order=c(0,1,1), seasonal=list(order =c(0,1,1), period=12)) 
summary(fit_co2)
#p value > 0.05 is gud model, no autocorelation
checkresiduals(fit_co2)

fr <- forecast(fit_co2, h=length(test))
layout(matrix(c(1,1)))
plot(fr)
lines(test, col="turquoise2")
accuracy(fr, test)
# aic = 103.7
# p-value = 0.5513
#                      ME      RMSE       MAE         MPE       MAPE      MASE      ACF1 Theil's U
#Training set 0.009949836 0.3460144 0.2590066 0.002351138 0.06446156 0.1074196 0.1234335        NA
#Test set     0.183010099 0.7311082 0.5839001 0.042337332 0.13796737 0.2421650 0.8123295 0.5494502
# p+q+P+Q = 2


#Second model  
#AR(1) MASEASONAL(1)
fit_co22 <- arima(train, 
                  order=c(1,1,0), 
                  seasonal=list(order =c(0,1,1), period=12)) 
summary(fit_co22)
checkresiduals(fit_co22)

fr2 <- forecast(fit_co22, h=length(test))
layout(matrix(c(1,1)))
plot(fr2)
lines(test, col="turquoise2")
accuracy(fr2, test)
# aic = 107.87
# p-value = 0.4756
#                        ME      RMSE       MAE           MPE       MAPE      MASE       ACF1 Theil's U
#Training set -0.0004559044 0.3545363 0.2705150 -0.0002249015 0.06731026 0.1121926 0.05329466        NA
#Test set      0.2519680055 0.7500253 0.5968222  0.0587113131 0.14096006 0.2475242 0.80776155 0.5633874


#Auto ARIMA
library(forecast)
fit_co23 <- auto.arima(train, ic="aic", trace=TRUE)
summary(fit_co22)
checkresiduals(fit_co23)
fr3 <- forecast(fit_co23, h=length(test))
layout(matrix(c(1,1)))
plot(fr3)
lines(test, col="turquoise2")
accuracy(fr3, test)

# auto ARIMA(0,1,1)(0,1,1)[12] 
# aic = 107.87
# p-value = 0.5513
#                     ME      RMSE       MAE         MPE       MAPE      MASE      ACF1 Theil's U
#Training set 0.009949836 0.3460144 0.2590066 0.002351138 0.06446156 0.1074196 0.1234335        NA
#Test set     0.183010099 0.7311082 0.5839001 0.042337332 0.13796737 0.2421650 0.8123295 0.5494502
# p+q+P+Q = 2


### conclusion
# arima(train, order=c(0,1,1), seasonal=list(order =c(0,1,1), period=12))
# All 3 passed the residual which p value > 0.05
# (1,1,0),(0,1,1) have worse rmse compared to the other two model
