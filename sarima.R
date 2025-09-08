library(forecast)
library(tseries)

#p-value default already < 0.05, reject H null, stationary
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(train, ylab="TWH Electricity", main="Electricity Demand")
acf(as.numeric(train), lag.max = 48, main = "ACF (lags in months)")
pacf(as.numeric(train), lag.max = 48, main = "PACF (lags in months)")
adf.test(train)

ndiffs(train)
nsdiffs(train)
# getting the result of non-seasonal diff = 0, seasonal diff = 1
# but after differencing seasonal, p-value > 0.05

z <- ts(diff(train, lag=12)) # lag=12, seasonal differences
ts.plot(z,gpars=list(main= "First (Seasonal) Differences", xlab="Week",
                     ylab="Sales", lty=1))

acf(z, main="z", lag.max=40)
pacf(z, main="z", lag.max=40)
adf.test(z) #Result getting p-value > 0.05
# Non-stationary, perform regular differencing

# Second differncing
z2 <- ts(diff(z, lag=1)) # lag=12, seasonal differences
ts.plot(z,gpars=list(main= "First (non-seasonal) Differences", xlab="Week",
                     ylab="Sales", lty=1))

acf(z2, main="z2", lag.max=40)
pacf(z2, main="z2", lag.max=40)
adf.test(z2)
#Result getting p-value < 0.05, stationary



# # Residual p value < 0.05, not-random
# model1 <- arima(europe_ts, order = c(0, 0, 0), seasonal = list(order=c(0, 1, 1), period=12))
# summary(model1)
# checkresiduals(model1)
# 
# plot(forecast(model1, h=12))

## ===================================== Model Fitting ===================================== ##  
#Importance: 
#Ljung-box test, p-value > 0.05 = randomness in residuals
#ADF test, p-value < 0.05 = stationary


# ==== First Fit Model ==== #
# Residual p value < 0.05, not-random
model1 <- arima(train, order = c(0, 1, 2), seasonal = list(order=c(0, 1, 1), period=12))
summary(model1)
checkresiduals(model1)
fc <- forecast(model1, h = h)
plot(fc)
lines(test, col="turquoise2")
accuracy(fc, test)
#Result(Full set): Pass, inside confidence interval, random residuals(Ljung-Box p-value = 0.2083), aic = 834.06
#Result(Train set): Pass, inside confidence interval, random residuals(Ljung-Box p-value = 0.7083), aic = 536.59
#Test result accuracy: RMSE Test set = 23.849848

# ==== Second Fit Model ==== #
model2 <- arima(train, order = c(2, 1, 0), seasonal = list(order=c(1, 1, 0), period=12))
summary(model2)
checkresiduals(model2)
fc <- forecast(model2, h = h)
plot(fc)
lines(test, col = "turquoise2")
accuracy(fc, test)
#Result(Full set): (Not sure), outside confidence interval, random residuals(Ljung-Box p-value = 0.08392), aic = 843.1
#Result(Train set): (Not sure), outside confidence interval, random residuals(Ljung-Box p-value = 0.1449), aic = 543.63
#Test result accuracy: RMSE Test set = 35.859274

# ==== Third Fit Model ==== #
model3 <- arima(train, order = c(2, 1, 0), seasonal = list(order=c(0, 1, 2), period=12))
summary(model3)
checkresiduals(model3)
fc <- forecast(model3, h = h)
plot(fc)
lines(test, col = "turquoise2")
accuracy(fc, test)
#Result: (Not sure), outside confidence interval, random residuals(Ljung-Box p-value = 0.1746), aic = 833.69
#Result(Train set): Pass, a bit outside confidence interval, random residuals(Ljung-Box p-value = 0.4906), aic = 538.36
#Test result accuracy: RMSE Test set = 23.158305

# ==== Fourth Fit Model ==== #
model4 <- arima(train, order = c(0, 1, 2), seasonal = list(order=c(0, 1, 2), period=12))
summary(model4)
checkresiduals(model4)
fc <- forecast(model4, h = h)
plot(fc)
lines(test, col = "turquoise2")
accuracy(fc, test)
#Result: Pass, inside confidence interval, random residuals(Ljung-Box p-value = 0.3042), aic = 833.54
#Result(Train set): Pass, inside confidence interval, random residuals(Ljung-Box p-value = 0.9144), aic = 536.69
#Test result accuracy: RMSE Test set = 21.048186

# ==== Auto Arima Cheking ==== #
auto <- auto.arima(train, ic="aic", trace=TRUE)
summary(auto)
checkresiduals(auto, lag=24)

fc <- forecast(auto, h = h)
plot(fc)
lines(test, col = "turquoise2")
accuracy(fc, test)
# Model found(Full set) = Arima(1, 0, 2)(0, 1, 1)(12)
# AIC value = 836.97

# Train set = Arima(1,0,0)(1,1,1)(12)
# AIC = 544.73
# Test set RMSE = 9.487547 


## ==== Best Model ==== ##
# Question: Using RMSE as performance tracking or AIC