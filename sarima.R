library(forecast)
library(tseries)

#p-value default already < 0.05, reject H null, stationary
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(europe_ts, ylab="TWH Electricity", main="Electricity Demand")
acf(as.numeric(europe_ts), lag.max = 48, main = "ACF (lags in months)")
pacf(as.numeric(europe_ts), lag.max = 48, main = "PACF (lags in months)")
adf.test(europe_ts)

ndiffs(europe_ts)
nsdiffs(europe_ts)
# getting the result of non-seasonal diff = 0, seasonal diff = 1
# but after differencing seasonal, p-value > 0.05

z <- ts(diff(europe_ts, lag=12)) # lag=12, seasonal differences
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
model1 <- arima(europe_ts, order = c(0, 1, 2), seasonal = list(order=c(0, 1, 1), period=12))
summary(model1)
checkresiduals(model1)
plot(forecast(model1, h=12))
#Result: Pass, inside confidence interval, random residuals(Ljung-Box p-value = 0.2083), aic = 834.06


# ==== Second Fit Model ==== #
model2 <- arima(europe_ts, order = c(2, 1, 0), seasonal = list(order=c(1, 1, 0), period=12))
summary(model2)
checkresiduals(model2)
plot(forecast(model2, h=12))
#Result: (Not sure), outside confidence interval, random residuals(Ljung-Box p-value = 0.08392), aic = 843.1


# ==== Third Fit Model ==== #
model3 <- arima(europe_ts, order = c(2, 1, 0), seasonal = list(order=c(0, 1, 2), period=12))
summary(model3)
checkresiduals(model3)
plot(forecast(model3, h=12))
#Result: (Not sure), outside confidence interval, random residuals(Ljung-Box p-value = 0.1746), aic = 833.69


# ==== Fourth Fit Model ==== #
model4 <- arima(europe_ts, order = c(0, 1, 2), seasonal = list(order=c(0, 1, 2), period=12))
summary(model4)
checkresiduals(model4)
plot(forecast(model4, h=12))
#Result: Pass, inside confidence interval, random residuals(Ljung-Box p-value = 0.3042), aic = 833.54



# ==== Auto Arima Cheking ==== #
auto <- auto.arima(europe_ts, ic="aic", trace=TRUE)
summary(auto)
checkresiduals(auto, lag=24)
plot(forecast(auto, h=12))
# Model found = Arima(1, 0, 2)(0, 1, 1)
# AIC value = 836.97