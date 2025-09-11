library(tseries)


# Check number of differencing
#how many diference need to do
ndiffs(train)
#how many seasonal difference nddeed to do
nsdiffs(train)


# 2row and 2column, fill matrix row by row (c(1,1,2,3) is location)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(sg_ts,xlab = "Year", ylab="Electricity Demand (TWh)", main="Monthly Electricity Demand in Singapora")
acf(sg_ts,lag.max=40)
pacf(sg_ts,lag.max=40)

# want p < 0.05 only for non-seasonal
adf.test(train)
# p-value = 0.01

# first differencing (seasonal)
z <- ts(diff(train, lag=12))
#plot time series
ts.plot(z,gpars=list(main= "First Differences (Seasonal)", xlab="Year",
                     ylab="Electricity Demand (TWh)", lty=1))
acf(z, main="z",lag.max=40)
pacf(z, main="z",lag.max=40)
adf.test(z)
#p-value = 0.01


#second differencing
z2 <- ts(diff(z, lag=1))  
ts.plot(z2,gpars=list(main= "Second Differences (Non-Seasonal)", xlab="Year"
                      ,ylab="Electricity Demand (TWh)",lty=1))
acf(z2, main="z2",lag.max=40)
pacf(z2, main="z2",lag.max=40)
adf.test(z2)
#p-value = 0.01



#arima  (0,1,1) (0) PACF (1) differencing one time       (1) ACF lag 1 signifincance
#s      (0,1,1) (0 PACF) (seaonal differencing one time) (1 seasonal at acf)
fit_singapore <- arima(train, order=c(0,1,1), seasonal=list(order =c(0,1,1), period=12)) 
summary(fit_singapore)
#p value > 0.05 is gud model, no autocorelation
checkresiduals(fit_singapore)

fr <- forecast(fit_singapore, h=length(test))
layout(matrix(c(1,1)))
plot(fr)
lines(test, col="turquoise2")
accuracy(fr, test)
# aic = -266.37
# p-value = 0.2595
#                       ME      RMSE        MAE        MPE     MAPE      MASE        ACF1 Theil's U
# Training set -0.01498707 0.0678987 0.04968995 -0.4107052 1.230196 0.4327243 -0.01230199        NA
# Test set     -0.07832038 0.1454749 0.10464740 -1.7563729 2.326582 0.9113205  0.67363185 0.6223243
# p+q+P+Q = 2


#Second model  (NOT SURE YET)
#AR(2) ARSEASONAL(1)
fit_singapore2 <- arima(train, 
                        order=c(2,1,0), 
                        seasonal=list(order =c(0,1,1), period=12)) 
summary(fit_singapore2)
checkresiduals(fit_singapore2)

fr2 <- forecast(fit_singapore2, h=length(test))
layout(matrix(c(1,1)))
plot(fr2)
lines(test, col="turquoise2")
accuracy(fr2, test)



#Auto ARIMA
library(forecast)
fit_singapore3 <- auto.arima(train, ic="aic", trace=TRUE)
summary(fit_singapore2)
checkresiduals(fit_singapore3)
fr3 <- forecast(fit_singapore3, h=length(test))
layout(matrix(c(1,1)))
plot(fr3)
lines(test, col="turquoise2")
accuracy(fr3, test)
# auto ARIMA(0,1,1)(0,1,2)[12] 
# AIC=-267.55
# p-value = 0.5581
#                       ME       RMSE        MAE        MPE     MAPE      MASE        ACF1 Theil's U
# Training set -0.01365663 0.06721348 0.04969441 -0.3786241 1.226256 0.4327631 -0.01815232        NA
# Test set     -0.03208778 0.13120897 0.09344862 -0.7674617 2.072869 0.8137961  0.70226083 0.5633306
# p+q+P+Q = 3


### conclusion
# arima(train, order=c(0,1,1), seasonal=list(order =c(0,1,1), period=12)) better
# All 3 passed the residual which p value > 0.05
# although RMSE auto arima is show a slightly lower RMSE than manual arima, but
# manual arima (0,1,1) (0,1,1)
#   - have lower aic than auto arima 
#   - (p+q+P+Q) parameter manual arima only 2 while auto arima is 3, which means manual arima is more simpler with the similiar accuracy
# (1,1,0),(0,1,1) have worse rmse compared to the other two model
#P-value is smaller than both of the model too

