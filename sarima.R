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
#p-value = 0.01


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

# p-value = 0.5471
#ME      RMSE       MAE         MPE       MAPE      MASE      ACF1 Theil's U
#Training set 0.01212178 0.3545717 0.2654615 0.002873610 0.06614668 0.1105637 0.1232532        NA
#Test set     0.04046994 0.6496957 0.5120617 0.008750748 0.12125558 0.2132718 0.8035594 0.4869918
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
# AIC=107.48
# p-value = 0.5471
#ME      RMSE       MAE         MPE       MAPE      MASE      ACF1 Theil's U
#Training set 0.01212178 0.3545717 0.2654615 0.002873610 0.06614668 0.1105637 0.1232532        NA
#Test set     0.04046994 0.6496957 0.5120617 0.008750748 0.12125558 0.2132718 0.8035594 0.4869918
# p+q+P+Q = 2


### conclusion
# arima(train, order=c(0,1,1), seasonal=list(order =c(0,1,1), period=12))
# All 3 passed the residual which p value > 0.05
# (1,1,0),(0,1,1) have worse rmse compared to the other two model
