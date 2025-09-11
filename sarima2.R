library(tseries)

rawData <- read.csv("monthly_full_release_long_format.csv")
library(dplyr)


singapore <- rawData %>%
  filter(Area == "Singapore",
         Category == 'Electricity demand')

singapore <- singapore %>%
  select(Area, Date, Value)

View(singapore)

library(lubridate)
singapore$Date <- as.Date(singapore$Date)
start_year <- year(min(singapore$Date))
start_month <- month(min(singapore$Date))

singapore_ts <- ts(singapore$Value,
                   start = c(start_year, start_month),
                   frequency = 12)

plot(singapore_ts,
     ylab = "Electricity Demand (TWh)",
     xlab = "Year",
     main = "Monthly Electricity Demand in Singapore")

# Split data by percentage
library(forecast)

train_singapore <- head(singapore_ts, round(length(singapore_ts) * 0.66))
h <- length(singapore_ts) - length(train_singapore)
test_singapore <- tail(singapore_ts, h)

train_singapore
test_singapore

autoplot(train_singapore) + autolayer(test_singapore)



# Check number of differencing
#how many diference need to do
ndiffs(train_singapore)
#how many seasonal difference nddeed to do
nsdiffs(train_singapore)


# 2row and 2column, fill matrix row by row (c(1,1,2,3) is location)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(singapore_ts,xlab = "Year", ylab="Electricity Demand (TWh)", main="Monthly Electricity Demand in Singapora")
acf(singapore_ts,lag.max=40)
pacf(singapore_ts,lag.max=40)

# want p < 0.05 only for non-seasonal
adf.test(train_singapore)
# p-value = 0.01

# first differencing (seasonal)
z <- ts(diff(train_singapore, lag=12))
#plot time series
ts.plot(z,gpars=list(main= "First Differences", xlab="Year",
                     ylab="Electricity Demand (TWh)", lty=1))
acf(z, main="z",lag.max=40)
pacf(z, main="z",lag.max=40)
adf.test(z)
#p-value = 0.01

#second differencing
z2 <- ts(diff(z, lag=1))  
ts.plot(z2,gpars=list(main= "Second Differences", xlab="Year"
                      ,ylab="Electricity Demand (TWh)",lty=1))
acf(z2, main="z2",lag.max=40)
pacf(z2, main="z2",lag.max=40)
adf.test(z2)
#p-value = 0.01


#arima  (0,1,1) (0) PACF (1) differencing one time       (1) ACF lag 1 signifincance
#s      (0,1,1) (0 PACF) (seaonal differencing one time) (1 seasonal at acf)
fit_singapore <- arima(train_singapore, order=c(0,1,1), seasonal=list(order =c(0,1,1), period=12)) 
summary(fit_singapore)
#p value > 0.05 is gud model, no autocorelation
checkresiduals(fit_singapore)

fr <- forecast(fit_singapore, h=12*4)
layout(matrix(c(1,1)))
plot(fr)
lines(test_singapore, col="turquoise2")
accuracy(fr, test_singapore)
# aic = -266.37
# p-value = 0.2595
#                       ME      RMSE        MAE        MPE     MAPE      MASE        ACF1 Theil's U
# Training set -0.01498707 0.0678987 0.04968995 -0.4107052 1.230196 0.4327243 -0.01230199        NA
# Test set     -0.07832038 0.1454749 0.10464740 -1.7563729 2.326582 0.9113205  0.67363185 0.6223243
# p+q+P+Q = 2



#Auto ARIMA
library(forecast)
fit_singapore2 <- auto.arima(train_singapore, ic="aic", trace=TRUE)
summary(fit_singapore2)
checkresiduals(fit_singapore2)
fr <- forecast(fit_singapore2, h=12*4)
layout(matrix(c(1,1)))
plot(fr)
lines(test_singapore, col="turquoise2")
accuracy(fr, test_singapore)
# auto ARIMA(0,1,1)(0,1,2)[12] 
# AIC=-267.55
# p-value = 0.5581
#                       ME       RMSE        MAE        MPE     MAPE      MASE        ACF1 Theil's U
# Training set -0.01365663 0.06721348 0.04969441 -0.3786241 1.226256 0.4327631 -0.01815232        NA
# Test set     -0.03208778 0.13120897 0.09344862 -0.7674617 2.072869 0.8137961  0.70226083 0.5633306
# p+q+P+Q = 3


### conclusion
# arima(train_singapore, order=c(0,1,1), seasonal=list(order =c(0,1,1), period=12)) better
# both is pass the residual which p value > 0.05
# although RMSE auto arima is show a slightly lower RMSE than manual arima, but
# manual arima
#   - have lower aic than auto arima 
#   - (p+q+P+Q) parameter manual arima only 2 while auto arima is 3, which means manual arima is more simpler with the similiar accuracy


