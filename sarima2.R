library(tseries)

rawData <- read.csv("monthly_full_release_long_format.csv")
library(dplyr)

europe <- rawData %>%
  filter(Area == "Europe",
         Category == 'Electricity demand')

europe <- europe %>%
  select(Area, Date, Value)

View(europe)

library(lubridate)
europe$Date <- as.Date(europe$Date)
start_year <- year(min(europe$Date))
start_month <- month(min(europe$Date))

europe_ts <- ts(europe$Value,
                start = c(start_year, start_month),
                frequency = 12)

plot(europe_ts,
     ylab = "Electricity Demand (TWh)",
     xlab = "Year",
     main = "Monthly Electricity Demand in Europe")

# Check number of differencing
#how many diference need to do
ndiffs(europe_ts)
#how many seasonal difference nddeed to do
nsdiffs(europe_ts)


# 2row and 2column, fill matrix row by row (c(1,1,2,3) is location)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(europe_ts,xlab = "Year", ylab="Electricity Demand (TWh)", main="Monthly Electricity Demand in Europe")
acf(europe_ts,lag.max=40)
pacf(europe_ts,lag.max=40)

# want p < 0.05
adf.test(europe_ts)
# KPSS: null = stationary -> want p large (>0.05) for stationary
kpss.test(europe_ts)

# first differencing (seasonal)
z <- ts(diff(europe_ts, lag=12))
#plot time series
ts.plot(z,gpars=list(main= "First Differences", xlab="Year",
                     ylab="Electricity Demand (TWh)", lty=1))
acf(z, main="z",lag.max=40)
pacf(z, main="z",lag.max=40)
adf.test(z)

#second differencing
z2 <- ts(diff(z, lag=1))  
ts.plot(z2,gpars=list(main= "Second Differences", xlab="Year"
                      ,ylab="Electricity Demand (TWh)",lty=1))
acf(z2, main="z2",lag.max=40)
pacf(z2, main="z2",lag.max=40)
adf.test(z2)
kpss.test(z2)


#arima (2) PACF lag2 significance  (1)  differencing one time  (2) ACF lag 2 signifincance
#s (1,1,1) (one seasonal at pacf)(seaonal differencing one time)(one seasonal at acf)
fit <- arima(train, order=c(2,1,2), seasonal=list(order =c(1,1,1), period=12)) 
summary(fit)
#p value > 0.05 is gud model, no autocorelation
checkresiduals(fit)

fr <- forecast(fit, h=12*4)
layout(matrix(c(1,1)))
plot(fr)
lines(test, col="turquoise2")
accuracy(fr, test)


#Auto ARIMA
library(forecast)
fit <- auto.arima(train, ic="aic", trace=TRUE)
summary(fit)
checkresiduals(fit)
fr <- forecast(fit, h=12*4)
layout(matrix(c(1,1)))
plot(fr)
lines(test, col="turquoise2")
accuracy(fr, test)


