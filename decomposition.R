library(forecast)
library(seastests)

components <- decompose(co2_ts_2010)
trend <- components$trend
seasonal <- components$seasonal
remainder <- components$random
plot(components)
checkresiduals(remainder)

uroot::hegy.test(co2_ts_2010)
seastests::combined_test(co2_ts_2010)

par(mfrow = c(1, 2))
acf(co2_ts_2010, lag.max = 48)
pacf(co2_ts_2010, lag.max = 48)

par(mfrow = c(1, 1))

co2_ts_2010