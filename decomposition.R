library(forecast)


components <- decompose(co2_ts_2010)
trend <- components$trend
seasonal <- components$seasonal
remainder <- components$random
plot(components)
checkresiduals(remainder)

acf(co2_ts_2010, lag.max = 48)
pacf(co2_ts_2010, lag.max = 48)
