library(forecast)


components <- decompose(co2_ts)
trend <- components$trend
seasonal <- components$seasonal
remainder <- components$random
plot(components)
checkresiduals(remainder)