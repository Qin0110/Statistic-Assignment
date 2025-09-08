library(dplyr)

mls <- rawData %>%
  filter(Area == "Malaysia",
         Category == 'Electricity demand')

mls <- mls %>%
  select(Date, Value)

# View(mls)

library(lubridate)
mls$Date <- as.Date(mls$Date)
start_year <- year(min(mls$Date))
start_month <- month(min(mls$Date))

mls_ts <- ts(mls$Value,
            start = c(start_year, start_month),
            frequency = 12)

plot(mls_ts,
     ylab = "Electricity Demand (TWh)",
     xlab = "Year",
     main = "Monthly Electricity Demand in Malaysia")

# Split data by percentage
library(forecast)

train <- head(mls_ts, round(length(mls_ts) * 0.66))
h <- length(mls_ts) - length(train)
test <- tail(mls_ts, h)

train
test

autoplot(train) + autolayer(test)