library(dplyr)

sg <- rawData %>%
  filter(Area == "Singapore",
         Category == 'Electricity demand')

sg <- sg %>%
  select(Date, Value)

# View(sg)

library(lubridate)
sg$Date <- as.Date(sg$Date)
start_year <- year(min(sg$Date))
start_month <- month(min(sg$Date))

sg_ts <- ts(sg$Value,
            start = c(start_year, start_month),
            frequency = 12)

plot(sg_ts,
     ylab = "Electricity Demand (TWh)",
     xlab = "Year",
     main = "Monthly Electricity Demand in Singapore")

# Split data by percentage
library(forecast)

train <- head(sg_ts, round(length(sg_ts) * 0.66))
h <- length(sg_ts) - length(train)
test <- tail(sg_ts, h)

train
test

autoplot(train) + autolayer(test)