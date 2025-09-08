getwd()

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

# Split data by percentage
library(forecast)

train <- head(europe_ts, round(length(europe_ts) * 0.66))
h <- length(europe_ts) - length(train)
test <- tail(europe_ts, h)

train
test

autoplot(train) + autolayer(test)