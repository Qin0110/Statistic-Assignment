
library(dplyr)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
rawData <- read.csv("co2_mm_mlo.csv", 
                    header = TRUE, 
                    comment.char = "#")

# Check the first rows and column names
head(rawData)
colnames(rawData)

rawData <- rawData %>%
  select(year, month, average)

rawData$Date <- as.Date(paste(rawData$year, rawData$month, "01", sep = "-"))

start_year <- 2010
start_month <- 1

co2_ts <- ts(rawData$average,
             start = c(1958, 3),   
             frequency = 12)

co2_ts_2010 <- window(co2_ts, start = c(start_year, start_month))
plot(co2_ts_2010,
     ylab = "CO₂ (ppm)",
     xlab = "Year",
     main = "Monthly CO₂ Concentration at Mauna Loa (2010 onwards)")


train <- head(co2_ts_2010, round(length(co2_ts_2010) * 0.66))
h <- length(co2_ts_2010) - length(train)
test <- tail(co2_ts_2010, h)


train
test


# Plot train vs test
autoplot(train) + 
  autolayer(test, series = "Test", color = "red") +
  labs(title = "Train/Test Split of Electricity Consumption",
       y = "Electricity Consumption (TWh)", x = "Year")