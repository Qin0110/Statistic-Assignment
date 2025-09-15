library(xgboost)
library(lubridate)
library(dplyr)
library(caret)
library(ggplot2)
library(tsibble)
library(zoo)
library(forecast)

xg_data <- read.csv("co2_mm_mlo.csv", header = TRUE, comment.char = "#")

start_year <- 2010

df <- xg_data %>% 
  filter(year >= start_year) %>%
  mutate(
    month_sin = sin(2*pi*month/12),
    month_cos = cos(2*pi*month/12),
    lag12 = lag(average, 12),
  ) %>%
  select(decimal.date, deseasonalized, month_cos, month_sin, lag12, average) %>%
  arrange(decimal.date)
  
train <- head(df, round(nrow(df) * 0.7))
df_h <- nrow(df) - nrow(train)
test <- tail(df, df_h)

features <- c("decimal.date", "deseasonalized", "month_cos", "month_sin", "lag12")
target <- "average"

# Fit in xgBoost using caret
xgb_trcontrol <- trainControl(
  method = "cv",
  number = 10,
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgb_grid <- expand.grid(
  list(
    nrounds = c(200, 400),
    max_depth = c(6,10),          # maximum depth of tree, xgBoost(a ensemble of desicion tree)
    colsample_bytree = c(0.6, 0.8),  # subsample ratio of columns when construction each tree
    eta = c(0.05, 0.1),     # learning rate
    gamma = 0,                    # minimum loss reduction
    min_child_weight = 1,         # minimum sum of instance weight (hessian) needed in a child
    subsample = c(0.7, 1)                 # subsample ratio of the training instances
  )
)

xgb_model <- train(
  x = as.data.frame(train[, features]), 
  y = train[[target]],
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)

xgb_model$bestTune

xgb_pred <- xgb_model %>% predict(newdata = test[, features])

results_df <- data.frame(
  Date = test$decimal.date,        
  Actual = test$average,     
  Forecast = xgb_pred   
)

historical_df <- df %>% 
  mutate(DataType = "Historical")

# 2. Create a data frame for just the forecast
# This will contain the dates from the test set and your predictions
forecast_df <- data.frame(
  decimal.date = test$decimal.date,
  average = xgb_pred, # The predictions from your xgb_model
  DataType = "Forecast"
)

# 3. Combine them into one data frame for plotting
plot_df <- bind_rows(historical_df, forecast_df)

# 4. Create the plot
ggplot(plot_df, aes(x = decimal.date, y = average, color = DataType)) +
  
  # Plot both historical and forecast lines
  geom_line(size = 1) +
  
  # Add a vertical line to clearly show where the forecast begins
  geom_vline(xintercept = min(test$decimal.date), linetype = "dashed", color = "blue", size = 1) +
  
  # Add text to label the sections
  annotate("text", x = train$decimal.date[round(nrow(train)*0.5)], y = max(df$average), label = "Training Data", color = "black") +
  annotate("text", x = test$decimal.date[round(nrow(test)*0.5)], y = max(df$average), label = "Test Data", color = "black") +
  
  # 5. Customize the plot's appearance
  scale_color_manual(values = c("Historical" = "black", "Forecast" = "red")) +
  labs(
    title = "XGBoost Forecast vs. Full Historical Data",
    subtitle = "Model forecast overlaid on the training and test periods",
    y = "Concentration of CO2",
    x = "Date",
    color = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid")))) # Ensure legend lines are solid

forecast::accuracy(xgb_pred, test$average)
checkresiduals(test$average - xgb_pred)
