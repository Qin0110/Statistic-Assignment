library(xgboost)
library(lubridate)
library(dplyr)
library(caret)
library(ggplot2)

head(europe)

# Mutate column since xg_boost doesn't recognize date column well
df <- europe %>%
  arrange(Date) %>%
  mutate(
    month = month(Date),
    year = year(Date)
  )

df

train <- head(df, round(nrow(df) * 0.66))
df_h <- nrow(df) - nrow(train)
test <- tail(df, df_h)

# # Xg_boost need a matrix form
# x_train <- xgb.DMatrix(as.matrix(train %>%
#                                    select(month, year)))
# 
# x_test <- xgb.DMatrix(as.matrix(test %>%
#                                   select(month, year)))
# 
# y_train <- train$Value

features <- c("month", "year")
target <- "Value"

# Fit in xgBoost using caret
xgb_trcontrol <- trainControl(
  method = "cv",
  number = 5,
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgb_grid <- expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20),    # maximum depth of tree, xgBoost(a ensemble of desicion tree)
    colsample_bytree = seq(0.5),  # subsample ratio of columns when construction each tree
    eta = 0.1,                    # learning rate
    gamma = 0,                    # minimum loss reduction
    min_child_weight = 1,         # minimum sum of instance weight (hessian) needed in a child
    subsample = 1                 # subsample ratio of the training instances
  )
)

xgb_model <- train(
  x = train[, features], 
  y = train[, target],
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1
)

xgb_model$bestTune

xgb_pred <- xgb_model %>% predict(newdata = test[, features])

results_df <- data.frame(
  Date = test$Date,        
  Actual = test$Value,     
  Forecast = xgb_pred   
)

historical_df <- df %>% 
  mutate(DataType = "Historical")

# 2. Create a data frame for just the forecast
# This will contain the dates from the test set and your predictions
forecast_df <- data.frame(
  Date = test$Date,
  Value = xgb_pred, # The predictions from your xgb_model
  DataType = "Forecast"
)

# 3. Combine them into one data frame for plotting
plot_df <- bind_rows(historical_df, forecast_df)

# 4. Create the plot
ggplot(plot_df, aes(x = Date, y = Value, color = DataType)) +
  
  # Plot both historical and forecast lines
  geom_line(size = 1) +
  
  # Add a vertical line to clearly show where the forecast begins
  geom_vline(xintercept = min(test$Date), linetype = "dashed", color = "blue", size = 1) +
  
  # Add text to label the sections
  annotate("text", x = train$Date[round(nrow(train)*0.5)], y = max(df$Value), label = "Training Data", color = "black") +
  annotate("text", x = test$Date[round(nrow(test)*0.5)], y = max(df$Value), label = "Test Data", color = "black") +
  
  # 5. Customize the plot's appearance
  scale_color_manual(values = c("Historical" = "black", "Forecast" = "turquoise2")) +
  labs(
    title = "XGBoost Forecast vs. Full Historical Data",
    subtitle = "Model forecast overlaid on the training and test periods",
    y = "Electricity Demand (TWh)",
    x = "Date",
    color = "Legend"
  ) +
  theme_minimal(base_size = 14) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid")))) # Ensure legend lines are solid