library(dplyr)
library(prophet)
library(forecast)
library(ggplot2)
library(forecast)
library(lubridate)


#data preprocessing
train_df <- data.frame(ds = seq.POSIXt(from = as.POSIXct(paste0(start_year, "-", start_month, "-01")),by = "month",length.out = length(train)),y = as.numeric(train))
test_df <- data.frame(ds = seq.POSIXt(from = max(train_df$ds) + 1, by = "month",length.out = length(test)),y = as.numeric(test))


# Hyperparameter tuning
# param_grid <- expand.grid(
#   changepoint_prior_scale = c(0.05, 0.1, 0.5),
#   seasonality_prior_scale = c(0.001, 0.01, 1.0, 5.0, 10.0),
#   stringsAsFactors = FALSE
# )
# 
# results <- data.frame(
#   changepoint_prior_scale = numeric(),
#   seasonality_prior_scale = numeric(),
#   seasonality_mode        = character(),
#   rmse = numeric(),
#   mae  = numeric(),
#   mape = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# for (i in 1:nrow(param_grid)) {
#   params <- param_grid[i, ]
# 
# m <- prophet(
#   df = train_df,
#   changepoint.prior.scale = params$changepoint_prior_scale,
#   seasonality.prior.scale = params$seasonality_prior_scale,
#   weekly.seasonality      = FALSE,
#   yearly.seasonality      = TRUE,
#   daily.seasonality       = FALSE,
#   verbose = FALSE
# )
# 
# # Cross validation: since monthly data, use horizon = 12 months
# df_cv <- cross_validation(
#   m,
#   initial = as.difftime(1095, units = "days"),
#   period  = as.difftime(365,  units = "days"),
#   horizon = as.difftime(365,  units = "days")
# )
# 
# df_p <- performance_metrics(df_cv, rolling_window = 1)
# 
# results <- rbind(
#   results,
#   data.frame(
#     changepoint_prior_scale = params$changepoint_prior_scale,
#     seasonality_prior_scale = params$seasonality_prior_scale,
#     rmse = mean(df_p$rmse, na.rm = TRUE),
#     mae  = mean(df_p$mae, na.rm = TRUE),
#     mape = mean(df_p$mape, na.rm = TRUE),
#     stringsAsFactors = FALSE
# ))}
# 
# # rank by RMSE
# results <- results %>% arrange(rmse)
# print(head(results, 10))
# 
# # best parameter set
# best_params <- results %>% slice(1)
# print(best_params)
# 
# # final model using best params
# model <- prophet(
#   df = train_df,
#   changepoint.prior.scale = best_params$changepoint_prior_scale,
#   seasonality.prior.scale = best_params$seasonality_prior_scale,
#   weekly.seasonality      = FALSE,
#   yearly.seasonality      = TRUE,
#   daily.seasonality       = FALSE
# )

# # 
model <- prophet(
  train_df,
  seasonality.mode = 'additive',
  seasonality.prior.scale = 0.001,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  yearly.seasonality = TRUE,
  changepoint.prior.scale = 0.05,
)

#future = total data of train + test
future <- make_future_dataframe(model, periods = nrow(test_df), freq = "month")
future$ds <- as.POSIXct(future$ds)

forecast <- predict(model, future)
forecast$ds <- as.POSIXct(forecast$ds)

#plot graph
p <- ggplot() +
  # forecast confidence interval
  geom_ribbon(data = forecast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "red", alpha = 0.2) +
  # forecast line
  geom_line(data = forecast[forecast$ds >= min(test_df$ds), ], aes(x = ds, y = yhat), color = "red", size = 1) +
  geom_line(data = train_df, aes( x = ds, y = y), color = 'blue', size =1)+
  # test data
  geom_line(data = test_df, aes(x = ds, y = y), color = "blue", size = 1) +
  labs(title = "Prophet Forecast with Actual Test Data",
       x = "Date", y = "Value")

print(p)

train_pred <- head(forecast$yhat, nrow(train_df))  
test_pred <- tail(forecast$yhat, nrow(test_df))  

#residual checks
train_residuals <- train_df$y - train_pred 

train_res_ts <- ts(
  train_residuals, 
  frequency = 12,   
  start = c(
    year(min(train_df$ds)), 
    month(min(train_df$ds))  
  )
)

# plot ACF of residuals
Acf(train_res_ts, lag.max = 40, main = "ACF of Training Residuals")


checkresiduals(train_res_ts, lag = 40)

train_actual_ts <- ts(
  train_df$y, 
  frequency = 12,
  start = c(year(min(train_df$ds)), month(min(train_df$ds)))
)


test_actual_ts <- ts(
  test_df$y, 
  frequency = 12,
  start = c(year(min(test_df$ds)), month(min(test_df$ds)))
)

# now convert predictions to ts objects 
train_pred_ts <- ts(
  train_pred,  # Now this object exists
  frequency = 12,
  start = start(train_actual_ts)
)

test_pred_ts <- ts(
  test_pred,
  frequency = 12,
  start = start(test_actual_ts)
)

# calculate full accuracy metrics
train_accuracy_full <- accuracy(train_pred_ts, train_actual_ts)
test_accuracy_full <- accuracy(test_pred_ts, test_actual_ts)

# display results
combined_accuracy_full <- rbind(train_accuracy_full, test_accuracy_full)
rownames(combined_accuracy_full) <- c("Training Set", "Test Set")
print(combined_accuracy_full)

prophet_plot_components(model, forecast)




