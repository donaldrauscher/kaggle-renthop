library(xgboost)
library(Matrix)
library(dplyr)

# model header
source("./snippets/model_header.R")

# build universe
source(sprintf("./universes/%s.R", step_def$univ))

# model parameters
if(is.null(model_param$eta)) model_param$eta <- 0.02
if(is.null(model_param$min_child_weight)) model_param$min_child_weight <- 3
if(is.null(model_param$max_depth)) model_param$max_depth <- 6
if(is.null(model_param$colsample_bytree)) model_param$colsample_bytree <- 0.4
if(is.null(model_param$subsample)) model_param$subsample <- 1
if(is.null(model_param$gamma)) model_param$gamma <- 0
if(is.null(model_param$nrounds)) model_param$nrounds <- 2000

params <- list(
  eta = model_param$eta, min_child_weight = model_param$min_child_weight,
  max_depth = model_param$max_depth, colsample_bytree = model_param$colsample_bytree, subsample = model_param$subsample,
  gamma = model_param$gamma, objective = "reg:linear", eval_metric = "rmse"  
)

# build models for each cross validation fold
validate_predictions <- rep(NA, length(ydata))
best_ntreelimit <- c()
for (i in 1:max(cv)){
  cat("Starting CV fold ", i, " of ", max(cv), "...\n", sep = "")
  
  # run model
  dtrain <- xgb.DMatrix(data = cv_data[[i]]$xdata_train, label = ydata[cv_data[[i]]$train])
  dtest <- xgb.DMatrix(data = cv_data[[i]]$xdata_test, label = ydata[cv_data[[i]]$test])
  watchlist <- list(train=dtrain, test = dtest)
  callbacks <- list(cb.early.stop(stopping_rounds = 25, maximize = FALSE, metric_name = "test-rmse", verbose = TRUE))
  xgb <- xgb.train(
    data = dtrain, params = params, nrounds = model_param$nrounds, 
    watchlist = watchlist, callbacks = callbacks
  )
  
  # generate predictions
  validate_predictions_temp <- predict(xgb, cv_data[[i]]$xdata_test)
  print(rmse(validate_predictions_temp, ydata[cv_data[[i]]$test]))
  validate_predictions[cv_data[[i]]$test] <- validate_predictions_temp
  
  # record optimal number of trees
  best_ntreelimit[i] <- xgb$best_ntreelimit
}

# calculate error for validate predictions
validate_error <- rmse(validate_predictions, ydata)
validate_error

# create final model without cross-validation
xgb2_base <- xgboost(data = xdata, label = ydata, params = params, nrounds = round(mean(best_ntreelimit)))
test_predictions <- data.frame(listing_id = xdata2_id, y = predict(xgb2_base, xdata2))

# save
source("./snippets/model_export.R")
