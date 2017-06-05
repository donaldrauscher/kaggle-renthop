library(xgboost)
library(Matrix)
library(argparse)
library(dplyr)

set.seed(1)
source("util.R")

# load universe
parser <- ArgumentParser()
parser$add_argument("--univ", type = "character")
parser$add_argument("--univ-param", type = "character", default = "{}")
parser$add_argument("--model-param", type = "character", default = "{}")
args <- parser$parse_args()

source(sprintf("./universes/%s.R", args$univ))

# model parameters
model_param <- fromJSON(args$model_param)
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
  gamma = model_param$gamma, objective = "multi:softprob", num_class = 3, eval_metric = "mlogloss"  
)

# build models for each cross validation fold
validate_predictions <- matrix(ncol = 3, nrow = length(ydata))
best_ntreelimit <- c()
for (i in 1:max(cv)){
  cat("Starting CV fold ", i, " of ", max(cv), "...\n", sep = "")
  
  # run model
  dtrain <- xgb.DMatrix(data = cv_data[[i]]$xdata_train, label = ydata[cv_data[[i]]$train])
  dtest <- xgb.DMatrix(data = cv_data[[i]]$xdata_test, label = ydata[cv_data[[i]]$test])
  watchlist <- list(train=dtrain, test = dtest)
  callbacks <- list(cb.early.stop(stopping_rounds = 25, maximize = FALSE, metric_name = "test-mlogloss", verbose = TRUE))
  xgb <- xgb.train(
    data = dtrain, params = params, nrounds = nrounds, 
    watchlist = watchlist, callbacks = callbacks
  )

  # generate predictions
  validate_predictions_temp <- predict(xgb, cv_data[[i]]$xdata_test, reshape = TRUE)
  print(multiloss(validate_predictions_temp, ydata[cv_data[[i]]$test]))
  validate_predictions[cv_data[[i]]$test,] <- validate_predictions_temp
  
  # record optimal number of trees
  best_ntreelimit[i] <- xgb$best_ntreelimit
}

# calculate multilogloss for validate predictions
validate_multiloss <- multiloss(validate_predictions, ydata)
validate_multiloss

# create final model without cross-validation
xgb_base <- xgboost(data = xdata, label = ydata, params = params, nrounds = round(mean(best_ntreelimit)))
test_predictions <- as.data.frame(predict(xgb_base, xdata2, reshape = TRUE))
names(test_predictions) <- c("low", "medium", "high")
test_predictions$listing_id <- xdata2_id

# save
model_name <- sprintf("%s_xgb", args$univ)
assign(model_name, list(model = xgb_base, validate_multiloss = validate_multiloss, validate_predictions = validate_predictions, test_predictions = test_predictions))
save(list = c(model_name), file = sprintf("./models/%s.Rdata", model_name))
write.csv(test_predictions, sprintf("./models/test_predictions/%s.csv", model_name), row.names = FALSE)
