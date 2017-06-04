library(xgboost)
library(Matrix)
library(argparse)
library(dplyr)

set.seed(1)
source("util.R")

# load universe
parser <- ArgumentParser()
parser$add_argument("--u", type = "character")
args <- parser$parse_args()

source(sprintf("./universes/%s.R", args$u))

# parameters
eta <- 0.02
min_child_weight <- 3
max_depth <- 6
colsample_bytree <- 0.4
subsample <- 1
nrounds <- 2000

# xgb params
params <- list(
  eta = eta, min_child_weight = min_child_weight,
  max_depth = max_depth, colsample_bytree = colsample_bytree, subsample = subsample,
  gamma = 0, objective = "multi:softprob", num_class = 3, eval_metric = "mlogloss"  
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
model_name <- sprintf("%s_xgb", args$u)
assign(model_name, list(model = xgb_base, validate_multiloss = validate_multiloss, validate_predictions = validate_predictions, test_predictions = test_predictions))
save(list = c(model_name), file = sprintf("./models/%s.Rdata", model_name))
write.csv(test_predictions, sprintf("./models/test_predictions/%s.csv", model_name), row.names = FALSE)
