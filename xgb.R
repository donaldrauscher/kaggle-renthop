library(xgboost)
library(Matrix)
library(dplyr)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./data/extract_test.Rdata")
source("./setup/model_feature.R")
source("extra.R")

# parameters
eta <- 0.02
min_child_weight <- 3
max_depth <- 6
colsample_bytree <- 0.4
subsample <- 1
nrounds <- 2000

high_card_small_sample_cutoff <- 5
high_card_round <- 10
high_card_leave_one_out <- 1

# set up datasets on all data
data_train_processed2 <- add_high_card_weights_train(data_train_processed, high_card_leave_one_out)
data_train_processed2 <- add_noise(data_train_processed2)
data_train_processed2 <- smooth_low_sample_weights(data_train_processed2, high_card_small_sample_cutoff, high_card_round)
data_train_processed2 <- make_binary(data_train_processed2, grep("[k][0-9]{3}", names(data_train_processed2), value=TRUE), 0)

data_test_processed2 <- add_high_card_weights_test(data_train_processed2, data_test_processed)
data_test_processed2 <- smooth_low_sample_weights(data_test_processed2, high_card_small_sample_cutoff, high_card_round)
data_test_processed2 <- make_binary(data_test_processed2, grep("[k][0-9]{3}", names(data_test_processed2), value=TRUE), 0)

# experiment with some other variable manipulations
data_train_processed2$price[data_train_processed2$price > 30000] <- 30000
data_test_processed2$price[data_test_processed2$price > 30000] <- 30000

data_train_processed2 <- data_train_processed2 %>%
  mutate(
    logprice = log(price)
  )

holdouts <- c(model_exclude_var, "n_neighborhood_weight", "n_building_weight", "n_manager_weight")
ydata <- as.numeric(data_train_processed2$interest_level)-1
xvar <- setdiff(names(data_train_processed2), c("interest_level", holdouts))
xdata <- Matrix(as.matrix(data_train_processed2[,xvar]), sparse = TRUE)
xdata2 <- Matrix(as.matrix(data_test_processed2[,xvar]), sparse = TRUE)

# xgb params
params <- list(
  eta = eta, min_child_weight = min_child_weight,
  max_depth = max_depth, colsample_bytree = colsample_bytree, subsample = subsample,
  gamma = 0, objective = "multi:softprob", num_class = 3, eval_metric = "mlogloss"  
)

# build models for each cross validation fold
validate_predictions <- matrix(ncol = 3, nrow = length(ydata))
for (i in 1:5){
  cat("Starting CV fold ", i, " of 5...\n", sep = "")
  
  # set up test and train
  train <- which(cv != i)
  test <- which(cv == i)
  
  xdata_i_train <- add_high_card_weights_train(data_train_processed[train,], high_card_leave_one_out)
  xdata_i_train <- add_noise(xdata_i_train)
  xdata_i_train <- smooth_low_sample_weights(xdata_i_train, high_card_small_sample_cutoff, high_card_round)  
  xdata_i_train <- make_binary(xdata_i_train, grep("[k][0-9]{3}", names(xdata_i_train), value=TRUE), 0)
  xdata_i_train <- Matrix(as.matrix(xdata_i_train[,xvar]), sparse = TRUE)

  # use train averages
  xdata_i_test1 <- add_high_card_weights_test(data_train_processed[train,], data_train_processed[test,])
  xdata_i_test1 <- smooth_low_sample_weights(xdata_i_test1, high_card_small_sample_cutoff, high_card_round)  
  xdata_i_test1 <- make_binary(xdata_i_test1, grep("[k][0-9]{3}", names(xdata_i_test1), value=TRUE), 0)
  xdata_i_test1 <- Matrix(as.matrix(xdata_i_test1[,xvar]), sparse = TRUE)
  
  # leave one out on CV test+train
  xdata_i_test2 <- xdata[test,]
  
  # run model
  xgb <- xgboost(data = xdata_i_train, label = ydata[train], params = params, nrounds = nrounds, metrics = c("mlogloss"))

  # generate predictions
  validate_predictions1 <- predict(xgb, xdata_i_test1, reshape = TRUE)
  validate_predictions2 <- predict(xgb, xdata_i_test2, reshape = TRUE)
  print(multiloss(validate_predictions1, ydata[test]))
  print(multiloss(validate_predictions2, ydata[test]))
  validate_predictions[test,] <- (validate_predictions1 + validate_predictions2) / 2
}

# calculate multilogloss for validate predictions
validate_multiloss <- multiloss(validate_predictions, ydata)
validate_multiloss

# create final model without cross-validation
xgb_base <- xgboost(data = xdata, label = ydata, params = params, nrounds = nrounds, metrics = c("mlogloss"))
test_predictions <- as.data.frame(predict(xgb_base, xdata2, reshape = TRUE))
names(test_predictions) <- c("low", "medium", "high")
test_predictions$listing_id <- data_test_processed$listing_id

# save
save(xgb_base, validate_multiloss, validate_predictions, test_predictions, file = "./models/xgb.Rdata")
write.csv(test_predictions, "./data/predictions_xgb.csv", row.names = FALSE)
