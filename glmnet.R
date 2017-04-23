library(glmnet)
library(dplyr)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./data/extract_test.Rdata")
source("./setup/model_feature.R")
source("extra.R")

# parameters
alpha <- 1
nlambda <- 100

high_card_small_sample_cutoff <- 5
high_card_round <- 1
high_card_leave_one_out <- 1

# set up datasets on all data
data_train_processed2 <- add_high_card_weights_train(data_train_processed, high_card_leave_one_out)
data_train_processed2 <- add_noise(data_train_processed2)
data_train_processed2 <- smooth_low_sample_weights(data_train_processed2, high_card_small_sample_cutoff, high_card_round)
data_train_processed2 <- make_binary(data_train_processed2, grep("[k][0-9]{3}", names(data_train_processed2), value=TRUE), 0)

data_test_processed2 <- add_high_card_weights_test(data_train_processed2, data_test_processed)
data_test_processed2 <- smooth_low_sample_weights(data_test_processed2, high_card_small_sample_cutoff, high_card_round)
data_test_processed2 <- make_binary(data_test_processed2, grep("[k][0-9]{3}", names(data_test_processed2), value=TRUE), 0)

holdouts <- c(
  model_exclude_var, 
  "n_neighborhood_weight", "n_building_weight", "n_manager_weight",
  grep("[k][0-9]{3}", names(data_train_processed2), value=TRUE)
)
ydata <- as.numeric(data_train_processed2$interest_level)-1
xvar <- setdiff(names(data_train_processed2), c("interest_level", holdouts))
xdata <- Matrix(as.matrix(data_train_processed2[,xvar]), sparse = TRUE)
xdata2 <- Matrix(as.matrix(data_test_processed2[,xvar]), sparse = TRUE)

# base model on all data; use lambda path for cross-validation samples
glm_base <- glmnet(x = xdata, y = ydata, family = "multinomial", alpha = alpha, nlambda = nlambda)
lambda_path <- glm_base$lambda

# build overall model
validate_predictions <- matrix(ncol = 3, nrow = length(ydata))
optimal_lambda <- c()
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
  glm <- glmnet(x = xdata_i_train, y = ydata[train], family = "multinomial", alpha = alpha, lambda = lambda_path)
  
  # generate predictions
  lambda_mlogloss <- sapply(glm$lambda, function(l){
    predictions <- matrix(predict(glm, xdata_i_test1,  s = l, type = "response"), ncol = 3, byrow = FALSE)
    return(multiloss(predictions, ydata[test]))
  })
  
  # determine optimal lambda
  optimal_lambda[i] <- glm$lambda[which.min(lambda_mlogloss)]
  
  # generate predictions
  validate_predictions1 <- matrix(predict(glm, xdata_i_test1,  s = optimal_lambda[i], type = "response"), ncol = 3, byrow = FALSE)
  validate_predictions2 <- matrix(predict(glm, xdata_i_test2,  s = optimal_lambda[i], type = "response"), ncol = 3, byrow = FALSE)
  print(multiloss(validate_predictions1, ydata[test]))
  print(multiloss(validate_predictions2, ydata[test]))
  validate_predictions[test,] <- (validate_predictions1 + validate_predictions2) / 2
    
}

# calculate multilogloss
validate_multiloss <- multiloss(validate_predictions, ydata)
validate_multiloss

# generate predictions on test set
optimal_lambda_final <- min(lambda_path[lambda_path >= mean(optimal_lambda)])
test_predictions <- as.data.frame(matrix(predict(glm_base, xdata2,  s = optimal_lambda_final, type = "response"), ncol = 3, byrow = FALSE))
names(test_predictions) <- c("low", "medium", "high")
test_predictions$listing_id <- data_test_processed$listing_id

# save
save(glm_base, optimal_lambda_final, validate_multiloss, validate_predictions, test_predictions, file = "./models/glmnet.Rdata")
write.csv(test_predictions, "./data/predictions_glmnet.csv", row.names = FALSE)
