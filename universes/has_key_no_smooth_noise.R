library(dplyr)
library(Matrix)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./data/extract_test.Rdata")
source("./setup/feature_functions.R")

# parameters
high_card_noise <- 0.1
high_card_leave_one_out <- 1

# set up datasets on all data
data_train_processed2 <- add_high_card_weights_train(data_train_processed, high_card_leave_one_out)
data_train_processed2 <- add_noise(data_train_processed2, std = high_card_noise)
data_train_processed2 <- make_binary(data_train_processed2, grep("[k][0-9]{3}", names(data_train_processed2), value=TRUE), 0)

data_test_processed2 <- add_high_card_weights_test(data_train_processed2, data_test_processed)
data_test_processed2 <- make_binary(data_test_processed2, grep("[k][0-9]{3}", names(data_test_processed2), value=TRUE), 0)

holdouts <- c(
  model_exclude_var, 
  "n_neighborhood_weight", "n_building_weight", "n_manager_weight",
  grep("[k,m,b][0-9]{3}", names(data_test_processed2), value=TRUE)
)
ydata <- as.numeric(data_train_processed2$interest_level)-1
xvar <- setdiff(names(data_train_processed2), c("interest_level", holdouts))
xdata <- Matrix(as.matrix(data_train_processed2[,xvar]), sparse = TRUE)
xdata2 <- Matrix(as.matrix(data_test_processed2[,xvar]), sparse = TRUE)

# set up models for cross validation samples
cv_data <- lapply(1:max(cv), function(i){

  # set up test and train indexes
  train <- which(cv != i)
  test <- which(cv == i)
  
  # set up train set
  xdata_i_train <- add_high_card_weights_train(data_train_processed[train,], high_card_leave_one_out)
  xdata_i_train <- add_noise(xdata_i_train, std = high_card_noise)
  xdata_i_train <- make_binary(xdata_i_train, grep("[k][0-9]{3}", names(xdata_i_train), value=TRUE), 0)
  xdata_i_train <- Matrix(as.matrix(xdata_i_train[,xvar]), sparse = TRUE)
  
  # set up test set; leave one out on test+train for cross validation test set
  xdata_i_test <- xdata[test,]
  
  # return
  return(list(train=train, test=test, xdata_train=xdata_i_train, xdata_test=xdata_i_test))
})

