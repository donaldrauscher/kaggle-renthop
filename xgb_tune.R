library(xgboost)
library(doParallel)
library(dplyr)

set.seed(1)
registerDoParallel(8)

# load data
load("extract_train.Rdata")
source("extra.R")

# set up x and y
ydata <- as.numeric(data_model_train$interest_level)-1
xdata <- as.matrix(data_model_train[,setdiff(names(data_model_train), c("interest_level"))])

# set up tune grid
tune_grid <- expand.grid(
  eta = seq(0.1, 0.5, 0.1), 
  min_child_weight = 1:5, 
  mlogloss = NA
)

# set up cross-validation
cv <- sample(1:3, length(ydata), replace=TRUE)

# create models with each tuning parameter
for (i in 1:nrow(tune_grid)){
  cat(sprintf("Testing %d of %d parameter combinations...\n", i, nrow(tune_grid)))
  mlogloss <- foreach(j=1:3) %dopar% {
    # seperate into test/train
    ydata_train <- ydata[-which(cv == j)]
    ydata_test <- ydata[which(cv == j)]
    xdata_train <- xdata[-which(cv == j),]
    xdata_test <- xdata[which(cv == j),]
    
    # run model
    params <- list(
      eta = tune_grid$eta[i], min_child_weight = tune_grid$min_child_weight[i], 
      max_depth = 6, gamma = 0, colsample_bytree = 0.4, subsample = 1, nthread = 1,
      objective = "multi:softprob", num_class = 3, eval_metric = "mlogloss"  
    )
    xgb <- xgboost(data = xdata_train, label = ydata_train, params = params, nrounds = 250)
    
    # generate some predictions and calculate log loss
    predictions <- predict(xgb, xdata_test, reshape = TRUE)
    multiloss(predictions, ydata_test)
  }
  tune_grid$mlogloss[i] <- mean(unlist(mlogloss))
}
