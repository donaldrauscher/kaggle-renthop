library(glmnet)
library(dplyr)
library(argparse)

set.seed(1)

# load universe
parser <- ArgumentParser()
parser$add_argument("--u", type = "character")
args <- parser$parse_args()

source(sprintf("./universes/%s.R", args$u))

# load eval function
source("multiloss.R")

# parameters
alpha <- 1
nlambda <- 100

# base model on all data; use lambda path for cross-validation samples
glm_base <- glmnet(x = xdata, y = ydata, family = "multinomial", alpha = alpha, nlambda = nlambda)
lambda_path <- glm_base$lambda

# build overall model
validate_predictions <- matrix(ncol = 3, nrow = length(ydata))
optimal_lambda <- c()
for (i in 1:max(cv)){
  cat("Starting CV fold ", i, " of ", max(cv), "...\n", sep = "")
  
  # run model
  xdata_train <- cv_data[[i]]$xdata_train
  xdata_test <- cv_data[[i]]$xdata_test
  train <- cv_data[[i]]$train
  test <- cv_data[[i]]$test
  glm <- glmnet(x = xdata_train, y = ydata[train], family = "multinomial", alpha = alpha, lambda = lambda_path)
  
  # generate predictions
  lambda_mlogloss <- sapply(glm$lambda, function(l){
    predictions <- matrix(predict(glm, xdata_test,  s = l, type = "response"), ncol = 3, byrow = FALSE)
    return(multiloss(predictions, ydata[test]))
  })
  
  # determine optimal lambda
  optimal_lambda[i] <- glm$lambda[which.min(lambda_mlogloss)]
  
  # generate predictions
  validate_predictions_temp <- matrix(predict(glm, xdata_test,  s = optimal_lambda[i], type = "response"), ncol = 3, byrow = FALSE)
  print(multiloss(validate_predictions_temp, ydata[test]))
  validate_predictions[test,] <- validate_predictions_temp
    
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
save(glm_base, validate_multiloss, validate_predictions, test_predictions, file = sprintf("./models/%s_glmnet.Rdata", args$u))
write.csv(test_predictions, sprintf("./models/test_predictions/%s_glmnet.csv", args$u), row.names = FALSE)
