library(glmnet)
library(dplyr)
library(argparse)

set.seed(1)
source("./snippets/util.R")

# load universe
parser <- ArgumentParser()
parser$add_argument("--univ", type = "character")
parser$add_argument("--univ-param", type = "character", default = "{}")
parser$add_argument("--model-param", type = "character", default = "{}")
args <- parser$parse_args()

source(sprintf("./universes/%s.R", args$univ))

# model parameters
model_param <- fromJSON(args$model_param)
if(is.null(model_param$alpha)) model_param$alpha <- 1
if(is.null(model_param$nlambda)) model_param$nlambda <- 100

# base model on all data; use lambda path for cross-validation samples
glm_base <- glmnet(x = xdata, y = ydata, family = "multinomial", alpha = model_param$alpha, nlambda = model_param$nlambda)
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
  glm <- glmnet(x = xdata_train, y = ydata[train], family = "multinomial", alpha = model_param$alpha, lambda = lambda_path)
  
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
test_predictions$listing_id <- xdata2_id

# save
model_name <- sprintf("%s_glmnet", args$univ)
assign(model_name, list(model = glm_base, validate_multiloss = validate_multiloss, validate_predictions = validate_predictions, test_predictions = test_predictions))
save(list = c(model_name), file = sprintf("./data/models/%s.Rdata", model_name))
write.csv(test_predictions, sprintf("./data/test_predictions/%s.csv", model_name), row.names = FALSE)
