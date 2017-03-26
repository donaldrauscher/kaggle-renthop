library(xgboost)
library(glmnet)

set.seed(1)

# load data
load("extract_train.Rdata")

# function for multinomial log loss
multiloss <- function(predicted, actual){
  predicted_m <- apply(predicted, c(1,2), function(x) max(min(x, 1-10^(-15)), 10^(-15)))
  actual_m <- as.matrix(sapply(0:max(actual), function(x) ifelse(actual == x, 1, 0)))
  return(-sum(actual_m*log(predicted_m)) / nrow(actual_m))
}

# set up x and y
ydata <- as.numeric(data_model$interest_level)-1
xdata <- as.matrix(data_model[,setdiff(names(data_model), c("interest_level"))])

# split into test and train sets
n <- length(ydata)
train_flag <- sample(n, round(n*0.9))
ydata_train <- ydata[train_flag]
ydata_test <- ydata[-train_flag]
xdata_train <- xdata[train_flag,]
xdata_test <- xdata[-train_flag,]

# create xgb model
params <- list(
  min_child_weight = 4, eta = 0.3, 
  max_depth = 6, gamma = 0, colsample_bytree = 0.4, subsample = 1,
  objective = "multi:softprob", num_class = 3, eval_metric = "mlogloss"  
)
xgb <- xgboost(data = xdata_train, label = ydata_train, params = params, nrounds = 250)
pred_xgb <- predict(xgb, xdata_test, reshape = TRUE)

# create glmnet model and determine optimal lambda
glm <- glmnet(x = xdata_train, y = ydata_train, family = "multinomial", alpha = 1, nlambda = 250)

pred_glm_mlogloss <- sapply(glm$lambda, function(l){
  pred_glm <- matrix(predict(glm, xdata_test,  s = l, type = "response"), ncol = 3, byrow = FALSE)
  has_na <- apply(pred_glm, 1, function(x) sum(is.na(x))>0)
  return(multiloss(pred_glm[!has_na,], ydata_test[!has_na]))
})

optimal_lambda <- glm$lambda[which.min(pred_glm_mlogloss)]
pred_glm <- matrix(predict(glm, xdata_test,  s = optimal_lambda, type = "response"), ncol = 3, byrow = FALSE)

# blend the two models
has_na <- apply(pred_xgb, 1, function(x) sum(is.na(x))>0) | apply(pred_glm, 1, function(x) sum(is.na(x))>0) 
pred_xgb2 <- pred_xgb[!has_na,]
pred_glm2 <- pred_glm[!has_na,]
ydata_test2 <- ydata_test[!has_na]

weights <- seq(0, 1, 0.1)
blend_mlogloss <- sapply(weights, function(x){
  pred_blend <- pred_xgb2 * x + pred_glm2 * (1 - x)
  return(multiloss(pred_blend, ydata_test2))
})
optimal_weight <- weights[which.min(blend_mlogloss)]

