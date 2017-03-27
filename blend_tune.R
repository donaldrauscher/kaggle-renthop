library(xgboost)
library(glmnet)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./data/glm_tune.Rdata")
load("./data/xgb_tune.Rdata")
source("extra.R")

# set up x and y
ydata <- as.numeric(data_model_train$interest_level)-1
xdata <- Matrix(as.matrix(data_model_train[,setdiff(names(data_model_train), c("interest_level"))]), sparse = TRUE)

# split into test and train sets
n <- length(ydata)
train_flag <- sample(n, round(n*0.9))
ydata_train <- ydata[train_flag]
ydata_test <- ydata[-train_flag]
xdata_train <- xdata[train_flag,]
xdata_test <- xdata[-train_flag,]

# create xgb model
params <- list(
  min_child_weight = xgb_params$min_child_weight, eta = xgb_params$eta, 
  max_depth = 6, gamma = 0, colsample_bytree = 0.4, subsample = 1,
  objective = "multi:softprob", num_class = 3, eval_metric = "mlogloss"  
)
xgb <- xgboost(data = xdata_train, label = ydata_train, params = params, nrounds = 250)
pred_xgb <- predict(xgb, xdata_test, reshape = TRUE)

# create glmnet model and determine optimal lambda
glm <- glmnet(x = xdata_train, y = ydata_train, family = "multinomial", alpha = glm_params$alpha, lambda = glm_params$lambda)
pred_glm <- matrix(predict(glm, xdata_test,  s = min(glm_params$lambda), type = "response"), ncol = 3, byrow = FALSE)

# blend the two models
has_na <- apply(pred_xgb, 1, function(x) sum(is.na(x))>0) | apply(pred_glm, 1, function(x) sum(is.na(x))>0) 
pred_xgb2 <- pred_xgb[!has_na,]
pred_glm2 <- pred_glm[!has_na,]
ydata_test2 <- ydata_test[!has_na]

# determine optimal blend weight
blend_weights <- seq(0, 1, 0.1)
blend_mlogloss <- sapply(blend_weights, function(x){
  pred_blend <- pred_xgb2 * x + pred_glm2 * (1 - x)
  return(multiloss(pred_blend, ydata_test2))
})
optimal_blend_weight <- blend_weights[which.min(blend_mlogloss)]
optimal_blend_mlogloss <- min(blend_mlogloss)

# save
save(file="./data/blend_tune.Rdata", list = c("optimal_blend_weight", "optimal_blend_mlogloss"))

