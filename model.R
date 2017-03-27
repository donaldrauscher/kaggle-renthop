library(xgboost)
library(glmnet)
library(dplyr)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./data/extract_test.Rdata")
load("./data/glm_tune.Rdata")
load("./data/xgb_tune.Rdata")
load("./data/blend_tune.Rdata")
source("extra.R")

# set up x and y
ydata_train <- as.numeric(data_model_train$interest_level)-1
xdata_train <- Matrix(as.matrix(data_model_train[,setdiff(names(data_model_train), c("interest_level"))]), sparse = TRUE)
xdata_test <- Matrix(as.matrix(data_model_test), sparse = TRUE)

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

# blend the predictions
pred_blend <- pred_xgb * optimal_blend_weight + pred_glm * (1 - optimal_blend_weight)

# save
pred_blend2 <- as.data.frame(pred_blend)
names(pred_blend2) <- c("low", "medium", "high")
pred_blend2$listing_id <- data_initial_test$listing_id
pred_blend2 <- pred_blend2 %>% select(listing_id, high, medium, low)
write.csv(pred_blend2, "./data/predictions.csv", row.names = FALSE)

