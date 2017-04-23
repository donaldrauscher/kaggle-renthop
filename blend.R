library(xgboost)
library(glmnet)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./models/glmnet.Rdata")
glm_validate_predictions <- validate_predictions
glm_test_predictions <- test_predictions
load("./models/xgb.Rdata")
xgb_validate_predictions <- validate_predictions
xgb_test_predictions <- test_predictions
source("extra.R")

# blend the two models
ydata <- as.numeric(data_train_processed$interest_level)-1

# determine optimal blend weight
blend_weights <- seq(0, 1, 0.1)
blend_mlogloss <- sapply(blend_weights, function(x){
  blend_validate_predictions <- xgb_validate_predictions * x + glm_validate_predictions * (1 - x)
  return(multiloss(blend_validate_predictions, ydata))
})
validate_multiloss <- min(blend_mlogloss)
optimal_blend_weight <- blend_weights[which.min(blend_mlogloss)]
validate_multiloss
optimal_blend_weight

# created blended predictions for test set
validate_predictions <- xgb_validate_predictions * optimal_blend_weight + glm_validate_predictions * (1 - optimal_blend_weight)
test_predictions <- xgb_test_predictions * optimal_blend_weight + glm_test_predictions * (1 - optimal_blend_weight)

# save
save(optimal_blend_weight, validate_multiloss, validate_predictions, test_predictions, file = "./models/blend.Rdata")
write.csv(test_predictions, "./data/predictions_blend.csv", row.names = FALSE)

