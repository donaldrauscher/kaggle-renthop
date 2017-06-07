library(dplyr)

# model header
source("./snippets/model_header.R")

# determine optimal weights across models
pred_list1 <- lapply(dep_outputs, function(x) get(x)$validate_predictions)
pred_list2 <- lapply(dep_outputs, function(x) get(x)$test_predictions)

blend <- function(par, pred_list){
  return(Reduce('+', lapply(seq_along(pred_list), function(x) pred_list[[x]] * par[x])))
}

multiloss_blend <- function(par, pred_list, actuals){
  par2 <- c(par, 1 - sum(par))
  return(multiloss(blend(par2, pred_list), actuals))
}

blender <- function(pred_list, actuals){
  n_model <- length(pred_list)
  return(optim(
    par = rep(1 / n_model, n_model - 1), 
    fn=multiloss_blend, method="L-BFGS-B", 
    lower = rep(0, n_model - 1), upper = rep(1, n_model - 1),
    pred_list = pred_list, actuals = actuals
  ))
}

optimal_blend_summary <- blender(pred_list1, ydata)
optimal_blend_weights <- c(optimal_blend_summary$par, 1 - sum(optimal_blend_summary$par))
validate_predictions <- blend(optimal_blend_weights, pred_list1)
validate_multiloss <- optimal_blend_summary$value

# generate predictions on test set
test_predictions <- as.data.frame(blend(optimal_blend_weights, pred_list2))
names(test_predictions) <- c("low", "medium", "high")
test_predictions$listing_id <- xdata2_id

# save
blend_base <- optimal_blend_weights
source("./snippets/model_export.R")

