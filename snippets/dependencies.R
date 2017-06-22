# determine dependencies
dep <- step_def$dependencies
if(is.null(dep)) dep <- c()
dep_outputs <- sapply(dep, function(d) get_step_output(args$pipeline, d))

# load dependencies
for (d in dep_outputs){
  load(sprintf("./data/models/%s.Rdata", d))
  var_list <- seq(ncol(as.data.frame(step_outputs$validate_predictions)))

  # clean up validate predictions
  step_outputs$validate_predictions <- as.data.frame(step_outputs$validate_predictions)
  names(step_outputs$validate_predictions) <- paste(d, var_list, sep = "_")

  # clean up test predictions
  step_outputs$test_predictions <- step_outputs$test_predictions %>% select(-listing_id)
  names(step_outputs$test_predictions) <- paste(d, var_list, sep = "_")

  assign(d, step_outputs)
}

# concatenate together
dep_validate_predictions <- do.call(cbind, lapply(dep_outputs, function(x) get(x)$validate_predictions))
dep_test_predictions <- do.call(cbind, lapply(dep_outputs, function(x) get(x)$test_predictions))