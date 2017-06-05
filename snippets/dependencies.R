# determine dependencies
dep <- step_def$dependencies
if(is.null(dep)) dep <- c()
dep_outputs <- sapply(dep, function(d) get_step_output(args$pipeline, d))

# load dependencies
for (d in dep_outputs){
  load(sprintf("./data/models/%s.Rdata", d))
  dep_output <- get(d)
  
  # clean up validate predictions
  dep_output$validate_predictions <- as.data.frame(dep_output$validate_predictions[,2:3])
  names(dep_output$validate_predictions) <- paste(d, c("medium", "high"), sep = "_")

  # clean up test predictions
  dep_output$test_predictions <- dep_output$test_predictions[,2:3]
  names(dep_output$test_predictions) <- paste(d, names(dep_output$test_predictions), sep = "_")

  assign(d, dep_output)
}

# concatenate together
dep_validate_predictions <- do.call(cbind, lapply(dep_outputs, function(x) get(x)$validate_predictions))
dep_test_predictions <- do.call(cbind, lapply(dep_outputs, function(x) get(x)$test_predictions))