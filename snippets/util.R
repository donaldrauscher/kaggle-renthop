library(yaml)

# function for multinomial log loss
multiloss <- function(predicted, actual){
  predicted_m <- apply(predicted, c(1,2), function(x) max(min(x, 1-10^(-15)), 10^(-15)))
  actual_m <- as.matrix(sapply(min(actual):max(actual), function(x) ifelse(actual == x, 1, 0)))
  return(-sum(actual_m*log(predicted_m)) / nrow(actual_m))
}

# inverse of a set of indices
inv_which <- function(indices, tot) setdiff(seq_len(tot), indices)

# returns the name of the output file from a step
get_step_output <- function(pipeline, step){
  pipeline_def <- yaml.load_file(sprintf('./pipelines/%s.yaml', pipeline))
  step_def <- pipeline_def[[step]]
  return(paste(pipeline, step, step_def$univ, step_def$model, sep = "_"))
}
