library(yaml)
library(digest)
library(rjson)

# function for multinomial log loss
multiloss <- function(predicted, actual){
  predicted_m <- apply(predicted, c(1,2), function(x) max(min(x, 1-10^(-15)), 10^(-15)))
  actual_m <- as.matrix(sapply(min(actual):max(actual), function(x) ifelse(actual == x, 1, 0)))
  return(-sum(actual_m*log(predicted_m)) / nrow(actual_m))
}

# inverse of a set of indices
inv_which <- function(indices, tot) setdiff(seq_len(tot), indices)

# returns step def from pipeline yaml
get_step_def <- function(pipeline, step){
  pipeline_def <- yaml.load_file(sprintf('./pipelines/%s.yaml', pipeline))
  return(pipeline_def[[step]])
}

# returns the name of the output file from a step
get_step_output <- function(pipeline, step, step_def = NULL){
  if(is.null(step_def)) step_def <- get_step_def(pipeline, step)
  return(paste(pipeline, step, step_def$univ, step_def$model, sep = "_"))
}

get_step_output_hashed <- function(pipeline, step, step_def = NULL){
  if(is.null(step_def)) step_def <- get_step_def(pipeline, step)
  return(paste(step_def$univ, step_def$model, digest(toJSON(get_all_dep(pipeline, step))), sep = "_"))
}

# pulls all parameters recursively
get_all_dep <- function(pipeline, step){
  step_def <- get_step_def(pipeline, step)
  if(!is.null(step_def$dependencies)){
    step_def$dependencies <- setNames(sapply(step_def$dependencies, function(x) get_all_dep(pipeline, x)), step_def$dependencies)
  }
  return(step_def)
}

# check if a step has been run already
check_step <- function(pipeline, step){
  step_output <- paste0(get_step_output(pipeline, step), ".Rdata")
  step_output_hashed <- paste0(get_step_output_hashed(pipeline, step), ".Rdata")
  if (step_output %in% list.files("./data/models")) stop("Step has already been run!")
  if (step_output_hashed %in% list.files("./data/models/hashed")){
    system(sprintf("ln -sr ./data/models/hashed/%s ./data/models/%s", step_output_hashed, step_output))
    stop("Step has already been run but needed to set symlink!")
  }
}