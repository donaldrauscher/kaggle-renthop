library(yaml)

# returns the name of the output file from a step
get_step_output <- function(pipeline, step){
  pipeline_def <- yaml.load_file(sprintf('./pipelines/%s.yaml', pipeline))
  step_def <- pipeline_def[[step]]
  return(paste(pipeline, step, step_def$univ, step_def$model, sep = "_"))
}

# load all of the dependencies
for (d in arg$dep){
  load(sprintf("./data/models/%s.Rdata", get_step_output(d)))
}