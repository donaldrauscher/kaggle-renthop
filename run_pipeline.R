library(argparse)
library(rjson)
library(yaml)

parser <- ArgumentParser()
parser$add_argument("--pipeline", type = "character")
args <- parser$parse_args()

# load pipeline
pipeline_def <- yaml.load_file(sprintf('./pipelines/%s.yaml', args$pipeline))

# iterate through and run each step
for (step in names(pipeline_def)){
  step_def <- pipeline_def[[step]]
  command <- sprintf("Rscript ./models/%s.R --pipeline %s --step %s", step_def$model, args$pipeline, step)
  print(command)
  system(command)
}

