library(argparse)

set.seed(1)
source("./snippets/util.R")

# load universe
parser <- ArgumentParser()
parser$add_argument("--pipeline", type = "character")
parser$add_argument("--step", type = "character")
args <- parser$parse_args()

# load step def
step_def <- get_step_def(args$pipeline, args$step)
univ_param <- step_def$univ_param
if(is.null(univ_param)) univ_param <- list()
model_param <- step_def$model_param
if(is.null(model_param)) model_param <- list()

# check to see if model has been run already
check_step(args$pipeline, args$step)

# load dependencies
source("./snippets/dependencies.R")

# build universe
source(sprintf("./universes/%s.R", step_def$univ))
