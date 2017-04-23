export R_LIBS="/home/rstudio/R/x86_64-pc-linux-gnu-library/3.2"
Rscript ./setup/extract_train.R
Rscript ./setup/extract_test.R
Rscript glmnet.R
Rscript xgb.R
Rscript blend.R