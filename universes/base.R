library(dplyr)
library(Matrix)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./data/extract_test.Rdata")
source("./setup/feature_functions.R")

# some useful functions
inv_which <- function(indices, tot) setdiff(seq_len(tot), indices)

# combine datasets
data_all_processed <- bind_rows(data_train_processed, data_test_processed)
is_train <- which(!is.na(data_all_processed$interest_level))
is_test <- inv_which(is_train, nrow(data_all_processed))

# add high-cardinality categorical
add_high_card_cat <- function(df){
  df$building_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), building_id, c(0, 10, 20, 30, Inf))
  df$building_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), building_id, c(0, 10, 20, 30, Inf))
  df$manager_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), manager_id, c(0, 10, 20, 30, Inf))
  df$manager_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), manager_id, c(0, 10, 20, 30, Inf))
  df$neighborhood_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), neighborhood_id, c(0, 10, 20, 30, Inf))
  df$neighborhood_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), neighborhood_id, c(0, 10, 20, 30, Inf))
  return(df)
}

data_all_processed2 <- add_high_card_cat(data_all_processed)

# add noise to train / impute for test
high_card_cat <- c("building_interest_h", "building_interest_m", "manager_interest_h", "manager_interest_m", "neighborhood_interest_h", "neighborhood_interest_m") 
data_all_processed2 <- add_noise(data_all_processed2, is_train, high_card_cat)

# make some variables binary
kw_var <- grep("[k][0-9]{3}", names(data_all_processed2), value=TRUE)
data_all_processed2 <- make_binary(data_all_processed2, kw_var, 0)

holdouts <- c(model_exclude_var, grep("[m,b][0-9]{3}", names(data_all_processed2), value=TRUE))
ydata <- as.numeric(data_all_processed2$interest_level[is_train])-1
xvar <- setdiff(names(data_all_processed2), c("interest_level", holdouts))
xdata <- Matrix(as.matrix(data_all_processed2[is_train,xvar]), sparse = TRUE)
xdata2 <- Matrix(as.matrix(data_all_processed2[is_test,xvar]), sparse = TRUE)
xdata2_id <- data_all_processed2$listing_id[is_test]

# set up models for cross validation samples
cv <- sample(1:5, length(is_train), replace=TRUE)
cv_data <- lapply(1:5, function(i){

  # set up test and train indexes
  is_train2 <- is_train[cv != i]
  is_test2 <- is_train[cv == i]

  # set up train and test sets
  xdata_i <- data_all_processed
  xdata_i$interest_level[is_test2] <- NA
  xdata_i <- add_high_card_cat(xdata_i)
  xdata_i <- add_noise(xdata_i, is_train2, high_card_cat)
  xdata_i <- make_binary(xdata_i, kw_var, 0)

  xdata_i_train <- Matrix(as.matrix(xdata_i[is_train2,xvar]), sparse = TRUE)
  xdata_i_test <- Matrix(as.matrix(xdata_i[is_test2,xvar]), sparse = TRUE)
  
  # return
  return(list(train=is_train2, test=is_test2, xdata_train=xdata_i_train, xdata_test=xdata_i_test))
})
