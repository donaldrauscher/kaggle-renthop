library(dplyr)
library(Matrix)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./data/extract_test.Rdata")
source("./setup/feature_functions.R")

# add high-cardinality categorical
add_high_card_cat_train <- function(df){
  df$building_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), building_id)
  df$building_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), building_id)
  df$manager_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), manager_id)
  df$manager_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), manager_id)
  df$neighborhood_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), neighborhood_id)
  df$neighborhood_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), neighborhood_id)
  return(df)
}

add_high_card_cat_test <- function(df_train, df_test){
  building_interest_var <- df_train %>% select(building_id, building_interest_h, building_interest_m) %>% distinct()
  manager_interest_var <- df_train %>% select(manager_id, manager_interest_h, manager_interest_m) %>% distinct()
  neighbor_interest_var <- df_train %>% select(neighborhood_id, neighborhood_interest_h, neighborhood_interest_m) %>% distinct()
  df_test <- df_test %>% left_join(building_interest_var, by = c("building_id")) %>% left_join(manager_interest_var, by = c("manager_id")) %>% left_join(neighbor_interest_var, by = c("neighborhood_id"))
  return(df_test)
}

data_train_processed2 <- add_high_card_cat_train(data_train_processed)
data_test_processed2 <- add_high_card_cat_test(data_train_processed2, data_test_processed)

# add noise to train / impute for test
high_card_cat <- c("building_interest_h", "building_interest_m", "manager_interest_h", "manager_interest_m", "neighborhood_interest_h", "neighborhood_interest_m") 
data_train_processed2 <- add_noise(data_train_processed2, high_card_cat)
data_test_processed2 <- impute(data_test_processed2, high_card_cat, mean)

# make some variables binar
data_train_processed2 <- make_binary(data_train_processed2, grep("[k][0-9]{3}", names(data_train_processed2), value=TRUE), 0)
data_test_processed2 <- make_binary(data_test_processed2, grep("[k][0-9]{3}", names(data_test_processed2), value=TRUE), 0)

holdouts <- c(model_exclude_var, grep("[m,b][0-9]{3}", names(data_test_processed2), value=TRUE))
ydata <- as.numeric(data_train_processed2$interest_level)-1
xvar <- setdiff(names(data_train_processed2), c("interest_level", holdouts))
xdata <- Matrix(as.matrix(data_train_processed2[,xvar]), sparse = TRUE)
xdata2 <- Matrix(as.matrix(data_test_processed2[,xvar]), sparse = TRUE)

# set up models for cross validation samples
cv_data <- lapply(1:max(cv), function(i){

  # set up test and train indexes
  train <- which(cv != i)
  test <- which(cv == i)
  
  # set up train and test sets
  xdata_i_train <- add_high_card_cat_train(data_train_processed[train,])
  xdata_i_test <- add_high_card_cat_test(xdata_i_train, data_train_processed[test,])
  
  xdata_i_train <- add_noise(xdata_i_train, high_card_cat)
  xdata_i_test <- impute(xdata_i_test, high_card_cat, mean)
  
  xdata_i_train <- make_binary(xdata_i_train, grep("[k][0-9]{3}", names(xdata_i_train), value=TRUE), 0)
  xdata_i_test <- make_binary(xdata_i_test, grep("[k][0-9]{3}", names(xdata_i_test), value=TRUE), 0)
  
  xdata_i_train <- Matrix(as.matrix(xdata_i_train[,xvar]), sparse = TRUE)
  xdata_i_test <- Matrix(as.matrix(xdata_i_test[,xvar]), sparse = TRUE)
  #xdata_i_test <- xdata[test,] # if want to use 'interest' data in test set in computation of high-card cat
  
  # return
  return(list(train=train, test=test, xdata_train=xdata_i_train, xdata_test=xdata_i_test))
})

