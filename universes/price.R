library(dplyr)
library(lubridate)
library(rjson)
library(Matrix)

set.seed(1)
source("./snippets/util.R")

# load data
load("./data/extract_train.Rdata")
load("./data/extract_test.Rdata")
source("./snippets/feature_functions.R")

# combine datasets
data_all_processed <- bind_rows(data_train_processed, data_test_processed)
if(length(dep_outputs)>0) data_all_processed <- cbind(data_all_processed,  bind_rows(dep_validate_predictions, dep_test_predictions))
is_train <- which(!is.na(data_all_processed$interest_level))
is_test <- inv_which(is_train, nrow(data_all_processed))

# some base variables
data_all_processed <- data_all_processed %>% 
  mutate(
    has_half_bath = as.integer(round(bathrooms) != bathrooms),
    bedrooms_minus_bathrooms = bedrooms - bathrooms, 
    total_rooms = bedrooms + bathrooms,
    total_rooms2 = ifelse(bedrooms <= 1, 1, ifelse(bedrooms >= 4, 4, bedrooms)) + 0.5 * ifelse(bathrooms <= 0, 0, ifelse(bathrooms >= 2, 2, bathrooms))
  )

# assemble x and y
feature_var <- grep("[f][0-9]{3}", names(data_all_processed), value=TRUE)
ydata <- data_all_processed$price[is_train]
xvar <- c("has_half_bath", "bedrooms_minus_bathrooms", "total_rooms", "total_rooms2", "neighborhood_id", "latitude", "longitude", feature_var)
xdata <- Matrix(as.matrix(data_all_processed[is_train,xvar]), sparse = TRUE)
xdata2 <- Matrix(as.matrix(data_all_processed[is_test,xvar]), sparse = TRUE)
xdata2_id <- data_all_processed$listing_id[is_test]

# set up models for cross validation samples
cv <- sample(1:5, length(is_train), replace=TRUE)
cv_data <- lapply(1:5, function(i){
  
  # set up test and train indexes
  is_train2 <- is_train[cv != i]
  is_test2 <- is_train[cv == i]
  
  # set up train and test sets
  xdata_i <- data_all_processed
  xdata_i$price[is_test2] <- NA
  
  xdata_i_train <- Matrix(as.matrix(xdata_i[is_train2,xvar]), sparse = TRUE)
  xdata_i_test <- Matrix(as.matrix(xdata_i[is_test2,xvar]), sparse = TRUE)
  
  # return
  return(list(train=is_train2, test=is_test2, xdata_train=xdata_i_train, xdata_test=xdata_i_test))
})

