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

# pull in universe params and set defaults if missing
if(is.null(univ_param$include_kw_var)) univ_param$include_kw_var <- 1
if(is.null(univ_param$include_hcc)) univ_param$include_hcc <- 1
if(is.null(univ_param$add_hcc_noise)) univ_param$add_hcc_noise <- 1
if(is.null(univ_param$hcc_loo)) univ_param$hcc_loo <- 1

# combine datasets
data_all_processed <- bind_rows(data_train_processed, data_test_processed)
if(length(dep_outputs)>0){
  all_pred <- bind_rows(dep_validate_predictions, dep_test_predictions)
  if(!is.null(step_def$dependency_normalizer)){
    dependency_normalizer <- step_def$dependency_normalizer
    for (i in names(all_pred)){
      all_pred[[i]] <- all_pred[[i]] / data_all_processed[[dependency_normalizer]]
    }
  }
  data_all_processed <- cbind(data_all_processed, all_pred)
}
is_train <- which(!is.na(data_all_processed$interest_level))
is_test <- inv_which(is_train, nrow(data_all_processed))

# some base variables
# price per square foot from https://www.kaggle.com/arnaldcat/a-proxy-for-sqft-and-the-interest-on-1-2-baths
data_all_processed <- data_all_processed %>% 
  mutate(
    logprice = log(price),
    price_per_sqft = price / (1 + ifelse(bedrooms <= 1, 1, ifelse(bedrooms >= 4, 4, bedrooms)) + 0.5 * ifelse(bathrooms <= 0, 0, ifelse(bathrooms >= 2, 2, bathrooms))),
    has_half_bath = as.integer(round(bathrooms) != bathrooms),
    bedrooms_minus_bathrooms = bedrooms - bathrooms, 
    price_per_bedroom = price / ifelse(bedrooms == 0, 1, bedrooms),
    total_rooms = bedrooms + bathrooms,
    price_per_room = price / ifelse(total_rooms == 0, 1, total_rooms)
  )

# days of week / hour of day variables
weekdays <- weekdays(data_all_processed$created)
data_all_processed$is_monday <- as.integer(weekdays == "Monday")
data_all_processed$is_tuesday <- as.integer(weekdays == "Tuesday")
data_all_processed$is_wednesday <- as.integer(weekdays == "Wednesday")
data_all_processed$is_thursday <- as.integer(weekdays == "Thursday")
data_all_processed$is_friday <- as.integer(weekdays == "Friday")
data_all_processed$is_saturday <- as.integer(weekdays == "Saturday")
data_all_processed$is_sunday <- as.integer(weekdays == "Sunday")

hours <- hour(data_all_processed$created)
hours2 <- ifelse(hours <= 5, "Early Morning", ifelse(hours <= 11, "Late Morning", ifelse(hours <= 16, "Afternoon", "Evening")))
data_all_processed$hour <- hours
data_all_processed$is_early_morning <- as.integer(hours2 == "Early Morning")
data_all_processed$is_late_morning <- as.integer(hours2 == "Late Morning")
data_all_processed$is_afternoon <- as.integer(hours2 == "Afternoon")
data_all_processed$is_evening <- as.integer(hours2 == "Evening")

# building / manager / neighborhood sizes
data_all_processed <- data_all_processed %>% 
  group_by(building_id) %>% mutate(building_n = n()) %>% ungroup() %>%
  group_by(neighborhood_id) %>% mutate(neighborhood_n = n()) %>% ungroup() %>%
  group_by(manager_id) %>% mutate(manager_n = n()) %>% ungroup()

# building / manager / neighborhood averages
data_all_processed <- data_all_processed %>% 
  group_by(building_id) %>% 
  mutate(
    building_mean_price = mean(price),
    building_mean_bedroom = mean(bedrooms),
    building_mean_price_per_sqft = mean(price_per_sqft),
    building_mean_price_per_bedroom = mean(price_per_bedroom),
    building_mean_price_per_room = mean(price_per_room),
    building_mean_price_rel = ifelse(building_mean_price == 0, 1, price / building_mean_price),
    building_mean_bedroom_rel = ifelse(building_mean_bedroom == 0, 1, bedrooms / building_mean_bedroom),
    building_mean_price_per_sqft_rel = ifelse(building_mean_price_per_sqft == 0, 1, price_per_sqft / building_mean_price_per_sqft),
    building_mean_price_per_bedroom_rel = ifelse(building_mean_price_per_bedroom == 0, 1, price_per_bedroom / building_mean_price_per_bedroom),
    building_mean_price_per_room_rel = ifelse(building_mean_price_per_room == 0, 1, price_per_room / building_mean_price_per_room)
  ) %>% ungroup() %>%
  group_by(neighborhood_id, bedrooms) %>%
  mutate(
    neighborhood_mean_price = mean(price),
    neighborhood_mean_bedroom = mean(bedrooms),
    neighborhood_mean_price_per_sqft = mean(price_per_sqft),
    neighborhood_mean_price_per_bedroom = mean(price_per_bedroom),
    neighborhood_mean_price_per_room = mean(price_per_room),
    neighborhood_mean_price_rel = ifelse(neighborhood_mean_price == 0, 1, price / neighborhood_mean_price),
    neighborhood_mean_bedroom_rel = ifelse(neighborhood_mean_bedroom == 0, 1, bedrooms / neighborhood_mean_bedroom),
    neighborhood_mean_price_per_sqft_rel = ifelse(neighborhood_mean_price_per_sqft == 0, 1, price_per_sqft / neighborhood_mean_price_per_sqft),
    neighborhood_mean_price_per_bedroom_rel = ifelse(neighborhood_mean_price_per_bedroom == 0, 1, price_per_bedroom / neighborhood_mean_price_per_bedroom),
    neighborhood_mean_price_per_room_rel = ifelse(neighborhood_mean_price_per_room == 0, 1, price_per_room / neighborhood_mean_price_per_room)
  ) %>% ungroup() %>%
  group_by(manager_id) %>%
  mutate(
    manager_mean_price = mean(price),
    manager_mean_bedroom = mean(bedrooms),
    manager_mean_price_per_sqft = mean(price_per_sqft),
    manager_mean_price_per_bedroom = mean(price_per_bedroom),
    manager_mean_price_per_room = mean(price_per_room),
    manager_mean_price_rel = ifelse(manager_mean_price == 0, 1, price / manager_mean_price),
    manager_mean_bedroom_rel = ifelse(manager_mean_bedroom == 0, 1, bedrooms / manager_mean_bedroom),
    manager_mean_price_per_sqft_rel = ifelse(manager_mean_price_per_sqft == 0, 1, price_per_sqft / manager_mean_price_per_sqft),
    manager_mean_price_per_bedroom_rel = ifelse(manager_mean_price_per_bedroom == 0, 1, price_per_bedroom / manager_mean_price_per_bedroom),
    manager_mean_price_per_room_rel = ifelse(manager_mean_price_per_room == 0, 1, price_per_room / manager_mean_price_per_room)
  ) %>% ungroup()

# add high-cardinality categorical
add_high_card_cat <- function(df){
  df <- df %>% group_by(building_id) %>% mutate(seg = cut(n(), breaks = c(0, 10, 30, Inf))) %>% ungroup()
  df$building_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), building_id, seg, univ_param$hcc_loo)
  df$building_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), building_id, seg, univ_param$hcc_loo)
  df <- df %>% group_by(manager_id) %>% mutate(seg = cut(n(), breaks = c(0, 10, 30, Inf))) %>% ungroup()
  df$manager_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), manager_id, seg, univ_param$hcc_loo)
  df$manager_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), manager_id, seg, univ_param$hcc_loo)
  df <- df %>% mutate(seg = 1)
  df$neighborhood_interest_h <- probabilize_high_card_cat(df, ifelse(interest_level == "high", 1, 0), neighborhood_id, seg, univ_param$hcc_loo)
  df$neighborhood_interest_m <- probabilize_high_card_cat(df, ifelse(interest_level == "medium", 1, 0), neighborhood_id, seg, univ_param$hcc_loo)
  df <- df %>% select(-seg)
  return(df)
}

data_all_processed2 <- add_high_card_cat(data_all_processed)

# add noise to train
high_card_cat <- c("building_interest_h", "building_interest_m", "manager_interest_h", "manager_interest_m", "neighborhood_interest_h", "neighborhood_interest_m") 
if (univ_param$add_hcc_noise){
  data_all_processed2 <- add_noise(data_all_processed2, is_train, high_card_cat)
}

# make some variables binary
kw_var <- grep("[k][0-9]{3}", names(data_all_processed2), value=TRUE)
data_all_processed2 <- make_binary(data_all_processed2, kw_var, 0)

# assemble x and y
holdouts <- setdiff(model_exclude_var, c("latitude", "longitude"))
building_var <- grep("[b][0-9]{3}", names(data_all_processed2), value=TRUE)
manager_var <- grep("[m][0-9]{3}", names(data_all_processed2), value=TRUE)
neighborhood_var <- grep("[n][0-9]{3}", names(data_all_processed2), value=TRUE)

if (!univ_param$include_hcc){
  holdouts <- c(holdouts, high_card_cat)
}

if (!univ_param$include_kw_var){
  holdouts <- c(holdouts, kw_var)
}

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
  if (univ_param$add_hcc_noise){
    xdata_i <- add_noise(xdata_i, is_train2, high_card_cat)
  }
  xdata_i <- make_binary(xdata_i, kw_var, 0)

  xdata_i_train <- Matrix(as.matrix(xdata_i[is_train2,xvar]), sparse = TRUE)
  xdata_i_test <- Matrix(as.matrix(xdata_i[is_test2,xvar]), sparse = TRUE)
  
  # return
  return(list(train=is_train2, test=is_test2, xdata_train=xdata_i_train, xdata_test=xdata_i_test))
})

