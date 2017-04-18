library(glmnet)
library(dplyr)

set.seed(1)

# load data
load("./data/extract_train.Rdata")
load("./data/extract_test.Rdata")
source("extra.R")

# parameters
alpha <- 1
nlambda <- 100
small_sample_cutoff <- 5

# create modeling datasets
low_weights <- c("low_neighborhood_weight", "low_building_weight", "low_manager_weight")
med_weights <- c("medium_neighborhood_weight", "medium_building_weight", "medium_manager_weight")
high_weights <- c("high_neighborhood_weight", "high_building_weight", "high_manager_weight")

data_train_processed[,low_weights][is.na(data_train_processed[,low_weights])] <- perc_low
data_train_processed[,med_weights][is.na(data_train_processed[,med_weights])] <- perc_med
data_train_processed[,high_weights][is.na(data_train_processed[,high_weights])] <- perc_high

data_train_processed <- data_train_processed %>%
  mutate(w1 = ifelse(n_neighborhood_weight > small_sample_cutoff, 1, n_neighborhood_weight / small_sample_cutoff), w2 = 1 - w1) %>%
  mutate(low_neighborhood_weight = w1 * low_neighborhood_weight + w2 * perc_low) %>%
  mutate(medium_neighborhood_weight = w1 * medium_neighborhood_weight + w2 * perc_med) %>%
  mutate(high_neighborhood_weight = w1 * high_neighborhood_weight + w2 * perc_high) %>%
  select(-w1, -w2) %>%
  mutate(w1 = ifelse(n_building_weight > small_sample_cutoff, 1, n_building_weight / small_sample_cutoff), w2 = 1 - w1) %>%
  mutate(low_building_weight = w1 * low_building_weight + w2 * perc_low) %>%
  mutate(medium_building_weight = w1 * medium_building_weight + w2 * perc_med) %>%
  mutate(high_building_weight = w1 * high_building_weight + w2 * perc_high) %>%
  select(-w1, -w2) %>%
  mutate(w1 = ifelse(n_manager_weight > small_sample_cutoff, 1, n_manager_weight / small_sample_cutoff), w2 = 1 - w1) %>%
  mutate(low_manager_weight = w1 * low_manager_weight + w2 * perc_low) %>%
  mutate(medium_manager_weight = w1 * medium_manager_weight + w2 * perc_med) %>%
  mutate(high_manager_weight = w1 * high_manager_weight + w2 * perc_high) %>%
  select(-w1, -w2)

data_test_processed[,low_weights][is.na(data_test_processed[,low_weights])] <- perc_low
data_test_processed[,med_weights][is.na(data_test_processed[,med_weights])] <- perc_med
data_test_processed[,high_weights][is.na(data_test_processed[,high_weights])] <- perc_high

data_test_processed <- data_test_processed %>%
  mutate(w1 = ifelse(n_neighborhood_weight > small_sample_cutoff, 1, n_neighborhood_weight / small_sample_cutoff), w2 = 1 - w1) %>%
  mutate(low_neighborhood_weight = w1 * low_neighborhood_weight + w2 * perc_low) %>%
  mutate(medium_neighborhood_weight = w1 * medium_neighborhood_weight + w2 * perc_med) %>%
  mutate(high_neighborhood_weight = w1 * high_neighborhood_weight + w2 * perc_high) %>%
  select(-w1, -w2) %>%
  mutate(w1 = ifelse(n_building_weight > small_sample_cutoff, 1, n_building_weight / small_sample_cutoff), w2 = 1 - w1) %>%
  mutate(low_building_weight = w1 * low_building_weight + w2 * perc_low) %>%
  mutate(medium_building_weight = w1 * medium_building_weight + w2 * perc_med) %>%
  mutate(high_building_weight = w1 * high_building_weight + w2 * perc_high) %>%
  select(-w1, -w2) %>%
  mutate(w1 = ifelse(n_manager_weight > small_sample_cutoff, 1, n_manager_weight / small_sample_cutoff), w2 = 1 - w1) %>%
  mutate(low_manager_weight = w1 * low_manager_weight + w2 * perc_low) %>%
  mutate(medium_manager_weight = w1 * medium_manager_weight + w2 * perc_med) %>%
  mutate(high_manager_weight = w1 * high_manager_weight + w2 * perc_high) %>%
  select(-w1, -w2)

holdouts <- c(model_exclude_var, grep("[k][0-9]{3}", names(data_train_processed), value=TRUE))
ydata <- as.numeric(data_train_processed$interest_level)-1
xdata <- Matrix(as.matrix(data_train_processed[,setdiff(names(data_train_processed), c("interest_level", holdouts))]), sparse = TRUE)
xdata2 <- Matrix(as.matrix(data_test_processed[,setdiff(names(data_test_processed), holdouts)]), sparse = TRUE)

# base glm model on all data
glm_base <- glmnet(x = xdata, y = ydata, family = "multinomial", alpha = alpha, nlambda = nlambda)
lambda_path <- glm_base$lambda

# build overall model
validate_predictions <- matrix(ncol = 3, nrow = length(ydata))
optimal_lambda <- c()
for (i in 1:5){
  cat("Starting CV fold ", i, " of 5...\n", sep = "")
  
  # set up test and train
  train <- which(cv != i)
  test <- which(cv == i)
  
  # run model
  glm <- glmnet(x = xdata[train,], y = ydata[train], family = "multinomial", alpha = alpha, lambda = lambda_path)
  
  # generate predictions
  lambda_mlogloss <- sapply(glm$lambda, function(l){
    predictions <- matrix(predict(glm, xdata[test,],  s = l, type = "response"), ncol = 3, byrow = FALSE)
    return(multiloss(predictions, ydata[test]))
  })
  
  # determine optimal lambda
  optimal_lambda[i] <- glm$lambda[which.min(lambda_mlogloss)]
  
  # generate predictions
  validate_predictions[test,] <- matrix(predict(glm, xdata[test,],  s = optimal_lambda[i], type = "response"), ncol = 3, byrow = FALSE)
}

# calculate multilogloss
validate_multiloss <- multiloss(validate_predictions, ydata)
validate_multiloss

# generate predictions on test set
optimal_lambda_final <- min(lambda_path[lambda_path >= mean(optimal_lambda)])
test_predictions <- as.data.frame(matrix(predict(glm_base, xdata2,  s = optimal_lambda_final, type = "response"), ncol = 3, byrow = FALSE))
names(test_predictions) <- c("low", "medium", "high")
test_predictions$listing_id <- data_test_processed$listing_id

# save
save(glm_base, optimal_lambda_final, validate_multiloss, validate_predictions, test_predictions, file = "./models/glmnet.Rdata")

