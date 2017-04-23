library(dplyr)

# weights will be creating
low_weights <- c("low_neighborhood_weight", "low_building_weight", "low_manager_weight")
med_weights <- c("medium_neighborhood_weight", "medium_building_weight", "medium_manager_weight")
high_weights <- c("high_neighborhood_weight", "high_building_weight", "high_manager_weight")
all_weights <- c(low_weights, med_weights, high_weights)

# create high-cardinality weights in train sets
add_high_card_weights_train <- function(df, leave_one_out = 1){

  df <- mutate(df, match = 1)
  
  # add neighborhood weights
  df <- df %>%
    group_by(neighborhood_id) %>% 
    mutate(low = as.integer(interest_level == "low"), medium = as.integer(interest_level == "medium"), high = as.integer(interest_level == "high")) %>%
    mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
    ungroup() %>%
    mutate(
      low_holdout = low_total - ifelse(leave_one_out == match, low, 0), 
      medium_holdout = medium_total - ifelse(leave_one_out == match, medium, 0), 
      high_holdout = high_total - ifelse(leave_one_out == match, high, 0), 
      total_holdout = low_holdout + medium_holdout + high_holdout
    ) %>%
    mutate(
      low_neighborhood_weight = low_holdout / total_holdout, 
      medium_neighborhood_weight = medium_holdout / total_holdout, 
      high_neighborhood_weight = high_holdout / total_holdout
    ) %>%
    rename(n_neighborhood_weight = total_holdout) %>%
    select(-low, -medium, -high, -low_total, -medium_total, -high_total, -low_holdout, -medium_holdout, -high_holdout)
  
  df$low_neighborhood_weight[is.na(df$neighborhood_id) | df$n_neighborhood_weight == 0] <- NA
  df$medium_neighborhood_weight[is.na(df$neighborhood_id) | df$n_neighborhood_weight == 0] <- NA
  df$high_neighborhood_weight[is.na(df$neighborhood_id) | df$n_neighborhood_weight == 0] <- NA
  
  # add building weights
  df <- df %>%
    group_by(building_id) %>% 
    mutate(low = as.integer(interest_level == "low"), medium = as.integer(interest_level == "medium"), high = as.integer(interest_level == "high")) %>%
    mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
    ungroup() %>%
    mutate(
      low_holdout = low_total - ifelse(leave_one_out == match, low, 0), 
      medium_holdout = medium_total - ifelse(leave_one_out == match, medium, 0), 
      high_holdout = high_total - ifelse(leave_one_out == match, high, 0), 
      total_holdout = low_holdout + medium_holdout + high_holdout
    ) %>%
    mutate(
      low_building_weight = low_holdout / total_holdout, 
      medium_building_weight = medium_holdout / total_holdout, 
      high_building_weight = high_holdout / total_holdout
    ) %>%
    rename(n_building_weight = total_holdout) %>%
    select(-low, -medium, -high, -low_total, -medium_total, -high_total, -low_holdout, -medium_holdout, -high_holdout)
  
  df$low_building_weight[df$building_id == 0 | df$n_building_weight == 0] <- NA
  df$medium_building_weight[df$building_id == 0 | df$n_building_weight == 0] <- NA
  df$high_building_weight[df$building_id == 0 | df$n_building_weight == 0] <- NA
  
  # add manager weights
  df <- df %>%
    group_by(manager_id) %>% 
    mutate(low = as.integer(interest_level == "low"), medium = as.integer(interest_level == "medium"), high = as.integer(interest_level == "high")) %>%
    mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
    ungroup() %>%
    mutate(
      low_holdout = low_total - ifelse(leave_one_out == match, low, 0), 
      medium_holdout = medium_total - ifelse(leave_one_out == match, medium, 0), 
      high_holdout = high_total - ifelse(leave_one_out == match, high, 0), 
      total_holdout = low_holdout + medium_holdout + high_holdout
    ) %>%
    mutate(
      low_manager_weight = low_holdout / total_holdout, 
      medium_manager_weight = medium_holdout / total_holdout, 
      high_manager_weight = high_holdout / total_holdout
    ) %>%
    rename(n_manager_weight = total_holdout) %>%
    select(-low, -medium, -high, -low_total, -medium_total, -high_total, -low_holdout, -medium_holdout, -high_holdout)
  
  df$low_manager_weight[df$n_manager_weight == 0] <- NA
  df$medium_manager_weight[df$n_manager_weight == 0] <- NA
  df$high_manager_weight[df$n_manager_weight == 0] <- NA  
  
  df <- df %>% select(-match)

  return(df)
  
}

# create high-cardinality weights in train sets
add_high_card_weights_test <- function(df_train, df_test){

  # create neighborhood weights
  neighborhood_weights <- df_train %>%
    filter(!is.na(neighborhood_id)) %>% group_by(neighborhood_id) %>% 
    summarise(low = sum(interest_level == "low"), medium = sum(interest_level == "medium"), high = sum(interest_level == "high")) %>%
    mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
    mutate(total = low + medium + high, total_total = low_total + medium_total + high_total) %>%
    mutate(low_neighborhood_weight = low / total, medium_neighborhood_weight = medium / total, high_neighborhood_weight = high / total) %>%
    rename(n_neighborhood_weight = total) %>%
    select(neighborhood_id, low_neighborhood_weight, medium_neighborhood_weight, high_neighborhood_weight, n_neighborhood_weight)

  # create building weights
  building_weights <- df_train %>%
    filter(building_id != 0) %>% group_by(building_id) %>% 
    summarise(low = sum(interest_level == "low"), medium = sum(interest_level == "medium"), high = sum(interest_level == "high")) %>%
    mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
    mutate(total = low + medium + high, total_total = low_total + medium_total + high_total) %>%
    mutate(low_building_weight = low / total, medium_building_weight = medium / total, high_building_weight = high / total) %>%
    rename(n_building_weight = total) %>%
    select(building_id, low_building_weight, medium_building_weight, high_building_weight, n_building_weight)

  # create manager weights
  manager_weights <- df_train %>%
    group_by(manager_id) %>% 
    summarise(low = sum(interest_level == "low"), medium = sum(interest_level == "medium"), high = sum(interest_level == "high")) %>%
    mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
    mutate(total = low + medium + high, total_total = low_total + medium_total + high_total) %>%
    mutate(low_manager_weight = low / total, medium_manager_weight = medium / total, high_manager_weight = high / total) %>%
    rename(n_manager_weight = total) %>%
    select(manager_id, low_manager_weight, medium_manager_weight, high_manager_weight, n_manager_weight)
 
  # merge into test set
  df_test <- df_test %>% left_join(neighborhood_weights, by = c("neighborhood_id"))
  df_test$n_neighborhood_weight[is.na(df_test$n_neighborhood_weight)] <- 0 

  df_test <- df_test %>% left_join(building_weights, by = c("building_id"))
  df_test$n_building_weight[is.na(df_test$n_building_weight)] <- 0

  df_test <- df_test %>% left_join(manager_weights, by = c("manager_id"))
  df_test$n_manager_weight[is.na(df_test$n_manager_weight)] <- 0
    
  return(df_test)
  
}

# smooth weights for small sample sizes
smooth_low_sample_weights <- function(df, low_sample_cutoff = 5, high_card_round = 2){

  df[,low_weights][is.na(df[,low_weights])] <- perc_low
  df[,med_weights][is.na(df[,med_weights])] <- perc_med
  df[,high_weights][is.na(df[,high_weights])] <- perc_high
  
  df <- df %>%
    mutate(w1 = ifelse(n_neighborhood_weight > low_sample_cutoff, 1, n_neighborhood_weight / low_sample_cutoff), w2 = 1 - w1) %>%
    mutate(low_neighborhood_weight = w1 * low_neighborhood_weight + w2 * perc_low) %>%
    mutate(medium_neighborhood_weight = w1 * medium_neighborhood_weight + w2 * perc_med) %>%
    mutate(high_neighborhood_weight = w1 * high_neighborhood_weight + w2 * perc_high) %>%
    select(-w1, -w2) %>%
    mutate(w1 = ifelse(n_building_weight > low_sample_cutoff, 1, n_building_weight / low_sample_cutoff), w2 = 1 - w1) %>%
    mutate(low_building_weight = w1 * low_building_weight + w2 * low_neighborhood_weight) %>%
    mutate(medium_building_weight = w1 * medium_building_weight + w2 * medium_neighborhood_weight) %>%
    mutate(high_building_weight = w1 * high_building_weight + w2 * high_neighborhood_weight) %>%
    select(-w1, -w2) %>%
    mutate(w1 = ifelse(n_manager_weight > low_sample_cutoff, 1, n_manager_weight / low_sample_cutoff), w2 = 1 - w1) %>%
    mutate(low_manager_weight = w1 * low_manager_weight + w2 * perc_low) %>%
    mutate(medium_manager_weight = w1 * medium_manager_weight + w2 * perc_med) %>%
    mutate(high_manager_weight = w1 * high_manager_weight + w2 * perc_high) %>%
    select(-w1, -w2)

  df[,all_weights] <- round(df[,all_weights] * 100 / high_card_round) * high_card_round
  
  return(df)
  
}

# add noise
add_noise <- function(df, fields = all_weights){
  df[,fields] <- df[,fields] * 
    matrix(rnorm(length(fields) * nrow(df), 1, 0.05), ncol = length(fields), nrow = nrow(df))
  return(df)
}

# make binary
make_binary <- function(df, fields, cutoff){
  df[,fields] <- as.integer(df[,fields] > cutoff)
  return(df)
}
