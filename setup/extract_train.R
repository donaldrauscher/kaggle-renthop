library(jsonlite)
library(dplyr)
library(tidyr)
library(tidytext)
library(lubridate)
library(ggplot2)
library(maptools)
library(tm)
library(psych)
library(corpcor)

set.seed(1)

# pull in data
data <- fromJSON("./data/train.json")
data2 <- data.frame(
  "bathrooms"=unlist(data$bathrooms),
  "bedrooms"=unlist(data$bedrooms),
  "building_id"=unlist(data$building_id),
  "manager_id"=unlist(data$manager_id),
  "listing_id"=unlist(data$listing_id),
  "created"=ymd_hms(unlist(data$created)),
  "description"=unlist(data$description),
  "display_address"=unlist(data$display_address),
  "street_address"=unlist(data$street_address),
  "latitude"=unlist(data$latitude),
  "longitude"=unlist(data$longitude),
  "price"=unlist(data$price),
  "n_photo"=unlist(lapply(data$features, function(x) length(x))),
  "interest_level"=unlist(data$interest_level),
  stringsAsFactors=FALSE
)

# create a few base variables
data3 <- data2
data3$interest_level <- factor(data3$interest_level, levels = c("low", "medium", "high"), ordered = TRUE)
data3 <- data3 %>% mutate(bedrooms_minus_bathrooms = bedrooms - bathrooms, price_per_bedroom = price / ifelse(bedrooms == 0, 1, bedrooms))

# overall weights
n_low_total <- sum(data3$interest_level == "low")
n_med_total <- sum(data3$interest_level == "medium")
n_high_total <- sum(data3$interest_level == "high")
n_total <- n_low_total + n_med_total + n_high_total
perc_low <- n_low_total / n_total
perc_med <- n_med_total / n_total
perc_high <- n_high_total / n_total

# dimensions for features
top_features <- as.data.frame(table(unlist(data$features))) %>% 
  rename(feature=Var1, freq=Freq) %>% mutate(feature = toupper(feature)) %>%
  group_by(feature) %>% summarise(freq = sum(freq)) %>%
  filter(freq >= 30) %>% mutate(feature_id = row_number())

# combine like stuff
groupings = list(
  "AIR CONDITIONING" = c("AIR CONDITIONING", "CENTRAL A/C"),
  "BALCONY" = c("BALCONY", "PRIVATE BALCONY", "PRIVATE-BALCONY"),
  "CHILDRENS PLAYROOM" = c("CHILDREN'S PLAYROOM", "CHILDRENS PLAYROOM"),
  "COMMON OUTDOOR SPACE" = c("COMMON OUTDOOR SPACE", "BUILDING-COMMON-OUTDOOR-SPACE", "PUBLICOUTDOOR"),
  "OUTDOOR SPACE" = c("OUTDOOR AREAS", "OUTDOOR SPACE"),
  "DOORMAN" = c("DOORMAN", "FULL-TIME DOORMAN", "CONCIERGE"),
  "GARDEN" = c("GARDEN", "GARDEN/PATIO"),
  "GYM" = c("GYM", "GYM/FITNESS", "FITNESS CENTER"),
  "HARDWOOD FLOORS" = c("HARDWOOD", "HARDWOOD FLOORS"),
  "HIGH CEILINGS" = c("HIGH CEILING", "HIGH CEILINGS"),
  "LIVE-IN SUPER" = c("ON-SITE SUPER", "LIVE-IN SUPER", "LIVE IN SUPER", "LIVE-IN SUPERINTENDENT"),
  "PRE-WAR" = c("PRE-WAR", "PREWAR"),
  "ROOF DECK" = c("ROOF DECK", "ROOF-DECK", "COMMON ROOF DECK", "ROOFDECK"),
  "LAUNDRY IN BUILDING" = c("LAUNDRY IN BUILDING", "LAUNDRY ROOM", "ON-SITE LAUNDRY"),
  "LAUNDRY IN UNIT" = c("LAUNDRY IN UNIT", "WASHER/DRYER", "WASHER & DRYER", "WASHER IN UNIT", "DRYER IN UNIT"),
  "LOUNGE" = c("LOUNGE", "LOUNGE ROOM", "RESIDENTS LOUNGE"),
  "OUTDOOR SPACE" = c("OUTDOOR SPACE", "OUTDOOR AREAS"),
  "PARKING" = c("PARKING", "PARKING SPACE", "GARAGE", "COMMON PARKING/GARAGE", "ON-SITE GARAGE"),
  "POOL" = c("POOL", "SWIMMING POOL"),
  "VALET" = c("VALET", "VALET PARKING"),
  "WHEELCHAIR ACCESS" = c("WHEELCHAIR ACCESS", "WHEELCHAIR RAMP")
)
for (g in groupings){
  top_features$feature_id[top_features$feature %in% g] <- min(top_features$feature_id[top_features$feature %in% g])
}
top_features <- top_features %>% 
  arrange(feature_id, freq) %>% 
  mutate(feature_id = cumsum(ifelse(feature_id>lag(feature_id, 1, default=0),1,0))) %>%
  mutate(feature_id = paste0("f", sprintf("%03d", feature_id)))

# create binary flags for each feature
feature_map <- data.frame(
  "listing_id"=rep(data3$listing_id, times=unlist(lapply(data$features, function(x) length(x)))),
  "feature"=unlist(data$features),
  stringsAsFactors=FALSE
)

feature_map <- feature_map %>%
  mutate(feature = toupper(feature)) %>%
  inner_join(top_features, by=c("feature")) %>% 
  select(listing_id, feature_id) %>% distinct() %>%
  group_by(listing_id) %>% mutate(tag=1) %>% spread(feature_id, tag, fill=0)

data3 <- data3 %>% left_join(feature_map, by="listing_id")
data3[,names(feature_map)[-1]][is.na(data3[,names(feature_map)[-1]])] <- 0 

# align listings with neighborhoods from zillow
neighborhoods <- readShapePoly("ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp")
neighborhood_points <- fortify(neighborhoods) # fortify from ggplot2

neighborhood_data <- attr(neighborhoods,"data")
neighborhood_data$id <- rownames(neighborhood_data)

neighborhood_points <- neighborhood_points %>% 
  inner_join(neighborhood_data, by="id") %>%
  mutate(id=as.numeric(as.character(id)), group=as.numeric(as.character(group)), Name=as.character(Name))

neighborhood_names <- neighborhood_points %>% 
  select(id, Name) %>% distinct() %>% 
  rename(neighborhood_id=id, neighborhood=Name)

data3$neighborhood_id <- NA
for (g in unique(neighborhood_points$group)){
  temp <- filter(neighborhood_points, group == g)
  data3$neighborhood_id[point.in.polygon(data3$longitude, data3$latitude, temp$long, temp$lat)>0] <- floor(g)
}

data3 <- data3 %>% left_join(neighborhood_names, by=c("neighborhood_id"))

# create neighborhood weights (leave one out cv)
neighborhood_weights <- data3 %>%
  filter(!is.na(neighborhood_id)) %>% group_by(neighborhood_id) %>% 
  summarise(low = sum(interest_level == "low"), medium = sum(interest_level == "medium"), high = sum(interest_level == "high")) %>%
  mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
  mutate(total = low + medium + high, total_total = low_total + medium_total + high_total) %>%
  mutate(low_neighborhood_weight = low / total, medium_neighborhood_weight = medium / total, high_neighborhood_weight = high / total) %>%
  rename(n_neighborhood_weight = total) %>%
  select(neighborhood_id, low_neighborhood_weight, medium_neighborhood_weight, high_neighborhood_weight, n_neighborhood_weight)

data3 <- data3 %>%
  group_by(neighborhood_id) %>% 
  mutate(low = as.integer(interest_level == "low"), medium = as.integer(interest_level == "medium"), high = as.integer(interest_level == "high")) %>%
  mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
  ungroup() %>%
  mutate(low_holdout = low_total - low, medium_holdout = medium_total - medium, high_holdout = high_total - high, total_holdout = low_holdout + medium_holdout + high_holdout) %>%
  mutate(low_neighborhood_weight = low_holdout / total_holdout, medium_neighborhood_weight = medium_holdout / total_holdout, high_neighborhood_weight = high_holdout / total_holdout) %>%
  rename(n_neighborhood_weight = total_holdout) %>%
  select(-low, -medium, -high, -low_total, -medium_total, -high_total, -low_holdout, -medium_holdout, -high_holdout)

data3$low_neighborhood_weight[is.na(data3$neighborhood_id) | data3$n_neighborhood_weight == 0] <- NA
data3$medium_neighborhood_weight[is.na(data3$neighborhood_id) | data3$n_neighborhood_weight == 0] <- NA
data3$high_neighborhood_weight[is.na(data3$neighborhood_id) | data3$n_neighborhood_weight == 0] <- NA

# create intercepts for top neighborhoods
top_neighborhoods <- data3 %>% 
  filter(!is.na(neighborhood_id)) %>%
  group_by(neighborhood_id, neighborhood) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% filter(freq >= 30) %>% arrange(-freq)

for (n in top_neighborhoods$neighborhood_id){
  n2 <- paste0("n", sprintf("%03d", as.integer(n)))
  data3[[n2]] <- ifelse(is.na(data3$neighborhood_id), 0, ifelse(data3$neighborhood_id == n, 1, 0))
}

# create building weights (leave one out cv)
building_weights <- data3 %>%
  filter(building_id != 0) %>% group_by(building_id) %>% 
  summarise(low = sum(interest_level == "low"), medium = sum(interest_level == "medium"), high = sum(interest_level == "high")) %>%
  mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
  mutate(total = low + medium + high, total_total = low_total + medium_total + high_total) %>%
  mutate(low_building_weight = low / total, medium_building_weight = medium / total, high_building_weight = high / total) %>%
  rename(n_building_weight = total) %>%
  select(building_id, low_building_weight, medium_building_weight, high_building_weight, n_building_weight)

data3 <- data3 %>%
  group_by(building_id) %>% 
  mutate(low = as.integer(interest_level == "low"), medium = as.integer(interest_level == "medium"), high = as.integer(interest_level == "high")) %>%
  mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
  ungroup() %>%
  mutate(low_holdout = low_total - low, medium_holdout = medium_total - medium, high_holdout = high_total - high, total_holdout = low_holdout + medium_holdout + high_holdout) %>%
  mutate(low_building_weight = low_holdout / total_holdout, medium_building_weight = medium_holdout / total_holdout, high_building_weight = high_holdout / total_holdout) %>%
  rename(n_building_weight = total_holdout) %>%
  select(-low, -medium, -high, -low_total, -medium_total, -high_total, -low_holdout, -medium_holdout, -high_holdout)

data3$low_building_weight[data3$building_id != 0 | data3$n_building_weight == 0] <- NA
data3$medium_building_weight[data3$building_id != 0 | data3$n_building_weight == 0] <- NA
data3$high_building_weight[data3$building_id != 0 | data3$n_building_weight == 0] <- NA

# create intercepts for top buildings
top_buildings <- data3 %>%
  filter(building_id != 0) %>% arrange(desc(created)) %>%
  group_by(building_id) %>% summarise(n = n(), street_address = first(street_address)) %>%
  arrange(-n) %>% filter(n >= 30) %>%
  mutate(building_id2 = paste0("b", sprintf("%03d", row_number())))

for (i in 1:nrow(top_buildings)){
  b1 <- top_buildings$building_id[i]
  b2 <- top_buildings$building_id2[i]
  data3[[b2]] <- ifelse(data3$building_id == b1, 1, 0)
}

# create manager weights (leave one out cv)
manager_weights <- data3 %>%
  group_by(manager_id) %>% 
  summarise(low = sum(interest_level == "low"), medium = sum(interest_level == "medium"), high = sum(interest_level == "high")) %>%
  mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
  mutate(total = low + medium + high, total_total = low_total + medium_total + high_total) %>%
  mutate(low_manager_weight = low / total, medium_manager_weight = medium / total, high_manager_weight = high / total) %>%
  rename(n_manager_weight = total) %>%
  select(manager_id, low_manager_weight, medium_manager_weight, high_manager_weight, n_manager_weight)

data3 <- data3 %>%
  group_by(manager_id) %>% 
  mutate(low = as.integer(interest_level == "low"), medium = as.integer(interest_level == "medium"), high = as.integer(interest_level == "high")) %>%
  mutate(low_total = sum(low), medium_total = sum(medium), high_total = sum(high)) %>%
  ungroup() %>%
  mutate(low_holdout = low_total - low, medium_holdout = medium_total - medium, high_holdout = high_total - high, total_holdout = low_holdout + medium_holdout + high_holdout) %>%
  mutate(low_manager_weight = low_holdout / total_holdout, medium_manager_weight = medium_holdout / total_holdout, high_manager_weight = high_holdout / total_holdout) %>%
  rename(n_manager_weight = total_holdout) %>%
  select(-low, -medium, -high, -low_total, -medium_total, -high_total, -low_holdout, -medium_holdout, -high_holdout)

data3$low_manager_weight[data3$n_manager_weight == 0] <- NA
data3$medium_manager_weight[data3$n_manager_weight == 0] <- NA
data3$high_manager_weight[data3$n_manager_weight == 0] <- NA

# create intercepts for top managers
top_managers <- data3 %>%
  group_by(manager_id) %>% summarise(n = n()) %>% 
  arrange(-n) %>% filter(n >= 30) %>% 
  mutate(manager_id2 = paste0("m", sprintf("%03d", row_number())))

for (i in 1:nrow(top_managers)){
  m1 <- top_managers$manager_id[i]
  m2 <- top_managers$manager_id2[i]
  data3[[m2]] <- ifelse(data3$manager_id == m1, 1, 0)
}

# use odds rations to select top ngrams
data3 <- data3
data3$description <- toupper(data3$description)
data3$description <- sapply(data3$description, function(x) gsub("<[^>]*>", " ", x))
data3$description <- sapply(data3$description, function(x) gsub("[ ]{2,}", " ", x))
data3$description <- sapply(data3$description, function(x) gsub("[[:punct:]]", " ", x))

make_ngram <- function(df, n = 1){
  return(df %>% select(listing_id, interest_level, description) %>% 
    unnest_tokens(ngram, description, token = "ngrams", n = n) %>%
    mutate(ngram = toupper(ngram), ngram_size = n) %>%
    filter(!(ngram %in% toupper(stopwords(kind = "en"))) & grepl("[a-zA-Z]", ngram)) %>%
    group_by(ngram, ngram_size, listing_id, interest_level) %>% summarise(n = n()) %>% ungroup())
}

ngrams <- do.call(rbind, lapply(1:3, function(x) make_ngram(data3, x)))

top_ngrams <- ngrams %>% 
  group_by(ngram, ngram_size) %>% summarise(n = n(), n_low = sum(interest_level == "low"), n_med = sum(interest_level == "medium"), n_high = sum(interest_level == "high")) %>% ungroup() %>% 
  filter(n >= 100) %>%
  mutate(n_low_remain = n_low_total - n_low, n_med_remain = n_med_total - n_med, n_high_remain = n_high_total - n_high) %>%
  mutate(odds_ratio = (n_low / (n_med + n_high)) / (n_low_remain / (n_med_remain + n_high_remain))) %>%
  mutate(odds_ratio_norm = ifelse(odds_ratio < 1, 1 / odds_ratio, odds_ratio)) %>% filter(!is.infinite(odds_ratio_norm)) %>%
  mutate(odds_ratio_norm2 = odds_ratio_norm * n / n_total) %>% 
  arrange(-odds_ratio_norm2) %>% head(.,500) %>%
  mutate(ngram_id = paste0("k", sprintf("%03d", row_number())))

ngrams2 <- ngrams %>% inner_join(top_ngrams %>% select(ngram, ngram_id), by = c("ngram")) %>%
  select(ngram_id, listing_id) %>% mutate(flag = 1) %>%
  group_by(listing_id) %>% spread(ngram_id, flag, fill=0) %>% ungroup()

# perform factor analysis
factor_data <- as.matrix(ngrams2[,-1])
cor_mat <- cor.shrink(factor_data)
eigenvalues <- eigen(cor_mat)
num_factors <- length(which(eigenvalues$values >= 1))

ngram_principal_factors <- principal(
  r = matrix(cor_mat, ncol=500), 
  rotate = "varimax", nfactors = num_factors, covar = TRUE
)

ngrams3 <- as.data.frame(factor_data %*% as.matrix(ngram_principal_factors$loadings))
ngram_field_names <- paste0("k", sprintf("%03d", 1:num_factors))
names(ngrams3) <- ngram_field_names
ngrams3$listing_id <- ngrams2$listing_id

data3 <- data3 %>% left_join(ngrams3, by = c("listing_id"))
data3[,ngram_field_names][is.na(data3[,ngram_field_names])] <- 0

# qc that we made all our features correctly
apply(data3[,grep("[f,n,b,m,k][0-9]{3}", names(data3), value=TRUE)], 2, sum)
model_exclude_var <- c("description", "created", "listing_id", "building_id", "manager_id", "neighborhood_id", "neighborhood", "latitude", "longitude", "display_address", "street_address", "cv", "n_building_weight", "n_manager_weight")

# set up cross-validation
cv <- sample(1:5, nrow(data3), replace=TRUE)

# save
data_train_raw <- data2
data_train_processed <- data3
save(data_train_raw, data_train_processed, cv, model_exclude_var, 
  perc_low, perc_med, perc_high, top_features,
  neighborhood_weights, top_neighborhoods, 
  building_weights, top_buildings, 
  manager_weights, top_managers, 
  top_ngrams, ngram_principal_factors, 
  file = "./data/extract_train.Rdata")

