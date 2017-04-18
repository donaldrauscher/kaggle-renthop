library(jsonlite)
library(dplyr)
library(tidyr)
library(tidytext)
library(lubridate)
library(ggplot2)
library(maptools)
library(tm)

# load the train data
load("./data/extract_train.Rdata")

# pull in data
data <- fromJSON("./data/test.json")
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
  stringsAsFactors=FALSE
)

# create a few base variables
data3 <- data2
data3 <- data3 %>% mutate(bedrooms_minus_bathrooms = bedrooms - bathrooms, price_per_bedroom = price / ifelse(bedrooms == 0, 1, bedrooms))

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

# create neighborhood weights
data3 <- data3 %>% left_join(neighborhood_weights, by = c("neighborhood_id"))
for (n in top_neighborhoods$neighborhood_id){
  n2 <- paste0("n", sprintf("%03d", as.integer(n)))
  data3[[n2]] <- ifelse(is.na(data3$neighborhood_id), 0, ifelse(data3$neighborhood_id == n, 1, 0))
}
data3$n_neighborhood_weight[is.na(data3$n_neighborhood_weight)] <- 0

# create building weights
data3 <- data3 %>% left_join(building_weights, by = c("building_id"))
for (i in 1:nrow(top_buildings)){
  b1 <- top_buildings$building_id[i]
  b2 <- top_buildings$building_id2[i]
  data3[[b2]] <- ifelse(data3$building_id == b1, 1, 0)
}
data3$n_building_weight[is.na(data3$n_building_weight)] <- 0

# create manager weights
data3 <- data3 %>% left_join(manager_weights, by = c("manager_id"))
for (i in 1:nrow(top_managers)){
  m1 <- top_managers$manager_id[i]
  m2 <- top_managers$manager_id2[i]
  data3[[m2]] <- ifelse(data3$manager_id == m1, 1, 0)
}
data3$n_manager_weight[is.na(data3$n_manager_weight)] <- 0

# flags for top keywords
data3 <- data3
data3$description <- toupper(data3$description)
data3$description <- sapply(data3$description, function(x) gsub("<[^>]*>", " ", x))
data3$description <- sapply(data3$description, function(x) gsub("[ ]{2,}", " ", x))
data3$description <- sapply(data3$description, function(x) gsub("[[:punct:]]", " ", x))

keywords <- list()
for (i in 1:nrow(top_ngrams)){
  id <- top_ngrams$ngram_id[i]
  keyword <- top_ngrams$ngram[i]
  keywords[[id]] <- as.integer(grepl(keyword, data3$description))
}
keywords <- as.matrix(do.call(cbind, keywords))

ngrams <- as.data.frame(keywords %*% as.matrix(ngram_principal_factors$loadings))
names(ngrams) <- paste0("k", sprintf("%03d", 1:ncol(ngram_principal_factors$loadings)))

data3 <- cbind(data3, ngrams)

# set up final dataframes
data_test_raw <- data2
data_test_processed <- data3

# qc that test and train have same variables
table(sort(setdiff(names(data_train_processed), "interest_level")) == sort(names(data_test_processed)))

# save
save(data_test_raw, data_test_processed, file = "./data/extract_test.Rdata")

