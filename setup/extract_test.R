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
  "n_photo"=unlist(lapply(data$photos, function(x) length(x))),
  "n_features"=unlist(lapply(data$features, function(x) length(x))),
  stringsAsFactors=FALSE
)

# create a few base variables
data3 <- data2
data3$price[data3$price > 30000] <- 30000
data3 <- data3 %>% 
  mutate(
    logprice = log(price),
    bedrooms_minus_bathrooms = bedrooms - bathrooms, 
    price_per_bedroom = price / ifelse(bedrooms == 0, 1, bedrooms),
    total_rooms = bedrooms + bathrooms,
    price_per_room = price / ifelse(total_rooms == 0, 1, total_rooms)
  )

# days of week / hour of day variables
weekdays <- weekdays(data3$created)
data$is_monday <- as.integer(weekdays == "Monday")
data$is_tuesday <- as.integer(weekdays == "Tuesday")
data$is_wednesday <- as.integer(weekdays == "Wednesday")
data$is_thursday <- as.integer(weekdays == "Thursday")
data$is_friday <- as.integer(weekdays == "Friday")
data$is_saturday <- as.integer(weekdays == "Saturday")
data$is_sunday <- as.integer(weekdays == "Sunday")

hours <- hour(data3$created)
hours2 <- ifelse(hours <= 5, "Early Morning", ifelse(hours <= 11, "Late Morning", ifelse(hours <= 16, "Afternoon", "Evening")))
data$is_early_morning <- as.integer(hours2 == "Early Morning")
data$is_late_morning <- as.integer(hours2 == "Late Morning")
data$is_afternoon <- as.integer(hours2 == "Afternoon")
data$is_evening <- as.integer(hours2 == "Evening")

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
neighborhoods <- readShapePoly("./resources/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp")
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
for (n in top_neighborhoods$neighborhood_id){
  n2 <- paste0("n", sprintf("%03d", as.integer(n)))
  data3[[n2]] <- ifelse(is.na(data3$neighborhood_id), 0, ifelse(data3$neighborhood_id == n, 1, 0))
}

# create building weights
for (i in 1:nrow(top_buildings)){
  b1 <- top_buildings$building_id[i]
  b2 <- top_buildings$building_id2[i]
  data3[[b2]] <- ifelse(data3$building_id == b1, 1, 0)
}

# create manager weights
for (i in 1:nrow(top_managers)){
  m1 <- top_managers$manager_id[i]
  m2 <- top_managers$manager_id2[i]
  data3[[m2]] <- ifelse(data3$manager_id == m1, 1, 0)
}

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

# add average price per room for each neighborhood
neighborhood_price <- rbind(
  data_train_processed %>% select(neighborhood_id, bedrooms, price),
  data3 %>% select(neighborhood_id, bedrooms, price)
)

neighborhood_price <- neighborhood_price %>% 
  group_by(neighborhood_id, bedrooms) %>%
  summarise(neighborhood_price = median(price))

data3 <- data3 %>% 
  left_join(neighborhood_price, by = c("neighborhood_id", "bedrooms")) %>%
  mutate(price_vs_neighborhood = price / neighborhood_price) %>% select(-neighborhood_price)

# add average price per room for each building
building_price <- rbind(
  data_train_processed %>% select(building_id, bedrooms, price),
  data3 %>% select(building_id, bedrooms, price)
)

building_price <- building_price %>% 
  group_by(building_id, bedrooms) %>%
  summarise(building_price = median(price))

data3 <- data3 %>% 
  left_join(building_price, by = c("building_id", "bedrooms")) %>%
  mutate(price_vs_building = price / building_price) %>% select(-building_price)

# set up final dataframes
data_test_raw <- data2
data_test_processed <- data3

# qc that test and train have same variables
setdiff(names(data_train_processed), names(data_test_processed))
setdiff(names(data_test_processed), names(data_train_processed))

# save
save(data_test_raw, data_test_processed, file = "./data/extract_test.Rdata")

