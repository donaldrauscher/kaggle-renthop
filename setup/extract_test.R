library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(maptools)

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
data2 <- data2 %>% mutate(bedrooms_minus_bathrooms = bedrooms - bathrooms, price_per_bedroom = price / ifelse(bedrooms == 0, 1, bedrooms))

# create binary flags for each feature
feature_map <- data.frame(
  "listing_id"=rep(data2$listing_id, times=unlist(lapply(data$features, function(x) length(x)))),
  "feature"=unlist(data$features),
  stringsAsFactors=FALSE
)

feature_map <- feature_map %>%
  mutate(feature = toupper(feature)) %>%
  inner_join(top_features, by=c("feature")) %>% 
  select(listing_id, feature_id) %>% distinct() %>%
  group_by(listing_id) %>% mutate(tag=1) %>% spread(feature_id, tag, fill=0)

data3 <- data2 %>% left_join(feature_map, by="listing_id")
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

data4 <- data3 %>% left_join(neighborhood_names, by=c("neighborhood_id"))

# create intercepts for top neighborhoods
data5 <- data4
for (n in top_neighborhoods$neighborhood_id){
  n2 <- paste0("n", sprintf("%03d", as.integer(n)))
  data5[[n2]] <- ifelse(is.na(data5$neighborhood_id), 0, ifelse(data5$neighborhood_id == n, 1, 0))
}

# create intercepts for top buildings
data6 <- data5
for (i in 1:nrow(top_buildings)){
  b1 <- top_buildings$building_id[i]
  b2 <- top_buildings$building_id2[i]
  data6[[b2]] <- ifelse(data6$building_id == b1, 1, 0)
}

# create intercepts for top managers
data7 <- data6
for (i in 1:nrow(top_managers)){
  m1 <- top_managers$manager_id[i]
  m2 <- top_managers$manager_id2[i]
  data7[[m2]] <- ifelse(data7$manager_id == m1, 1, 0)
}

# final model universe
data_model_test <- data7 %>% select(-description, -created, -listing_id, -building_id, -manager_id, -neighborhood_id, -neighborhood, -latitude, -longitude, -display_address, -street_address)

# qc that test and train have same variables
table(sort(setdiff(names(data_model_train), "interest_level")) == sort(names(data_model_test)))

# save
data_initial_test <- data2
save(file="./data/extract_test.Rdata", list = c("data_initial_test", "data_model_test"))

