library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(maptools)

# pull in data
data <- fromJSON("train.json")
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
data2$interest_level <- factor(data2$interest_level, levels = c("low", "medium", "high"), ordered = TRUE)
data2 <- data2 %>% mutate(bedrooms_minus_bathrooms = bedrooms - bathrooms, price_per_bedroom = price / ifelse(bedrooms == 0, 1, bedrooms))

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
top_neighborhoods <- data4 %>% 
  filter(!is.na(neighborhood_id)) %>%
  group_by(neighborhood_id, neighborhood) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% filter(freq >= 30) %>% arrange(-freq)

data5 <- data4
for (n in top_neighborhoods$neighborhood_id){
  n2 <- paste0("n", sprintf("%03d", as.integer(n)))
  data5[[n2]] <- ifelse(is.na(data5$neighborhood_id), 0, ifelse(data5$neighborhood_id == n, 1, 0))
}

# create intercepts for top buildings
top_buildings <- data5 %>%
  filter(building_id != 0) %>% arrange(desc(created)) %>%
  group_by(building_id) %>% summarise(n = n(), street_address = first(street_address)) %>%
  arrange(-n) %>% filter(n >= 30) %>%
  mutate(building_id2 = paste0("b", sprintf("%03d", row_number())))

data6 <- data5
for (i in 1:nrow(top_buildings)){
  b1 <- top_buildings$building_id[i]
  b2 <- top_buildings$building_id2[i]
  data6[[b2]] <- ifelse(data6$building_id == b1, 1, 0)
}

# create intercepts for top managers
top_managers <- data6 %>%
  group_by(manager_id) %>% summarise(n = n()) %>% 
  arrange(-n) %>% filter(n >= 30) %>% 
  mutate(manager_id2 = paste0("m", sprintf("%03d", row_number())))

data7 <- data6
for (i in 1:nrow(top_managers)){
  m1 <- top_managers$manager_id[i]
  m2 <- top_managers$manager_id2[i]
  data7[[m2]] <- ifelse(data7$manager_id == m1, 1, 0)
}

# qc that we made all our features correctly
apply(data7[,grep("[f,n,b,m][0-9]{3}", names(data7), value=TRUE)], 2, sum)

# final model universe
data_model_train <- data7 %>% select(-description, -created, -listing_id, -building_id, -manager_id, -neighborhood_id, -neighborhood, -latitude, -longitude, -display_address, -street_address)

# save
data_initial_train <- data2
save(file="extract_train.Rdata", list = c("data_initial_train", "data_model_train", "top_features", "top_neighborhoods", "top_buildings", "top_managers"))

