##### k_min_vals_vec_idx
# Identifies the indices of the first (minimum) k values in a vector x
k_min_vals_vec_idx <- function(x,k) {
  sort.int(x, index.return = T)$ix[1:k]
}

##### k_min_vals_vec
# Identifies the values of the first (minimum) k values in a vector x
k_min_vals_vec <- function(x,k) {
  idx <- sort.int(x, index.return = T)$ix[1:k]
  x[idx]
}

#####registry_KNN_dist
# For each element included in reg_X, it dentifies the k-nearest neighbours (among those included in reg_Y)
# according to an Euclidean distance metric
registry_KNN_dist <- function(reg_X,reg_Y,k){
  
  ### Computing distance
  dist_matrix <- sf::st_distance(reg_X,reg_Y)
  ### Identifying the k-NN IDStation of reg_Y
  min_dist_idx <- apply(dist_matrix,MARGIN = 1, FUN = k_min_vals_vec_idx, k=k)
  min_dist <- apply(dist_matrix,MARGIN = 1, FUN = k_min_vals_vec, k=k)
  if (k == 1) {
    min_dist_idx <- t(as.matrix(min_dist_idx)) 
  }
  ### Extracting for each k the corresponding ID and NameStation of reg_Y
  knn_list <- list(length = k)
  for (j in 1:k) {
    knn_prog <- data.frame(cbind(reg_Y[min_dist_idx[j,],]$NameStation,
                                 reg_Y[min_dist_idx[j,],]$IDStation,
                                 round(min_dist[j,],2)))
    colnames(knn_prog) <- c(paste0("reg_Y_nn",j,"_name"),
                            paste0("reg_Y_nn",j,"_ID"),
                            paste0("reg_Y_nn",j,"_dist"))
    knn_list[[j]] <- knn_prog 
  }
  ### Combining dataframes
  output_tab <- dplyr::bind_cols(reg_X_name,knn_list)
  
  return(list(output_tab))
}

#Start of the script

library(ARPALData)
library(tidyverse)
library(sf)



registry <- get_ARPA_Lombardia_AQ_registry()

IDStat <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         is.na(DateStop),
         year(DateStart)<=2017) %>%
  distinct(IDSensor) %>% pull() %>% sort() # Stations that measure all three since 2017 and haven't been decomessioned

RegistryRed <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         IDSensor%in% IDStat) 

bestcentralines <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n>=2) %>% 
  distinct(IDStation) %>%
  pull()

regAQ <- bestcentralines
regAQ <- regAQ %>%
  filter(Pollutant %in% c("PM10"), is.na(DateStop)) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) %>%
  mutate(lng = Longitude, lat = Latitude) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs=4326)
regW <- get_ARPA_Lombardia_W_registry()
regW <- regW %>%
  filter(is.na(DateStop)) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) %>%
  mutate(lng = Longitude, lat = Latitude) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs=4326)

dist_mat <- sf::st_distance(regAQ,regW)

reg_X <- regAQ
reg_Y <- regW

k <- 2
registry_KNN_dist(reg_X,reg_Y,k)

