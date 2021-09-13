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
  output_tab <- dplyr::bind_cols(reg_X,knn_list)
  
  return(list(output_tab))
}



library(ARPALData)
library(tidyverse)
library(sf)

regAQ <- get_ARPA_Lombardia_AQ_registry()
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
distance <- registry_KNN_dist(reg_X,reg_Y,k)
distance <- data.frame(distance[[1]])
distance <- distance[distance[,'IDStation'] %in% threeYesPlot[[1]],]


we <- get_ARPA_Lombardia_W_data(
  #ID_station = distance[,'reg_Y_nn1_ID'], 
  ID_station = 642,
  Year = 2019,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)


table19 <- cast[[2]]
equiv <- distance[,c('IDStation','reg_Y_nn1_ID')]

aqwe19<- sqldf('select *
      from table19 t join equiv e on t.IDStation = e.IDStation join we on e.reg_Y_nn1_ID = we.IDStation
               where t.Date = we.Date')
write_csv(aqwe19,'aqwePavia19.csv')
