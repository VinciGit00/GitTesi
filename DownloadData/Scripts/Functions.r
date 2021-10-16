MissingTable <- function(Variable, Table) {
  library(sqldf)
  
  tableMissing <- sqldf(paste('SELECT IDStation, NameStation, count(*) as Missing', Variable,
                              ' FROM ', Table,
                              ' WHERE ',Variable,' is null 
                                GROUP BY IDStation
                                ORDER BY Missing', Variable, sep = ''))  # stations where there minimun 1 missing value
  
  rest <- sqldf(paste(' SELECT IDStation, NameStation, 0 as Missing', Variable,
                      ' FROM ', Table,
                      ' EXCEPT
                        SELECT IDStation, NameStation, 0 as Missing', Variable,
                      ' FROM ', Table,
                      ' WHERE ', Variable,' is null 
                        GROUP BY IDStation', sep = '' )) # stations where there are no missing values
  
  return ( sqldf(paste('SELECT *
                               FROM tableMissing
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY Missing', Variable , sep = ''
  )) ) # all stations with regards to this variable
  
}


MissingAll <- function(Table) {
  library(sqldf)
  
  missingAll <- sqldf(paste(
    'SELECT IDStation, NameStation, count(*) as MissingAllThree
                        FROM ', Table,
    ' WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation
                        ORDER BY MissingAllThree', sep = ''
  ))
  
  rest <- sqldf(paste('         SELECT IDStation, NameStation, 0 as MissingAllThree
                        FROM ', Table,'
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingAllThree
                        FROM ', Table,' 
                        WHERE Ammonia is null AND PM10 is null AND PM25 is null
                        GROUP BY IDStation', sep = ''))
  
  return (                     sqldf('SELECT *
                               FROM missingAll
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingAllThree
                              '))
}

BlueStripes <- function(vector,year){
  
  for (i in 1:length(vector)) {
    c9a <- ggplot(vector[[i]], aes(x = Date, y = Ammonia)) +
      geom_line()  + labs(title = paste(vector[[i]][1,3],year))
    nada <- is.na(vector[[i]]["Ammonia"])
    c9a <- c9a + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.5)
    
    c910 <- ggplot(vector[[i]], aes(x = Date, y = PM10)) +
      geom_line() 
    nada <- is.na(vector[[i]]["PM10"])
    c910 <- c910 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "blue", size=1.5)
    
    
    c925 <- ggplot(vector[[i]], aes(x = Date, y = PM25)) +
      geom_line() 
    nada <- is.na(vector[[i]]["PM25"])
    c925 <- c925 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "blue", size=1.5)
    
    jpeg(filename =paste(vector[[i]][1,3],paste(year,".jpeg")),width = 1280, height = 720 )
    multiplot(c9a, c910, c925)
    dev.off()
    
  }
  
}

OrangeStripes <- function(vector,year){
  
  for (i in 1:length(vector)) {
    c9a <- ggplot(vector[[i]], aes(x = Date, y = Wind_speed)) +
    geom_line()  + labs(title = paste(vector[[i]][1,3],year))
    nada <- is.na(vector[[i]]["Wind_speed"])
    c9a <- c9a + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                            color = "orange", size=1.5)
    
    c910 <- ggplot(vector[[i]], aes(x = Date, y = Wind_direction)) +
      geom_line() 
    nada <- is.na(vector[[i]]["Wind_direction"])
    c910 <- c910 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "orange", size=1.5)
    
    
    c925 <- ggplot(vector[[i]], aes(x = Date, y = Temperature)) +
      geom_line() 
    nada <- is.na(vector[[i]]["Temperature"])
    c925 <- c925 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "orange", size=1.5)
    
    c92 <- ggplot(vector[[i]], aes(x = Date, y = Rainfall)) +
      geom_line() 
    nada <- is.na(vector[[i]]["Rainfall"])
    c92 <- c92 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "orange", size=1.5)
    
    jpeg(filename =paste(vector[[i]][1,3],paste(year,".jpeg")),width = 1280, height = 720 )
    multiplot(c9a, c910, c925, c92)
    dev.off()
    
  }
  
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list FROM the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated FROM # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


map_Lombardia_stations_custom <- function (data, title = "Map of ARPA stations in Lombardy", 
          prov_line_type = 1, prov_line_size = 1, col_points = "blue", 
          xlab = "Longitude", ylab = "Latitude") 
{
  Lombardia <- get_Lombardia_geospatial(NUTS_level = "NUTS3")
  if (is_ARPALdf_AQ(Data = data) == T) {
    Stats <- get_ARPA_Lombardia_AQ_registry()
  } else if (is_ARPALdf_W(Data = data) == T) {
    Stats <- get_ARPA_Lombardia_W_registry()
  }
  IDstats <- data %>%
    distinct(IDStation) %>%
    pull()
  d <- data %>%
    filter(IDStation %in% IDstats)
  # data <- data.frame(IDStation = unique(data$IDStation))
  # d <- dplyr::left_join(data, Stats, by = "IDStation")
  d <- d %>% sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  geo_plot <- Lombardia %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_sf(linetype = prov_line_type, size = prov_line_size) +
    ggplot2::geom_sf(data = d, aes(col = Tag), size=3) + 
    ggplot2::labs(title = title)
  print(geo_plot)
}

#Functions for downloading pollutants datas
Download <- function(startyear, lastyear, centraline) {
  #Libraries
  library(ARPALData)
  library(tidyverse)
  library(sqldf)
  library(ggplot2)
  library(ggfortify)
  library(lubridate)
  
  data <- NULL
  cast <- NULL
  
  for(index in startyear:lastyear) {
    #Downloading
    data[[1+index-startyear]] <- get_ARPA_Lombardia_AQ_data(
      ID_station = c(centraline),
      Year = index,
      Frequency = "daily",
      Var_vec = NULL,
      Fns_vec = NULL,
      by_sensor = 0,
      verbose = T
    )
    #Casting
    cast[[1+index-startyear]] <- data.frame(data[[1+index-startyear]] )
    
    #Renaming
    cast[[1+index-startyear]] <- cast[[1+index-startyear]]  %>%
      rename(
        PM25 = PM2.5
      )
  }
  
  return(cast)
}

Easydownload<- function(startyear, endyear, Station) {
  
  #Downloading
  data<- get_ARPA_Lombardia_AQ_data(
    ID_station = Station,
    Year = c(startyear:endyear),
    Frequency = "hourly",
    Var_vec = NULL,
    Fns_vec = NULL,
    by_sensor = 0,
    verbose = T
  )
  
  #Casting
  cast <- data.frame(data)
  
  #Renaming
  cast <- cast  %>%
    rename(
      PM25 = PM2.5
    )

  return(cast)
}

#Nearest neighbor functions

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
