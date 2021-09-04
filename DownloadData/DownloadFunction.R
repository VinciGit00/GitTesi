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