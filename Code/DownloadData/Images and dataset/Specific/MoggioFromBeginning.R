library(ARPALData)
library(sqldf)

startyear <- 2017
endyear   <- 2020

pollution<- get_ARPA_Lombardia_AQ_data(
  ID_station = 681,
  Year = c(startyear:endyear),
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

w <-  get_ARPA_Lombardia_W_data(
  ID_station = c(111), 
  Year = c(startyear:endyear),
  Frequency = "daily")


aqw<- sqldf('SELECT *
      FROM pollution t JOIN equiv e ON t.IDStation = e.IDStation JOIN w ON e.reg_Y_nn1_ID = w.IDStation
               WHERE t.Date = w.Date')