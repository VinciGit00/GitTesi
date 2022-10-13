#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(sf)

#0. Source functions ----------------------------------------------------------
source("~/GitHub/GitTesi/DownloadData/Scripts/Functions.R", encoding = 'UTF-8')
#Stations we are interested in:
#2 681: Moggio (R)
#3 677: Cremona Via Fatebenefratelli (U) 

#Array with station IDs
arrayStations <-c(677, 703, 693, 642, 705, 681)


#Time-period of interest
startyear <- 2018
endyear   <- 2020

#Downloading
data<- get_ARPA_Lombardia_AQ_data(
  ID_station = arrayStations,
  Year = c(startyear:endyear),
  Frequency = "hourly",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)
 