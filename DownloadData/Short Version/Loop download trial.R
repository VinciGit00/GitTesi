#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)

#Filter datas
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
  pull() # stations that measure at least 2 of the variables at the same time

map_Lombardia_stations(bestcentralines)

#Starting with the loop for downloading the data + casting of the data

startyear = 2018
lastyear  = 2020

data <- NULL
cast <- NULL

for(index in startyear:lastyear) {
    #Downloading
    data[[1+index-startyear]] <- get_ARPA_Lombardia_AQ_data(
    ID_station = c(bestcentralines),
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

#Starting with the queries
for(index in startyear:lastyear) {
  castindex <- paste('cast',index, sep = '')
  #Ammonia
  missingAmmonia[[1+index-startyear]]  = MissingTable('Ammonia', 'castindex')
  #PM10
  MissingPM10[[1+index-startyear]]     = MissingTable('PM10', 'castindex')
  #PM2.5
  MissingPM25[[1+index-startyear]]     = MissingTable('PM25', 'castindex')
  #All datas
  MissingallDatas[[1+index-startyear]] = MissingAll('castindex')
  
  #Saving the datas on a .csv file
  setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/Short Version/MissingTables')
  filename <- paste('Missing',index, sep = '')
  write_csv(MissingallDatas[[index]], filename)
  write_csv(result2020or, "MissingData2020.csv")
  
}


