#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(sf)

#Source functions
source("~/GitHub/GitTesi/DownloadData/Scripts/Functions.R", encoding = 'UTF-8')

#Filter datas with registry
registry <- get_ARPA_Lombardia_AQ_registry()

IDStat <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         is.na(DateStop),
         year(DateStart)<=2017) %>%
  distinct(IDSensor) %>% pull() %>% sort() # Stations that measure all three since 2017 and haven't been decomessioned

RegistryRed <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         IDSensor%in% IDStat) 

beststations <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n>=2) %>% 
  distinct(IDStation) %>%
  pull() # Stations that measure at least 2 of the variables at the same time

startyear <- 2018
lastyear  <- 2020

#PART 1: looking for stations with 2 or more pollutants using SQL queries
#Starting with the loop for downloading the data + casting of the data

cast <- Download(startyear, lastyear, beststations)

#Queries for counting the amount of missing datas
tableMissingAmmmonia<-NULL
tableMissingPM10<-NULL
tableMissingPM25<-NULL
tableMissingallDatas <-NULL
tableMissingDatasTotal<-NULL

for(index in startyear:lastyear) {
  interestedTable <- cast[[index-startyear+1]]
  
  tableMissingAmmmonia[[index-startyear+1]] <- MissingTable('Ammonia', 'interestedTable')
  
  tableMissingPM10[[index-startyear+1]] <- MissingTable('PM10', 'interestedTable')
  
  tableMissingPM25[[index-startyear+1]] <- MissingTable('PM25', 'interestedTable')
  
  tableMissingallDatas[[index-startyear+1]] <- MissingAll('interestedTable')
  
  tableMissingAmmmoniatemp <- tableMissingAmmmonia[[index-startyear+1]]
  
  tableMissingPM10temp     <- tableMissingPM10[[index-startyear+1]]
  
  tableMissingPM25temp     <- tableMissingPM25[[index-startyear+1]]
  
  tableMissingallDatastemp <- tableMissingallDatas[[index-startyear+1]]
  
  tableMissingDatasTotal[[index-startyear+1]] <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingAllThree
                                  FROM tableMissingAmmmoniatemp ma  JOIN tableMissingPM10temp m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN tableMissingPM25temp m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN tableMissingallDatastemp mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')
}

#PART 2: looking for stations with 1 or more pollutants
CentralineMorethan1 <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n>=1) %>% 
  distinct(IDStation) %>%
  pull() # Stations that measure at least 2 of the variables at the same time

cast2 <- Download(startyear, lastyear, CentralineMorethan1)

#Queries for counting the amount of missing datas
tableMissingAmmmonia2<-NULL
tableMissingPM102<-NULL
tableMissingPM252<-NULL
tableMissingallDatas2 <-NULL
tableMissingDatasTotal2<-NULL

for(index in startyear:lastyear) {
  interestedTable2 <- cast2[[index-startyear+1]]
  
  tableMissingAmmmonia2[[index-startyear+1]] <- MissingTable('Ammonia', 'interestedTable2')
  
  tableMissingPM102[[index-startyear+1]] <- MissingTable('PM10', 'interestedTable2')
  
  tableMissingPM252[[index-startyear+1]] <- MissingTable('PM25', 'interestedTable2')
  
  tableMissingallDatas2[[index-startyear+1]] <- MissingAll('interestedTable2')
  
  tableMissingAmmmoniatemp2 <- tableMissingAmmmonia2[[index-startyear+1]]
  
  tableMissingPM10temp2     <- tableMissingPM102[[index-startyear+1]]
  
  tableMissingPM25temp2     <- tableMissingPM252[[index-startyear+1]]
  
  tableMissingallDatastemp2 <- tableMissingallDatas2[[index-startyear+1]]
  
  tableMissingDatasTotal2[[index-startyear+1]] <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingAllThree
                                  FROM tableMissingAmmmoniatemp2 ma  JOIN tableMissingPM10temp2 m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN tableMissingPM25temp2 m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN tableMissingallDatastemp2 mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')
}


#COUNT OF THE TOTAL OF THE MISSING DATAS for every station
totalMissingFromBeginning <- NULL
temp <- NULL
for(index in 1:(length(tableMissingDatasTotal2))) {
  if(index==1) {
    totalMissingFromBeginning <- tableMissingDatasTotal2[[index]]
  } else {
    temp <- tableMissingDatasTotal2[[index]]
    auxiliaryTable <- totalMissingFromBeginning 
    auxiliaryTable
    totalMissingFromBeginning <- sqldf('SELECT t.IDStation,t.NameStation, 
                                      SUM(t.MissingAmmonia+a.MissingAmmonia)  
                                      as MissingAmmonia,
                                      SUM(t.MissingPM10+a.MissingPM10)
                                      as MissingPM10 ,
                                      SUM(t.MissingPM25+a.MissingPM25)
                                      as MissingPM25,
                                      SUM(t.MissingAllThree+a.MissingAllThree)
                                      as t.MissingAllThree
                                      FROM temp t 
                                      JOIN auxiliaryTable a
                                      ON t.IDstation = a.IDStation
                                      GROUP BY t.IDStation
                                       ')
  } 
}

#YES/NO TABLE
#Creating the table of yes/no
#queries yes/no table 

TableA <- tableMissingAmmmonia2[[1]]

ColumnA <- sqldf('SELECT IDStation, NameStation, 1 as Ammonia
      FROM TableA 
      WHERE MissingAmmonia < 365
      union
      SELECT IDStation, NameStation, 0 as Ammonia
      FROM TableA 
      WHERE MissingAmmonia >= 365
      order by IDStation')