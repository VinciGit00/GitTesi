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
#1 703: Schivenoglia (R)
#2 681: Moggio (R)
#3 677: Cremona Via Fatebenefratelli (U) 

#Array with station IDs
arrayStations <-c(703, 681, 677)


#Time-period of interest
startyear <- 2014
endyear   <- 2020

#1. AQ dataset download--------------------------------------------------------
total <- NULL

for(i in 1:length(arrayStations)){
  
  data <- Easydownload(startyear, endyear, arrayStations[i])
  
  total[[i]] <- data
}
ast2 <- Download(startyear, endyear, arrayStations)

#Queries for counting the amount of missing datas
tableMissingAmmmonia2<-NULL
tableMissingPM102<-NULL
tableMissingPM252<-NULL
tableMissingallDatas2 <-NULL
tableMissingDatasTotal2<-NULL

for(index in startyear:endyear) {
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
    totalMissingFromBeginning <- sqldf('SELECT t.IDStation,t.NameStation, 
                                      SUM(t.MissingAmmonia+a.MissingAmmonia)  
                                      as MissingAmmonia,
                                      SUM(t.MissingPM10+a.MissingPM10)
                                      as MissingPM10 ,
                                      SUM(t.MissingPM25+a.MissingPM25)
                                      as MissingPM25,
                                      SUM(t.MissingAllThree+a.MissingAllThree)
                                      as MissingAllThree
                                      FROM temp t 
                                      JOIN auxiliaryTable a
                                      ON t.IDstation = a.IDStation
                                      GROUP BY t.IDStation
                                       ')
  } 
}

cast2 <- Download(startyear, endyear, arrayStations)

#Queries for counting the amount of missing datas
tableMissingAmmmonia2<-NULL
tableMissingPM102<-NULL
tableMissingPM252<-NULL
tableMissingallDatas2 <-NULL
tableMissingDatasTotal2<-NULL

for(index in startyear:endyear) {
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
    totalMissingFromBeginning <- sqldf('SELECT t.IDStation,t.NameStation, 
                                      SUM(t.MissingAmmonia+a.MissingAmmonia)  
                                      as MissingAmmonia,
                                      SUM(t.MissingPM10+a.MissingPM10)
                                      as MissingPM10 ,
                                      SUM(t.MissingPM25+a.MissingPM25)
                                      as MissingPM25,
                                      SUM(t.MissingAllThree+a.MissingAllThree)
                                      as MissingAllThree
                                      FROM temp t 
                                      JOIN auxiliaryTable a
                                      ON t.IDstation = a.IDStation
                                      GROUP BY t.IDStation
                                       ')
  } 
}
