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
#AGGIUNGERE UN CICLO FOR CHE SCARICA I DATI E FA LA QUERY TUTTO DA SOLO
#DAVID LO FACCIO IO PERO'
#SE LO FAI TI MANGIO, NON STO SCHERZANDO
#Utilizzo la concatenazione
#2018
table2018MissingAmmmonia <- MissingTable('Ammonia', 'cast2018')

table2018MissingPM10 <- MissingTable('PM10', 'cast2018')

table2018MissingPM25 <- MissingTable('PM25', 'cast2018')

table2018MissingallDatas <- MissingAll('cast2018')

#All the missing datas for every station
tableMissingDatasTotal2018 <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingAllThree
                                  FROM table2018MissingAmmmonia ma  JOIN table2018MissingPM10 m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN table2018MissingPM25 m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN table2018MissingallDatas mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')

#2019
table2019MissingAmmmonia <- MissingTable('Ammonia', 'cast2019')

table2019MissingPM10 <- MissingTable('PM10', 'cast2019')

table2019MissingPM25 <- MissingTable('PM25', 'cast2019')

table2019MissingallDatas <- MissingAll('cast2019')

#All the missing datas for every station
tableMissingDatasTotal2019 <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingAllThree
                                  FROM table2019MissingAmmmonia ma  JOIN table2019MissingPM10 m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN table2019MissingPM25 m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN table2019MissingallDatas mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')

#2020
table2020MissingAmmmonia <- MissingTable('Ammonia', 'cast2020')

table2020MissingPM10 <-  MissingTable('PM10', 'cast2020')

table2020MissingPM25 <- MissingTable('PM25', 'cast2020')

table2020MissingallDatas <- MissingAll('cast2020')

#All the missing datas for every station
tableMissingDatasTotal2020 <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingAllThree
                                  FROM table2020MissingAmmmonia ma  JOIN table2020MissingPM10 m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN table2020MissingPM25 m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN table2020MissingallDatas mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')
#For loop for downloading data and queries
for(val in 2018:2020) {
  
}


#Total of missing data from 2018 to 2020
totalethreeyears = sqldf('SELECT T18.IDStation, sum(T18.MissingAmmonia+T19.MissingAmmonia+T20.MissingAmmonia), sum(T18.MissingPM10+T19.MissingPM10+T20.MissingPM10), 
                         sum(T18.MissingPM25+T19.MissingPM25+T20.MissingPM25)
                         FROM tableMissingDatasTotal2018 AS T18 JOIN tableMissingDatasTotal2019 AS T19
                         ON  T18.IDStation = T19.IDStation
                         JOIN tableMissingDatasTotal2020 AS T20
                         ON  T18.IDStation = T20.IDStation
                         GRUOUP BY T19.IDStation')


####IN progress
#Creating the table of yes/no
threepresents <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n=3) %>% 
  distinct(IDStation) %>%
  pull()

twopresents <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n=2) %>% 
  distinct(IDStation) %>%
  pull()

yesword = "yes"
noword  = "no" 

matriceyes <- NULL

for(val in 1:length(threpresents)) {
  for(otherindex in 1:3) {
    matriceyes[val, otherindex] = yesword
  }
}

print(matriceyes)
