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

map_Lombardia_stations(bestcentralines) # TODO ld-style crs object detected; please recreate object with a recent sf::st_crs()

#Downloading data from the best stations (the ones that measure at least 2 of the variables since 2017)
#2018
data2018 <- get_ARPA_Lombardia_AQ_data(
  ID_station = c(bestcentralines),
  Year = 2018,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

#2019
data2019 <- get_ARPA_Lombardia_AQ_data(
  ID_station = c(bestcentralines),
  Year = 2019,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

#2020
data2020 <- get_ARPA_Lombardia_AQ_data(
  ID_station = c(bestcentralines),
  Year = 2020,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

#Casting of the variables (allows for manipulation with SQL)
cast2018 <- data.frame(data2018)
cast2019 <- data.frame(data2019)
cast2020 <- data.frame(data2020)

#Renaming the variables (SQL misinterprets periods)
cast2018 = cast2018 %>%
  rename(
    PM25 = PM2.5
  )
cast2019 = cast2019 %>%
  rename(
    PM25 = PM2.5
  )
cast2020 = cast2020 %>%
  rename(
    PM25 = PM2.5
  )

# Helper functions 

MissingTable <- function(Variable, Table) {
  library(sqldf)
  
  tableMissing <- sqldf(paste('SELECT IDStation, NameStation, count(*) as Missing', Variable,
                              ' FROM ', Table,
                              ' WHERE ',Variable,' is null 
                        GROUP BY IDStation
                        ORDER BY Missing', Variable, sep = ''))  # stations where there is at least 1 missing value
  
  rest <- sqldf(paste(' SELECT IDStation, NameStation, 0 as Missing', Variable,
                      ' FROM ', Table,
                      ' EXCEPT
                        SELECT IDStation, NameStation, 0 as Missing', Variable,
                      ' FROM ', Table,
                      ' WHERE ', Variable,' is null 
                        GROUP BY IDStation', sep = '' )) # stations where there is at least no missing values
  
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
  
  rest <- sqldf(paste('         SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM ', Table,
                      ' EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM ', Table,
                      ' WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation', sep = ''))
  
  return (sqldf('SELECT *
                               FROM missingAll
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingAllThree
                              '))
}

#Starting with the queries
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
