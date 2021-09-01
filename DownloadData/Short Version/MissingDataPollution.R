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
#Supporting functions 
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
  name = paste("Missing",index,".csv",sep="" )
  setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData/Short Version/MissingTables")
  write_csv(tableMissingDatasTotal[[index-startyear+1]], name)
}

####IN progress
#Creating the table of yes/no
#Three yes
threepresents <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n==3) %>% 
  distinct(IDStation) %>%
  pull()

yesword = "yes"
noword  = "no" 

matriceyes <- matrix(0, nrow =length(threepresents) , ncol = 3)

for(val in 1:length(threepresents)) {
  for(otherindex in 1:3) {
    matriceyes[val, otherindex] = yesword
  }
}

#Two yes

twopresents <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n==2) %>% 
  distinct(IDStation) %>%
  pull()

#Casting + temporary variables
Searchpollution <- data.frame(twopresents)
cast18 <- cast[[1]]

#Starting with queries
columnPM10 <- sqldf("SELECT s.IDStattion, s.NameStation, yes
                     FROM Searchpollution s JOIN cast18 c
                     ON  s.twopresents = c.IDStation
                     WHERE c.PM10 is not null")

# queries yes/no table 

TableA <- tableMissingAmmmonia[[1]]

ColumnA <- sqldf('select IDStation, NameStation, 1 as Ammonia
      from TableA 
      where MissingAmmonia < 365
      union
      select IDStation, NameStation, 0 as Ammonia
      from TableA 
      where MissingAmmonia >= 365
      order by IDStation')

Table10 <- tableMissingPM10[[1]]

Column10 <- sqldf('select IDStation, NameStation, 1 as PM10
      from Table10 
      where MissingAmmonia < 365
      union
      select IDStation, NameStation, 0 as PM10
      from Table10 
      where MissingAmmonia >= 365
      order by IDStation')

TableA <- tableMissingAmmmonia[[1]]

Column25 <- sqldf('select IDStation, NameStation, 1 as Ammonia
      from TableA 
      where MissingAmmonia < 365
      union
      select IDStation, NameStation, 0 as Ammonia
      from TableA 
      where MissingAmmonia >= 365
      order by IDStation')



