#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)

source("/Users/marcovinciguerra/Github/GitTesi/DownloadData/Functions.R", encoding = 'UTF-8')
source("/Users/marcovinciguerra/Github/GitTesi/DownloadData/DownloadFunction.R", encoding = 'UTF-8')

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
  pull() # Stations that measure at least 2 of the variables at the same time


startyear <- 2018
lastyear  <- 2020

#PART 1: looking for centralines with 2 or more pollutants
#Starting with the loop for downloading the data + casting of the data

cast <- Download(startyear, lastyear, bestcentralines)

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
  name = paste("Missing",index,".csv",sep="" )
  setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData/MissingTables")
  write_csv(tableMissingDatasTotal[[index-startyear+1]], name)
}

#PART 2: looking for centralines with 1 or more pollutants
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

Table10 <- tableMissingPM102[[1]]

Column10 <- sqldf('SELECT IDStation, NameStation, 1 as PM10
      FROM Table10 
      WHERE MissingPM10 < 365
      union
      SELECT IDStation, NameStation, 0 as PM10
      FROM Table10 
      WHERE MissingPM10 >= 365
      order by IDStation')

Table25 <- tableMissingPM252[[1]]

Column25 <- sqldf('SELECT IDStation, NameStation, 1 as PM25
      FROM Table25
      WHERE MissingPM25 < 365
      union
      SELECT IDStation, NameStation, 0 as PM25
      FROM Table25
      WHERE MissingPM25 >= 365
      order by IDStation')
#Legend
#1 means presence
#0 means absence
presencetable <- sqldf("SELECT c25.IDStation, C25.NameStation, c25.PM25, c10.PM10, ca.Ammonia, SUM(c25.PM25+c10.PM10+ca.Ammonia) as Somma
                 FROM Column25 c25 JOIN Column10 c10
                 ON c25.IDStation = c10.IDStation
                 JOIN ColumnA ca
                 ON c25.IDStation = ca.IDStation
                 GROUP BY c25.IDStation ") 

presencetable <- sqldf('SELECT IDStation, NameStation, PM25, PM10, Ammonia
                        FROM presencetable 
                        GROUP BY  Somma, IDStation')

#Plot of the centralines with at least 1 observation for one of the pollutant
presencetable <- presencetable %>%
  mutate(Etichetta = case_when(PM10 == 1 & PM25 == 1 & Ammonia == 1 ~ "Tutti",
                               PM10 == 1 & PM25 == 1 & Ammonia == 0 ~ "PM10-PM2.5",
                               PM10 == 1 & PM25 == 0 & Ammonia == 1 ~ "PM10-NH3",
                               PM10 == 0 & PM25 == 1 & Ammonia == 1 ~ "PM2.5-NH3",
                               PM10 == 1 & PM25 == 0 & Ammonia == 0 ~ "PM10",
                               PM10 == 0 & PM25 == 1 & Ammonia == 0 ~ "PM2.5",
                               PM10 == 0 & PM25 == 0 & Ammonia == 1 ~ "NH3"))

presencetable_red <- presencetable %>%
  select(IDStation,Etichetta)

RegistryRed <- full_join(RegistryRed,presencetable_red,by = c("IDStation"))

setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData")
write.table(presencetable, "presencetable_red.csv")

#PART 3: plot of the Lombardy map
map_Lombardia_stations_custom(RegistryRed,col_points = Etichetta)

#win.graph()
map_Lombardia_stations_custom(RegistryRed)


#PART 4: Plot of the time series

presencetableYear <- NULL

for (i in 1:length(tableMissingAmmmonia)) {
  
  TableA <- tableMissingAmmmonia[[i]]
  
  ColumnA <- sqldf('SELECT IDStation, NameStation, 1 as Ammonia
      FROM TableA 
      WHERE MissingAmmonia < 365
      union
      SELECT IDStation, NameStation, 0 as Ammonia
      FROM TableA 
      WHERE MissingAmmonia >= 365
      order by IDStation')
  
  Table10 <- tableMissingPM10[[i]]
  
  Column10 <- sqldf('SELECT IDStation, NameStation, 1 as PM10
      FROM Table10 
      WHERE MissingPM10 < 365
      union
      SELECT IDStation, NameStation, 0 as PM10
      FROM Table10 
      WHERE MissingPM10 >= 365
      order by IDStation')
  
  Table25 <- tableMissingPM25[[i]]
  
  Column25 <- sqldf('SELECT IDStation, NameStation, 1 as PM25
      FROM Table25
      WHERE MissingPM25 < 365
      union
      SELECT IDStation, NameStation, 0 as PM25
      FROM Table25
      WHERE MissingPM25 >= 365
      order by IDStation')
  #Legend
  #1 means presence
  #0 means absence
  presencetableYear[[i]] <- sqldf("SELECT c25.IDStation, C25.NameStation, c25.PM25, c10.PM10, ca.Ammonia
                 FROM Column25 c25 JOIN Column10 c10
                 ON c25.IDStation = c10.IDStation
                 JOIN ColumnA ca
                 ON c25.IDStation = ca.IDStation")
  #setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData")
  #write.csv(presencetableYear, paste("presencetableYear",i,".csv",sep = ""))
}


threeYesPlot <- NULL
#setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData/Dataplot")

for (i in 1:length(presencetableYear)) {
  
  pt <- presencetableYear[[i]]
  
  threeYesPlot[[i]] <- sqldf("SELECT IDStation 
                      FROM pt
                      WHERE Ammonia = 1 and PM10 = 1 and PM25=1")
  threeYesPlot[[i]] <- as.vector(t(threeYesPlot[[i]]))
  
}

FullStations <- NULL
n<-0
for (i in 1:length(threeYesPlot)) {
  
  table <- get_ARPA_Lombardia_AQ_data(
    ID_station = threeYesPlot[[i]],
    Year = startyear+i-1,
    Frequency = "daily",
    Var_vec = NULL,
    Fns_vec = NULL,
    by_sensor = 0,
    verbose = T
  ) %>%
    rename(
      PM25 = PM2.5
    )
  
  table1 <- data.frame(table)
  
  m <- 0
  for (j in (1+n):(length(threeYesPlot[[i]])+n)) {
    m = m+1
    FullStations[[j]] <- sqldf(paste("SELECT *
                             FROM table1
                             WHERE IDStation = ", threeYesPlot[[i]][m],sep = ""))
  }
  BlueStripes(FullStations[(1+n):(length(threeYesPlot[[i]])+n)],startyear+i-1)
  n <- j
}

lastYearStations <- threeYesPlot[[length(threeYesPlot)]]

table18_20 <- get_ARPA_Lombardia_AQ_data(
  ID_station = lastYearStations,
  Year = c(startyear:lastyear),
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
) %>%
  rename(
    PM25 = PM2.5
  )

FullStations <- NULL

for (i in 1:length(lastYearStations)) {
  
  FullStations[[i]] <- sqldf(paste("SELECT *
                             FROM table18_20
                             WHERE IDStation = ", lastYearStations[i],sep = ""))
  
}

BlueStripes(FullStations,"2018-2020")


