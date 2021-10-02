#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(sf)

#Source functions
source("~/GitHub/BachelorThesis/Rstudio code/Functions.R", encoding = 'UTF-8')


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
  name = paste("Missing",index,".csv",sep="" )
  setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData/MissingTables")
  write_csv(tableMissingDatasTotal[[index-startyear+1]], name)
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
  mutate(Tag = case_when(PM10 == 1 & PM25 == 1 & Ammonia == 1 ~ "All",
                               PM10 == 1 & PM25 == 1 & Ammonia == 0 ~ "PM10-PM2.5",
                               PM10 == 1 & PM25 == 0 & Ammonia == 1 ~ "PM10-NH3",
                               PM10 == 0 & PM25 == 1 & Ammonia == 1 ~ "PM2.5-NH3",
                               PM10 == 1 & PM25 == 0 & Ammonia == 0 ~ "PM10",
                               PM10 == 0 & PM25 == 1 & Ammonia == 0 ~ "PM2.5",
                               PM10 == 0 & PM25 == 0 & Ammonia == 1 ~ "NH3"))

presencetable_red <- presencetable %>%
  select(IDStation,Tag)

RegistryRed <- full_join(RegistryRed,presencetable_red,by = c("IDStation"))

write.table(presencetable, "presencetable_red.csv")

#PART 3: plot of the Lombardy map
map_Lombardia_stations_custom(RegistryRed,col_points = Tag)

#win.graph()
map_Lombardia_stations_custom(RegistryRed)

regAQ <- get_ARPA_Lombardia_AQ_registry()
regAQ <- regAQ[,c(1,2,4,5,6,8:11)]
regW <- get_ARPA_Lombardia_W_registry()
regW <- regW[,c(1,2,4:10)]
map_Lombardia_stations(rbind(regAQ,regW))

map_Lombardia_stations(regAQ,col_points = 'red')
map_Lombardia_stations(regW)

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

#PART 5: Nearest Neighbor
#Calculate the distances of the 2 nearest stations 
regAQ <- get_ARPA_Lombardia_AQ_registry()

regAQ <- regAQ %>%
  filter(Pollutant %in% c("PM10"), is.na(DateStop)) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) %>%
  mutate(lng = Longitude, lat = Latitude) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs=4326)

regW <- get_ARPA_Lombardia_W_registry()

regW <- regW %>% 
  filter(Measure %in% c("Wind_speed","Wind_direction","Temperature","Rainfall"),
         is.na(DateStop),
         year(DateStart)<=2017) 


regW <- regW %>%
  filter(is.na(DateStop)) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) %>%
  mutate(lng = Longitude, lat = Latitude) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs=4326)

dist_mat <- sf::st_distance(regAQ,regW)

reg_X <- regAQ
reg_Y <- regW

k <- 2
distance <- registry_KNN_dist(reg_X,reg_Y,k)
#distance contains the 6 best stations
distance <- data.frame(distance[[1]])
distance <- distance[distance[,'IDStation'] %in% threeYesPlot[[1]],]

#Download the meteo-stations

equiv <- distance[,c('IDStation','reg_Y_nn1_ID')]

we <-  get_ARPA_Lombardia_W_data(
  ID_station = distance[,'reg_Y_nn1_ID'], 
  Year = c(2018:2020),
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

tableAllyears <- get_ARPA_Lombardia_AQ_data(
  ID_station = threeYesPlot[[1]],
  Year = c(startyear:lastyear),
  Frequency = "daily",
)

we <- data.frame(we)
tableAllyears <- data.frame(tableAllyears)

#Table with pollution stations and the first nearest weather station 
aqwe<- sqldf('select *
      from tableAllyears t join equiv e on t.IDStation = e.IDStation join we on e.reg_Y_nn1_ID = we.IDStation
               where t.Date = we.Date')


write_csv(aqwe,'NNdata.csv')

# single out the weather variables to study regarding their missing values

regW <- get_ARPA_Lombardia_W_registry()

regW <- regW %>% 
  filter(Measure %in% c("Wind_speed","Wind_direction","Temperature","Rainfall"),
         is.na(DateStop),
         year(DateStart)<=2017)

regW <- data.frame(regW)

miniregW <- sqldf('select *
      from regW a
      where exists (select b.IDStation 
                    from regW b
                    where a.IDStation = b.IDStation and b.Measure = "Wind_speed" )
     and exists (select b.IDStation 
                    from regW b
                    where  a.IDStation = b.IDStation and b.Measure = "Wind_direction" )
      and exists (select b.IDStation 
                    from regW b
                    where  a.IDStation = b.IDStation and b.Measure = "Temperature" )
      and exists (select b.IDStation 
                    from regW b
                    where  a.IDStation = b.IDStation and b.Measure = "Rainfall" )
      order by a.IDStation asc')


weStations <- sqldf('select distinct IDStation from miniregW ')

we1820 <-  get_ARPA_Lombardia_W_data(
  ID_station = weStations[1:20,1],
  #Year = c(2018:2020),
  Year = 2020,
  Frequency = "daily",
)

miniregW <- miniregW %>%
  filter(is.na(DateStop)) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) %>%
  mutate(lng = Longitude, lat = Latitude) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs=4326)

dist_mat <- sf::st_distance(regAQ,regW)

reg_X <- regAQ
reg_Y <- miniregW

k <- 2
distance <- registry_KNN_dist(reg_X,reg_Y,k)
distance <- data.frame(distance[[1]])
distance <- distance[distance[,'IDStation'] %in% threeYesPlot[[1]],]
equiv <- distance[,c('IDStation','reg_Y_nn1_ID')]

we <-  get_ARPA_Lombardia_W_data(
  ID_station = distance[,'reg_Y_nn1_ID'], 
  Year = c(2018:2020),
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

# missingWindSpeed <- MissingTable('Wind_speed','we1820')

FullStationsW <- NULL

WStations <- sqldf('select distinct IDStation
                   from we')
WStations <- as.matrix(WStations)

for (i in 1:length(WStations)) {
  
  FullStationsW[[i]] <- sqldf(paste("SELECT *
                             FROM we
                             WHERE IDStation = ", WStations[i],sep = ""))
  
}


BlueStripesW(FullStationsW,"2018-2020")

# part 6: Scatterplots

plot(table18_20[,c('Ammonia','PM10','PM25')], pch = 16,  col = alpha("red", 0.3))
plot(table18_20[,c(4:19)],  pch = 16,  col = alpha("salmon3", 0.45))
plot(aqwe19[,c('Ammonia','PM10','PM25','Temperature','Relative_humidity','Global_radiation','Rainfall')])

library(colorspace) 
df <- table18_20
df$color <- factor(df$NameStation,
                   labels=c("blue", "red","green","orange","coral","brown"))

plot(df[,c('Ammonia','PM10','PM25')], pch = 16,  col = alpha(as.character(df$color),0.45))