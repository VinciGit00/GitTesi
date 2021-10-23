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
startyear <- 2018
endyear   <- 2020

#1. AQ dataset download--------------------------------------------------------
total <- NULL

for(i in 1:length(arrayStations)){
  
  data <- Easydownload(startyear, endyear, arrayStations[i])
  
  total[[i]] <- data
}

#2. W dataset download of stations close to the AQ stations

#2.1 Dowload of W datasets with no contraints on avialable info----------------

regAQ <- get_ARPA_Lombardia_AQ_registry()

regAQl <- regAQ %>% 
  filter(IDStation %in% arrayStations) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) %>%
  mutate(lng = Longitude, lat = Latitude) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs=4326)


regW <- get_ARPA_Lombardia_W_registry()

regW <- regW %>% # Narrowing down amount of weather stations to be downloaded
  filter(Measure %in% c("Wind_speed","Wind_direction","Temperature","Rainfall"),
         is.na(DateStop),
         year(DateStart)<=2017) 

regWl <- regW %>% # Keeping only active weather stations, without duplicates
  filter(is.na(DateStop)) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) %>%
  mutate(lng = Longitude, lat = Latitude) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs=4326)

dist_mat <- sf::st_distance(regAQl,regWl) #Calculating distances

k <- 4

distance <- registry_KNN_dist(regAQl,regWl,k)[[1]] #Table containing potential weather stations for a given AQ station
write_csv(distance,'distance.csv')

#2.2 Dowload of W datasets with contraints on avialable info-------------------

regWConstrained <- data.frame(regW)

regWConstrained <- sqldf('select * 
      from regWConstrained a
      where exists (select b.IDStation 
                    from regWConstrained b
                    where a.IDStation = b.IDStation and b.Measure = "Wind_speed" )
     and exists (select b.IDStation 
                    from regWConstrained b
                    where  a.IDStation = b.IDStation and b.Measure = "Wind_direction" )
      and exists (select b.IDStation 
                    from regWConstrained b
                    where  a.IDStation = b.IDStation and b.Measure = "Temperature" )
      and exists (select b.IDStation 
                    from regWConstrained b
                    where  a.IDStation = b.IDStation and b.Measure = "Rainfall" )
      order by a.IDStation asc') # Singling out W stations that have "Wind_speed","Wind_direction","Temperature","Rainfall" among their variables

regWConstrained1 <- regWConstrained %>% filter(is.na(DateStop)) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) %>%
  mutate(lng = Longitude, lat = Latitude) %>%
  sf::st_as_sf(coords = c("lng", "lat"), crs=4326)

dist_matConstrained <- sf::st_distance(regAQl,regWConstrained1) #Calculating distances

distanceConstrained  <- registry_KNN_dist(regAQl,regWConstrained1,k)[[1]] #Table containing potential weather stations for a given AQ station
write_csv(distanceConstrained,'distanceConstrained.csv')



#3 Generation of maps

#3.1 Map of AQ stations--------------------------------------------------------

regAQ <- regAQ %>% 
  filter(IDStation %in% arrayStations) %>%
  distinct(IDStation,NameStation,Longitude,Latitude) 

regAQtags <- regAQ %>% rename(Tag = NameStation)

jpeg(filename ='AQStationsOfInterest.jpeg',width = 619, height = 471 )
map_Lombardia_stations_custom(regAQtags)
dev.off()

#3.2 Map of W stations---------------------------------------------------------

WeatherTags <- function(distance) {
  dis <- data.frame(distance)
  dis['geometry'] <- NULL
  
  regWtags  <- NULL
  for (i in 1:k) {
    
    a <- dis[,paste('reg_Y_nn',i,'_ID',sep = '')]
    a <- data.frame(a)
    temp <- sqldf('select * from regW where IDStation in a')
    temp$Tag <- rep(0,nrow(temp))
    regWtags <- rbind(regWtags,temp)
    
    for (j in 1:nrow(regWtags)) {
      temp <- dis[,paste('reg_Y_nn',i,'_ID',sep = '')]==regWtags[j,'IDStation']
      temp <- dis[temp,'NameStation']
      if (!(is_empty(temp))) {
        regWtags[j,'Tag'] <- temp
      }
    }
    
  } # adding tags corresponding to the associated AQ station 
  return(regWtags)
}
regWtags <- WeatherTags(distance)
regWtagsConstrained <- WeatherTags(distanceConstrained)

jpeg(filename ='WStationsOfInterest.jpeg',width = 619, height = 471 )
map_Lombardia_stations_custom(regWtags)
dev.off()

jpeg(filename ='WStationsOfInterestConstrained.jpeg',width = 619, height = 471 )
map_Lombardia_stations_custom(regWtagsConstrained)
dev.off()




#4.1 Missing Values for AQ Stations--------------------------------------------

BlueStripes(total,paste(startyear,endyear,sep = '-'))

#4.2 Missing Values for W Stations---------------------------------------------

for (j in 1:k) {
  
  totalW <- NULL
  for (i in 1:nrow(distance[,paste('reg_Y_nn',j,'_ID',sep = '')])) {
    
    w <-  get_ARPA_Lombardia_W_data(
      ID_station = distance[i,paste('reg_Y_nn',j,'_ID',sep = '')], 
      Year = c(startyear:endyear),
      Frequency = "daily")
    
    totalW[[i]] <- data.frame(w)
    
  }
  
  
  OrangeStripes(totalW, paste('k=',j,startyear,endyear,sep = ' '))
  
}

#4.3 Missing Values for constrained W Stations---------------------------------

distanceConstrained$geometry <- NULL

for (j in 1:k) {
  
  totalW <- NULL
  for (i in 1:nrow(distanceConstrained[,paste('reg_Y_nn',j,'_ID',sep = '')])) {
    
    w <-  get_ARPA_Lombardia_W_data(
      ID_station = distanceConstrained[i,paste('reg_Y_nn',j,'_ID',sep = '')], 
      Year = c(startyear:endyear),
      Frequency = "daily")
    
    totalW[[i]] <- data.frame(w)
    
  }
  
  
  OrangeStripes(totalW, paste('k=',j,startyear,endyear,'Constrained',sep = ' '))
  
}


#4.4 Count of missing datas---------------------------------

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
#4.5 Barplot/Piechart of missing data-------------------------------------------------

temp <- totalMissingFromBeginning # preparing the table to be used for Barplots and Piecharts
row.names(temp) <- temp$NameStation
temp$IDStation <- NULL
temp$NameStation <- NULL
temp[] <- lapply(temp, as.numeric)
MissingCount <- t(temp)

for (i in 1:ncol(MissingCount)) { # saving all piecharts
  
  jpeg(filename =paste(colnames(MissingCount)[i],' BarPlotMV.jpeg',sep = ''),width = 619, height = 471 )
  barplot(height = MissingCount[,i])
  dev.off()
  jpeg(filename =paste(colnames(MissingCount)[i],' PieChartMV.jpeg',sep = ''),width = 619, height = 471 )
  pie( MissingCount[,i])
  dev.off()
  
  
}

#5 Unique table for AQ and W--------------------------------------------

aq <- Easydownload(startyear,endyear,arrayStations)

w <-  get_ARPA_Lombardia_W_data(
  ID_station = c(677 , 671, 111), 
  Year = c(startyear:endyear),
  Frequency = "daily")


equiv <- distanceConstrained[,c(1,6)]

aqw<- sqldf('SELECT *
      FROM aq t JOIN equiv e ON t.IDStation = e.IDStation JOIN w ON e.reg_Y_nn1_ID = w.IDStation
               WHERE t.Date = w.Date')


write_csv(aqw,'NNdata.csv')


