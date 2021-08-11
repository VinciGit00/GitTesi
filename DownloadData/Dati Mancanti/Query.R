#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)

#Data download with ARPALData
#2019
data2019 <- get_ARPA_Lombardia_AQ_data(
  ID_station = NULL,
  Year = 2019,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

#Casting of the tables for queries
cast2019 <- data.frame(data2019)

#Renaming the PM2.5 column
cast2019 = cast2019 %>%
  rename(
    PM25 =PM2.5
  )

#Queries 2019
result2019Ammonia <- sqldf('SELECT IDStation, NameStation, COUNT(*) as Ammonia
                    FROM  cast2019 
                    WHERE Ammonia is null
                    GROUP BY IDStation')

result2019PM10 <- sqldf('SELECT IDStation, NameStation, COUNT(*) as PM10
                    FROM  cast2019 
                    WHERE PM10 is null
                    GROUP BY IDStation')

result2019PM25 <- sqldf('SELECT IDStation, NameStation, COUNT(*) as PM25
                    FROM  cast2019 
                    WHERE PM25 is null
                    GROUP BY IDStation')

result2019or <- sqldf('SELECT IDStation, NameStation, COUNT(*) as nada
                    FROM  cast2019 
                    WHERE Ammonia is null or PM10 is null or PM25 is null
                    GROUP BY IDStation
                    ORDER BY nada')

result2019total <- sqldf('SELECT A.IDStation, A.NameStation, A.Ammonia, R10.PM10, R25.PM25
                    FROM  result2019Ammonia as A, result2019PM10 as R10, result2019PM25 as R25
                    WHERE A.NameStation  = R10.NameStation
                    AND   R10.NameStation = R25.NameStation 
                    ')
 # based on result2019or, we choose stations: 677-Cremona, 681-Moggio, 693-Sannazzaro de' Burgondi and 642-Pavia
 # given that they have the least missing observations. 

bestdata2019 <- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2019
                       WHERE IDStation = 677 or IDStation = 681 or IDStation = 693 or IDStation = 642
                       ')

Cremona2019 <- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2019
                       WHERE IDStation = 677
                       ')
Moggio2019 <-  sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2019
                       WHERE IDStation = 681
                       ')

Sannazzaro2019 <-  sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2019
                       WHERE IDStation = 693
                       ')
Pavia2019 <-  sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2019
                       WHERE IDStation = 642
                       ')

autoplot(ts(Cremona2019[,c(4:6)]))

plot(Cremona2019[,"Date"], Cremona2019[,"Ammonia"] )

vettore   <-ARPALdf_Summary(data2019)
variabile <- vettore$Gap_length$Ammonia

#2020
data2020 <- get_ARPA_Lombardia_AQ_data(
  ID_station = NULL,
  Year = 2020,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

cast2020 <- data.frame(data2020)

cast2020 = cast2020 %>%
  rename(
    PM25 = PM2.5
  )

#Queries 202o
result2020Ammonia <- sqldf('SELECT IDStation, NameStation, COUNT(*) as Ammonia
                    FROM  cast2020 
                    WHERE Ammonia is null
                    GROUP BY IDStation')

result2020PM10 <- sqldf('SELECT IDStation, NameStation, COUNT(*) as PM10
                    FROM  cast2020
                    WHERE PM10 is null
                    GROUP BY IDStation')

result2020PM25 <- sqldf('SELECT IDStation, NameStation, COUNT(*) as PM25
                    FROM  cast2020
                    WHERE PM25 is null
                    GROUP BY IDStation')

result2020or <- sqldf('SELECT IDStation, NameStation, COUNT(*) as nada
                    FROM  cast2020
                    WHERE Ammonia is null or PM10 is null or PM25 is null
                    GROUP BY IDStation
                    ORDER BY nada')

result2020total <- sqldf('SELECT A.IDStation, A.NameStation, A.Ammonia, R10.PM10, R25.PM25
                    FROM  result2020Ammonia as A, result2020PM10 as R10, result2020PM25 as R25
                    WHERE A.NameStation  = R10.NameStation
                    AND   R10.NameStation = R25.NameStation 
                    ')

#Comment for result2020or:
#There are 7 control units with an acceptable number of missing data
#1: 693 Sannazzaro de' Burgondi - AGIP, missing data: 31
#2: 681 Moggio, missing data: 37
#3: 705 Milano - Pascal Città Studi, missing data: 39
#4: 677 Cremona - Via Fatebenefratelli, missing data: 59
#5: 642 Pavia - via Folperti, missing data: 71
#6: 703 Schivenoglia, missing data:79

bestdata2020 <- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2020
                       WHERE IDStation = 693 or IDStation = 681 or IDStation = 705 or IDStation = 677
                       or IDStation = 642 or IDStation = 703
                       ')

Sannazzaro2020 <- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2020
                       WHERE IDStation = 693
                       ')
Moggio2020 <- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2020
                       WHERE IDStation = 681
                       ')
Milano2020<-sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2020
                       WHERE IDStation = 705
                       ')
Cremona2020<- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2020
                       WHERE IDStation = 677
                       ')

Pavia2020<-sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2020
                       WHERE IDStation = 693
                       ')

Schivenoglia2020<-sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2020
                       WHERE IDStation = 703
                       ')