#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)

#Data download with ARPALData
data2019 <- get_ARPA_Lombardia_AQ_data(
  ID_station = NULL,
  Year = 2019,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

data2020 <- get_ARPA_Lombardia_AQ_data(
  ID_station = NULL,
  Year = 2020,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

my_data %>% 
  rename(
    sepal_length = Sepal.Length,
    sepal_width = Sepal.Width
  )

#Casting of the tables for queries
cast2019 <- data.frame(data2019)
cast2020 <- data.frame(data2020)

#Renaming the PM2.5 column
cast2019 = cast2019 %>%
  rename(
    PM25 =PM2.5
  )

cast2020 = cast2020 %>%
  rename(
    PM25 =PM2.5
  )


#Queries
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

result2019or <- sqldf('SELECT IDStation, NameStation, COUNT(*) as missing
                    FROM  cast2019 
                    WHERE Ammonia is null or PM10 is null or PM25 is null
                    GROUP BY IDStation')

result2019total <- sqldf('SELECT A.IDStation, A.NameStation, A.Ammonia, R10.PM10, R25.PM25
                    FROM  result2019Ammonia as A, result2019PM10 as R10, result2019PM25 as R25
                    WHERE A.NameStation  = R10.NameStation
                    AND   R10.NameStation = R25.NameStation 
                    ')
 # based on result2019or, we choose stations: 677-Cremona, 681-Moggio, 693-Sannazzaro de' Burgondi and 642-Pavia
 # given that they have the least missing observations. 

bestdata2019 <- sqldf('SELECT IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2019
                       WHERE IDStation = 677 or IDStation = 681 or IDStation = 693 or IDStation = 642
  
                       ')
Cremona2019 <- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM cast2019
                       WHERE IDStation = 677
  
                       ')

autoplot(ts(Cremona2019[,c(4:6)]))

plot(Cremona2019[,"Date"], Cremona2019[,"Ammonia"] )


#resultmix <- [result2019, result2020]

vettore <-ARPALdf_Summary(data2019)
variabile <- vettore$Gap_length$Ammonia

