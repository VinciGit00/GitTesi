#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)

#Filter datas
registry <- get_ARPA_Lombardia_AQ_registry()

RegistryRed <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         IDSensor%in% IDStat) 

bestcentralines <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n>=2) %>% 
  distinct(IDStation) %>%
  pull()

map_Lombardia_stations(bestcentralines)

#Downloading best datas
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

#Casting of the variables
cast2018 <- data.frame(data2018)
cast2019 <- data.frame(data2019)
cast2020 <- data.frame(data2020)

#Renaming the variables
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

#Starting with the queries
#2018
table2018MissingAmmmonia <- sqldf('SELECT IDStation, NameStation, count(*) as MissingAmmonia
                        FROM cast2018
                        WHERE Ammonia is null 
                        GROUP BY IDStation
                        ORDER BY MissingAmmonia')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingAmmonia
                        FROM cast2018
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingAmmonia
                        FROM cast2018
                        WHERE Ammonia is null 
                        GROUP BY IDStation')



table2018MissingAmmmonia <- sqldf('SELECT *
                               FROM table2018MissingAmmmonia
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingAmmonia
                              ')

table2018MissingPM10 <- sqldf('SELECT IDStation, NameStation, count(*) as MissingPM10
                        FROM cast2018
                        WHERE PM10 is null 
                        GROUP BY IDStation
                        ORDER BY MissingPM10')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2018
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2018
                        WHERE PM10 is null 
                        GROUP BY IDStation')

table2018MissingPM10 <- sqldf('SELECT *
                               FROM table2018MissingPM10
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingPM10
                              ')

table2018MissingPM25 <- sqldf('SELECT IDStation, NameStation, count(*) as MissingPM25
                        FROM cast2018
                        WHERE PM25 is null 
                        GROUP BY IDStation
                        ORDER BY MissingPM25')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM25
                        FROM cast2018
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM25
                        FROM cast2018
                        WHERE PM25 is null 
                        GROUP BY IDStation')
table2018MissingPM25 <- sqldf('SELECT *
                               FROM table2018MissingPM25
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingPM25
                              ')

table2018MissingallDatas <- sqldf('SELECT IDStation, NameStation, count(*) as MissingDatas
                        FROM cast2018
                        WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation
                        ORDER BY MissingDatas')

rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2018
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2018
                        WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation')

table2018MissingallDatas <- sqldf('SELECT *
                               FROM table2018MissingallDatas
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingDatas
                              ')

#All the missing datas for every station
tableMissingDatasTotal2018 <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingDatas
                                  FROM table2018MissingAmmmonia ma  JOIN table2018MissingPM10 m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN table2018MissingPM25 m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN table2018MissingallDatas mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')

#2019
table2019MissingAmmmonia <- sqldf('SELECT IDStation, NameStation, count(*) as MissingAmmonia
                        FROM cast2019
                        WHERE Ammonia is null 
                        GROUP BY IDStation
                        ORDER BY MissingAmmonia')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingAmmonia
                        FROM cast2019
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingAmmonia
                        FROM cast2019
                        WHERE Ammonia is null 
                        GROUP BY IDStation')


table2019MissingAmmmonia <- sqldf('SELECT *
                               FROM table2019MissingAmmmonia
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingAmmonia
                              ')

table2019MissingPM10 <- sqldf('SELECT IDStation, NameStation, count(*) as MissingPM10
                        FROM cast2019
                        WHERE PM10 is null 
                        GROUP BY IDStation
                        ORDER BY MissingPM10')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2019
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2019
                        WHERE PM10 is null 
                        GROUP BY IDStation')

table2019MissingPM10 <- sqldf('SELECT *
                               FROM table2019MissingPM10
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingPM10
                              ')

table2019MissingPM25 <- sqldf('SELECT IDStation, NameStation, count(*) as MissingPM25
                        FROM cast2019
                        WHERE PM25 is null 
                        GROUP BY IDStation
                        ORDER BY MissingPM25')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM25
                        FROM cast2019
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM25
                        FROM cast2019
                        WHERE PM25 is null 
                        GROUP BY IDStation')
table2019MissingPM25 <- sqldf('SELECT *
                               FROM table2019MissingPM25
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingPM25
                              ')

table2019MissingallDatas <- sqldf('SELECT IDStation, NameStation, count(*) as MissingDatas
                        FROM cast2019
                        WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation
                        ORDER BY MissingDatas')

rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2019
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2019
                        WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation')

table2019MissingallDatas <- sqldf('SELECT *
                               FROM table2019MissingallDatas
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingDatas
                              ')

#All the missing datas for every station
tableMissingDatasTotal2019 <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingDatas
                                  FROM table2019MissingAmmmonia ma  JOIN table2019MissingPM10 m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN table2019MissingPM25 m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN table2019MissingallDatas mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')

#2020
table2020MissingAmmmonia <- sqldf('SELECT IDStation, NameStation, count(*) as MissingAmmonia
                        FROM cast2020
                        WHERE Ammonia is null 
                        GROUP BY IDStation
                        ORDER BY MissingAmmonia')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingAmmonia
                        FROM cast2020
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingAmmonia
                        FROM cast2020
                        WHERE Ammonia is null 
                        GROUP BY IDStation')

table2020MissingAmmmonia <- sqldf('SELECT *
                               FROM table2020MissingAmmmonia
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingAmmonia
                              ')

table2020MissingPM10 <- sqldf('SELECT IDStation, NameStation, count(*) as MissingPM10
                        FROM cast2020
                        WHERE PM10 is null 
                        GROUP BY IDStation
                        ORDER BY MissingPM10')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2020
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2020
                        WHERE PM10 is null 
                        GROUP BY IDStation')

table2020MissingPM10 <- sqldf('SELECT *
                               FROM table2020MissingPM10
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingPM10
                              ')

table2020MissingPM25 <- sqldf('SELECT IDStation, NameStation, count(*) as MissingPM25
                        FROM cast2020
                        WHERE PM25 is null 
                        GROUP BY IDStation
                        ORDER BY MissingPM25')
rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM25
                        FROM cast2020
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM25
                        FROM cast2020
                        WHERE PM25 is null 
                        GROUP BY IDStation')
table2020MissingPM25 <- sqldf('SELECT *
                               FROM table2019MissingPM25
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingPM25
                              ')

table2020MissingallDatas <- sqldf('SELECT IDStation, NameStation, count(*) as MissingDatas
                        FROM cast2020
                        WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation
                        ORDER BY MissingDatas')

rest <- sqldf('         SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2020
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingPM10
                        FROM cast2020
                        WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation')

table2020MissingallDatas <- sqldf('SELECT *
                               FROM table2020MissingallDatas
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingDatas
                              ')

#All the missing datas for every station
tableMissingDatasTotal2020 <- sqldf(' SELECT ma.IDStation, ma.NameStation, ma.MissingAmmonia, m10.MissingPM10, m25.MissingPM25,mtodos.MissingDatas
                                  FROM table2020MissingAmmmonia ma  JOIN table2020MissingPM10 m10
                                  ON ma.IDStation = m10.IDStation
                                  JOIN table2020MissingPM25 m25
                                  ON ma.IDStation = m25.IDStation
                                  JOIN table2020issingallDatas mtodos
                                  on ma.IDStation = mtodos.IDStation
                                ')













#Creating the table of yes/no
threpresents <- RegistryRed %>% 
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


