#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)

# add 2018

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
    PM25 = PM2.5
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

# Based on result2019or, we choose stations: 
#1: 677-Cremona 
#2: 681-Moggio 
#3: 693-Sannazzaro de' Burgondi 
#4: 642-Pavia
# Given that they have the least missing observations. 

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
# add extra stations

all2019 <- NULL

all2019[[1]] <- Cremona2019
all2019[[2]] <- Moggio2019
all2019[[3]] <- Sannazzaro2019
all2019[[4]] <- Pavia2019

vettore   <-ARPALdf_Summary(data2019)
variabile <- vettore$Gap_length$Ammonia

#Plot and save of the graphs

for (i in 1:length(all2019)) {
  c9a <- ggplot(all2019[[i]], aes(x = Date, y = Ammonia)) +
    geom_line()  + labs(title = paste(all2019[[i]][1,3],"2019"))
  nada <- is.na(all2019[[i]]["Ammonia"])
  c9a <- c9a + geom_vline(xintercept = all2019[[i]][nada,1], alpha = 0.3, 
                          color = "blue", size=1.5)
  
  c910 <- ggplot(all2019[[i]], aes(x = Date, y = PM10)) +
    geom_line() 
  nada <- is.na(all2019[[i]]["PM10"])
  c910 <- c910 + geom_vline(xintercept = all2019[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.5)
  
  
  c925 <- ggplot(all2019[[i]], aes(x = Date, y = PM25)) +
    geom_line() 
  nada <- is.na(all2019[[i]]["PM25"])
  c925 <- c925 + geom_vline(xintercept = all2019[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.7)
  
  jpeg(filename =paste(all2019[[i]][1,3],"2019.jpeg"),width = 1280, height = 720 )
  multiplot(c9a, c910, c925)
  dev.off()
  
}

setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2019/Ammonia') 

#2019 Ammonia
jpeg(file="Moggio2019.jpeg")
plot(Cremona2019[,"Date"], Cremona2019[,"Ammonia"] )
dev.off()


jpeg(file="Sannazzaro2019.jpeg")
plot(Sannazzaro2019[,"Date"], Sannazzaro2019[,"Ammonia"] )
dev.off()

jpeg(file="Pavia2019.jpeg")
plot(Pavia2019[,"Date"], Pavia2019[,"Ammonia"] )
dev.off()

#2019 PM10
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2019/PM10') 

jpeg(file="Moggio2019.jpeg")
plot(Cremona2019[,"Date"], Cremona2019[,"PM10"] )
dev.off()


jpeg(file="Sannazzaro2019.jpeg")
plot(Sannazzaro2019[,"Date"], Sannazzaro2019[,"PM10"] )
dev.off()

jpeg(file="Pavia2019.jpeg")
plot(Pavia2019[,"Date"], Pavia2019[,"PM10"] )
dev.off()

#2019 PM 2.5
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2019/PM25') 

jpeg(file="Moggio2019.jpeg")
plot(Cremona2019[,"Date"], Cremona2019[,"PM25"] )
dev.off()


jpeg(file="Sannazzaro2019.jpeg")
plot(Sannazzaro2019[,"Date"], Sannazzaro2019[,"PM25"] )
dev.off()

jpeg(file="Pavia2019.jpeg")
plot(Pavia2019[,"Date"], Pavia2019[,"PM25"] )
dev.off()

setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti') 
write_csv(result2019or, "MissingData2019.csv")

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

autoplot(ts(Cremona2019[,c(4:6)]))

#Queries 2020
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

setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti') 
write_csv(result2020or, "MissingData2020.csv")

all2020 <- NULL

all2020[[1]] <- Sannazzaro2020
all2020[[2]] <- Moggio2020
all2020[[3]] <- Milano2020
all2020[[4]] <- Cremona2020
all2020[[5]] <- Pavia2020
all2020[[6]] <- Schivenoglia2020

for (i in 1:length(all2020)) {
  c9a <- ggplot(all2020[[i]], aes(x = Date, y = Ammonia)) +
    geom_line()  + labs(title = paste(all2020[[i]][1,3],"2020"))
  nada <- is.na(all2020[[i]]["Ammonia"])
  c9a <- c9a + geom_vline(xintercept = all2020[[i]][nada,1], alpha = 0.3, 
                          color = "blue", size=1.5)
  
  c910 <- ggplot(all2020[[i]], aes(x = Date, y = PM10)) +
    geom_line() 
  nada <- is.na(all2020[[i]]["PM10"])
  c910 <- c910 + geom_vline(xintercept = all2020[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.5)
  
  
  c925 <- ggplot(all2020[[i]], aes(x = Date, y = PM25)) +
    geom_line() 
  nada <- is.na(all2020[[i]]["PM25"])
  c925 <- c925 + geom_vline(xintercept = all2020[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.7)
  
  jpeg(filename =paste(all2020[[i]][1,3],"2020.jpeg"),width = 1280, height = 720 )
  multiplot(c9a, c910, c925)
  dev.off()
  
}

#2020 Ammonia
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2020/Ammonia') 

jpeg(file="Sannazzaro2020.jpeg")
plot(Sannazzaro2020[,"Date"], Sannazzaro2020[,"Ammonia"] )
dev.off()

jpeg(file="Moggio2020.jpeg")
plot(Moggio2020[,"Date"], Moggio2020[,"Ammonia"] )
dev.off()

jpeg(file="Milano2020.jpeg")
plot(Milano2020[,"Date"], Milano2020[,"Ammonia"] )
dev.off()

jpeg(file="Cremona2020.jpeg")
plot(Cremona2020[,"Date"], Cremona2020[,"Ammonia"] )
dev.off()

jpeg(file="Pavia2020.jpeg")
plot(Pavia2020[,"Date"], Pavia2020[,"Ammonia"] )
dev.off()

jpeg(file="Schivenoglia2020.jpeg")
plot(Schivenoglia2020[,"Date"], Schivenoglia2020[,"Ammonia"] )
dev.off()

#2020 PM10
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2020/Ammonia/PM10') 

jpeg(file="Sannazzaro2020.jpeg")
plot(Sannazzaro2020[,"Date"], Sannazzaro2020[,"PM10"] )
dev.off()

jpeg(file="Moggio2020.jpeg")
plot(Moggio2020[,"Date"], Moggio2020[,"PM10"] )
dev.off()

jpeg(file="Milano2020.jpeg")
plot(Milano2020[,"Date"], Milano2020[,"PM10"] )
dev.off()

jpeg(file="Cremona2020.jpeg")
plot(Cremona2020[,"Date"], Cremona2020[,"PM10"] )
dev.off()

jpeg(file="Pavia2020.jpeg")
plot(Pavia2020[,"Date"], Pavia2020[,"PM10"] )
dev.off()

jpeg(file="Schivenoglia2020.jpeg")
plot(Schivenoglia2020[,"Date"], Schivenoglia2020[,"PM10"] )
dev.off()

#2020 PM25
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2020/PM25') 

jpeg(file="Sannazzaro2020.jpeg")
plot(Sannazzaro2020[,"Date"], Sannazzaro2020[,"PM25"] )
dev.off()

jpeg(file="Moggio2020.jpeg")
plot(Moggio2020[,"Date"], Moggio2020[,"PM25"] )
dev.off()

jpeg(file="Milano2020.jpeg")
plot(Milano2020[,"Date"], Milano2020[,"PM25"] )
dev.off()

jpeg(file="Cremona2020.jpeg")
plot(Cremona2020[,"Date"], Cremona2020[,"PM25"] )
dev.off()

jpeg(file="Pavia2020.jpeg")
plot(Pavia2020[,"Date"], Pavia2020[,"PM25"] )
dev.off()

jpeg(file="Schivenoglia2020.jpeg")
plot(Schivenoglia2020[,"Date"], Schivenoglia2020[,"PM25"] )
dev.off()

#2019 mixed with 2020
result2019 = result2019or
result2020 = result2020or

total2019_2020 <- sqldf('SELECT r19.IDStation, r19.NameStation, sum(r19.nada)+sum(r20.nada) as nada
                       FROM result2019 as r19 JOIN result2020 as r20
                       ON r19.IDStation = r20.IDStation
                       GROUP BY r19.IDStation
                       ORDER BY nada
                       ')

setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti') 

write_csv(total2019_2020, "MissingData20192020Combined.csv")

#Based on total2019_2020 we choose this stations:
#1: 681 Moggio, 85 missing datas
#2: 693 Sannazzaro De Burgondi Agip, 97 missing datas
#3: 677 Cremona Via Fatebenefratelli, 103 missing datas
#4: 642 Pavia Via Folperti, 147 missing datas
#5: 703 Schivenoglia, 294 missing datas

tabletotal = rbind(cast2019[, c(1:4, 12:13)], cast2020[, c(1:4, 17:18)])

Moggio20192020 <- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM tabletotal
                       WHERE IDStation = 681
                       ')
Sannazzaro20192020 <- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM tabletotal
                       WHERE IDStation = 693
                       ')
Cremona20192020<-sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM tabletotal
                       WHERE IDStation = 677
                       ')
Pavia20192020<- sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM tabletotal
                       WHERE IDStation = 642
                       ')

Schivenoglia20192020<-sqldf('SELECT Date, IDStation, NameStation, Ammonia, PM10, PM25
                       FROM tabletotal
                       WHERE IDStation = 703
                       ')

all20192020 <- NULL

all20192020[[1]] <- Moggio20192020
all20192020[[2]] <- Sannazzaro20192020
all20192020[[3]] <- Cremona20192020
all20192020[[4]] <- Pavia20192020
all20192020[[5]] <- Schivenoglia20192020

for (i in 1:length(all20192020)) {
  c9a <- ggplot(all20192020[[i]], aes(x = Date, y = Ammonia)) +
    geom_line()  + labs(title = paste(all20192020[[i]][1,3],"2019-2020"))
  nada <- is.na(all20192020[[i]]["Ammonia"])
  c9a <- c9a + geom_vline(xintercept = all20192020[[i]][nada,1], alpha = 0.3, 
                          color = "blue", size=1.5)
  
  c910 <- ggplot(all20192020[[i]], aes(x = Date, y = PM10)) +
    geom_line() 
  nada <- is.na(all20192020[[i]]["PM10"])
  c910 <- c910 + geom_vline(xintercept = all20192020[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.5)
  
  
  c925 <- ggplot(all20192020[[i]], aes(x = Date, y = PM25)) +
    geom_line() 
  nada <- is.na(all20192020[[i]]["PM25"])
  c925 <- c925 + geom_vline(xintercept = all20192020[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.7)
  
  jpeg(filename =paste(all20192020[[i]][1,3],"2019-2020.jpeg"),width = 1280, height = 720 )
  multiplot(c9a, c910, c925)
  dev.off()
  
}


#2019-2020 mixed Ammonia
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2019-2020Combined/Ammonia') 

jpeg(file="Moggio20192020.jpeg")
plot(Moggio20192020[,"Date"], Moggio20192020[,"Ammonia"] )
dev.off()

jpeg(file="Sannazzaro20192020.jpeg")
plot(Sannazzaro20192020[,"Date"], Sannazzaro20192020[,"Ammonia"] )
dev.off()

jpeg(file="Cremona20192020.jpeg")
plot(Cremona20192020[,"Date"], Cremona20192020[,"Ammonia"] )
dev.off()

jpeg(file="Pavia20192020.jpeg")
plot(Pavia20192020[,"Date"], Pavia20192020[,"Ammonia"] )
dev.off()

jpeg(file="Schivenoglia2019020.jpeg")
plot(Schivenoglia2019020[,"Date"], Schivenoglia2019020[,"Ammonia"] )
dev.off()

#2019-2020 mixed PM10
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2019-2020Combined/PM10') 

jpeg(file="Moggio20192020.jpeg")
plot(Moggio20192020[,"Date"], Moggio20192020[,"PM10"] )
dev.off()

jpeg(file="Sannazzaro20192020.jpeg")
plot(Sannazzaro20192020[,"Date"], Sannazzaro20192020[,"PM10"] )
dev.off()

jpeg(file="Cremona20192020.jpeg")
plot(Cremona20192020[,"Date"], Cremona20192020[,"PM10"] )
dev.off()

jpeg(file="Pavia20192020.jpeg")
plot(Pavia20192020[,"Date"], Pavia20192020[,"PM10"] )
dev.off()

jpeg(file="Schivenoglia2019020.jpeg")
plot(Schivenoglia2019020[,"Date"], Schivenoglia2019020[,"PM10"] )
dev.off()

#2019-2020 mixed PM25
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/DatiMancanti/PlotDati/2019-2020Combined/PM25') 

jpeg(file="Moggio20192020.jpeg")
plot(Moggio20192020[,"Date"], Moggio20192020[,"PM25"] )
dev.off()

jpeg(file="Sannazzaro20192020.jpeg")
plot(Sannazzaro20192020[,"Date"], Sannazzaro20192020[,"PM25"] )
dev.off()

jpeg(file="Cremona20192020.jpeg")
plot(Cremona20192020[,"Date"], Cremona20192020[,"PM25"] )
dev.off()

jpeg(file="Pavia20192020.jpeg")
plot(Pavia20192020[,"Date"], Pavia20192020[,"PM25"] )
dev.off()

jpeg(file="Schivenoglia2019020.jpeg")
plot(Schivenoglia2019020[,"Date"], Schivenoglia2019020[,"PM25"] )
dev.off()

# Mapping 

#Based on total2019_2020 we choose this stations:
#1: 681 Moggio, 85 missing datas
#2: 693 Sannazzaro De Burgondi Agip, 97 missing datas
#3: 677 Cremona Via Fatebenefratelli, 103 missing datas
#4: 642 Pavia Via Folperti, 147 missing datas
#5: 703 Schivenoglia, 294 missing datas

data19 <- get_ARPA_Lombardia_AQ_data(
  ID_station = c(642,677,681,693),
  Year = 2019,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

data20 <- get_ARPA_Lombardia_AQ_data(
  ID_station = c(681,693,677,642,705,703),
  Year = 2020,
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)


data1920 <- get_ARPA_Lombardia_AQ_data(
  ID_station = c(681,693,677,642,703),
  Year = c(2019,2020),
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)
jpeg(filename = "mappa 2019",width = 1280, height = 720 )
map_Lombardia_stations(data19)
dev.off()
jpeg(filename = "mappa 2020",width = 1280, height = 720 )
map_Lombardia_stations(data20)
dev.off()
jpeg(filename = "mappa 2019-2020",width = 1280, height = 720 )
map_Lombardia_stations(data1920)
dev.off()

library(lubridate)

registry <- get_ARPA_Lombardia_AQ_registry()

registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5")) %>%
  View

IDStat <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         is.na(DateStop),
         year(DateStart)<=2017) %>%
  distinct(IDSensor) %>% pull() %>% sort() 

RegistryRed <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         IDSensor%in% IDStat) 

map_Lombardia_stations(RegistryRed)

RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n>=2) %>% 
  distinct(IDStation) %>%
  pull()
  
