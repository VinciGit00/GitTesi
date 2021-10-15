#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(sf)

#Source functions
source("~/GitHub/GitTesi/DownloadData/Scripts/Functions.R", encoding = 'UTF-8')

#Stations we are interested:
#1 703: Schivenoglia (R)
#2 681: Moggio (R)
#3 627: Cremona P.zza Cadorna (U) 

#Array with the numbers of the stations
arrayStations <-c(703,681, 627)


#Years interested
startyear <- 2018
endyear   <- 2020

#Datas stations
datas <- Download(startyear, endyear, arrayStations)

#TOD: weather stations anda plot of the datas