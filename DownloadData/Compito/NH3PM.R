library(ARPALData)
library(tidyverse)

datipoll = get_ARPA_Lombardia_AQ_data(
  ID_station = NULL,
  Year = c(2019), #singolo anno: numero, oppure parentesi per costruire un vettore di anni
  Frequency = "daily", #Frequenza: oraria
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

datashort = datipoll[, c(1:4, 12:13)]

setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData/Compito') 
write_csv(datashort, "datipoll.csv")