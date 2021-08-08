library(ARPALData)
library(tidyverse)

data2 = get_ARPA_Lombardia_AQ_data(
  ID_station = c(584,501),
  Year = c(2019,2020), #singolo anno: numero, oppure parentesi per costruire un vettore di anni
  Frequency = "hourly", #Frequenza: oraria
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData') 
write_csv(data2, "tabella.csv")