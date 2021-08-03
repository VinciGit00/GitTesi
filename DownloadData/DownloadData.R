library(ARPALData)
library(tidyverse)

#Dati sulla qualit?? dell'aria
#Passo 1: scaricare i dati di qualit?? dell'aria (inquinanti) 
data1 = get_ARPA_Lombardia_AQ_data(
  ID_station = NULL,
  Year = c(2019,2020), #singolo anno: numero, oppure parentesi per costruire un vettore di anni
  Frequency = "hourly", #Frequenza
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

data2 = get_ARPA_Lombardia_AQ_data(
  ID_station = c(584,501),
  Year = c(2019,2020), #singolo anno: numero, oppure parentesi per costruire un vettore di anni
  Frequency = "hourly", #Frequenza: oraria
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

reg = get_ARPA_Lombardia_AQ_registry()

sum = ARPALdf_Summary(
  data2,
  by_IDStat = 1,
  by_Year = 1,
  gap_length = 1,
  correlation = 1,
  histogram = 1,
  density = 1,
  outlier = 0,
  verbose = T
)

#sono una colonna della lista
sum$Descr
sum$Descr_by_IDStat #Statistiche descrittive divise stazione per stazione

#Come esportare i file
setwd('/Users/marcovinciguerra/Github/GitTesi/DownloadData') #Bisogna aggiungere il path di destinazione qui
write_csv(data2, "tabella.csv")


#I comandi per il meteo sono gli stessi (stessa struttura)