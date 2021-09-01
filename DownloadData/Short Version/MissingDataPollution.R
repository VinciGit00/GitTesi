#Libraries
library(ARPALData)
library(tidyverse)
library(sqldf)
library(ggplot2)
library(ggfortify)
library(lubridate)

#Filter datas
registry <- get_ARPA_Lombardia_AQ_registry()

IDStat <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         is.na(DateStop),
         year(DateStart)<=2017) %>%
  distinct(IDSensor) %>% pull() %>% sort() # Stations that measure all three since 2017 and haven't been decomessioned

RegistryRed <- registry %>% 
  filter(Pollutant%in% c("Ammonia","PM10","PM2.5"),
         IDSensor%in% IDStat) 

bestcentralines <- RegistryRed %>% 
  group_by(IDStation) %>% 
  summarise(n=n()) %>%
  filter(n>=2) %>% 
  distinct(IDStation) %>%
  pull() # stations that measure at least 2 of the variables at the same time

map_Lombardia_stations(bestcentralines)

#Starting with the loop for downloading the data + casting of the data

startyear = 2018
lastyear  = 2020

data <- NULL
cast <- NULL

for(index in startyear:lastyear) {
  #Downloading
  data[[1+index-startyear]] <- get_ARPA_Lombardia_AQ_data(
    ID_station = c(bestcentralines),
    Year = index,
    Frequency = "daily",
    Var_vec = NULL,
    Fns_vec = NULL,
    by_sensor = 0,
    verbose = T
  )
  #Casting
  cast[[1+index-startyear]] <- data.frame(data[[1+index-startyear]] )
  
  #Renaming
  cast[[1+index-startyear]] <- cast[[1+index-startyear]]  %>%
    rename(
      PM25 = PM2.5
    )
}
#Supporting functions 
MissingTable <- function(Variable, Table) {
  library(sqldf)
  
  tableMissing <- sqldf(paste('SELECT IDStation, NameStation, count(*) as Missing', Variable,
                              ' FROM ', Table,
                              ' WHERE ',Variable,' is null 
                                GROUP BY IDStation
                                ORDER BY Missing', Variable, sep = ''))  # stations where there minimun 1 missing value
  
  rest <- sqldf(paste(' SELECT IDStation, NameStation, 0 as Missing', Variable,
                      ' FROM ', Table,
                      ' EXCEPT
                        SELECT IDStation, NameStation, 0 as Missing', Variable,
                      ' FROM ', Table,
                      ' WHERE ', Variable,' is null 
                        GROUP BY IDStation', sep = '' )) # stations where there are no missing values
  
  return ( sqldf(paste('SELECT *
                               FROM tableMissing
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY Missing', Variable , sep = ''
  )) ) # all stations with regards to this variable
  
}


MissingAll <- function(Table) {
  library(sqldf)
  
  missingAll <- sqldf(paste(
    'SELECT IDStation, NameStation, count(*) as MissingAllThree
                        FROM ', Table,
    ' WHERE Ammonia is null and PM10 is null and PM25 is null
                        GROUP BY IDStation
                        ORDER BY MissingAllThree', sep = ''
  ))
  
  rest <- sqldf(paste('         SELECT IDStation, NameStation, 0 as MissingAllThree
                        FROM ', Table,'
                        EXCEPT
                        SELECT IDStation, NameStation, 0 as MissingAllThree
                        FROM ', Table,' 
                        WHERE Ammonia is null AND PM10 is null AND PM25 is null
                        GROUP BY IDStation', sep = ''))
  
  return (                     sqldf('SELECT *
                               FROM missingAll
                               UNION
                               SELECT *
                               FROM rest
                               ORDER BY MissingAllThree
                              '))
}

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
  setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData/Short Version/MissingTables")
  write_csv(tableMissingDatasTotal[[index-startyear+1]], name)
}

#Creating the table of yes/no
# queries yes/no table 

TableA <- tableMissingAmmmonia[[1]]

ColumnA <- sqldf('SELECT IDStation, NameStation, 1 as Ammonia
      FROM TableA 
      WHERE MissingAmmonia < 365
      union
      SELECT IDStation, NameStation, 0 as Ammonia
      FROM TableA 
      WHERE MissingAmmonia >= 365
      order by IDStation')

Table10 <- tableMissingPM10[[1]]

Column10 <- sqldf('SELECT IDStation, NameStation, 1 as PM10
      FROM Table10 
      WHERE MissingPM10 < 365
      union
      SELECT IDStation, NameStation, 0 as PM10
      FROM Table10 
      WHERE MissingPM10 >= 365
      order by IDStation')

Table25 <- tableMissingPM25[[1]]

Column25 <- sqldf('SELECT IDStation, NameStation, 1 as PM25
      FROM Table25
      WHERE MissingPM25 < 365
      union
      SELECT IDStation, NameStation, 0 as PM25
      FROM Table25
      WHERE MissingPM25 >= 365
      order by IDStation')
#N.B: 1 means presence
#0 means absence
presencetable <- sqldf("SELECT c25.IDStation, C25.NameStation, c25.PM25, c10.PM10, ca.Ammonia
                 FROM Column25 c25 JOIN Column10 c10
                 ON c25.IDStation = c10.IDStation
                 JOIN ColumnA ca
                 ON c25.IDStation = ca.IDStation")

####IN progress
#Data plot

BlueStripes <- function(vector,year){
  
  for (i in 1:length(vector)) {
    c9a <- ggplot(vector[[i]], aes(x = Date, y = Ammonia)) +
      geom_line()  + labs(title = paste(vector[[i]][1,3],year))
    nada <- is.na(vector[[i]]["Ammonia"])
    c9a <- c9a + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                            color = "blue", size=1.5)
    
    c910 <- ggplot(vector[[i]], aes(x = Date, y = PM10)) +
      geom_line() 
    nada <- is.na(vector[[i]]["PM10"])
    c910 <- c910 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "blue", size=1.5)
    
    
    c925 <- ggplot(vector[[i]], aes(x = Date, y = PM25)) +
      geom_line() 
    nada <- is.na(vector[[i]]["PM25"])
    c925 <- c925 + geom_vline(xintercept = vector[[i]][nada,1], alpha = 0.3, 
                              color = "blue", size=1.5)
    
    jpeg(filename =paste(vector[[i]][1,3],paste(year,".jpeg")),width = 1280, height = 720 )
    multiplot(c9a, c910, c925)
    dev.off()
    
  }
  
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list FROM the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated FROM # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


threeYesPlot <- sqldf("SELECT IDStation 
                      FROM presencetable
                      WHERE Ammonia = 1 and PM10 = 1 and PM25=1")
threeYesPlot <- as.vector(t(threeYesPlot))
FullStations <- NULL
table18 <- cast[[1]]

for (i in 1:length(threeYesPlot)) {
  
  FullStations[[i]] <- sqldf(paste("SELECT *
                             FROM table18
                             WHERE IDStation = ", threeYesPlot[i],sep = ""))
  
}

setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData/Short Version/PlotDatas")
BlueStripes(FullStations,2018)

table18_20 <- get_ARPA_Lombardia_AQ_data(
  ID_station = threeYesPlot,
  Year = c(startyear:lastyear),
  Frequency = "daily",
  Var_vec = NULL,
  Fns_vec = NULL,
  by_sensor = 0,
  verbose = T
)

FullStations <- NULL

for (i in 1:length(threeYesPlot)) {
  
  FullStations[[i]] <- sqldf(paste("SELECT *
                             FROM table18_20
                             WHERE IDStation = ", threeYesPlot[i],sep = ""))
  
}

setwd("/Users/marcovinciguerra/Github/GitTesi/DownloadData/Short Version/PlotDatas")
BlueStripes(FullStations,"2018-2020")
