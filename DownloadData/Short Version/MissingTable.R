# Helper functions 

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