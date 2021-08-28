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
