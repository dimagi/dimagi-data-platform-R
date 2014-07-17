# functions to get test data from csv. 
# should return whatever the DB query would return 
# i.e. the function should set the correct data types before returning the data frame.

get_interaction_table_from_csv <- function (filename) {
v <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
return (v)
}