# functions to get test data from csv. 
# should return whatever the DB query would return 
# i.e. the function should set the correct data types before returning the data frame.

get_test_data_csv <- function(path, filename){
  fullpath <- file.path(path,filename, fsep = .Platform$file.sep)
  print(sprintf("Loading table %s from csv, looking in path %s",filename,fullpath))
  csvfile <- read.csv(fullpath, header = TRUE, stringsAsFactors = FALSE)
  return(csvfile)
}

get_interaction_table_from_csv <- function (path) {
  filename = "interactions.csv"
  return (get_test_data_csv(path,filename))
}

get_domain_table_from_csv <- function (path) {
  filename = "domains.csv"
  return (get_test_data_csv(path,filename))
}