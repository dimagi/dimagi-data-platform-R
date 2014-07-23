# FILE csv_sources.R
# functions to get test data from csv. 
# should return whatever the DB query would return 
# i.e. the function should set the correct data types before returning the data frame.

# FUNCTION get_test_data_csv
# reads the file filename at the specified path path
# returns the result of read.csv
#
# PARAMS 
# path: the path
# filename: the filename
get_test_data_csv <- function(path, filename){
  fullpath <- file.path(path,filename, fsep = .Platform$file.sep)
  print(sprintf("Loading table %s from csv, looking in path %s",filename,fullpath))
  csvfile <- read.csv(fullpath, header = TRUE, stringsAsFactors = FALSE)
  return(csvfile)
}

# FUNCTION get_interaction_table_from_csv
# reads the interactions table from the file interactions.csv at the specified path
# returns the interactions table equivalent to the result of get_interaction_table in db_queries.R
#
# PARAMS 
# path: the path to R test data csv files
get_interaction_table_from_csv <- function (path) {
  filename = "interactions.csv"
  return (get_test_data_csv(path,filename))
}

# FUNCTION single_vec_split
# splits a string at the character specified by split.
# returns the first element of the list returned by strsplit
#
# PARAMS 
# s: the string to split
# split: the character to split on
single_vec_split <- function(s, split=","){
  spl <- strsplit(s, split)
  unlisted <- spl[[1]]
  return(unlisted)
}

# FUNCTION get_domain_table_from_csv
# reads the domain table from the file domains.csv at the specified path
# returns the domains table equivalent to the result of get_domain_table in db_queries.R
#
# PARAMS 
# path: the path to R test data csv files
get_domain_table_from_csv <- function (path) {
  filename = "domains.csv"
  dt <- get_test_data_csv(path,filename)
  dt$sector <-lapply(dt$sector,single_vec_split)
  return (dt)
}