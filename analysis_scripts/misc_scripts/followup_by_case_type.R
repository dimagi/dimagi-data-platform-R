library(dplyr)
library(lubridate)

# load system conf
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))

# get db connection
db <- get_db_connection(system_conf)

# get interactions data source
source(file.path("data_sources.R", fsep = .Platform$file.sep))
interactions <- get_data_source(db, "interactions", limit = -1) # gets the whole table and takes a while, change the limit to for e.g. 1000 to get just 1000 rows

# head(interactions) # check what columns we have

domains <- c('crs-remind','aaharbaseline','myrada', 'spandan', 'rdi-hiht', 'care-ecd')
interactions_subset <- interactions[interactions$domain %in% domains,] #subset of interactions table - just the domains were analysing






