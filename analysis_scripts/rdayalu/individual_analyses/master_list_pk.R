#This code is to create the master list that matches primary key values with identified domains, users, etc. 

#------------------------------------------------------------------------#
# ACCESS ANY DATABASE TABLE
#------------------------------------------------------------------------#

library(dplyr)

# Load system config file
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))

# Get db connection
db <- get_db_connection(system_conf)

#------------------------------------------------------------------------#
# CREATE MASTER LISTS
#------------------------------------------------------------------------#

#Domain master list
domain <- get_domain_table(db)
domain <- select(domain, name, id)
names(domain)[names(domain) == "id"] <- "domain_id"
names(domain)[names(domain) == "name"] <- "domain"
#Write to csv
write.csv(domain, file = "domain_master_list.csv", row.names = F)