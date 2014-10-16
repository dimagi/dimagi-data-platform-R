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

#Get interactions table or device_type table (or other db table)
inter <- get_data_source(db, "interactions", 1000)
inter <- get_data_source(db, "interactions", -1)

#Get form table (or another straight dplyr table - a list of these tables is available
#when you print db)
form_table <- tbl(db, "form")

#------------------------------------------------------------------------#
#Calculations on DB tables
#------------------------------------------------------------------------#

# Count forms per app_id
sum_forms <- 
    form_table %.% 
    group_by(app_id) %.%
    summarise(form_total = count(form_id))

summary(sum_forms)
dim(sum_forms)
head(sum_forms$select)



