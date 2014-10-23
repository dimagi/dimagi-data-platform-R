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

#Get interactions table or device_type table (or other db source)
#Limit interactions by specified number. If want entire interaction table, enter "-1".
#device_type is also a data source
inter <- get_data_source(db, "interactions", 1000)
inter <- get_data_source(db, "interactions", -1)

#Get form table (or another straight db dplyr table - a list of these tables is available
#when you print db)
form_table <- tbl(db, "form")
form_table <- get_data_source(db, "form", 1000)
collect(form_table)
app <- tbl(db, "application")
device_log <- tbl(db, "device_log")
domain_table <- tbl(db, "domain")

#------------------------------------------------------------------------#
#Calculations on DB tables
#------------------------------------------------------------------------#

#Use collect() to bring these results into a dataframe
# Count forms per app_id
sum_forms <- 
    form_table %.% 
    group_by(app_id) %.%
    summarise(form_total = count(form_id))

summary(sum_forms)
dim(sum_forms)
head(sum_forms$select)



