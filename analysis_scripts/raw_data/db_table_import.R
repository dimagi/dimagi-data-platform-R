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
form_table <- get_data_source(db, "form", 1000) #limited number of forms
form_table <- collect(form_table)

formdef <- get_data_source(db, "formdef", 1000)
formdef <- collect(formdef)

visit <- tbl(db, "visit")
visit <- collect(visit)
visit <- get_data_source(db, "visit", 1000)
app <- tbl(db, "application")
device_log <- tbl(db, "device_log")
domain <- tbl(db, "domain")

users <- tbl(db, "users") 
#Get table for user_type (mobile, web, superuser, etc.)
#This function has been defined in data_sources.R
user_type <- get_user_type_table(db)

#Get visit_detail data source
#First run functions in data_sources.R
visit_detail <- get_visit_detail(db, 1000)

#Get case tables
cases <- tbl(db, "cases")
cases <- collect(cases)



