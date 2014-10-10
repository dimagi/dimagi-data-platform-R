#------------------------------------------------------------------------#
# ACCESS ANY DATABASE TABLE
#------------------------------------------------------------------------#

library(dplyr)

# Load system config file
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))

# Get form table from db
db <- get_db_connection(system_conf)
form_table <- tbl(db, "form")

# Count forms per app_id
sum_forms <- 
    form_table %.% 
    group_by(app_id) %.%
    summarise(form_total = count(form_id))

summary(sum_forms)
dim(sum_forms)
head(sum_forms$select)



