library(dplyr)

source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
source(file.path("function_libraries/db_queries.R"))
system_conf <- get_system_config(file.path("config_system.json"))

# Get db connection
db <- get_db_connection(system_conf)


logs <- get_data_source(db, 'device_log_types_by_user', 1000)

logs_by_type <- logs %.%
  group_by (log_type) %.% 
  summarise (total_logs = count(id), with_user_id = count(user_id))

logs_by_type_by_domain <- logs %.%
  group_by (log_type, name) %.% 
  summarise (total_logs = count(id), with_user_id = count(user_id))
