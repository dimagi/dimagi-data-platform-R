library(dplyr)

# load config files
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))
source(file.path("config_setup.R", fsep = .Platform$file.sep))
run_conf <-get_run_config(config_run_path)

db <- get_db_connection(system_conf)

source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
domain_table <- get_domain_table(db)
domain_table.permitted <- domain_table[name %in% get_permitted_domains(domain_table),]
monthly_table <- tbl(db, "aggregate_monthly_interactions")
monthly.permitted <- monthly_table[domain %in% get_permitted_domains(domain_table),]