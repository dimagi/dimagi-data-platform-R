#The purpose of this code is to import the all_monthly dataset 
#as defined in config_run.R, which is referenced by config_setup.R

library(dplyr)

#------------------------------------------------------------------------#
#DATA IMPORT
#------------------------------------------------------------------------#

# load config files
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))
source(file.path("config_setup.R", fsep = .Platform$file.sep))
run_conf <-get_run_config(config_run_path)

#Get db connection
db <- get_db_connection(system_conf)

# get domain table from db
source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
domain_table <- get_domain_table(db)

# get the domains to run on based on the filters/names in run conf
domains_for_run <- get_domains_for_run(domain_table,run_conf)

# get the monthly table domains to run on
source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
monthly_table <- get_aggregate_table(db, "aggregate_monthly_interactions", domains_for_run)

# write to csv
output_directory <- system_conf$directories$output
#write.csv(monthly_table, file.path(output_directory, "monthly_table.csv", fsep = .Platform$file.sep))

