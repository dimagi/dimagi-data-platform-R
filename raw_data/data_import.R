#The purpose of this code is import the all_monthly dataset as defined in the config_run file
#specified in config_setup.R

library(plyr)
library(dplyr)

#------------------------------------------------------------------------#
#DATA IMPORT
#------------------------------------------------------------------------#

# load config file - only system conf for this, not doing a report run so no run conf
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
system_conf <- get_system_config(file.path("config_system.json"))
aggregate_tables_dir <- file.path(system_conf$directories$output,"aggregate_tables")

source(file.path("config_setup.R", fsep = .Platform$file.sep)) # sets the path to the run config to use
run_conf <-get_run_config(config_run_path)

# get domain table from db
source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
con <- get_con(dbname=system_conf$database$dbname,
               user=system_conf$database$user,
               pass=system_conf$database$pass,
               host=system_conf$database$host, 
               port=system_conf$database$port)
domain_table <- get_domain_table(con)
close_con(con) # DON'T FORGET TO CLOSE THE DB CONNECTION

# get the domains to run on based on the filters/names in run conf
domains_for_run <- get_domains_for_run(domain_table,run_conf)

# get the monthly table for all domains
source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
all_monthly <- merged_monthly_table (domains_for_run, aggregate_tables_dir)

# write to csv
output_directory <- system_conf$directories$output
write.csv(all_monthly, file.path(output_directory, "all_monthly.csv", fsep = .Platform$file.sep))
