source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))

run_conf <-get_run_config(getwd())
system_conf <- get_system_config(getwd())
output_dir <- system_conf$directories$output
aggtables_output_subdir <- file.path(output_dir, "aggregate_tables")
dir.create(aggtables_output_subdir, showWarnings = FALSE)

# in debug mode, csv files from the dir r_test_data_dir are used instead of db queries
debug_mode <- run_conf$debug
if (debug_mode == T) {
  test_data_dir <- system_conf$directories$r_test_data_dir
} else {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  con <- get_con(dbname=system_conf$database$dbname,
                 user=system_conf$database$user,
                 pass=system_conf$database$pass,
                 host=system_conf$database$host, 
                 port=system_conf$database$port)
}
aggtables <- get_aggregate_table_names(system_conf)

# run the aggregate table scripts
for (aggtable in aggtables) {
  aggtable_file <- sprintf("%s.R", aggtable)
  source(file.path("aggregate_tables",aggtable_file, fsep = .Platform$file.sep))
  
  if (debug_mode == F) {
    create_tables(con,aggtables_output_subdir)
  } else {
    create_tables_debug(test_data_dir,aggtables_output_subdir)
  }
  
}

if (debug_mode == F) {
  close_con(con)
}