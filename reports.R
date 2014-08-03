source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))

run_conf <-get_run_config(".")
system_conf <- get_system_config(".")
output_dir <- system_conf$directories$output

# in debug mode, csv files from the dir r_test_data_dir are used instead of db queries
debug_mode <- run_conf$debug
if (debug_mode == T) {
  source(file.path(getwd(),"function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  test_data_dir <- system_conf$directories$r_test_data_dir
  domain_table <- get_domain_table_from_csv (test_data_dir)
} else {
  source(file.path(getwd(),"function_libraries","db_queries.R", fsep = .Platform$file.sep))
  con <- get_con(dbname=system_conf$database$dbname,
                 user=system_conf$database$user,
                 pass=system_conf$database$pass,
                 host=system_conf$database$host, 
                 port=system_conf$database$port)
  domain_table <- get_domain_table(con)
}
domains_for_run <- get_domains_for_run(domain_table,run_conf)

if (length(domains_for_run) == 0) {
  warning("No domains matched config file domain section, nothing to run on.")
} else {
  reports <- get_report_module_names(run_conf)
  
  # run the reports
  for (report in reports) {
    print(sprintf("Running report module: %s", report))
    report_file <- sprintf("%s.R", report)
    report_options <- get_report_options(run_conf,report)
    source(file.path("report_modules",report_file, fsep = .Platform$file.sep))
    
    if (debug_mode == F) {
      render(con,domains_for_run,report_options,output_dir)
    } else {
      render_debug(test_data_dir,domains_for_run,report_options,output_dir)
    }
  }
}

if (debug_mode == F) {
  close_con(con)
}


