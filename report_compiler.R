setwd("/home/rstudio/workspace/dimagi-data-platform-R/")
source(file.path(getwd(),"function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))

run_conf <-get_run_config(getwd())
system_conf <- get_system_config(getwd())

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
reports <- get_report_module_names(run_conf)
# run the reports
for (report in reports) {
  report_file <- sprintf("%s.R", report)
  report_options <- get_report_options(run_conf,report)
  
  # we should call the render() method for each report and pass params
  # params for render_debug are domains_for_run, test_data_dir, report_options, output_dir
  # params for render are con, domains_for_run, report_options, output_dir
  # for now just make sure we have set whatever variables it needs and source it.
  source(file.path(getwd(),"report_modules",report_file, fsep = .Platform$file.sep))
}

if (debug_mode == F) {
  close_con(con)
}
