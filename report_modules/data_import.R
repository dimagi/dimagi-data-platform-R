#The purpose of this code is import the all_monthly dataset as defined in the config_run file
#specified in config_setup.R

library(zoo) #work with mm/yy calendar dates without day
library(lubridate)

#------------------------------------------------------------------------#
#DATA IMPORT
#------------------------------------------------------------------------#

render_debug <- function (test_data_dir, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  source(file.path("function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table_from_csv (test_data_dir)
  module_pdfs <- create_monthly_usage(domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir)
  return(module_pdfs)
}

render <- function (con, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(con)
  module_pdfs <- create_monthly_usage(domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir)
  return(module_pdfs)
}

create_monthly_usage <- function (domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  output_directory <- tmp_report_pdf_dir
  read_directory <- aggregate_tables_dir
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  source(file.path("aggregate_tables","monthly_func.R", fsep = .Platform$file.sep))
  all_monthly <- merged_monthly_table (domains_for_run, read_directory)
  all_monthly <- add_splitby_col(all_monthly,domain_table,report_options$split_by)
  
  write.csv(all_monthly, file.path(output_directory, "all_monthly.csv", fsep = .Platform$file.sep))
  
}