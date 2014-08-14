library(Hmisc)
# import and merge monthly usage data per domain (at this stage we are not doing domain specific correlations)
render_debug <- function (test_data_dir, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  source(file.path("function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table_from_csv (test_data_dir)
  module_pdfs <- create_monthly_correlation_report(domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir)
  return(module_pdfs)
}

render <- function (con, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(con)
  module_pdfs <- create_monthly_correlation_report(domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir)
  return(module_pdfs)
}

create_monthly_correlation_report <- function (domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  output_directory <- tmp_report_pdf_dir
  read_directory <- file.path(output_directory,"aggregate_tables", fsep=.Platform$file.sep)
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  source(file.path("aggregate_tables","monthly_func.R", fsep = .Platform$file.sep))
  all_monthly <- merged_monthly_table (domains_for_run, read_directory)
  
  #Run beginning of monthly usage report
  all_monthly$batch_entry_percent= (all_monthly$batch_entry_percent)*100
  all_monthly$median_visit_duration = round(all_monthly$median_visit_duration/60, 2)
                                            
  
  # numeric indicators to be included in correlation report 
  cont_vars <- c("visits", "nforms_per_month", "numeric_index", "days_on_cc", "total_case_touched", "median_visit_duration", 
                 "median_visits_per_active_day", "active_day_percent") # continuous variables
  cont_vars <- all_monthly[, cont_vars]
  
  
  # Correlation triangle for continuous variables (this function lives in report_utils.R)
  Rnew <- corstarsl(cont_vars) 
  print(Rnew)   
  write.csv(Rnew, file.path(output_dir, "correlation table.csv", fsep = .Platform$file.sep))

}


