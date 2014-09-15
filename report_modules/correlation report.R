library(Hmisc)

# import and merge monthly usage data per domain (at this stage we are not doing domain specific correlations)
render <- function (db,domains_for_run,report_options,tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(db)
  
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  all_monthly <- get_aggregate_table (db, "aggregate_monthly_interactions", domains_for_run)
  
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
  write.csv(Rnew, file.path(tmp_report_pdf_dir, "correlation table.csv", fsep = .Platform$file.sep))

}