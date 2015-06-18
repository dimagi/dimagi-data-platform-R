library(plyr)
library(dplyr)

source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
source(file.path("config_setup.R", fsep = .Platform$file.sep)) # sets the path to the run config to use
run_conf <-get_run_config(config_run_path)
system_conf <- get_system_config(file.path("config_system.json"))

source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
db <- get_db_connection(system_conf)
domain_table <- get_post_processed_domain_table(db)
domains_for_run <- get_domains_for_run(domain_table,run_conf)

if (length(domains_for_run) == 0) {
  warning("No domains matched config file domain section, nothing to run on.")
} else {
  reports <- get_report_module_names(run_conf)
  tmp_report_pdf_dir <- file.path(system_conf$directories$tmp,"report_pdfs")
  dir.create(tmp_report_pdf_dir, showWarnings = FALSE)
  
  report_pdfs <- vector()
  # run the report modules
  for (report in reports) {
    print(sprintf("Running report module: %s", report))
    report_file <- sprintf("%s.R", report)
    report_options <- get_report_options(run_conf,report)
    
    source(file.path("report_modules",report_file, fsep = .Platform$file.sep))
    module_pdfs <- render(db,domains_for_run,report_options,tmp_report_pdf_dir)
    report_pdfs <- c(report_pdfs,module_pdfs)
  }
  
  report_file_name <- file.path(system_conf$directories$output,paste(run_conf$reports$report_file_name,"pdf",sep="."))
  system2(command = "pdftk",args = c(shQuote(report_pdfs), "cat output", shQuote(report_file_name)))
}

