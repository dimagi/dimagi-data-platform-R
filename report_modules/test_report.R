library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(scales)

render <- function (con,domains_for_run,report_options,aggregate_tables_dir,tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(con)
  module_pdfs <- create_test_report(domain_table, domains_for_run, report_options, aggregate_tables_dir,tmp_report_pdf_dir)
  return(module_pdfs)
}

render_debug <- function (test_data_dir,domains_for_run,report_options,aggregate_tables_dir,tmp_report_pdf_dir){
  source(file.path("function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table_from_csv (test_data_dir)
  module_pdfs <- create_test_report(domain_table, domains_for_run, report_options, aggregate_tables_dir)
  return(module_pdfs)
}

create_test_report <- function (domain_table, domains_for_run, report_options, aggregate_tables_dir,tmp_report_pdf_dir) {
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  monthly_merged <- merged_monthly_table (domains_for_run, aggregate_tables_dir)
  report_data <- add_splitby_col(monthly_merged,domain_table,report_options$split_by)
  
  overall <- report_data %.%
    group_by(split_by) %.%
    summarise(sum_visits = sum(visits, na.rm=T))
  
  chart <- ggplot(data=overall, aes(x=split_by, y=sum_visits, fill=split_by)) +
    geom_bar(colour="black", stat="identity") +
    guides(fill=FALSE) + xlab(report_options$split_by) + ylab("Total Visits") +
    ggtitle(sprintf("Total Visits by %s",report_options$split_by)) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ scale_y_continuous(labels = comma)
  
  outfile <- file.path(tmp_report_pdf_dir,"test_report.pdf")
  pdf(outfile)
  grid.arrange(chart, nrow=1)
  dev.off()
  return (c(outfile))
}