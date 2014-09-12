library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(scales)

render <- function (db,domains_for_run,report_options,tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(db$con)
  
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  monthly_table <- get_aggregate_table (db, "aggregate_interactions_monthly", domains_for_run)
  
  module_pdfs <- create_test_report(domain_table, monthly_table, report_options,tmp_report_pdf_dir)
  return(module_pdfs)
}

create_test_report <- function (domain_table, monthly_table, report_options,tmp_report_pdf_dir) {

  report_data <- add_splitby_col(monthly_table,domain_table,report_options$split_by)
  grouped <- group_by(report_data, split_by)
  overall <- summarise(grouped, count = n(), sum_visits = sum(nvisits, na.rm=T))
  
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