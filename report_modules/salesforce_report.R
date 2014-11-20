library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(scales)

render <- function (db,domains_for_run,report_options,tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(db)
  
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  lifetime_table <- get_aggregate_table (db, "aggregate_lifetime_interactions", domains_for_run)
  
  source("data_sources.R")
  sf_contracts <- get_data_source (db, "salesforce_contract_table")
  lifetime_table <- merge(lifetime_table,sf_contracts[,c('domain','all_time_income__c')], by = 'domain', all = F)

  chart <- ggplot(lifetime_table, aes(x=median_visits_per_month, y=all_time_income__c)) +
    geom_point(shape=1)
  
  outfile <- file.path(tmp_report_pdf_dir,"salesforce_report.pdf")
  pdf(outfile)
  grid.arrange(chart, nrow=1)
  dev.off()
  return (c(outfile))
}