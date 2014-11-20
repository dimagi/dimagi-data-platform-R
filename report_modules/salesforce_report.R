library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(googleVis)

render <- function (db,domains_for_run,report_options,tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(db)
  
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  monthly_table <- get_aggregate_table (db, "aggregate_monthly_interactions", domains_for_run)
  
  source("data_sources.R")
  sf_contracts <- get_data_source (db, "salesforce_contract_table")
  monthly_table <- merge(monthly_table,sf_contracts[,c('domain','all_time_income__c')], 
                          by = 'domain', all = F)

  overall <- monthly_table %.% group_by(domain, all_time_income__c) %.% 
                summarise(dtype = names(sort(table(summary_device_type),decreasing=TRUE)[1]), 
                          nusers = length(unique(user_id)),
                          max_visits_single_user_per_month = max(nvisits, na.rm = T))
  
  chart <- ggplot(overall, aes(x=max_visits_single_user_per_month, y=all_time_income__c)) +
    geom_point(shape=1)
  
  bubble_chart = gvisBubbleChart(overall, idvar="domain", xvar="max_visits_single_user_per_month", 
                                 yvar="all_time_income__c",sizevar="nusers", colorvar="dtype",
                  options=list(sizeAxis= "{minSize:5, maxSize:100}",
                               width=3000, height=2000, fontSize=32,
                               hAxis="{logScale:true, title: 'Max Visits For Any User in a Single Month'}",
                               vAxis= "{logScale:true, title: 'All Time Income (USD)'}"))

  outfile <- file.path(tmp_report_pdf_dir,"salesforce_report.pdf")
  pdf(outfile)
  grid.arrange(chart, nrow=1)
  dev.off()
  return (c(outfile))
}