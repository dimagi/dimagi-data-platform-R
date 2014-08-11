# Real-time usage report
# July 2, 2014
# The purpose of this script is to create a real-time report module for the
# data platform. This report will be based on the aggregate FLW monthly tables
# which were computed from the data platform visit tables.
# The batch entry section of this report must only be created for domains 
# that have manual form annotations. We can look at median visit duration and 
# visit time of day for all domains, regardless of hand annotations.
#------------------------------------------------------------------------------#

# Clear workspace and attach packages
# rm(list = ls())
#suppressPackageStartupMessages
library(zoo) #work with mm/yy calendar dates without day
library(ggplot2) #graphing across multiple domains
library(gridExtra) #graphing plots in columns/rows for ggplot
library(RColorBrewer) #Color palettes
library(plyr) #for ddply

#Need two different functions based on whether I am working with test data from the 
#test directory (render_debug) or with live data from the database connection (render)
#Debug mode is set in the config_run file.

render_debug <- function (test_data_dir, domains_for_run, report_options, aggregate_tables_dir,tmp_report_pdf_dir) {
  source(file.path("function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table_from_csv (test_data_dir)
  module_pdfs <- create_real_time(domain_table, domains_for_run, report_options, aggregate_tables_dir,tmp_report_pdf_dir)
  return(module_pdfs)
}

render <- function (con, domains_for_run, report_options, aggregate_tables_dir,tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(con)
  module_pdfs <- create_real_time(domain_table, domains_for_run, report_options, aggregate_tables_dir,tmp_report_pdf_dir)
  return(module_pdfs)
}

create_real_time <- function (domain_table, domains_for_run, report_options, aggregate_tables_dir,tmp_report_pdf_dir) {
  output_directory <- tmp_report_pdf_dir
  read_directory <- aggregate_tables_dir
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  source(file.path("aggregate_tables","monthly_func.R", fsep = .Platform$file.sep))
  monthly_merged <- merged_monthly_table (domains_for_run, read_directory)
  all_monthly <- add_splitby_col(monthly_merged,domain_table,report_options$split_by)
  
  #------------------------------------------------------------------------#
  
  #Remove demo users
  #We also need to find a way to exclude admin/unknown users
  all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
  names (all_monthly)[names(all_monthly) == "first_visit_date.x"] = "first_visit_date"
  
  #Remove any dates before report start_date and after report end_date
  all_monthly$first_visit_date = as.Date(all_monthly$first_visit_date)
  all_monthly$last_visit_date = as.Date(all_monthly$last_visit_date)
  start_date = as.Date(report_options$start_date)
  end_date = as.Date(report_options$end_date)
  all_monthly = subset(all_monthly, all_monthly$first_visit_date >= start_date
                       & all_monthly$last_visit_date <= end_date)
  
  #Convert calendar_month (character) to yearmon class since as.Date won't work 
  #without a day.
  all_monthly$month.index = as.yearmon(all_monthly$month.index, "%b %Y")
  
  #Change column names names as needed
  names (all_monthly)[names(all_monthly) == "X"] = "row_num"
  names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
  names (all_monthly)[names(all_monthly) == "active_day_percent"] = "active_days_percent"
  names (all_monthly)[names(all_monthly) == "domain"] = "domain_char"
  names (all_monthly)[names(all_monthly) == "numeric_index"] = "obsnum"
  
  all_monthly$batch_entry_percent= (all_monthly$batch_entry_percent)*100
  all_monthly$median_visit_duration = round(all_monthly$median_visit_duration/60,
                                            digits=2) 
  
  #-----------------------------------------------------------------------------#
  
  #CREATE PLOTS
  
  #-----------------------------------------------------------------------------#
  #Number of users by obsnum
  #Overall dataset
  #Number of users by monthly index
  all_monthly$count_user = 1
  overall = ddply(all_monthly, .(obsnum), summarise,
                  sum_user = sum(count_user, na.rm=T))
  #overall = ddply(all_monthly, .(domain_char), summarise,
  #               min_value = min(obsnum, na.rm=T))
  p_users = ggplot(overall, aes(x=obsnum, y=sum_user)) +
    geom_point(size = 6, shape = 19, alpha = 0.5, colour = "darkblue", 
               fill = "lightblue") +
    geom_line(colour = "darkblue") + 
    scale_size_area() +
    ggtitle("Number of users by monthly index") +
    theme(plot.title = element_text(size=14, face="bold"))
  
  #-----------------------------------------------------------------------------#
  #PRINT PLOTS AND EXPORT TO PDF
  #-----------------------------------------------------------------------------#
  require(gridExtra)
  module_pdfs <- list()
  report_output_dir <- file.path(tmp_report_pdf_dir, "reports")
  dir.create(report_output_dir, showWarnings = FALSE)
  
  outfile <- file.path(report_output_dir,"Number_users_real_time.pdf")
  pdf(outfile)
  grid.arrange(p_users, nrow=1)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#
  
  #Batch entry % by monthly index
  #Batch entry should be calculated in the monthly tables only for travel visits
  #between unrelated cases. The denominator is the total # of travel visits. 
  
  #By split-by
  #Calculate median within each obsum & split-by
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                        batch_per_med = median(batch_entry_percent, na.rm=T))
  maximum_ci = max(overall_split$batch_per_med, na.rm = T) + 5
  
  g_batch_med_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=batch_per_med)) +
      geom_line(aes(group=split_by), size=1.3, color="darkslateblue")) +
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Batch entry (%) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Batch entry (%), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  # Batch entry - by month index overall
  overall = ddply(all_monthly, .(obsnum), summarise,
                  batch_per_med = median(batch_entry_percent, na.rm = T),
                  sd = sd(batch_entry_percent, na.rm=T),
                  n = sum(!is.na(batch_entry_percent)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  maximum_ci = max(overall$batch_per_med, na.rm = T) + 5
  assign("maximum_ci", maximum_ci, envir=globalenv())
  assign("overall", overall, envir=globalenv())
  
  over_min_logical <- sapply(overall$batch_per_med - overall$ci95, 
                             function(x) 
                             {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  over_max_logical <- sapply(overall$batch_per_med + overall$ci95, 
                             function(x, maximum_ci) 
                             {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)}, maximum_ci)
  assign("over_min_logical",over_min_logical,envir=globalenv())
  assign("over_max_logical",over_max_logical,envir=globalenv())
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$batch_per_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$batch_per_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  assign("over_min",over_min,envir=globalenv())
  assign("over_max",over_max,envir=globalenv())
  
  g_batch_overall = 
    ggplot(overall, aes(x = obsnum, y = batch_per_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Batch entry (%) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Batch entry (%), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Batch_median.pdf")
  pdf(outfile)
  grid.arrange(g_batch_med_split, g_batch_overall, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#
  
  #Visit duration (mins) by obsnum
  
  #By split-by
  #Calculate median within each obsum & split-by
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                        visit_dur = median(median_visit_duration, na.rm=T))
  maximum_ci = max(overall_split$visit_dur, na.rm = T) + 5
  
  g_visit_dur_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=visit_dur)) +
      geom_line(aes(group=split_by), size=1.3, color="darkslateblue")) +
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Visit duration (mins) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Median visit duration (mins), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  # Visit duration - by month index overall
  overall = ddply(all_monthly, .(obsnum), summarise,
                  visit_dur = median(median_visit_duration, na.rm = T),
                  sd = sd(median_visit_duration, na.rm=T),
                  n = sum(!is.na(median_visit_duration)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  maximum_ci = max(overall$visit_dur, na.rm = T) + 5
  assign("maximum_ci", maximum_ci, envir=globalenv())
  assign("overall", overall, envir=globalenv())
  
  over_min_logical <- sapply(overall$visit_dur - overall$ci95, 
                             function(x) 
                             {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  over_max_logical <- sapply(overall$visit_dur + overall$ci95, 
                             function(x, maximum_ci) 
                             {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)}, maximum_ci)
  assign("over_min_logical",over_min_logical,envir=globalenv())
  assign("over_max_logical",over_max_logical,envir=globalenv())
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$visit_dur - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$visit_dur + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  assign("over_min",over_min,envir=globalenv())
  assign("over_max",over_max,envir=globalenv())
  
  g_visit_dur_overall = 
    ggplot(overall, aes(x = obsnum, y = visit_dur, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visit duration (mins) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Median visit duration (mins), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Visit_duration.pdf")
  pdf(outfile)
  grid.arrange(g_visit_dur_split, g_visit_dur_overall, nrow=2)
  dev.off() 
  module_pdfs <- c(module_pdfs,outfile)
  
  return(module_pdfs)}

