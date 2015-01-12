# Real-time usage report
# July 2, 2014
# The purpose of this script is to create a real-time report module for the
# data platform. This report will be based on the aggregate FLW monthly tables
# which were computed from the data platform visit tables.
# The batch entry section of this report must only be created for domains 
# that have manual form annotations for travel/home visits. 
# We can look at median visit duration and visit time of day for all domains, 
# regardless of hand annotations.
# Have not made graphs for visit time of day yet.
#------------------------------------------------------------------------------#

# Clear workspace and attach packages
# rm(list = ls())
#suppressPackageStartupMessages
detach("package:lubridate")
detach("package:plyr")
detach("package:dplyr")
library(zoo)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2) #graphing across multiple domains
library(gridExtra) #graphing plots in columns/rows for ggplot
library(RColorBrewer) #Color palettes


render <- function (db, domains_for_run, report_options, tmp_report_pdf_dir) {
  output_directory <- tmp_report_pdf_dir
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(db)
  
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  monthly_table <- get_aggregate_table (db, "aggregate_monthly_interactions", domains_for_run)
  all_monthly <- add_splitby_col(monthly_table,domain_table,report_options$split_by)
  #------------------------------------------------------------------------#
  
  #Remove demo users and NA/NONE users
  #We also need to find a way to exclude admin/unknown users
  all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
  all_monthly = all_monthly[!(all_monthly$user_id =="NONE"),]
  all_monthly = all_monthly[!(all_monthly$user_id =="none"),]
  all_monthly = all_monthly[!is.na(all_monthly$user_id),]
  
  #Remove any dates before report start_date and after report end_date
  all_monthly$date_first_visit = as.Date(all_monthly$date_first_visit)
  all_monthly$date_last_visit = as.Date(all_monthly$date_last_visit)
  start_date = as.Date(report_options$start_date)
  end_date = as.Date(report_options$end_date)
  all_monthly = subset(all_monthly, all_monthly$date_first_visit >= start_date
                       & all_monthly$date_last_visit <= end_date)
  
  #Change column names as needed
  names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
  names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
  
  #Convert calendar month to actual date
  all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
  all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
  all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)
  
  #Exclude any users that don't have a month_index = 1
  #These users have months that started outside our data range for this dataset
  #so we shouldn't include them
  all_monthly$has_index_1 <- all_monthly$month_index == 1
  user_index_1 <- all_monthly %.%
    group_by(user_pk) %.%
    summarise(keep_user = sum(has_index_1))
  user_index_1 <- filter(user_index_1, keep_user != 0)
  #Keep users that have a month_index = 1
  all_monthly <- 
    all_monthly[all_monthly$user_pk %in% user_index_1$user_pk, ]
  
  #-----------------------------------------------------------------------------#
  #CREATE PLOTS
  #-----------------------------------------------------------------------------#
  #Number of users by month_index
  #Overall dataset
  #Number of users by monthly index
  all_monthly$count_user = 1
  overall = ddply(all_monthly, .(month_index), summarise,
                  sum_user = sum(count_user, na.rm=T))

  p_users = ggplot(overall, aes(x=month_index, y=sum_user)) +
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
  
  #Batch entry % by monthly index: % of travel visits that are within < 10 mins of 
  #each other
  #The denominator is the total # of travel visits. 
  
  #By split-by
  #Calculate mean within each month_index & split-by
  overall_split = ddply(all_monthly, .(split_by, month_index), summarise,
                        batch_per_med = mean(travel_batch_percent, na.rm=T))
  maximum_ci = max(overall_split$batch_per_med, na.rm = T) + 5
  
  g_batch_med_split = (
    ggplot(data=overall_split, aes(x=month_index, y=batch_per_med)) +
      geom_line(aes(group=split_by), size=1.3, color="darkslateblue")) +
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Batch entry (%) of travel visits by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Batch entry (%) of travel visits, mean") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  # Batch entry - by month index overall
  overall = ddply(all_monthly, .(month_index), summarise,
                  batch_per_med = mean(travel_batch_percent, na.rm = T),
                  sd = sd(travel_batch_percent, na.rm=T),
                  n = sum(!is.na(travel_batch_percent)),
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
    ggplot(overall, aes(x = month_index, y = batch_per_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Batch entry (%) of travel visits by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Batch entry (%) of travel visits, mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Batch_mean.pdf")
  pdf(outfile)
  grid.arrange(g_batch_med_split, g_batch_overall, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#
  
  #Visit duration (mins) by month_index
  
  #By split-by
  #Calculate mean within each obsum & split-by
  overall_split = ddply(all_monthly, .(split_by, month_index), summarise,
                        visit_dur = mean(median_visit_duration, na.rm=T))
  maximum_ci = max(overall_split$visit_dur, na.rm = T) + 5
  
  g_visit_dur_split = (
    ggplot(data=overall_split, aes(x=month_index, y=visit_dur)) +
      geom_line(aes(group=split_by), size=1.3, color="darkslateblue")) +
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Visit duration (mins) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Median visit duration (mins), mean") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  # Visit duration - by month index overall
  overall = ddply(all_monthly, .(month_index), summarise,
                  visit_dur = mean(median_visit_duration, na.rm = T),
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
    ggplot(overall, aes(x = month_index, y = visit_dur, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visit duration (mins) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Median visit duration (mins), mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Visit_duration.pdf")
  pdf(outfile)
  grid.arrange(g_visit_dur_split, g_visit_dur_overall, nrow=2)
  dev.off() 
  module_pdfs <- c(module_pdfs,outfile)
  
  return(module_pdfs)
}

