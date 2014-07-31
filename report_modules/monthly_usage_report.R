# Monthly usage report
# July 2, 2014
# The purpose of this script is to create a monthly usage report module for the
# data platform. This report is based on the aggregate FLW monthly tables
# which are computed from the data platform visit tables.
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

render_debug <- function (test_data_dir, domains_for_run, report_options, output_dir) {
  source(file.path("function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table_from_csv (test_data_dir)
  create_monthly_usage(domain_table, domains_for_run, report_options, output_dir)
}

render <- function (con, domains_for_run, report_options, output_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(con)
  create_monthly_usage(domain_table, domains_for_run, report_options, output_dir)
}
  
create_monthly_usage <- function (domain_table, domains_for_run, report_options, output_dir) {
  output_directory <- output_dir
  read_directory <- file.path(output_directory,"aggregate_tables", fsep=.Platform$file.sep)
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  source(file.path("aggregate_tables","monthly_func.R", fsep = .Platform$file.sep))
  all_monthly <- merged_monthly_table (domains_for_run, read_directory)
  all_monthly <- add_splitby_col(monthly_merged,domain_table,report_options$split_by)
  
  #------------------------------------------------------------------------#
  
  #Remove demo users
  #We also need to find a way to exclude admin/unknown users
  all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
  
  #Remove any dates before report start_date and after report end_date
  all_monthly$first_visit_date = as.Date(all_monthly$first_visit_date)
  all_monthly$last_visit_date = as.Date(all_monthly$last_visit_date)
  start_date = as.Date(report_options$start_date)
  end_date = as.Date(report_options$end_date)
  all_monthly = subset(all_monthly, all_monthly$first_visit_date >= start_date
                       & all_monthly$last_visit_date <= end_date)
  
  #Convert calendar_month (character) to yearmon class since as.Date won't work 
  #without a day. Sort by calendar_month for each FLW and then label each 
  #month for each FLW in chronological order. 
  #The function calculates number of months of a given Date from the origin
  #Use this function to recalculate obsnum for FLWs that had weird first_visit_dates
  #for obsnum = 1. These cases threw off the rest of the obsnum calculations.
  #Now that we excluded those first_visit_dates, we are fine
  #e.g.pci-india,rmf,tns-sa
  #monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); 
                      #   lt$year*12 + lt$mon } 
  all_monthly$month.index = as.yearmon(all_monthly$month.index, "%b-%y")
  all_monthly <- all_monthly[order(all_monthly$domain, 
                                   all_monthly$user_id, all_monthly$month.index),]
  all_monthly <- ddply(all_monthly, .(domain, user_id), transform, 
                       numeric = monnb(first_visit_date))
  all_monthly <- ddply(all_monthly, .(domain, user_id), transform, 
                       diff = c(0, diff(numeric)))
  all_monthly <- ddply(all_monthly, .(domain, user_id), transform, 
                       numeric_index = cumsum(diff) + 1) 
  
  #Change column names names as needed
  names (all_monthly)[names(all_monthly) == "X"] = "row_num"
  names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
  names (all_monthly)[names(all_monthly) == "active_day_percent"] = "active_days_percent"
  names (all_monthly)[names(all_monthly) == "domain"] = "domain_char"
  names (all_monthly)[names(all_monthly) == "numeric_index"] = "obsnum"
  
  # Convert relevant indicators to percentages
  all_monthly$active_days_percent= (all_monthly$active_days_percent)*100
  #all_monthly$visits = as.numeric(all_monthly$visits)
  
  # Create new indicators as needed
  # % of unique cases followed-up (of cumulative open cases at month start)
  # Note that cum_case_registered is a cumulative of ALL cases registered by a FLW
  # TO DO: We need to change this to include only open cases belonging to a FLW in any
  # given month, otherwise the denominator will continue to inflate.
  # Will just use # of followed-up unique cases until we get the correct deonominator
  # Will also calculate total_case_modified as the sum of cases registered and unique 
  # cases followed-up, even though a case will be counted twice if registered and 
  # follow-ed up in the same month.
  # Keep these here as a reminder till we can make the necessary changes 
  all_monthly$case_fu_per = (all_monthly$follow_up_unique_case/all_monthly$cum_case_registered)*100
  all_monthly$total_cases_modified = (all_monthly$case_registered + 
                                        all_monthly$follow_up_unique_case) 
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
  
  #By split-by
  #Violin plots: Compare density estimates of split-by groups
  #Note that depending the adjust (smoothing), it might appear that the first month
  #has less users than other month, though this definitely can't be the case.
  p_violin_users = ggplot(all_monthly, aes(x=split_by, y=obsnum)) +
    geom_violin(scale = "count", adjust = .6, fill = "lightblue1") + 
    #geom_boxplot(width = .1, fill = "burlywood4", outlier.colour = NA) +
    #stat_summary(fun.y=median, geom = "point", fill="moccasin", shape = 21, 
                 #size = 2.5) +
    ggtitle("Density distribution of users by monthly index") +
    theme(plot.title = element_text(size=14, face="bold"))
  #-----------------------------------------------------------------------------#
  #PRINT PLOTS AND EXPORT TO PDF
  #-----------------------------------------------------------------------------#
  require(gridExtra)
  report_output_dir <- file.path(output_dir, "domain platform reports")
  dir.create(report_output_dir, showWarnings = FALSE)
  
  outfile <- file.path(report_output_dir,"Number_users.pdf")
  pdf(outfile)
  grid.arrange(p_users, p_violin_users, nrow=2)
  dev.off()

  #-----------------------------------------------------------------------------#
  #Visits by obsnum
  
  #Stacked area graph
  #Sum of visits by split-by by obsnum
  #First sum visits across each split-by for each obsnum
  overall = ddply(all_monthly, .(split_by, obsnum), summarise,
                  sum_visits = sum(visits, na.rm=T))
  #Create stacked area graph
  #Figure out how to do a reverse legend here
  g_stacked_visits = ggplot(overall, aes(x=obsnum, y=sum_visits, fill=split_by)) +
    geom_area() +
    scale_x_continuous(limits = c(0, max(all_monthly$obsnum))) +
    scale_fill_brewer(palette = "Accent") +
    ggtitle("Total visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold"))
  #                      breaks=rev(levels(overall$domain_char)))
  
  outfile <- file.path(report_output_dir,"Number_visits_total.pdf")
  pdf(outfile)
  grid.arrange(g_stacked_visits, nrow=1)
  dev.off()
  
  #By split-by
  #Calculate median within each obsum & split-by level
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                  visits_med = median(visits, na.rm=T))
  maximum_ci = max(overall_split$visits_med, na.rm = T) + 5
  
  g_visits_med_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=visits_med)) +
      geom_line(aes(group=split_by, colour=split_by), size=1.3)) +
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  # Visits - by month index overall
  overall = ddply(all_monthly, .(obsnum), summarise,
                  visits_med = median(visits, na.rm = T),
                  sd = sd(visits, na.rm=T),
                  n = sum(!is.na(visits)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  maximum_ci = max(overall$visits_med, na.rm = T) + 5
  assign("maximum_ci", maximum_ci, envir=globalenv())
  assign("overall", overall, envir=globalenv())

  over_min_logical <- sapply(overall$visits_med - overall$ci95, 
                             function(x) 
                               {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  over_max_logical <- sapply(overall$visits_med + overall$ci95, 
                             function(x, maximum_ci) 
                               {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)}, maximum_ci)
  assign("over_min_logical",over_min_logical,envir=globalenv())
  assign("over_max_logical",over_max_logical,envir=globalenv())
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$visits_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$visits_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  assign("over_min",over_min,envir=globalenv())
  assign("over_max",over_max,envir=globalenv())
  
  
  g_visits_overall = 
    ggplot(overall, aes(x = obsnum, y = visits_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Visits_median.pdf")
  pdf(outfile)
  grid.arrange(g_visits_overall, g_visits_med_split, nrow=2)
  dev.off()
  
  #-----------------------------------------------------------------------------#
  #active_days_per_month by obsnum
  
  #By split-by
  #Calculate median within each obsum & domain level
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                        active_days_med = median(active_days_per_month, na.rm=T))
  maximum_ci = max(overall_split$active_days_med, na.rm = T) + 5

  g_active_days_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=active_days_med)) +
      geom_line(aes(group=split_by, colour=split_by), size=1.3)) + 
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Active days (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Active days (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  # Active days per month - overall by month index
  overall = ddply(all_monthly, c("obsnum"), summarise,
                  act_days_med = median(active_days_per_month, na.rm = T),
                  sd = sd(active_days_per_month, na.rm=T),
                  n = sum(!is.na(active_days_per_month)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  maximum_ci = max(overall$act_days_med, na.rm = T) + 5
  assign("maximum_ci", maximum_ci, envir=globalenv())
  assign("overall", overall, envir=globalenv())
  
  over_min_logical <- sapply(overall$act_days_med - overall$ci95, 
                             function(x) 
                             {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  over_max_logical <- sapply(overall$act_days_med + overall$ci95, 
                             function(x, maximum_ci) 
                             {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)}, maximum_ci)
  assign("over_min_logical",over_min_logical,envir=globalenv())
  assign("over_max_logical",over_max_logical,envir=globalenv())
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$act_days_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$act_days_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  assign("over_min",over_min,envir=globalenv())
  assign("over_max",over_max,envir=globalenv())
  
  g_active_days_overall = 
    ggplot(overall, aes(x = obsnum, y = act_days_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Active days (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Active days (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Active_days_median.pdf")
  pdf(outfile)
  grid.arrange(g_active_days_overall, g_active_days_split, nrow=2)
  dev.off()
  
  #-----------------------------------------------------------------------------#
  
  #median_visits_per_active_day by obsnum
  
  #By multiple split-by
  #Calculate median within each obsum & split-by level. This creates an array.
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                        visits_active_day_med = median(median_visits_per_active_day, na.rm=T))
  maximum_ci = max(overall_split$visits_active_day_med, na.rm = T) + 5
  
  g_visits_per_day_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=visits_active_day_med)) +
      geom_line(aes(group=split_by, colour=split_by), size = 1.3)) +
    scale_y_continuous(limits = c(0, maximum_ci)) +
    ggtitle("Visits per active day (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits per active day (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold")) 
  
  # Visits per active day - by month index
  overall = ddply(all_monthly, .(obsnum), summarise,
                  visits_active_day_med = median(median_visits_per_active_day, 
                                                 na.rm = T),
                  sd = sd(median_visits_per_active_day, na.rm=T),
                  n = sum(!is.na(median_visits_per_active_day)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  maximum_ci = max(overall$visits_active_day_med, na.rm = T) + 5
  assign("maximum_ci", maximum_ci, envir=globalenv())
  assign("overall", overall, envir=globalenv())
  
  over_min_logical <- sapply(overall$visits_active_day_med - overall$ci95, 
                             function(x) 
                             {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  over_max_logical <- sapply(overall$visits_active_day_med + overall$ci95, 
                             function(x, maximum_ci) 
                             {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)}, maximum_ci)
  assign("over_min_logical",over_min_logical,envir=globalenv())
  assign("over_max_logical",over_max_logical,envir=globalenv())
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$visits_active_day_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$visits_active_day_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  assign("over_min",over_min,envir=globalenv())
  assign("over_max",over_max,envir=globalenv())
  
  g_visits_per_day_overall = 
    ggplot(overall, aes(x = obsnum, y = visits_active_day_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visits per active day (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Median visits per active day (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Visits_per_day_median.pdf")
  pdf(outfile)
  grid.arrange(g_visits_per_day_overall, g_visits_per_day_split, nrow=2)
  dev.off()
  
  #-----------------------------------------------------------------------------#
  
  # Cases modified (registered+followed-up) by obsnum
  #By split-by
  
  #Calculate median within each obsum & split-by level
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                        cases_mod_med = median(total_cases_modified, na.rm=T))
  maximum_ci = max(overall_split$cases_mod_med, na.rm = T) + 5
  
  g_cases_mod_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=cases_mod_med)) +
      geom_line(aes(group=split_by, colour=split_by), size=1.3)) +
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Cases modified (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases modified (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  # Cases modified - by month index overall
  overall = ddply(all_monthly, .(obsnum), summarise,
                  cases_mod_med = median(total_cases_modified, na.rm = T),
                  sd = sd(total_cases_modified, na.rm=T),
                  n = sum(!is.na(total_cases_modified)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  maximum_ci = max(overall$cases_mod_med, na.rm = T) + 5
  assign("maximum_ci", maximum_ci, envir=globalenv())
  assign("overall", overall, envir=globalenv())
  
  over_min_logical <- sapply(overall$cases_mod_med - overall$ci95, 
                             function(x) 
                             {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  over_max_logical <- sapply(overall$cases_mod_med + overall$ci95, 
                             function(x, maximum_ci) 
                             {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)}, maximum_ci)
  assign("over_min_logical",over_min_logical,envir=globalenv())
  assign("over_max_logical",over_max_logical,envir=globalenv())
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$cases_mod_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$cases_mod_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  assign("over_min",over_min,envir=globalenv())
  assign("over_max",over_max,envir=globalenv())
  
  g_cases_mod_overall = 
    ggplot(overall, aes(x = obsnum, y = cases_mod_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases modified (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases modified (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Cases_modified_median.pdf")
  pdf(outfile)
  grid.arrange(g_cases_mod_overall, g_cases_mod_split, nrow=2)
  dev.off()
  
  #-----------------------------------------------------------------------------#
  
  #Registered cases (#) by obsnum
  
  #Stacked area graph
  #Sum of registered cases by domain by obsnum
  #First sum registered across each domain for each obsnum
  overall = ddply(all_monthly, .(split_by, obsnum), summarise,
                  sum_registered = sum(case_registered, na.rm=T))
  #Create stacked area graph
  #Figure out how to do a reverse legend here
  g_case_reg_sum = ggplot(overall, aes(x=obsnum, y=sum_registered, 
                                       fill=split_by)) + 
    geom_area() +
    scale_x_continuous(limits = c(0, max(all_monthly$obsnum))) +
    scale_fill_brewer(palette = "YlOrRd") +
    ggtitle("Total registered cases (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold"))
  
  outfile <- file.path(report_output_dir,"Number_cases_reg_total.pdf")
  pdf(outfile)
  grid.arrange(g_case_reg_sum, nrow=1)
  dev.off()
  
  #By multiple domains
  #Calculate median within each obsum & split-by level
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                        reg_med = median(case_registered, na.rm=T))
  maximum_ci = max(overall_split$reg_med, na.rm = T) + 5
  
  g_reg_med_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=reg_med)) +
      geom_line(aes(group=split_by, colour=split_by), size = 1.3)) + 
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Registered cases (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Registered cases (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold")) 
  
  # Cases registered overall - by month index
  overall = ddply(all_monthly, .(obsnum), summarise,
                  reg_med = median(case_registered, na.rm = T),
                  sd = sd(case_registered, na.rm=T),
                  n = sum(!is.na(case_registered)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  maximum_ci = max(overall$reg_med, na.rm = T) + 5
  assign("maximum_ci", maximum_ci, envir=globalenv())
  assign("overall", overall, envir=globalenv())
  
  over_min_logical <- sapply(overall$reg_med - overall$ci95, 
                             function(x) 
                             {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  over_max_logical <- sapply(overall$reg_med + overall$ci95, 
                             function(x, maximum_ci) 
                             {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)}, maximum_ci)
  assign("over_min_logical",over_min_logical,envir=globalenv())
  assign("over_max_logical",over_max_logical,envir=globalenv())
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$reg_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$reg_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  assign("over_min",over_min,envir=globalenv())
  assign("over_max",over_max,envir=globalenv())
  
  g_reg_med_overall = 
    ggplot(overall, aes(x = obsnum, y = reg_med, 
                        ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases registered (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases registered (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Registered_cases.pdf")
  pdf(outfile)
  grid.arrange(g_reg_med_overall, g_reg_med_split, nrow=2)
  dev.off()
  
  #-----------------------------------------------------------------------------#
  
  #Unique cases followed-up (#) by obsnum (prefer % of total cumulative open cases)
  #The denominator in this measure needs to be developed: need to count open cases 
  #ONLY and exclude all cases that were closed in the previous month
  
  #By multiple domains
  #Calculate median within each obsum & split-by level
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                        case_fu_med = median(follow_up_unique_case, na.rm=T))
  maximum_ci = max(overall_split$case_fu_med, na.rm = T) + 5
  
  g_case_fu_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=case_fu_med)) +
      geom_line(aes(group=split_by, colour=split_by), size = 1.3)) + 
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Unique cases followed-up (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Unique cases followed-up (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  # Cases followed-up overall - by month index
  overall = ddply(all_monthly, .(obsnum), summarise,
                  case_fu_med = median(follow_up_unique_case, na.rm = T),
                  sd = sd(follow_up_unique_case, na.rm=T),
                  n = sum(!is.na(follow_up_unique_case)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  maximum_ci = max(overall$case_fu_med, na.rm = T) + 5
  assign("maximum_ci", maximum_ci, envir=globalenv())
  assign("overall", overall, envir=globalenv())
  
  over_min_logical <- sapply(overall$case_fu_med - overall$ci95, 
                             function(x) 
                             {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  over_max_logical <- sapply(overall$case_fu_med + overall$ci95, 
                             function(x, maximum_ci) 
                             {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)}, maximum_ci)
  assign("over_min_logical",over_min_logical,envir=globalenv())
  assign("over_max_logical",over_max_logical,envir=globalenv())
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$case_fu_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$case_fu_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  assign("over_min",over_min,envir=globalenv())
  assign("over_max",over_max,envir=globalenv())
  
  g_case_fu_overall = 
    ggplot(overall, aes(x = obsnum, y = case_fu_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases followed-up (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases followed-up (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Cases_followed_up.pdf")
  pdf(outfile)
  grid.arrange(g_case_fu_overall, g_case_fu_split, nrow=2)
  dev.off()
 

}

#-----------------------------------------------------------------------------#

