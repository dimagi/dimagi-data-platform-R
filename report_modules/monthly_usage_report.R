# Monthly usage report
# July 2, 2014
# The purpose of this script is to create a monthly usage report module for the
# data platform. This report is based on the aggregate FLW monthly tables
# which are computed from the data platform visit tables.
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
library(scales) #to customize ggplot axis labeling
library(gridExtra) #graphing plots in columns/rows for ggplot
library(RColorBrewer) #Color palettes
library(knitr) #for appending pdfs

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
    scale_x_continuous(breaks=c(seq(0, max(all_monthly$month_index), by = 10))) + 
    xlab("Month index") +
    ylab("Total # of users") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold")) + 
    ggtitle("Number of users by month index") +
    theme(plot.title = element_text(size=14, face="bold"))
  
  #By split-by for # of categories < = 10
  #Violin plots: Compare density estimates of split-by groups
  #Note that depending the adjust (smoothing), it might appear that the first month
  #has less users than other month, though this definitely can't be the case.
  if (nlevels(all_monthly$split_by) <= 10) {
  p_violin_users = ggplot(all_monthly, aes(x=split_by, y=month_index)) +
    geom_violin(scale = "count", adjust = .6, fill = "lightblue1") + 
    ggtitle("Density distribution of users by monthly index") +
    theme(plot.title = element_text(size=14, face="bold"))
}
  
  #-----------------------------------------------------------------------------#
  #PRINT PLOTS AND EXPORT TO PDF
  #-----------------------------------------------------------------------------#
  require(gridExtra)
  module_pdfs <- list()
  report_output_dir <- file.path(tmp_report_pdf_dir, "reports")
  dir.create(report_output_dir, showWarnings = FALSE)
  
  if (nlevels(all_monthly$split_by) > 10) {
  outfile <- file.path(report_output_dir,"Number_users_monthly_usage.pdf")
  pdf(outfile)
  grid.arrange(p_users, nrow=1)
  dev.off()
} else {
  outfile <- file.path(report_output_dir,"Number_users_monthly_usage.pdf")
  pdf(outfile)
  grid.arrange(p_users, p_violin_users, nrow=2)
  dev.off()
}
  module_pdfs <- c(module_pdfs,outfile)

  #-----------------------------------------------------------------------------#
  #Visits by month_index
  
  #Stacked area graph for total number of visits
  #Sum of visits by split-by by month_index
  #First sum visits across each split-by for each month_index
  overall = ddply(all_monthly, .(split_by, month_index), summarise,
                  sum_visits = sum(nvisits, na.rm=T))
  
  #First set the correct number of colors in the color scale
  #color_values = nlevels(all_monthly$split_by)
  #getPalette = colorRampPalette(brewer.pal(8, "Accent"))
  
  if (nlevels(all_monthly$split_by) > 10) {
  g_stacked_visits = ggplot(overall, aes(x=month_index, y=sum_visits, fill=split_by)) +
    geom_area() +
    scale_x_continuous(limits = c(0, max(all_monthly$month_index))) +
    scale_y_continuous(labels = comma) + 
    scale_fill_manual(values = c(rep("red", length(unique(all_monthly$split_by))))) +
    xlab("Month index") +
    ylab("Total # of visits") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14, face="bold")) +
    ggtitle("Total visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold"), 
          legend.position="none")
  } else {
    g_stacked_visits = ggplot(overall, aes(x=month_index, y=sum_visits, fill=split_by)) +
      geom_area() +
      scale_x_continuous(limits = c(0, max(all_monthly$month_index))) +
      scale_y_continuous(labels = comma) + 
      scale_fill_brewer(palette = "Accent") +
      xlab("Month index") +
      ylab("Total # of visits") +
      theme(axis.text=element_text(size=12), 
            axis.title=element_text(size=14, face="bold")) +
      ggtitle("Total visits (#) by month index") +
      theme(plot.title = element_text(size=14, face="bold"))
}
  
  outfile <- file.path(report_output_dir,"Number_visits_total.pdf")
  pdf(outfile)
  grid.arrange(g_stacked_visits, nrow=1)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #By split-by
  #Calculate mean within each month_index & split-by level
  overall_split = ddply(all_monthly, .(split_by, month_index), summarise,
                  visits_med = mean(nvisits, na.rm=T))
  maximum_ci = max(overall_split$visits_med, na.rm = T) + 5
  
if (nlevels(all_monthly$split_by) > 10) {
  g_visits_med_split = (
    ggplot(data=overall_split, aes(x=month_index, y=visits_med)) +
    geom_line(aes(group=split_by, colour = split_by), size=0.7)) + 
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    xlab("Month index") +
    ylab("Visits (#), mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14, face="bold")) +
    ggtitle("Visits (#) by month index") +
        theme(plot.title = element_text(size=14, face="bold"), 
          legend.position = "none")
    } else {
      g_visits_med_split = (
        ggplot(data=overall_split, aes(x=month_index, y=visits_med)) +
        geom_line(aes(group=split_by, colour=split_by), size=1.3)) + 
        scale_y_continuous(limits = c(0, maximum_ci)) + 
        xlab("Month index") +
        ylab("Visits (#), mean") +
        theme(axis.text=element_text(size=12), 
              axis.title=element_text(size=14, face="bold")) +
        ggtitle("Visits (#) by month index") +
        theme(plot.title = element_text(size=14, face="bold"))
    }
    
  
  # Visits - by month index overall
  overall = ddply(all_monthly, .(month_index), summarise,
                  visits_med = mean(nvisits, na.rm = T),
                  sd = sd(nvisits, na.rm=T),
                  n = sum(!is.na(nvisits)),
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
    ggplot(overall, aes(x = month_index, y = visits_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits (#), mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Visits_mean.pdf")
  pdf(outfile)
  grid.arrange(g_visits_overall, g_visits_med_split, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#
  #active_days_per_month by month_index
  
  #By split-by
  #Calculate mean within each month_index & domain level
  overall_split = ddply(all_monthly, .(split_by, month_index), summarise,
                        active_days_med = mean(active_days_per_month, na.rm=T))
  maximum_ci = max(overall_split$active_days_med, na.rm = T) + 5

  g_active_days_split = (
    ggplot(data=overall_split, aes(x=month_index, y=active_days_med)) +
      geom_line(aes(group=split_by, colour=split_by), size=1.3)) + 
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Active days (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Active days (#), mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  # Active days per month - overall by month index
  overall = ddply(all_monthly, .(month_index), summarise,
                  act_days_med = mean(active_days_per_month, na.rm = T),
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
    ggplot(overall, aes(x = month_index, y = act_days_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Active days (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Active days (#), mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Active_days_mean.pdf")
  pdf(outfile)
  grid.arrange(g_active_days_overall, g_active_days_split, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#
  
  #median_visits_per_day by month_index
  
  #By multiple split-by
  #Calculate mean within each month_index & split-by level. This creates an array.
  overall_split = ddply(all_monthly, .(split_by, month_index), summarise,
                        visits_active_day_med = mean(median_visits_per_day, na.rm=T))
  maximum_ci = max(overall_split$visits_active_day_med, na.rm = T) + 5
  
  g_visits_per_day_split = (
    ggplot(data=overall_split, aes(x=month_index, y=visits_active_day_med)) +
      geom_line(aes(group=split_by, colour=split_by), size = 1.3)) +
    scale_y_continuous(limits = c(0, maximum_ci)) +
    ggtitle("Visits per active day (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits per active day (#), mean") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold")) 
  
  # Visits per active day - by month index
  overall = ddply(all_monthly, .(month_index), summarise,
                  visits_active_day_med = mean(median_visits_per_day, 
                                                 na.rm = T),
                  sd = sd(median_visits_per_day, na.rm=T),
                  n = sum(!is.na(median_visits_per_day)),
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
    ggplot(overall, aes(x = month_index, y = visits_active_day_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visits per active day (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Median visits per active day (#), mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Visits_per_day_mean.pdf")
  pdf(outfile)
  grid.arrange(g_visits_per_day_overall, g_visits_per_day_split, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#

# Unique cases touched (#) by month_index (this includes cases registered and/or
# followed up or closed in any given month)

#By multiple domains
#Calculate mean within each month_index & split-by level
overall_split = ddply(all_monthly, .(split_by, month_index), summarise,
                      cases_touched_med = mean(ncases_touched, na.rm=T))
maximum_ci = max(overall_split$cases_touched_med, na.rm = T) + 5

g_case_touch_split = (
  ggplot(data=overall_split, aes(x=month_index, y=cases_touched_med)) +
    geom_line(aes(group=split_by, colour=split_by), size = 1.3)) + 
  scale_y_continuous(limits = c(0, maximum_ci)) + 
  ggtitle("Unique cases visited (#) by month index") +
  theme(plot.title = element_text(size=14, face="bold")) +
  xlab("Month index") +
  ylab("Unique cases visited (#), mean") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                 face="bold")) 

# Cases registered overall - by month index
overall = ddply(all_monthly, .(month_index), summarise,
                cases_touched_med = mean(ncases_touched, na.rm = T),
                sd = sd(ncases_touched, na.rm=T),
                n = sum(!is.na(ncases_touched)),
                se = sd/sqrt(n))
overall$ci95 = overall$se * qt(.975, overall$n-1)
maximum_ci = max(overall$cases_touched_med, na.rm = T) + 5
assign("maximum_ci", maximum_ci, envir=globalenv())
assign("overall", overall, envir=globalenv())

over_min_logical <- sapply(overall$cases_touched_med - overall$ci95, 
                           function(x) 
                           {if (is.na(x) | (x < -3) ) 
                             return (T) else return (F)})
over_max_logical <- sapply(overall$cases_touched_med + overall$ci95, 
                           function(x, maximum_ci) 
                           {if (is.na(x) | (x > maximum_ci) ) 
                             return (T) else return (F)}, maximum_ci)
assign("over_min_logical",over_min_logical,envir=globalenv())
assign("over_max_logical",over_max_logical,envir=globalenv())

over_min=over_min_logical
over_min[over_min_logical==F] = 
  (overall$cases_touched_med - overall$ci95)[over_min_logical==F]
over_min[over_min_logical==T] = -3

over_max=over_max_logical
over_max[over_max_logical==F] = 
  (overall$cases_touched_med + overall$ci95)[over_max_logical==F]
over_max[over_max_logical==T] = maximum_ci

assign("over_min",over_min,envir=globalenv())
assign("over_max",over_max,envir=globalenv())

g_case_touched_overall = 
  ggplot(overall, aes(x = month_index, y = cases_touched_med, 
                      ymax = maximum_ci)) + 
  geom_line(colour = "indianred1", size = 1.5) +
  geom_ribbon(aes(ymin = over_min, ymax = over_max),
              alpha = 0.2) +
  ggtitle("Unique cases visited (#) by month index") +
  theme(plot.title = element_text(size=14, face="bold")) +
  xlab("Month index") +
  ylab("Unique cases visited (#), mean") +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"))

outfile <- file.path(report_output_dir,"Unique_cases_visited.pdf")
pdf(outfile)
grid.arrange(g_case_touched_overall, g_case_touch_split, nrow=2)
dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#
  
  #Registered cases (#) by month_index
  
  #Stacked area graph
  #Sum of registered cases by domain by month_index
  #First sum registered across each domain for each month_index
  overall = ddply(all_monthly, .(split_by, month_index), summarise,
                  sum_registered = sum(ncases_registered, na.rm=T))
  #Create stacked area graph
  #Figure out how to do a reverse legend here
  g_case_reg_sum = ggplot(overall, aes(x=month_index, y=sum_registered, 
                                       fill=split_by)) + 
    geom_area() +
    scale_x_continuous(limits = c(0, max(all_monthly$month_index))) +
    scale_fill_brewer(palette = "YlOrRd") +
    ggtitle("Total registered cases (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold"))
  
  outfile <- file.path(report_output_dir,"Number_cases_reg_total.pdf")
  pdf(outfile)
  grid.arrange(g_case_reg_sum, nrow=1)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #By multiple domains
  #Calculate mean within each month_index & split-by level
  overall_split = ddply(all_monthly, .(split_by, month_index), summarise,
                        reg_med = mean(ncases_registered, na.rm=T))
  maximum_ci = max(overall_split$reg_med, na.rm = T) + 5
  
  g_reg_med_split = (
    ggplot(data=overall_split, aes(x=month_index, y=reg_med)) +
      geom_line(aes(group=split_by, colour=split_by), size = 1.3)) + 
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Registered cases (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Registered cases (#), mean") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold")) 
  
  # Cases registered overall - by month_index
  overall = ddply(all_monthly, .(month_index), summarise,
                  reg_med = mean(ncases_registered, na.rm = T),
                  sd = sd(ncases_registered, na.rm=T),
                  n = sum(!is.na(ncases_registered)),
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
    ggplot(overall, aes(x = month_index, y = reg_med, 
                        ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases registered (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases registered (#), mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Registered_cases.pdf")
  pdf(outfile)
  grid.arrange(g_reg_med_overall, g_reg_med_split, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#
  
  #Unique cases followed-up (#) by month_index
  
  #By multiple domains
  #Calculate mean within each obsum & split-by level
  overall_split = ddply(all_monthly, .(split_by, month_index), summarise,
                        case_fu_med = mean(nunique_followups, na.rm=T))
  maximum_ci = max(overall_split$case_fu_med, na.rm = T) + 5
  
  g_case_fu_split = (
    ggplot(data=overall_split, aes(x=month_index, y=case_fu_med))) +
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Unique cases followed-up (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Unique cases followed-up (#), mean") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  if (nlevels(all_monthly$split_by) > 10) {
    g_case_fu_split = g_case_fu_split + 
      geom_line(aes(group=split_by), size = 1.3, colour = "darkslateblue")
  } else {
    g_case_fu_split = g_case_fu_split + 
      geom_line(aes(group=split_by, colour=split_by), size = 1.3)
  }
   
  # Cases followed-up overall - by month index
  overall = ddply(all_monthly, .(month_index), summarise,
                  case_fu_med = mean(nunique_followups, na.rm = T),
                  sd = sd(nunique_followups, na.rm=T),
                  n = sum(!is.na(nunique_followups)),
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
    ggplot(overall, aes(x = month_index, y = case_fu_med, ymax = maximum_ci)) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases followed-up (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases followed-up (#), mean") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  outfile <- file.path(report_output_dir,"Cases_followed_up.pdf")
  pdf(outfile)
  grid.arrange(g_case_fu_overall, g_case_fu_split, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
 
}

#-----------------------------------------------------------------------------#

