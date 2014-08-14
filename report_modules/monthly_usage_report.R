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
library(scales) #to customize ggplot axis labeling
library(gridExtra) #graphing plots in columns/rows for ggplot
library(RColorBrewer) #Color palettes
library(knitr) #for appending pdfs

#Need two different functions based on whether I am working with test data from the 
#test directory (render_debug) or with live data from the database connection (render)
#Debug mode is set in the config_run file.

render_debug <- function (test_data_dir, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  source(file.path("function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table_from_csv (test_data_dir)
  module_pdfs <- create_monthly_usage(domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir)
  return(module_pdfs)
}

render <- function (con, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  source(file.path("function_libraries","db_queries.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table(con)
  module_pdfs <- create_monthly_usage(domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir)
  return(module_pdfs)
}
  
create_monthly_usage <- function (domain_table, domains_for_run, report_options, aggregate_tables_dir, tmp_report_pdf_dir) {
  output_directory <- tmp_report_pdf_dir
  read_directory <- aggregate_tables_dir
  source(file.path("function_libraries","report_utils.R", fsep = .Platform$file.sep))
  source(file.path("aggregate_tables","monthly_func.R", fsep = .Platform$file.sep))
  all_monthly <- merged_monthly_table (domains_for_run, read_directory)
  all_monthly <- add_splitby_col(all_monthly,domain_table,report_options$split_by)
  
  #------------------------------------------------------------------------#
  
  #Remove demo users
  #We also need to find a way to exclude admin/unknown users
  all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
 
  #Remove any dates before report start_date and after report end_date
  names (all_monthly)[names(all_monthly) == "first_visit_date.x"] = "first_visit_date"
  all_monthly$first_visit_date = as.Date(all_monthly$first_visit_date)
  all_monthly$last_visit_date = as.Date(all_monthly$last_visit_date)
  start_date = as.Date(report_options$start_date)
  end_date = as.Date(report_options$end_date)
  all_monthly = subset(all_monthly, all_monthly$first_visit_date >= start_date
                       & all_monthly$last_visit_date <= end_date)
  
  #Convert calendar_month (character) to yearmon class since as.Date won't work 
  #without a day.
  all_monthly$month.index = as.yearmon(all_monthly$month.index, "%b %Y")
  
  #Change column names as needed
  names (all_monthly)[names(all_monthly) == "X"] = "row_num"
  names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
  names (all_monthly)[names(all_monthly) == "active_day_percent"] = "active_days_percent"
  names (all_monthly)[names(all_monthly) == "domain"] = "domain_char"
  names (all_monthly)[names(all_monthly) == "numeric_index"] = "obsnum"
  
  # Convert relevant indicators to percentages
  all_monthly$active_days_percent= (all_monthly$active_days_percent)*100
  
  # Create new indicators as needed
  # TO DO: % of unique cases followed-up (of cumulative open cases at month start)
  # Note that cum_case_registered is a cumulative of ALL cases registered by a FLW
  # We need to change this to include only open cases belonging to a FLW in any
  # given month, otherwise the denominator will continue to inflate.
  # Will just use # of followed-up unique cases until we get the correct deonominator
  # Keep this here as a reminder till we can make the necessary changes 
  all_monthly$case_fu_per = (all_monthly$follow_up_unique_case/all_monthly$cum_case_registered)*100
  #-----------------------------------------------------------------------------#
  
  #CREATE PLOTS
  
  #-----------------------------------------------------------------------------#
  #Number of users by obsnum
  #Overall dataset
  #Number of users by monthly index
  all_monthly$count_user = 1
  overall = ddply(all_monthly, .(obsnum), summarise,
                  sum_user = sum(count_user, na.rm=T))

  p_users = ggplot(overall, aes(x=obsnum, y=sum_user)) +
    geom_point(size = 6, shape = 19, alpha = 0.5, colour = "darkblue", 
               fill = "lightblue") +
    geom_line(colour = "darkblue") + 
    scale_size_area() +
    scale_x_continuous(breaks=c(seq(0, max(all_monthly$obsnum), by = 10))) + 
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
  p_violin_users = ggplot(all_monthly, aes(x=split_by, y=obsnum)) +
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
  #Visits by obsnum
  
  #Stacked area graph for total number of visits
  #Sum of visits by split-by by obsnum
  #First sum visits across each split-by for each obsnum
  overall = ddply(all_monthly, .(split_by, obsnum), summarise,
                  sum_visits = sum(visits, na.rm=T))
  
  #First set the correct number of colors in the color scale
  #color_values = nlevels(all_monthly$split_by)
  #getPalette = colorRampPalette(brewer.pal(8, "Accent"))
  
  if (nlevels(all_monthly$split_by) > 10) {
  g_stacked_visits = ggplot(overall, aes(x=obsnum, y=sum_visits, fill=split_by)) +
    geom_area() +
    scale_x_continuous(limits = c(0, max(all_monthly$obsnum))) +
    scale_y_continuous(labels = comma) + 
    #scale_fill_brewer(getPalette(color_values)) +
    xlab("Month index") +
    ylab("Total # of visits") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14, face="bold")) +
    ggtitle("Total visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold"), 
          legend.position="none")
  } else {
    g_stacked_visits = ggplot(overall, aes(x=obsnum, y=sum_visits, fill=split_by)) +
      geom_area() +
      scale_x_continuous(limits = c(0, max(all_monthly$obsnum))) +
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
  #Calculate median within each obsum & split-by level
  overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                  visits_med = median(visits, na.rm=T))
  maximum_ci = max(overall_split$visits_med, na.rm = T) + 5
  
if (nlevels(all_monthly$split_by) > 10) {
  g_visits_med_split = (
    ggplot(data=overall_split, aes(x=obsnum, y=visits_med)) +
    geom_line(aes(group=split_by, colour = split_by), size=0.7)) + 
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    xlab("Month index") +
    ylab("Visits (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14, face="bold")) +
    ggtitle("Visits (#) by month index") +
        theme(plot.title = element_text(size=14, face="bold"), 
          legend.position = "none")
    } else {
      g_visits_med_split = (
        ggplot(data=overall_split, aes(x=obsnum, y=visits_med)) +
        geom_line(aes(group=split_by, colour=split_by), size=1.3)) + 
        scale_y_continuous(limits = c(0, maximum_ci)) + 
        xlab("Month index") +
        ylab("Visits (#), median") +
        theme(axis.text=element_text(size=12), 
              axis.title=element_text(size=14, face="bold")) +
        ggtitle("Visits (#) by month index") +
        theme(plot.title = element_text(size=14, face="bold"))
    }
    
  
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
  module_pdfs <- c(module_pdfs,outfile)
  
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
  overall = ddply(all_monthly, .(obsnum), summarise,
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
  module_pdfs <- c(module_pdfs,outfile)
  
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
  module_pdfs <- c(module_pdfs,outfile)
  
  #-----------------------------------------------------------------------------#

# Unique cases touched (#) by obsnum (this includes cases registered and/or
# followed up in any given month)

#By multiple domains
#Calculate median within each obsum & split-by level
overall_split = ddply(all_monthly, .(split_by, obsnum), summarise,
                      cases_touched_med = median(total_case_touched, na.rm=T))
maximum_ci = max(overall_split$cases_touched_med, na.rm = T) + 5

g_case_touch_split = (
  ggplot(data=overall_split, aes(x=obsnum, y=cases_touched_med)) +
    geom_line(aes(group=split_by, colour=split_by), size = 1.3)) + 
  scale_y_continuous(limits = c(0, maximum_ci)) + 
  ggtitle("Unique cases visited (#) by month index") +
  theme(plot.title = element_text(size=14, face="bold")) +
  xlab("Month index") +
  ylab("Unique cases visited (#), median") +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                 face="bold")) 

# Cases registered overall - by month index
overall = ddply(all_monthly, .(obsnum), summarise,
                cases_touched_med = median(total_case_touched, na.rm = T),
                sd = sd(total_case_touched, na.rm=T),
                n = sum(!is.na(total_case_touched)),
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
  ggplot(overall, aes(x = obsnum, y = cases_touched_med, 
                      ymax = maximum_ci)) + 
  geom_line(colour = "indianred1", size = 1.5) +
  geom_ribbon(aes(ymin = over_min, ymax = over_max),
              alpha = 0.2) +
  ggtitle("Unique cases visited (#) by month index") +
  theme(plot.title = element_text(size=14, face="bold")) +
  xlab("Month index") +
  ylab("Unique cases visited (#), median") +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"))

outfile <- file.path(report_output_dir,"Unique_cases_visited.pdf")
pdf(outfile)
grid.arrange(g_case_touched_overall, g_case_touch_split, nrow=2)
dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
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
  module_pdfs <- c(module_pdfs,outfile)
  
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
  module_pdfs <- c(module_pdfs,outfile)
  
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
    ggplot(data=overall_split, aes(x=obsnum, y=case_fu_med))) +
    scale_y_continuous(limits = c(0, maximum_ci)) + 
    ggtitle("Unique cases followed-up (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Unique cases followed-up (#), median") +
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
  module_pdfs <- c(module_pdfs,outfile)
 
}

#-----------------------------------------------------------------------------#

