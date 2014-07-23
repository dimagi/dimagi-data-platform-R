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

d1 = domains_for_run[1:8]
d2 = domains_for_run[10:14]
d3 = domains_for_run[20:23]
domains_for_run = append(d1,d2)
domains_for_run = append(domains_for_run,d3)
domains_for_run = domains_for_run[-10]

render_debug <- function (test_data_dir, domains_for_run, report_options, output_dir) {
  
  source(file.path(getwd(),"function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table_from_csv (test_data_dir)
  output_directory <- output_dir
  read_directory <- file.path(output_directory,"aggregate_tables", fsep=.Platform$file.sep)

  #Read in each individual aggregate table for each domain. Merge all together
  #Initialize all_monthly table with the first domain_for_run
  filename = file.path(read_directory, domains_for_run[1], "monthly.csv", fsep = .Platform$file.sep)
  all_monthly = read.csv(file=filename, header = TRUE, as.is = TRUE)
  all_monthly = all_monthly[,order(names(all_monthly))]
  #Import each of the other tables one by one and rbind to initialized all_monthly
  for (i in 1:length(domains_for_run)) {
    filename = file.path(read_directory, domains_for_run[i], "monthly.csv", fsep = .Platform$file.sep)
    import = read.csv(file=filename, header = TRUE, as.is = TRUE)
    import = import[,order(names(import))]
    names(import) = names(all_monthly) 
    if (i > 1) {all_monthly = rbind(all_monthly, import)}
  }
  
  # Set my working directories and load local test data here
  # Work laptop
  #read_directory = "/Users/Rashmi/Dropbox (Dimagi)/R analysis/Rashmi/Sample monthly tables/"
  #output_directory = "C:/Users/Rashmi/Dropbox (Dimagi)/R analysis/Visuals/DP demo/July 7 2014"
  # Home laptop
  # read_directory = "/Users/rdayalu/Dropbox (Dimagi)/R analysis/Rashmi/Sample monthly tables/"
  # output_directory = "/Users/rdayalu/Dropbox (Dimagi)/R analysis/Visuals/DP demo/July 7 2014"
  # Read in monthly tables - merged table
  #filename = paste(read_directory, "monthly_merge.csv", sep="")
  #all_monthly = read.csv(file=filename, header = TRUE, as.is = TRUE)
  # setwd(output_directory)
  # Give full path rather than setwd
  # fullpath <- file.path(output_directory,filename,fsep=.Platform$file.sep)
  # write.csv(dat,fullpath)
  #------------------------------------------------------------------------#
  
  #Remove demo users
  #We also need to find a way to exclude admin/unknown users
  all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
  all_monthly = all_monthly[!(all_monthly$obsnum==136),]
  
  #Change column names names as needed
  colnames(all_monthly)[32] = "row_num"
  colnames(all_monthly)[19] = "calendar_month"
  colnames(all_monthly)[1] = "active_days_percent"
  colnames(all_monthly)[28] = "obsnum"
  colnames(all_monthly)[11] = "domain_char"
  
  # Convert relevant indicators to percentages
  all_monthly$active_days_percent= (all_monthly$active_days_percent)*100
  #all_monthly$visits = as.numeric(all_monthly$visits)
  
  #Convert calendar_month (character) to yearmon class since as.Date won't work 
  #without a day. Sort by calendar_month for each FLW and then label each 
  #month for each FLW in chronological order. 
  #all_monthly$calendar_month = as.yearmon(all_monthly$calendar_month, "%b-%y")
  
  #Create monthly index for each FLW
  #Note: The code below doesn't care if an FLW has an inactive month. Only the
  #months with observations are counted, so no FLW will have any gaps, which
  #isn't correct. Mengji has rewritten this code.
  # all_monthly = all_monthly[order(all_monthly$user_id,all_monthly$calendar_month),]
  # all_monthly$obsnum = sequence(rle(all_monthly$user_id)$lengths)
  
  #Create a new domain variable with a character in the front so that it 
  #can be used as a column name later without a problem
  #all_monthly$domain_char = paste("d", all_monthly$domain.index, sep="")
  
  #Create new indicators as needed
  # % of unique cases followed-up (of cumulative open cases at month start)
  # Note that cum_case_registered is a cumulative of ALL cases registered by a FLW
  # We need to change this to include only open cases belonging to a FLW in any
  # given month, otherwise the denominator will continue to inflate.
  # Will just use # of followed-up unique cases until we get the correct deonominator
  # Keep this here as a reminder till that happens 
  all_monthly$case_fu_per = (all_monthly$follow_up_unique_case/all_monthly$cum_case_registered)*100
  
  # Extract unique levels from obsnum and domain_char
  f1 = as.factor(all_monthly$obsnum)
  obsnum_levels = as.numeric(levels(f1))
  f2 = as.factor(all_monthly$domain_char)
  domain_levels = levels(f2)
  #-----------------------------------------------------------------------------#
  
  #CREATE PLOTS
  
  #-----------------------------------------------------------------------------#
  #Number of users by obsnum
  #Overall dataset
  #2-D density plot of number of users by monthly index
  all_monthly$count_user = 1
  overall = ddply(all_monthly, .(obsnum), summarise,
                  sum_user = sum(count_user, na.rm=T))
  # p = ggplot(overall, aes(x=obsnum, y = sum_user))
  # p + geom_point() + stat_density2d()
  # p + stat_density2d(aes(fill=..density..), geom = "tile", contour = F)
  p_users = ggplot(overall, aes(x=obsnum, y=sum_user)) +
    geom_point(size = 6, shape = 19, alpha = 0.5, colour = "darkblue", 
               fill = "lightblue") +
    geom_line(colour = "darkblue") + 
    scale_size_area()
  
  #By split-by
  #Violin plots: Compare density estimates of split-by groups
  #Note that depending the adjust (smoothing), it might appear that the first month
  #has less users than other month, though this definitely can't be the case.
  p_violin_users = ggplot(all_monthly, aes(x=domain_char, y=obsnum)) +
    geom_violin(scale = "count", adjust = .6, fill = "lightblue1") + 
    geom_boxplot(width = .1, fill = "burlywood4", outlier.colour = NA) +
    stat_summary(fun.y=median, geom = "point", fill="moccasin", shape = 21, 
                 size = 2.5)
  
  #-----------------------------------------------------------------------------#
  #Visits by obsnum
  
  #Stacked area graph
  #Sum of visits by domain by obsnum
  #First sum visits across each domain for each obsnum
  overall = ddply(all_monthly, .(domain_char, obsnum), summarise,
                  sum_visits = sum(visits, na.rm=T))
  #Create stacked area graph
  #Figure out how to do a reverse legend here
  g_stacked_visits = ggplot(overall, aes(x=obsnum, y=sum_visits, fill=domain_char)) +
    geom_area() #+
    #scale_fill_brewer(palette = "Accent") #, 
  #                      breaks=rev(levels(overall$domain_char)))
  
  # Visits - by month index overall
  overall = ddply(all_monthly, .(obsnum), summarise,
                  visits_med <- median(visits, na.rm = T),
                  sd = sd(visits, na.rm=T),
                  n = sum(!is.na(visits)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  
  over_min_logical = (overall$visits_med - overall$ci95) < -3
  over_max_logical = (overall$visits_med + overall$ci95) > 
    (max(df$visits_med, na.rm = T) + 5)
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$visits_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$visits_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = (max(df$visits_med, na.rm = T) + 5)
  
  
  g_visits_overall = 
    ggplot(overall, aes(x = obsnum, y = visits_med, 
                        ymax = (max(df$visits_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  
  #By multiple domains
  #Calculate median within each obsum & domain level. This creates an array
  array_active = with(all_monthly, tapply(visits, 
                                          list(obsnum, domain_char), median, 
                                          na.rm = T))
  #Convert array to a data frame
  df_intermediate = as.data.frame(array_active)
  
  # Initialize vector and then append all medians together
  vector_median = c() 
  for (i in 1:(length(domain_levels))) {
    vector_median = append(vector_median, df_intermediate[,i])
  }
  # Convert vector to dataframe - add domain numbers and obsnum back in, based on
  # the obsnum_levels and domain_levels 
  df = as.data.frame(vector_median)
  colnames(df) = c("visits_med")
  df$domain_num = rep(1:length(domain_levels), each = length(obsnum_levels))   
  df$obsnum = rep(1:length(obsnum_levels), length(domain_levels))
  df$domain_num <- as.factor(df$domain_num)
  g_visits_med_domain = (
    ggplot(data=df, aes(x=obsnum, y=visits_med)) +
      geom_line(aes(group=domain_num, colour=domain_num))) +
    scale_y_continuous(limits = c(0, (max(df$visits_med, na.rm = T) + 5))) + 
    ggtitle("Visits (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  #-----------------------------------------------------------------------------#
  
  #active_days_per_month by obsnum
  
  # Active days per month - overall by month index
  overall = ddply(all_monthly, c("obsnum"), summarise,
                  act_days_med = median(active_days_per_month, na.rm = T),
                  sd = sd(active_days_per_month, na.rm=T),
                  n = sum(!is.na(active_days_per_month)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  
  g_active_days_overall = 
    ggplot(overall, aes(x = obsnum, y = act_days_med, 
                        ymax = (max(df$active_days_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = act_days_med - ci95, ymax = act_days_med + ci95),
                alpha = 0.2) +
    ggtitle("Active days (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Active days (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  #By multiple domains
  #Calculate median within each obsum & domain level. This creates an array.
  array_active = with(all_monthly, tapply(active_days_per_month, 
                                          list(obsnum, domain_char), median, 
                                          na.rm = T))
  #Convert array to a data frame
  df_intermediate = as.data.frame (array_active)
  # Initialize a vector and then append all medians together in this vector
  vector_median = c() 
  for (i in 1:length(domain_levels)) {
    vector_median = append(vector_median, df_intermediate[,i])
  }
  # Convert vector to dataframe - add domain numbers and obsnum back in, based on
  # the obsnum_levels and domain_levels
  df = as.data.frame (vector_median)
  colnames(df) = c("active_days_med")
  df$domain_num = rep(1:length(domain_levels), each = length(obsnum_levels))   
  df$obsnum = rep(1:length(obsnum_levels), length(domain_levels))
  df$domain_num <- as.factor(df$domain_num)
  g_active_days_domains = (
    ggplot(data=df, aes(x=obsnum, y=active_days_med)) +
      geom_line(aes(group=domain_num, colour=domain_num))) + 
    scale_y_continuous(limits = c(0, (max(df$active_days_med, na.rm = T) + 5))) + 
    ggtitle("Active days (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Active days (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold")) 
  
  #-----------------------------------------------------------------------------#
  
  #median_visits_per_active_day by obsnum
  
  # Visits per active day - by month index
  
  overall = ddply(all_monthly, .(obsnum), summarise,
                  visits_active_day_med = median(median_visits_per_active_day, 
                                                 na.rm = T),
                  sd = sd(median_visits_per_active_day, na.rm=T),
                  n = sum(!is.na(median_visits_per_active_day)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  
  over_min_logical = (overall$visits_active_day_med - overall$ci95) < -3
  over_max_logical = (overall$visits_active_day_med + overall$ci95) > 
    (max(df$visits_active_day_med, na.rm = T) + 5)
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$visits_active_day_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$visits_active_day_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = (max(df$visits_active_day_med, na.rm = T) + 5)
  
  
  g_visits_active_overall = 
    ggplot(overall, aes(x = obsnum, y = visits_active_day_med, 
                        ymax = (max(df$visits_active_day_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Visits per active day (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Median visits per active day (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  #By multiple domains
  #Calculate median within each obsum & domain level. This creates an array.
  array_active = with(all_monthly, tapply(median_visits_per_active_day, 
                                          list(obsnum, domain_char), median, 
                                          na.rm = T))
  #Convert array to a data frame
  df_intermediate = as.data.frame (array_active)
  # Initialize a vector and then append all medians together in this vector
  vector_median = c() 
  for (i in 1:length(domain_levels)) {
    vector_median = append(vector_median, df_intermediate[,i])
  }
  # Convert vector to dataframe - add domain numbers and obsnum back in, based on
  # the obsnum_levels and domain_levels
  df = as.data.frame (vector_median)
  colnames(df) = c("visits_active_day_med")
  df$domain_num = rep(1:length(domain_levels), each = length(obsnum_levels))   
  df$obsnum = rep(1:length(obsnum_levels), length(domain_levels))
  df$domain_num <- as.factor(df$domain_num)
  g_visits_active_day_domains = (
    ggplot(data=df, aes(x=obsnum, y=visits_active_day_med)) +
      geom_line(aes(group=domain_num, colour=domain_num))) + 
    scale_y_continuous(limits = c(0, (max(df$visits_active_day_med, na.rm = T) + 5))) + 
    ggtitle("Visits per active day (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Visits per active day (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold")) 
  
  #-----------------------------------------------------------------------------#
  
  #Registered cases (#) by obsnum
  
  #Stacked area graph
  #Sum of registered cases by domain by obsnum
  #First sum registered across each domain for each obsnum
  overall = ddply(all_monthly, .(domain_char, obsnum), summarise,
                  sum_registered = sum(case_registered, na.rm=T))
  #Create stacked area graph
  #Figure out how to do a reverse legend here
  g_case_reg_sum = ggplot(overall, aes(x=obsnum, y=sum_registered, 
                                       fill=domain_char)) + 
    geom_area() +
    scale_fill_brewer(palette = "YlOrRd")
  
  # Cases registered - by month index
  
  overall = ddply(all_monthly, .(obsnum), summarise,
                  reg_med = median(case_registered, na.rm = T),
                  sd = sd(case_registered, na.rm=T),
                  n = sum(!is.na(case_registered)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  
  over_min_logical = (overall$reg_med - overall$ci95) < -3
  over_max_logical = (overall$reg_med + overall$ci95) > 
    (max(df$reg_med, na.rm = T) + 5)
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$reg_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$reg_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = (max(df$reg_med, na.rm = T) + 5)
  
  
  g_reg_med_overall = 
    ggplot(overall, aes(x = obsnum, y = reg_med, 
                        ymax = (max(df$reg_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases registered (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases registered (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  
  #By multiple domains
  #Calculate median within each obsum & domain level. This creates an array
  array_active = with(all_monthly, tapply(case_registered, 
                                          list(obsnum, domain_char), median, 
                                          na.rm = T))
  #Convert array to a data frame
  df_intermediate = as.data.frame (array_active)
  
  # Initialize vector and then append all medians together
  vector_median = c() 
  for (i in 1:length(domain_levels)) {
    vector_median = append(vector_median, df_intermediate[,i])
  }
  # Convert vector to dataframe - add domain numbers and obsnum back in, based on
  # the obsnum_levels and domain_levels 
  df = as.data.frame(vector_median)
  colnames(df) = c("reg_med")
  df$domain_num = rep(1:length(domain_levels), each = length(obsnum_levels))   
  df$obsnum = rep(1:length(obsnum_levels), length(domain_levels))
  df$domain_num <- as.factor(df$domain_num)
  g_reg_med_domains = (
    ggplot(data=df, aes(x=obsnum, y=reg_med)) +
      geom_line(aes(group=domain_num, colour=domain_num))) + 
    scale_y_continuous(limits = c(0, (max(df$reg_med, na.rm = T) + 5))) + 
    ggtitle("Registered cases (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Registered cases (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold")) 
  
  #-----------------------------------------------------------------------------#
  #Unique cases followed-up (#) by obsnum (prefer % of total cumulative open cases)
  #The denominator in this measure needs to be developed: need to count open cases 
  #ONLY and exclude all cases that were closed in the previous month
  
  # Cases followed-up - by month index
  
  overall = ddply(all_monthly, .(obsnum), summarise,
                  case_fu_med = median(follow_up_unique_case, na.rm = T),
                  sd = sd(follow_up_unique_case, na.rm=T),
                  n = sum(!is.na(follow_up_unique_case)),
                  se = sd/sqrt(n))
  overall$ci95 = overall$se * qt(.975, overall$n-1)
  
  over_min_logical = (overall$case_fu_med - overall$ci95) < -3
  over_max_logical = (overall$case_fu_med + overall$ci95) > 
    (max(df$case_fu_med, na.rm = T) + 5)
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$case_fu_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$case_fu_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = (max(df$case_fu_med, na.rm = T) + 5)
  
  
  g_case_fu_overall = 
    ggplot(overall, aes(x = obsnum, y = case_fu_med, 
                        ymax = (max(df$case_fu_med, na.rm = T) + 5))) + 
    geom_line(colour = "indianred1", size = 1.5) +
    geom_ribbon(aes(ymin = over_min, ymax = over_max),
                alpha = 0.2) +
    ggtitle("Cases followed-up (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Cases followed-up (#), median") +
    theme(axis.text=element_text(size=12), 
          axis.title=element_text(size=14,face="bold"))
  
  #By multiple domains
  #Calculate median within each obsum & domain level. This creates an array
  array_active = with(all_monthly, tapply(follow_up_unique_case, 
                                          list(obsnum, domain_char), median, 
                                          na.rm = T))
  #Convert array to a data frame
  df_intermediate = as.data.frame(array_active)
  
  # Initialize vector and then append all medians together
  vector_median = c() 
  for (i in 1:length(domain_levels)) {
    vector_median = append(vector_median, df_intermediate[,i])
  }
  # Convert vector to dataframe - add domain numbers and obsnum back in, based on
  # the obsnum_levels and domain_levels 
  df = as.data.frame(vector_median)
  colnames(df) = c("case_fu_med")
  df$domain_num = rep(1:length(domain_levels), each = length(obsnum_levels))   
  df$obsnum = rep(1:length(obsnum_levels), length(domain_levels))
  df$domain_num <- as.factor(df$domain_num)
  g_case_fu_domains = (
    ggplot(data=df, aes(x=obsnum, y=case_fu_med)) +
      geom_line(aes(group=domain_num, colour=domain_num))) + 
    scale_y_continuous(limits = c(0, 
                                  (max(df$case_fu_med, na.rm = T) + 5))) + 
    ggtitle("Unique cases followed-up (#) by month index") +
    theme(plot.title = element_text(size=14, face="bold")) +
    xlab("Month index") +
    ylab("Unique cases followed-up (#), median") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  #-----------------------------------------------------------------------------#
  
  #PRINT PLOTS AND EXPORT TO PDF
  
  #-----------------------------------------------------------------------------#
  require(gridExtra)
  
  pdf("Users_visits_total.pdf")
  grid.arrange(p_users, g_stacked_visits, nrow=2)
  dev.off()
  
  pdf("Users_total_split.pdf")
  grid.arrange(p_violin_users, nrow=1)
  dev.off()
  
  pdf("Number_visits.pdf")
  grid.arrange(g_visits_overall, g_visits_med_domain, nrow=2)
  dev.off()
  
  pdf("Active_days.pdf")
  grid.arrange(g_active_days_overall, g_active_days_domains, nrow=2)
  dev.off()
  
  pdf("Visits_active_day.pdf")
  grid.arrange(g_visits_active_overall, g_visits_active_day_domains, nrow=2)
  dev.off()
  
  pdf("Registered_total.pdf")
  grid.arrange(g_case_reg_sum, nrow=1)
  dev.off()
  
  pdf("Registered_cases.pdf")
  grid.arrange(g_reg_med_overall, g_reg_med_domains, nrow=2)
  dev.off()
  
  pdf("Cases_followed_up.pdf")
  grid.arrange(g_case_fu_overall, g_case_fu_domains, nrow=2)
  dev.off()
}
#-----------------------------------------------------------------------------#

#By FLW
#NA values in active_days_% will lead to gaps in the graphs
#ggplot(data = all_monthly, aes(x = all_monthly$obsnum,
#                              y = all_monthly$active_days_percent, 
#                             colour = user_id)) + geom_line() + geom_point()

#By single domain
#Single domain plot
#Calculate median by obsnum. This creates an array
#list_active = tapply (all_monthly$active_days_percent, all_monthly$obsnum, 
#                     FUN = median, na.rm = T) 
#Convert to dataframe
#df_single_domain = as.data.frame (list_active)
#df_single_domain$obsnum = obsnum_levels
#Plot out single domain
#plot(df_single_domain$obsnum, df_single_domain$list_active, type = "b", col = "blue",  
#    xlim = c(1, max(df_single_domain$obsnum)+1),
#   ylim = c(1, max(df_single_domain$list_active, na.rm = T)+10),
#  main = "Days active (median %) by monthly index",
# xlab = "Monthly index", ylab = "Days active, median (%)") 
