# Monthly usage report
# July 2, 2014
# The purpose of this script is to create a monthly usage report module for the
# data platform. This report will be based on the aggregate FLW monthly tables
# which were computed from the data platform visit tables.
#------------------------------------------------------------------------------#

# Clear workspace and attach packages
# rm(list = ls())
#suppressPackageStartupMessages
library(zoo) #work with mm/yy calendar dates without day
library(ggplot2) #graphing across multiple domains
library(gridExtra) #graphing plots in columns/rows for ggplot
library(RColorBrewer) #Color palettes

render_debug <- function (test_data_dir, domains_for_run, report_options, output_dir) {
  
  source(file.path(getwd(),"function_libraries","csv_sources.R", fsep = .Platform$file.sep))
  domain_table <- get_domain_table_from_csv (test_data_dir)
  output_directory <- output_dir
  read_directory <- file.path(output_directory,"aggregate_tables", fsep=.Platform$file.sep)

  #Read in each individual agg table for each domain. Merge all together.
  
  # Set working directories here
  # Where my data tables are saved and where R output is saved
  # Work laptop
  read_directory = "/Users/Rashmi/Dropbox (Dimagi)/R analysis/Rashmi/Sample monthly tables/"
  output_directory = "C:/Users/Rashmi/Dropbox (Dimagi)/R analysis/Visuals/DP demo/July 7 2014"
  # Home laptop
  read_directory = "/Users/rdayalu/Dropbox (Dimagi)/R analysis/Rashmi/Sample monthly tables/"
  output_directory = "/Users/rdayalu/Dropbox (Dimagi)/R analysis/Visuals/DP demo/July 7 2014"
  #------------------------------------------------------------------------#
  
  # Read in monthly tables - merged table
  filename = paste(read_directory, "monthly_merge.csv", sep="")
  all_monthly = read.csv(file=filename, header = TRUE, as.is = TRUE)
  #setwd(output_directory)
  #Give full path rather than setwd
  fullpath <- file.path(output_directory,filename,fsep=.Platform$file.sep)
  write.csv(dat,fullpath)
  
  #Change column names names as needed
  colnames(all_monthly)[1] = "row_num"
  colnames(all_monthly)[2] = "calendar_month"
  
  # Convert relevant indicators to numeric
  # all_monthly$active_days_percent= as.numeric(all_monthly$active_days_percent)
  # all_monthly$visits = as.numeric(all_monthly$visits)
  
  #Convert calendar_month (character) to yearmon class since as.Date won't work 
  #without a day. Sort by calendar_month for each FLW and then label each 
  #month for each FLW in chronological order. 
  all_monthly$calendar_month = as.yearmon(all_monthly$calendar_month, "%b-%y")
  
  #Create monthly index for each FLW
  #Note: The code below doesn't care if an FLW has an inactive month. Only the
  #months with observations are counted, so no FLW will have any gaps, which
  #isn't correct. We need to rewrite this code.
  all_monthly = all_monthly[order(all_monthly$user_id,all_monthly$calendar_month),]
  all_monthly$obsnum = sequence(rle(all_monthly$user_id)$lengths)
  
  #Create a new domain variable with a character in the front so that it 
  #can be used as a column name later without a problem
  all_monthly$domain_char = paste("d", all_monthly$domain.index, sep="")
  
  #Create new indicators as needed
  # % of registration visits
  #Actually, this variable doesn't make much sense. Will just use # of registered
  # cases instead.
  all_monthly$reg_visits_per = (all_monthly$case_registered/all_monthly$visits)*100
  # % of unique cases followed-up (of cumulative open cases at month start)
  # Note that cum_case_registered is a cumulative of ALL cases registered by a FLW
  # We need to change this to include only open cases belonging to a FLW in any
  # given month, otherwise the denominator will continue to inflate.
  # Will just use # of followed-up unique cases until we get the correct deonominator
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
  #Number of visits by obsnum
  
  #Stacked area graph
  #Sum of visits by domain by obsnum
  #First sum visits across each domain for each obsnum
  overall = ddply(all_monthly, .(domain_char, obsnum), summarise,
                  sum_visits = sum(visits, na.rm=T))
  #Create stacked area graph
  #Figure out how to do a reverse legend here
  g_stacked_visits = ggplot(overall, aes(x=obsnum, y=sum_visits, fill=domain_char)) +
    geom_area() +
    scale_fill_brewer(palette = "Accent") #, 
  #                      breaks=rev(levels(overall$domain_char)))
  
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
