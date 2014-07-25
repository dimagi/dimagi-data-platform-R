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

# d1 = domains_for_run[1:8]
# d2 = domains_for_run[10:14]
#d3 = domains_for_run[20:23]
#domains_for_run = append(d1,d2)
#domains_for_run = append(domains_for_run,d3)
#domains_for_run = domains_for_run[-10]
#can use setdiff here to exclude domains by name

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
  monthly_merged <- merged_monthly_table (domains_for_run, read_directory)
  all_monthly <- add_splitby_col(monthly_merged,domain_table,report_options$split_by)
  

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
  # all_monthly = all_monthly[!(all_monthly$obsnum==136),]
  
  #Change column names names as needed
  names (all_monthly)[names(all_monthly) == "X"] = "row_num"
  names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
  names (all_monthly)[names(all_monthly) == "active_day_percent"] = "active_days_percent"
  names (all_monthly)[names(all_monthly) == "numeric_index"] = "obsnum"
  names (all_monthly)[names(all_monthly) == "domain"] = "domain_char"
  
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
  # TO DO: We need to change this to include only open cases belonging to a FLW in any
  # given month, otherwise the denominator will continue to inflate.
  # Will just use # of followed-up unique cases until we get the correct deonominator
  # Keep this here as a reminder till that happens 
  all_monthly$case_fu_per = (all_monthly$follow_up_unique_case/all_monthly$cum_case_registered)*100
  
  # Extract unique levels from obsnum and split_by
  obsnum_levels = as.numeric(levels(as.factor(all_monthly$obsnum)))
  split_by_levels = levels(all_monthly$split_by)
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

  
  #-----------------------------------------------------------------------------#
  #Visits by obsnum
  
  #Stacked area graph
  #Sum of visits by domain by obsnum
  #First sum visits across each domain for each obsnum
  overall = ddply(all_monthly, .(split_by, obsnum), summarise,
                  sum_visits = sum(visits, na.rm=T))
  #Create stacked area graph
  #Figure out how to do a reverse legend here
  g_stacked_visits = ggplot(overall, aes(x=obsnum, y=sum_visits, fill=split_by)) +
    geom_area() #+
    #scale_fill_brewer(palette = "Accent") #, 
  #                      breaks=rev(levels(overall$domain_char)))
  
  #By multiple domains
  #Calculate median within each obsum & domain level. This creates an array
  array_active = with(all_monthly, tapply(visits, 
                                          list(obsnum, split_by), median, 
                                          na.rm = T))
  #Convert array to a data frame
  df_intermediate = as.data.frame(array_active)
  
  # Initialize vector and then append all medians together
  vector_median = c() 
  for (i in 1:(length(split_by_levels))) {
    vector_median = append(vector_median, df_intermediate[,i])
  }
  # Convert vector to dataframe - add domain numbers and obsnum back in, based on
  # the obsnum_levels and domain_levels 
  df = as.data.frame(vector_median)
  colnames(df) = c("visits_med")
  df$split_num = rep(1:length(split_by_levels), each = length(obsnum_levels))   
  df$obsnum = rep(1:length(obsnum_levels), length(split_by_levels))
  df$split_num <- as.factor(df$split_num)
  g_visits_med_domain = (
    ggplot(data=df, aes(x=obsnum, y=visits_med)) +
      geom_line(aes(group=split_num, colour=split_num))) +
    scale_y_continuous(limits = c(0, (max(df$visits_med, na.rm = T) + 5))) + 
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
  
  over_min_logical <- sapply(overall$visits_med - overall$ci95, 
                             function(x) {if (is.na(x) | (x < -3) ) 
                               return (T) else return (F)})
  maximum_ci = max(df$visits_med, na.rm = T) + 5
  over_max_logical <- sapply(overall$visits_med + overall$ci95, 
                             function(x, maximum_ci) {if (is.na(x) | (x > maximum_ci) ) 
                               return (T) else return (F)},maximum_ci)
  
  over_min=over_min_logical
  over_min[over_min_logical==F] = 
    (overall$visits_med - overall$ci95)[over_min_logical==F]
  over_min[over_min_logical==T] = -3
  
  over_max=over_max_logical
  over_max[over_max_logical==F] = 
    (overall$visits_med + overall$ci95)[over_max_logical==F]
  over_max[over_max_logical==T] = maximum_ci
  
  
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
  
  
  #-----------------------------------------------------------------------------#
  
  #PRINT PLOTS AND EXPORT TO PDF
  
  #-----------------------------------------------------------------------------#
  require(gridExtra)
  report_output_dir <- file.path(output_dir, "reports")
  dir.create(report_output_dir, showWarnings = FALSE)
  
  outfile <- file.path(report_output_dir,"Users_visits_total.pdf")
  pdf(outfile)
  grid.arrange(p_users, g_stacked_visits, nrow=2)
  dev.off()
  
  outfile <- file.path(report_output_dir,"Number_visits.pdf")
  pdf(outfile)
  grid.arrange(g_visits_overall, g_visits_med_domain, nrow=2)
  dev.off()
 

}

#-----------------------------------------------------------------------------#

