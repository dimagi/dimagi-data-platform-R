# Attrition report
# July 17, 2014
# The purpose of this script is to create an FLW attrition report module for the
# data platform. This report will be based on the aggregate FLW monthly tables
# which were computed from the data platform visit tables.
#------------------------------------------------------------------------------#

# Clear workspace and attach packages
# rm(list = ls())
# suppressPackageStartupMessages
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
  
  #Perform the following function by flw
  #If the flw has been around for only one month
  #That month will always have retained = F and addition = T
  #If the flw has been around for more than one month
  #The last month for each flw will always have retained = F
  #The first month for each flw will always have addition = T 
  #The next t row should always be just one step up from the previous t row (retained = T)
  #If not equal, then the FLW was not retained (retained = F) 
  #The next t row should always be just one step up from the previous t row (addition = F)
  #If not equal, then the flw was added (addition = T) 
  
  retain_add <- function(x) {
    if (length(x$month_index) == 1) {
      x$retained <- FALSE
      x$addition <- TRUE
    }
    else {
      x$retained <- c(x$month_index[1:(length(x$month_index)-1)] + 1 == x$month_index[2:length(x$month_index)], FALSE)
      x$addition <- c(TRUE, x$month_index[2:length(x$month_index)] != x$month_index[1:(length(x$month_index)-1)] + 1)
    }
    return(x)
  }
  
  df1 = arrange(all_monthly, user_pk, month_index)
  df_group = group_by(df1, user_pk)
  df2 <- retain_add(df_group)
  
  #This gives the attrition rate in the next month, using the # in month_index as the denominator
  #Numerator is # of flws that are not retained in the next month from the previous month
  #This also gives the addition rate in the month_index month, using # in month_index as the denominator
  #Numerator is # of flws that were added in for that month_index  
  attrition_table = ddply(df2, .(month_index), 
                          function(x) c(attrition=mean(!x$retained, na.rm = T)*100, 
                                        additions=mean(x$addition, na.rm = T)*100))
  
  attrition_table_split = ddply(df2, .(month_index, split_by), 
                                function(x) c(attrition=mean(!x$retained, na.rm = T)*100, 
                                              additions=mean(x$addition, na.rm = T)*100))
  
  
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
  
  outfile <- file.path(report_output_dir,"Number_users_attrition.pdf")
  pdf(outfile)
  grid.arrange(p_users, nrow=1)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  #-----------------------------------------------------------------------------#
  # Attrition and Addition (%) graphs - overall
  
  g_attrition = 
    ggplot(data=attrition_table, aes(x=month_index, y=attrition)) +
    geom_line(color = "indianred1", size = 1.3) +
    ggtitle("FLWs lost in next month (%) by monthly index") +
    theme(plot.title = element_text(size=14, face="bold")) + 
    xlab("Month index") +
    ylab("Attrition (%)") + 
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  g_addition = 
    ggplot(data=attrition_table, aes(x=month_index, y=additions)) +
    geom_line(color = "darkblue", size = 1.3) +
    ggtitle("FLWs added each month (%) by monthly index") +
    theme(plot.title = element_text(size=14, face="bold")) + 
    xlab("Month index") +
    ylab("Addition (%)") + 
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  #-----------------------------------------------------------------------------#
  # Attrition and Addition (%) graphs - split-by
  
  g_attrition_split = 
    ggplot(data=attrition_table_split, aes(x=month_index, y=attrition)) +
    geom_line(aes(group=split_by, colour=split_by), size=1.3)+
    ggtitle("FLWs lost in next month (%) by monthly index") +
    theme(plot.title = element_text(size=14, face="bold")) + 
    xlab("Month index") +
    ylab("Attrition (%)") + 
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  g_addition_split = 
    ggplot(data=attrition_table_split, aes(x=month_index, y=additions)) +
    geom_line(aes(group=split_by, colour=split_by), size=1.3) +
    ggtitle("FLWs added each month (%) by monthly index") +
    theme(plot.title = element_text(size=14, face="bold")) + 
    xlab("Month index") +
    ylab("Addition (%)") + 
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14,
                                                                   face="bold"))
  
  #g_attrition_addition = 
  # ggplot(data=attrition_table, aes(x=obsnum)) + 
  #geom_line(aes(y = attrition, colour = "darkblue"), size = 1.25) + 
  #geom_line(aes(y = additions, colour = "indianred1"), size = 1.25)
  
  #-----------------------------------------------------------------------------#
  #PRINT PLOTS AND EXPORT TO PDF
  #-----------------------------------------------------------------------------#
  
  outfile <- file.path(report_output_dir,"Attrition.pdf")
  pdf(outfile)
  grid.arrange(g_attrition, g_attrition_split, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  outfile <- file.path(report_output_dir,"Addition.pdf")
  pdf(outfile)
  grid.arrange(g_addition, g_addition_split, nrow=2)
  dev.off()
  module_pdfs <- c(module_pdfs,outfile)
  
  return(module_pdfs)
}

