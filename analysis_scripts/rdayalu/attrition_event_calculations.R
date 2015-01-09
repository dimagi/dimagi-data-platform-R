#This code can be sourced to add calculations for 1-month, 2-month and 3-month attrition
#events to your monthly tables. 
#I've used for loops and append steps, so it takes a while to run.

library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate) #Need to install this after data.table otherwise we will lose some
#important lubridate functions

#First import monthly_table for all your domains of interest
#Be sure to set config_run first
#source(file.path("analysis_scripts","raw_data","data_import.R", fsep = .Platform$file.sep))
all_monthly <- monthly_table

#Set report_options
#Format data variables
all_monthly$date_first_visit = as.Date(all_monthly$date_first_visit)
all_monthly$date_last_visit = as.Date(all_monthly$date_last_visit)
report = run_conf$reports$modules$name
report_options <- get_report_options(run_conf,report)
report_end_date <- as.Date(report_options$end_date)
end_month <- as.yearmon(report_end_date)
end_month <- parse_date_time(paste('01', end_month), '%d %b %Y!')
end_month <- as.Date(end_month)

#Convert calendar month to an actual date to make it easier to work with
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)

#Calculate differences (in days) between consecutive monthly rows per user to calculate 
#next_month_active and previous_month_active variables
all_monthly <- arrange(all_monthly, user_pk, calendar_month)
df <- data.table(all_monthly)
setkey(df,user_pk)
df[,diff_days:=c(NA,diff(calendar_month)),by=user_pk]
all_monthly <- as.data.frame(df)
all_monthly$previous_month_active <- all_monthly$diff_days <= 31
all_monthly$previous_two_months_active <- all_monthly$diff_days <= 62
all_monthly$previous_three_months_active <- all_monthly$diff_days <= 93

#Vector of unique users
users <- unique(all_monthly$user_pk)

#1-month attrition events
next_month_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_pk == i,]
  next_active <- c()
  next_active <- append(single_user$previous_month_active[-1], F)
  next_month_active <- append(next_month_active, next_active)
}
all_monthly$next_month_active <- next_month_active

#2-month attrition events
next_two_months_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_pk == i,]
  next_active <- c()
  next_active <- append(single_user$previous_two_months_active[-1], F)
  next_two_months_active <- append(next_two_months_active, next_active)
}
all_monthly$next_two_months_active <- next_two_months_active

#3-month attrition events
next_three_months_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_pk == i,]
  next_active <- c()
  next_active <- append(single_user$previous_three_months_active[-1], F)
  next_three_months_active <- append(next_three_months_active, next_active)
}
all_monthly$next_three_months_active <- next_three_months_active

#Based on the end_month in our dataset, we don't know if the user will be active in any of
#the months following end_month. Must change all those attrition values to NA. 
is.na(all_monthly$next_month_active) <- all_monthly$calendar_month == end_month
is.na(all_monthly$next_two_months_active) <- all_monthly$calendar_month >= end_month - months(1) 
is.na(all_monthly$next_three_months_active) <- all_monthly$calendar_month >= end_month - months(2)
