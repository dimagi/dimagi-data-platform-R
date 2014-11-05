
library(data.table)
library(zoo)
library(lubridate)

#------------------------------------------------------------------------#
#DATA MANAGEMENT
#------------------------------------------------------------------------#

all_monthly <- monthly_table

#Set report_options
report = "monthly_usage_report"
report_options <- get_report_options(run_conf,report)

#Remove demo users and NA/NONE users
#We also need to find a way to exclude admin/unknown users
all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
all_monthly = all_monthly[!(all_monthly$user_id =="NONE"),]
all_monthly = all_monthly[!(all_monthly$user_id =="none"),]
all_monthly = all_monthly[!is.na(all_monthly$user_id),]

#Remove any dates before report start_date and after report end_date
names (all_monthly)[names(all_monthly) == "date_first_visit"] = "first_visit_date"
names (all_monthly)[names(all_monthly) == "date_last_visit"] = "last_visit_date"
all_monthly$first_visit_date = as.Date(all_monthly$first_visit_date)
all_monthly$last_visit_date = as.Date(all_monthly$last_visit_date)
start_date = as.Date(report_options$start_date)
end_date = as.Date(report_options$end_date)
all_monthly = subset(all_monthly, all_monthly$first_visit_date >= start_date
                     & all_monthly$last_visit_date <= end_date)

#Change column names as needed
names (all_monthly)[names(all_monthly) == "X"] = "row_num"
names (all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names (all_monthly)[names(all_monthly) == "active_day_percent"] = "active_days_percent"
names (all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"

#domain to numeric: First convert from character to factor
all_monthly$domain <- as.factor(all_monthly$domain)
all_monthly$domain_numeric = as.numeric(all_monthly$domain)

#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
all_monthly <- arrange(all_monthly, user_pk, calendar_month)
df <- data.table(all_monthly)
setkey(df,user_pk)
df[,diff_days:=c(NA,diff(calendar_month)),by=user_pk]
all_monthly <- as.data.frame(df)
all_monthly$previous_month_active <- all_monthly$diff_days <= 31

users <- unique(all_monthly$user_pk)

next_month_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_pk == i,]
  next_active <- c()
  next_active <- append(single_user$previous_month_active[-1], F)
  next_month_active <- append(next_month_active, next_active)
}
all_monthly$next_month_active <- next_month_active

#If calendar_month = 9/1/14 then next_month_active = NA
#because we don't know if the user will be active in the following month
is.na(all_monthly$next_month_active) <- all_monthly$calendar_month == "2014-09-01"


#Dag's suggestion for next_month_active
next_month_active_2 <- lapply(users, function(x) {
  single_user <- all_monthly[all_monthly$user_pk == x,]
  append(single_user$previous_month_active[-1], FALSE)
}) %>% unlist()
