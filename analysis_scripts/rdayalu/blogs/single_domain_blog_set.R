#The purpose of this code is to create a standard dataset that people can use for 
#the "Under the Data Tree" data blog series

library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate)
library(ggplot2)

#------------------------------------------------------------------------#
#DATA MANAGEMENT
#------------------------------------------------------------------------#

# Load system config file
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))

# Get db connection
# db <- get_db_connection(system_conf)

#List of users by user_type, keeping only mobile users
#Get user_type table from db (mobile, web, superuser, etc.)
user_type <- get_user_type_table(db)
user_type <- select(user_type, user_pk, user_type)

#Create aggregate monthly data set
test <- filter(monthly_table, domain == "ssqh-cs")

#Merge these two lists together, keeping only mobile users
test <- merge(test, user_type, by = "user_pk", all.x = T)

#Set report_options
report <- run_conf$reports$modules$name
report_options <- get_report_options(run_conf,report)

#Remove any dates before report start_date and after report end_date
test$date_first_visit = as.Date(test$date_first_visit)
test$date_last_visit = as.Date(test$date_last_visit)
start_date = as.Date(report_options$start_date)
end_date = as.Date(report_options$end_date)
report_end_date <- as.Date(report_options$end_date)
end_month <- as.yearmon(report_end_date)
end_month <- parse_date_time(paste('01', end_month), '%d %b %Y!')
end_month <- as.Date(end_month)

#Change column names as needed
names (test)[names(test) == "month.index"] = "calendar_month"
names (test)[names(test) == "numeric_index"] = "month_index"

#Convert calendar month to actual date
test$calendar_month <- parse_date_time(paste('01', test$calendar_month), '%d %b %Y!')
test$calendar_month <- as.Date(test$calendar_month)
test$month_abbr <- month(test$calendar_month, label = T, abbr = T)

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
test <- arrange(test, user_pk, calendar_month)
df <- data.table(test)
setkey(df,user_pk)
df[,diff_days:=c(NA,diff(calendar_month)),by=user_pk]
test <- as.data.frame(df)
test$previous_month_active <- test$diff_days <= 31
test$previous_two_months_active <- test$diff_days <= 62
test$previous_three_months_active <- test$diff_days <= 93

#We are working with user_pk/domain combination since user_pk
#might not be unique across domains. A single user_pk can submit to multiple domains. 
test$domain_user <- paste(test$domain, test$user_pk, sep = "_") 
users <- unique(test$domain_user)

next_month_active <- c()
for (i in users) {
  single_user <- test[test$domain_user == i,]
  next_active <- c()
  next_active <- c(single_user$previous_month_active[-1], F)
  next_month_active <- c(next_month_active, next_active)
}
test$next_month_active <- next_month_active

next_two_months_active <- c()
for (i in users) {
  single_user <- test[test$domain_user == i,]
  next_active <- c()
  next_active <- c(single_user$previous_two_months_active[-1], F)
  next_two_months_active <- c(next_two_months_active, next_active)
}
test$next_two_months_active <- next_two_months_active

next_three_months_active <- c()
for (i in users) {
  single_user <- test[test$domain_user == i,]
  next_active <- c()
  next_active <- c(single_user$previous_three_months_active[-1], F)
  next_three_months_active <- c(next_three_months_active, next_active)
}
test$next_three_months_active <- next_three_months_active

#Based on the end_month in our dataset, we don't know if the user will be active in any of
#the months following end_month. Must change all those attrition values to NA. 
is.na(test$next_month_active) <- test$calendar_month == end_month
is.na(test$next_two_months_active) <- test$calendar_month >= end_month - months(1) 
is.na(test$next_three_months_active) <- test$calendar_month >= end_month - months(2)

#Get lifetime table for total nunique_followups, active_months per user
lifetime_table <- get_aggregate_table(db, "aggregate_lifetime_interactions", domains_for_run)
lifetime_table <- filter(lifetime_table, domain == "ssqh-cs")

#Merge nunique_followups, active_months to test
lifetime_table <- select(lifetime_table, user_pk, nunique_followups, active_months, calendar_month_on_cc)
names(lifetime_table)[names(lifetime_table) == "nunique_followups"] = "lifetime_followup"
names(lifetime_table)[names(lifetime_table) == "calendar_month_on_cc"] = "months_on_cc"
test <- merge(test, lifetime_table, by = "user_pk", all.x = T)

#Was the user ever active again after an attrition event (defined as next_month_active == F)?
test$attrition_event <- !(test$next_month_active == T | is.na(test$next_month_active))
test$continuing <- test$month_index < test$months_on_cc
test$ever_active_again <- test$attrition_event == T & test$continuing == T
is.na(test$ever_active_again) <- test$attrition_event == F

write.csv(test, file = "ssqh_cs_data_platform.csv", row.names = F)

