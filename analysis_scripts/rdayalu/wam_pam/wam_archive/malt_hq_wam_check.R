
test <- filter(all_malt_user_month, elig_exp_using == T & wam_generated == F)

test2 <- filter(all_malt, user_id == "0495a9d4f4851fa63c6c039012eafcbb"& form_month == "2014-11-01")
length(unique(test2$app_id))

users$user_pk[users$user_id == "0495a9d4f4851fa63c6c039012eafcbb"]

user_test <- filter(users, user_id == "0495a9d4f4851fa63c6c039012eafcbb")

test3 <- filter(form_table, user_pk == 826 & form_month == "2014-10-01")

app$dup_app_id <- duplicated(app$app_id)
app_dup <- filter(app, dup_app_id == T)

#------------------------------------------------------------------------#
# The number of active users is much higher for DP MALT based on server time.
# Need to investigate why. 
# Neal suggests the following: (1) I’d suggest we generate a DP MALT Server and a DP MALT Phone, 
# and see how those match up with each other and (2) how the DP MALT Phone matches up with the DP Monthly Tables.  
# I’m thinking there might be an exclusion that went into the DP Monthly Table that we’re missing for the DP MALT.
#------------------------------------------------------------------------#

#First we need to generate DP MALT based on the current form_table in DP
#Import form table
form_table <- tbl(db, "form")
form_table <- collect(form_table)
names(form_table)[names(form_table) == "user_id"] = "user_pk"
form_table <- select(form_table, id, user_pk, received_on, time_start, formdef_id, visit_id, domain_id, application_id)
names(form_table)[names(form_table) == "id"] = "form_pk"
names(form_table)[names(form_table) == "domain_id"] = "domain_pk"
names(form_table)[names(form_table) == "application_id"] = "application_pk"

#forms_not_visit <- filter(form_table, is.na(visit_id))
#forms_not_visit$form_date <- as.Date(substr(forms_not_visit$received_on, 1, 10))
#forms_not_visit$form_month <- floor_date(forms_not_visit$form_date, "month") 
#Create fake monthly table
#monthly_fake <- forms_not_visit %>% group_by(domain_pk, user_pk, form_month) %>% 
#  summarise(nforms = length(unique(form_pk)), 
#            has_any_time_start_na = sum(is.na(time_start)) > 0)

#Create form_date and form_month based on time_start
#Need to remove forms with time_start = NA (4.7% of forms don't have a valud time_start)
#Also, about 15% of total forms don't have a visit_id. Want to investigate this later?
form_valid_start <- filter(form_table, !is.na(time_start))
form_valid_start$form_date <- as.Date(substr(form_valid_start$time_start, 1, 10))
form_valid_start$form_month <- floor_date(form_valid_start$form_date, "month") 
monthly_start <- form_valid_start %>% group_by(domain_pk, user_pk, form_month) %>% 
  summarise(nforms = length(unique(form_pk)))
monthly_start <- merge(monthly_start, select(domain_table, id, name), by.x = "domain_pk", by.y = "id", all.x = T)
names(monthly_start)[names(monthly_start) == "name"] = "domain"
names(monthly_start)[names(monthly_start) == "form_month"] = "calendar_month"
monthly_start$domain_user_month <- paste(monthly_start$domain, monthly_start$user_pk, monthly_start$calendar_month, sep="_")

#Import monthly_table for comparison
monthly_table <- select(monthly_table, domain, user_id, user_pk, month.index, nforms, nvisits)
#Convert calendar month to actual date
names(monthly_table)[names(monthly_table) == "month.index"] = "calendar_month"
monthly_table$calendar_month <- parse_date_time(paste('01', monthly_table$calendar_month), '%d %b %Y!')
monthly_table$calendar_month <- as.Date(monthly_table$calendar_month)
monthly_table$domain_user_month <- paste(monthly_table$domain, monthly_table$user_pk, monthly_table$calendar_month, sep="_")

#Flag common rows between monthly_start and monthly_table
monthly_start$in_monthly_table <- monthly_start$domain_user_month %in% monthly_table$domain_user_month
monthly_table$in_monthly_start <- monthly_table$domain_user_month %in% monthly_start$domain_user_month

table(monthly_start$calendar_month[monthly_start$in_monthly_table == F], useNA = "always")
table(monthly_table$calendar_month[monthly_table$in_monthly_start == F], useNA = "always")

#Create form_date and form_month based on server time
form_table$form_date <- as.Date(substr(form_table$received_on, 1, 10))
form_table$form_month <- floor_date(form_table$form_date, "month")
monthly_received <- form_table %>% group_by(domain_pk, user_pk, form_month) %>% 
  summarise(nforms = length(unique(form_pk)))
monthly_received <- merge(monthly_received, select(domain_table, id, name), by.x = "domain_pk", by.y = "id", all.x = T)
names(monthly_received)[names(monthly_received) == "name"] = "domain"
names(monthly_received)[names(monthly_received) == "form_month"] = "calendar_month"
monthly_received$domain_user_month <- paste(monthly_received$domain, monthly_received$user_pk, monthly_received$calendar_month, sep="_")

#Flag common rows between monthly_received and monthly_table
monthly_received$in_monthly_table <- monthly_received$domain_user_month %in% monthly_table$domain_user_month
monthly_table$in_monthly_received <- monthly_table$domain_user_month %in% monthly_received$domain_user_month

table(monthly_received$calendar_month[monthly_received$in_monthly_table == F], useNA = "always")
table(monthly_table$calendar_month[monthly_table$in_monthly_received == F], useNA = "always")

#Create overall nusers tables
nusers_monthly <- monthly_table %>% group_by(calendar_month) %>%
  summarise(nusers_monthly = length(unique(user_pk)))
nusers_start <- monthly_start %>% group_by(calendar_month) %>%
  summarise(nusers_start = length(unique(user_pk)))
nusers_received <- monthly_received %>% group_by(calendar_month) %>%
  summarise(nusers_received = length(unique(user_pk)))
nusers <- merge(nusers_monthly, nusers_start, by = "calendar_month", all = T)
nusers <- merge(nusers, nusers_received, by = "calendar_month", all = T)
nusers$diff_monthly_start <- nusers$nusers_monthly - nusers$nusers_start
nusers$diff_monthly_received <- nusers$nusers_monthly - nusers$nusers_received
nusers$diff_start_received <- nusers$nusers_start - nusers$nusers_received

#Who are the missing users?
test <- filter(monthly_start, calendar_month == "2015-03-01" & in_monthly_table == F)
#Import user table and user_type table
users <- tbl(db, "users") 
users <- collect(users)
names(users)[names(users) == "id"] = "user_pk"
test <- merge(test, select(users, user_pk, user_id), by = "user_pk", all.x = T)
form_test <- filter(form_table, user_pk %in% test$user_pk)
form_test$form_start_date <- as.Date(substr(form_test$time_start, 1, 10))
form_test$form_start_month <- floor_date(form_test$form_start_date, "month")
form_test$form_received_date <- as.Date(substr(form_test$received_on, 1, 10))
form_test$form_received_month <- floor_date(form_test$form_received_date, "month") 
form_test <- filter(form_test, form_start_month == "2015-03-01")

#Check individual users
user_test <- filter(monthly_table, user_id == "OL4XQJZ1BO0ZPBVVTPRVAZPZF")
table(user_test$calendar_month)
form_test <- filter(form_table, user_pk == 108954)
form_test$form_date <- as.Date(substr(form_test$received_on, 1, 10))
form_test$form_month <- floor_date(form_test$form_date, "month") 
table(form_test$form_month)

#Looks like these forms are mostly case edits or through the case importer
formdef <- tbl(db, "formdef")
formdef <- collect(formdef)
formdef_test <- filter(formdef, id == 3299)
