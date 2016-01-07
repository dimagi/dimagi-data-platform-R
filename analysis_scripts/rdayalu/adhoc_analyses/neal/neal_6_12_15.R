#Neal's request for self-start stats
#Email sent on 6/12/15

#Import user table and user_type table
users <- tbl(db, "users") 
users <- collect(users)
user_type <- get_user_type_table(db)
user_type <- collect(user_type)
#Merge user tables
users <- merge(users, user_type, by.x = "id", by.y = "user_pk", all.x = T)
users <- select(users, -c(user_id.y, username.y, first_name, last_name, default_phone_number,
                          groups, phone_numbers, user_id.x))
names(users)[names(users) == "username.x"] = "username"
names(users)[names(users) == "id"] = "user_pk"

#Load monthly agg table for all domains, all months
all_monthly <- monthly_table
#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)
all_monthly <- filter(all_monthly, !is.na(calendar_month))
all_monthly <- merge(all_monthly, users, by = "user_pk", all.x = T)

#Exclude dimagi users and superusers
#This excludes all obviously internal domains that only have
#dimagi users and/or superusers
exclude2 <- all_monthly[grep("dimagi", all_monthly$email, fixed=T),]
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude2$user_pk),]
all_monthly <- filter(all_monthly, is_superuser == F | is.na(is_superuser))

#Require domains that have at least one non-dimagi web user
user_type_summary <- all_monthly %>% group_by(domain) %>% 
  summarise(nweb_users = length(unique(user_pk[user_type == "web"])), 
            nmobile_users = length(unique(user_pk[user_type == "mobile"])), 
            na_users = length(unique(user_pk[is.na(user_type)])))
user_type_summary <- filter(user_type_summary, nweb_users >= 1)
all_monthly <- filter(all_monthly, domain %in% user_type_summary$domain)

#Merge is_test and self_start values
domains <- select(domain_table, name, id, is_test, internal.self_started)
all_monthly <- merge(all_monthly, domains, by.x = "domain", by.y = "name", all.x = T)
names(all_monthly)[names(all_monthly) == "id"] = "domain_id"

#Flag first calendar_month per domain for all rows
all_monthly <- all_monthly %>% group_by(domain) %>% 
  mutate(first_calendar_month = calendar_month == min(calendar_month, na.rm=T))

#Create domain activity table
domain_activity <- all_monthly %>% group_by(calendar_month) %>% 
  summarise(ndomains_active = length(unique(domain)),  
            ndomains_active_test_true = length(unique(domain[is_test == "true"])), 
            ndomains_active_test_false = length(unique(domain[is_test == "false"])), 
            ndomains_active_test_none = length(unique(domain[is_test == "none"])),
            ndomains_new = length(unique(domain[first_calendar_month == T])),
            ndomains_new_test_true = length(unique(domain[first_calendar_month == T & is_test == "true"])), 
            ndomains_new_test_false = length(unique(domain[first_calendar_month == T & is_test == "false"])), 
            ndomains_new_test_none = length(unique(domain[first_calendar_month == T & is_test == "none"])))

write.csv(domain_activity, file="domain_activity.csv")





#------------------------------------------------------------------------#

#This is just to check some of our numbers that Neal was wondering about
#See doc here: 
#https://docs.google.com/document/d/1DMCAyIzaL-5tputXufLPpFTYEtsVMzWa0njNJoMbAc4/edit
april_2015_dimagi <- filter(all_monthly, calendar_month == "2015-04-01")
april_2015_dimagi <- filter(april_2015_dimagi, user_pk %in% exclude2$user_pk | 
                              is_superuser == T)

april_2015_non_dimagi <- filter(all_monthly, calendar_month == "2015-04-01")
april_2015_non_dimagi <- april_2015_non_dimagi[!(april_2015_non_dimagi$user_pk %in% exclude2$user_pk),]
april_2015_non_dimagi <- filter(april_2015_non_dimagi, is_superuser == F | is.na(is_superuser))

#------------------------------------------------------------------------#

#Create domain table with number of dimagi users, non-dimagi users and super-users
#Also add numbers for user type.

#Import user table and user_type table
users <- tbl(db, "users") 
users <- collect(users)
user_type <- get_user_type_table(db)
user_type <- collect(user_type)
#Merge user tables
users <- merge(users, user_type, by.x = "id", by.y = "user_pk", all.x = T)
users <- select(users, -c(user_id.y, username.y, first_name, last_name, default_phone_number,
                          groups, phone_numbers, user_id.x))
names(users)[names(users) == "username.x"] = "username"
names(users)[names(users) == "id"] = "user_pk"

#Load monthly agg table for all domains, all months
all_monthly <- monthly_table
#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly <- filter(all_monthly, !is.na(calendar_month))
all_monthly <- merge(all_monthly, users, by = "user_pk", all.x = T)

#Flag dimagi users
dimagi_users <- all_monthly$user_pk[grep("dimagi", all_monthly$email, fixed=T)]
all_monthly$dimagi_user <- all_monthly$user_pk %in% dimagi_users

#Flag non-dimagi superusers
all_monthly <- all_monthly %>% 
  mutate(superuser_non_dimagi = is_superuser == T & dimagi_user == F)
all_monthly$superuser_non_dimagi[is.na(all_monthly$superuser_non_dimagi)] <- FALSE

#Flag non-dimagi users
all_monthly$non_dimagi_user <- (all_monthly$is_superuser == F | is.na(all_monthly$is_superuser)) & 
  all_monthly$dimagi_user == F

#Create user_type
all_monthly$web <- all_monthly$user_type == "web"
all_monthly$web[is.na(all_monthly$web)] <- F
all_monthly$mobile <- all_monthly$user_type == "mobile"
all_monthly$mobile[is.na(all_monthly$mobile)] <- F
all_monthly$na_type <- is.na(all_monthly$user_type)

#test <- filter(all_monthly, domain == "a5288-study")
#test <- select(test, dimagi_user, superuser_non_dimagi, non_dimagi_user)
#test$sum_row <- rowSums(test)
#test2 <- filter(test, non_dimagi_user == T)

domain_user_summary <- all_monthly %>% group_by(domain) %>% 
  summarise(nusers = length(unique(user_pk)),  
            nusers_dimagi_web = length(unique(user_pk[web == T & dimagi_user == T])), 
            nusers_dimagi_mobile = length(unique(user_pk[mobile == T & dimagi_user == T])), 
            nusers_dimagi_na = length(unique(user_pk[na_type == T & dimagi_user == T])), 
            nusers_non_dimagi_web = length(unique(user_pk[web == T & non_dimagi_user == T])), 
            nusers_non_dimagi_mobile = length(unique(user_pk[mobile == T & non_dimagi_user == T])), 
            nusers_non_dimagi_na = length(unique(user_pk[na_type == T & non_dimagi_user == T])), 
            nusers_non_dimagi_super_web = length(unique(user_pk[web == T & superuser_non_dimagi == T])), 
            nusers_non_dimagi_super_mobile = length(unique(user_pk[mobile == T & superuser_non_dimagi == T])), 
            nusers_non_dimagi_super_na = length(unique(user_pk[na_type == T & superuser_non_dimagi == T])))

#nusers <- domain_user_summary$nusers
#domain_user_summary <- select(domain_user_summary, -c(domain, nusers))
#domain_user_summary$sum_row <- rowSums(domain_user_summary)
#summary(nusers == domain_user_summary$sum_row)

write.csv(domain_user_summary, file="domain_user_type.csv")
