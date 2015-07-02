#Why do so many rows in monthly table have nforms and active_days = NA?
#Per Neal's suggestion, look at rows for March 2015
#Amelia reported 6268 active users in March 2015

mar_2015 <- filter(monthly_table, month.index == "Mar 2015")
#Change column names as needed
names(mar_2015)[names(mar_2015) == "month.index"] = "calendar_month"
names(mar_2015)[names(mar_2015) == "numeric_index"] = "month_index"
#Convert calendar month to actual date
mar_2015$calendar_month <- parse_date_time(paste('01', mar_2015$calendar_month), '%d %b %Y!')
mar_2015$calendar_month <- as.Date(mar_2015$calendar_month)
mar_2015$month_abbr <- month(mar_2015$calendar_month, label = T, abbr = T)

#DP has 7270 rows (330 rows have user_pk = NA)
#There are 6769 unique user_pk (6768 unique user_ids)

#Check nforms/active_days
summary(mar_2015$nforms) 
#has 1295 NAs, 17.8% of March 2015 monthly rows
test <- filter(mar_2015, is.na(user_pk))
#330 of these observations are from user_pk = NA
summary(mar_2015$active_days) 
#has 1295 NAs

#Merge user_type variable
mar_2015 <- merge(mar_2015, users, by = "user_pk", all.x = T)

#Remove user_pk = NA: have 6940 rows with 6768 unique users 
test <- filter(mar_2015, !is.na(user_pk))

#Who are the duplicate users?
test$dup_user_pk <- duplicated(test$user_pk)
test2 <- filter(test, dup_user_pk == T)
dup_users <- test[test$user_pk %in% test2$user_pk,]
dup_users <- arrange(dup_users, user_pk)
#Dimagi, demo_user across domains has the same user_pk
#We also have some mvp users that submit to multiple domains
#Majority of these rows have nforms = NA, so we should exclude duplicate user_pks as well
#Which dup users have valid nforms and which have at least one row with nforms = NA
dup_users_summary <- dup_users %>% 
  group_by(user_pk) %>% 
  summarise(ndomains = length(unique(domain)),
            sum_nforms = sum(nforms, na.rm = T),
            nrows_nforms_na = sum(is.na(nforms)))
nusers <- arrange(nusers, desc(nusers_dup))


#What is the user_type for the remaining de-duped users with nforms = NA?
no_dup_mar_2015 <- filter(test, !(test$user_pk %in% test2$user_pk))
no_dup_mar_2015$dup_user_pk <- duplicated(no_dup_mar_2015$user_pk) #there are no duplicates
summary(no_dup_mar_2015$nforms)
#845(12.5%) of these users have nforms = NA. What are their user_types?
test2 <- filter(no_dup_mar_2015, is.na(nforms))
table(test2$user_type, useNA = "always") #841 are mobile users
table(test2$summary_device_type, useNA = "always") #summary_device_type = NA for all. No SMS users

#How many of these users have submitting nforms != NA for any other calendar month?
#Flag users by whether or not they had nforms != NA in other months
test <- filter(monthly_table, !is.na(nforms))
test2$other_valid_nforms <- test2$user_pk %in% test$user_pk

#How many users with nforms = NA per domain?
nusers <- test2 %>% 
  group_by(domain) %>% 
  summarise(nusers_nforms_na = length(unique(user_pk)),
            n_other_valid = sum(other_valid_nforms))
nusers$per_other_valid <- (nusers$n_other_valid/nusers$nusers_nforms_na)*100 
nusers <- arrange(nusers, desc(nusers_nforms_na))
single_domain <- filter(test2, domain == "healthforlife")

test2 <- filter(no_dup_mar_2015, !is.na(nforms)) #5890 users
#Note that summary_device_type = NA for none of these users


