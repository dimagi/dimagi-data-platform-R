
all_monthly_backup <- all_monthly

# Get db connection
db <- get_db_connection(system_conf)

# Get MALT table 
malt <- tbl(db, "malt_table")
malt <- collect(malt)

all_monthly <- malt
#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
#Concat user and month in monthly table
all_monthly$user_month_concat <- paste(all_monthly$user_pk, 
                                       all_monthly$calendar_month, sep = "_")
#Add unique integer column
all_monthly$unique_int <- 1:nrow(all_monthly)

#Run user exclusion section
#Run till line 201
#Then from line 243 to 259
#then from line 342 to 383
#Then from 398 to 450

#Merge previous_active_months_rolling to calculate wam_experienced
to_merge <- select(all_monthly_backup, c(domain, user_pk, calendar_month, 
                                         previous_active_months_rolling))
all_monthly <- merge(all_monthly, to_merge, by = c("user_pk", "calendar_month"), all.x = T)
#This merge isn't perfect because (1) new users in MALT won't have a match to the monthly table
#and (2) continuing users for the new MALT month won't have a match to the monthly table
all_monthly$user_not_in_monthly <- !(all_monthly$user_pk %in% all_monthly_backup$user_pk)
#Handle new users first
all_monthly <- all_monthly %>% group_by(user_pk) %>% 
  mutate(min_malt_month = min(calendar_month))
all_monthly$new_user <- all_monthly$user_not_in_monthly == T & all_monthly$calendar_month == all_monthly$min_malt_month
#Add rolling # active months to each new user's rows
all_monthly_new <- filter(all_monthly, user_not_in_monthly == T) 
all_monthly_new <- arrange(all_monthly_new, user_pk, calendar_month)
all_monthly_new <- all_monthly_new %>% group_by(user_pk) %>% 
  mutate(previous_active_mo_update = seq_along(calendar_month)-1)
all_monthly_new <- select(all_monthly_new, user_pk, calendar_month, previous_active_mo_update)
all_monthly <- merge(all_monthly, all_monthly_new, by = c("user_pk", "calendar_month"), 
                     all.x = T)
all_monthly$previous_active_months_rolling[is.na(all_monthly$previous_active_months_rolling & all_monthly$user_not_in_monthly == T)] <- 
  all_monthly$previous_active_mo_update[is.na(all_monthly$previous_active_months_rolling & all_monthly$user_not_in_monthly == T)]
#Handle continuing users next 
all_monthly_continuing <- filter(all_monthly, user_not_in_monthly == F) 
all_monthly_continuing <- arrange(all_monthly_continuing, user_pk, calendar_month)
all_monthly_backup <- all_monthly_backup %>% group_by(user_pk) %>% 
  mutate(previous_active_mo_max = max(previous_active_months_rolling))
to_merge <- all_monthly_backup %>% group_by(user_pk) %>% summarise(previous_active_mo_max = unique(previous_active_mo_max))
all_monthly_continuing <- merge(all_monthly_continuing, to_merge, by = "user_pk", all.x = T)
all_monthly_continuing <- filter(all_monthly_continuing, is.na(previous_active_months_rolling)) 
all_monthly_continuing <- arrange(all_monthly_continuing, user_pk, calendar_month)
all_monthly_continuing <- all_monthly_continuing %>% group_by(user_pk) %>% 
  mutate(previous_active_mo_offset = seq_along(calendar_month))
all_monthly_continuing$previous_active_mo_update2 <- all_monthly_continuing$previous_active_mo_max + all_monthly_continuing$previous_active_mo_offset
all_monthly_continuing <- select(all_monthly_continuing, user_pk, calendar_month, previous_active_mo_update2)
all_monthly <- merge(all_monthly, all_monthly_continuing, by = c("user_pk", "calendar_month"), all.x = T)
all_monthly$previous_active_months_rolling[is.na(all_monthly$previous_active_months_rolling & all_monthly$user_not_in_monthly == F)] <- 
  all_monthly$previous_active_mo_update2[is.na(all_monthly$previous_active_months_rolling & all_monthly$user_not_in_monthly == F)]

#wam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_monthly$wam_experienced <- all_monthly$previous_active_months_rolling >= 3
#pam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_monthly$pam_experienced <- all_monthly$previous_active_months_rolling >= 3

#wam_using
all_monthly$active_days <- as.numeric(all_monthly$active_days)
#Run lines 473-478
#Surprising that active_days calculated on receiving is so different from active_days calculated on time_start
#Run lines 481-553

#TESTING
test_monthly <- filter(all_monthly_backup, calendar_month == "2015-04-01")
test_monthly$active_days_monthly <- test_monthly$active_days
test_monthly <- select(test_monthly, user_pk, calendar_month, active_days_monthly, nforms)

test_malt <- filter(all_monthly, calendar_month == "2015-04-01")
test_malt$active_days_malt <- test_malt$active_days
test_malt <- select(test_malt, user_pk, calendar_month, active_days_malt)

check_using <- merge(test_monthly, test_malt, by = c("user_pk", "calendar_month"), all = T)
check_using$active_days_equal <- check_using$active_days_monthly == check_using$active_days_malt
check_using$active_days_malt_less <- check_using$active_days_monthly > check_using$active_days_malt
check_using <- filter(check_using, check_using$active_days_malt < 4 & check_using$active_days_malt_less == T)

form_check <- filter(form_table, user_pk ==  113)
form_check$date_received <- as.Date(substr(form_check$received_on, 1, 10))
form_check$date_start <- as.Date(substr(form_check$time_start, 1, 10))
form_check <- arrange(form_check, date_start)
form_check <- filter(form_check, (date_start <= "2015-05-31" & date_start >= "2015-05-01") | 
                       (date_received <= "2015-05-31" & date_received >= "2015-05-01"))
length(unique(form_check$date_start))
length(unique(form_check$date_received))
