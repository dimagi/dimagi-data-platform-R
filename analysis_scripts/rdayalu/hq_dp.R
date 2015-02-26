#This code is to get 100% alignment between DP and HQ, especially in terms 
#(1) number of active users per month, (2) user type, (3) general # of forms.
#Number of forms might be difficult to get 100% alignment on. 

# 2/16/15

form_table$form_date <- substr(form_table$time_start, 1, 10)
form_table$form_date <- as.Date(form_table$form_date)
form_table$form_date_received <- substr(form_table$received_on, 1, 10)
form_table$form_date_received <- as.Date(form_table$form_date_received)
forms_nov <- filter(form_table, form_date >= "2014-11-01" & form_date <= "2014-11-30")
dp_domain_pk_with_forms <- data.frame(unique(form_table$domain_id))
write.csv(dp_domain_pk_with_forms, file = "dp_domain_pk_with_forms.csv")
test <- filter(all_monthly, user_id == "1b9883444260f98aa44c2bc397dc9a9a")

# 2/4/15
hq <- read.csv(file="hq_nov_2014.csv")
dp <- read.csv(file="dp_nov_2014.csv")

hq$dup_id <- duplicated(hq$user_id) | duplicated(hq$user_id, fromLast=T)
dp$dup_id <- duplicated(dp$user_id) | duplicated(dp$user_id, fromLast=T)

dp_unique <- dp[dp$dup_id == F,]
test <- data.frame(unique(dp$user_id[duplicated(dp$user_id)]))

summary(dp_unique$user_id %in% hq$user_id)
summary(hq$user_id %in% dp_unique$user_id)
test <- data.frame(unique(dp_unique$user_id[!(dp_unique$user_id %in% hq$user_id)]))
test <- data.frame(unique(hq$user_id[!(hq$user_id %in% dp_unique$user_id)]))
test <- dp_unique[!(dp_unique$user_id %in% hq$user_id),]
test <- hq[!(hq$user_id %in% dp_unique$user_id),]

#Change column names as needed
names(dp_unique)[names(dp_unique) == "domain"] = "domain_dp"
names(dp_unique)[names(dp_unique) == "user_type"] = "user_type_dp"
names(dp_unique)[names(dp_unique) == "is_test"] = "is_test_dp"
names(dp_unique)[names(dp_unique) == "nforms"] = "nforms_dp"
names(dp_unique)[names(dp_unique) == "ncases_touched"] = "ncases_dp"
names(hq)[names(hq) == "domain"] = "domain_hq"
names(hq)[names(hq) == "user_type"] = "user_type_hq"
names(hq)[names(hq) == "is_test"] = "is_test_hq"
names(hq)[names(hq) == "nforms"] = "nforms_hq"
names(hq)[names(hq) == "ncase"] = "ncases_hq"

#Merge two tables
hq_dp <- merge(hq, dp_unique, by = "user_id", all.x = F)

#Check domains
hq_dp$domain_dp <- as.character(hq_dp$domain_dp)
hq_dp$domain_hq <- as.character(hq_dp$domain_hq)
summary(hq_dp$domain_dp == hq_dp$domain_hq)
test <- hq_dp[!(hq_dp$domain_dp == hq_dp$domain_hq),]
test <- select(test, user_id)

#Check user_type
test <- filter(hq_dp, user_type_dp == "web")

#Check test domains
hq_dp$is_test_hq <- as.character(hq_dp$is_test_hq)
hq_dp$is_test_dp <- as.character(hq_dp$is_test_dp)
hq_dp$is_test_hq[hq_dp$is_test_hq == ""] <- "none" 
hq_dp$is_test_hq[hq_dp$is_test_hq == "FALSE"] <- "false"
hq_dp$is_test_hq[hq_dp$is_test_hq == "TRUE"] <- "true"
summary(hq_dp$is_test_hq == hq_dp$is_test_dp)
test <- hq_dp[!(hq_dp$is_test_hq == hq_dp$is_test_dp),]

#Check # forms and # cases 
summary(hq_dp$nforms_dp == hq_dp$nforms_hq)
summary(hq_dp$ncases_dp == hq_dp$ncases_hq)

#1/9/2015
#First import monthly_table for all domains
#Set permitted_data_only = false
source(file.path("analysis_scripts","raw_data","data_import.R", fsep = .Platform$file.sep))

all_monthly <- monthly_table

#Merge domain facets from domain table into all_monthly table
facets_to_merge <- select(domain_table, name, is_test, active)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)

#Format date variables
all_monthly$date_first_visit = as.Date(all_monthly$date_first_visit)
all_monthly$date_last_visit = as.Date(all_monthly$date_last_visit)

#Convert calendar month to actual date
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)

#Keep only rows for Nov 2014 to compare with HQ
Nov_2014 <- filter(all_monthly, calendar_month == "2014-11-01")

#Get user_type from db and merge to Nov_2014
user_type <- get_user_type_table(db)
user_type <- select(user_type, user_pk, username, user_type, is_superuser)
Nov_2014 <- merge(Nov_2014, user_type, by.all = "user_pk", all.x = T)

#Keep variables of interest per Neal's 1/9/15 email
Nov_2014 <- select(Nov_2014, user_pk, user_id, username, domain, user_type, 
                   summary_device_type, is_test, nforms, ncases_touched, is_superuser)

#Create csv file
write.csv(Nov_2014, file = "Nov_2014_dp.csv")

#------------------------------------------------------------------------#
#Older code
#------------------------------------------------------------------------#
nusers <- monthly_table %.% 
  group_by(domain, month.index) %.% 
  summarise (nusers = length(unique(user_id)),
             nandroid = sum(summary_device_type == "Android", na.rm = T),
             nnokia = sum(summary_device_type == "Nokia", na.rm = T),
             ncloudcare = sum(summary_device_type == "Cloudcare", na.rm = T),
             nmulti = sum(summary_device_type == "Multi", na.rm = T),
             nnone = sum(summary_device_type == "None", na.rm = T),
             nother = sum(summary_device_type == "Other", na.rm = T),
             nmissing = sum(is.na(summary_device_type)))

nusers_aug <- filter(nusers, month.index == "Aug 2014")

#Merge domain facets from domain table into all_monthly table
to_merge <- select(domains_HQ, Project, X..Active.Mobile.Workers, X..Mobile.Workers, 
                   X..Mobile.Workers..Submitted.Form., X..Web.Users)

nusers_aug <- merge(nusers_aug, to_merge, by.x = "domain", 
                     by.y = "Project", all.x = T)

nusers_aug <- arrange(nusers_aug, desc(nusers))

nusers_aug$nusers_diff <- nusers_aug$nusers - nusers_aug$X..Active.Mobile.Workers

write.csv(nusers_aug, file = "nusers_dp_hq_v2.csv")