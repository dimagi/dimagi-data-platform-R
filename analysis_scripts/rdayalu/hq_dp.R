#This code is to get 100% alignment between DP and HQ, especially in terms 
#(1) number of active users per month, (2) user type, (3) general # of forms.
#Number of forms might be difficult to get 100% alignment on. 

#2/16/15
library(lubridate)
library(dplyr)
#Upload HQ data
forms_hq <- read.csv(file="test.csv")
names(forms_hq)[names(forms_hq) == "user_id"] = "user_id_hq"
names(forms_hq)[names(forms_hq) == "domain"] = "domain_hq"
names(forms_hq)[names(forms_hq) == "is_test"] = "is_test_hq"
names(forms_hq)[names(forms_hq) == "user_type"] = "user_type_hq"
forms_hq$domain_hq <- as.character(forms_hq$domain_hq)
forms_hq$hq <- T

#Upload DP data
#forms_dp <- read.csv(file="forms_dp.csv")
#forms_dp$form_date <- as.Date(forms_dp$form_date)
forms_dp <- read.csv(file="forms_nov_received.csv")
forms_dp$form_date_received <- as.Date(forms_dp$form_date_received)
forms_dp <- select(forms_dp, -X)
names(forms_dp)[names(forms_dp) == "user_id"] = "user_pk"
domains <- read.csv(file="domain_master_list.csv")
names(domains)[names(domains) == "id"] = "domain_pk"
users <- read.csv(file="users.csv")
names(users)[names(users) == "id"] = "user_pk"
domains <- select(domains, -X)
users <- select(users, user_pk, user_id)
users$user_id <- as.character(users$user_id)
forms_dp <- merge(forms_dp, domains, by.x = "domain_id", by.y = "domain_pk", 
                  all.x = T)
forms_dp <- merge(forms_dp, users, by="user_pk", all.x = T)
names(forms_dp)[names(forms_dp) == "user_id"] = "user_id_dp"
names(forms_dp)[names(forms_dp) == "name"] = "domain_dp"
forms_dp$dp <- T
forms_dp$domain_dp <- as.character(forms_dp$domain_dp)

#Merge together
hq_dp <- merge(forms_hq, forms_dp, by = "form_id", all = T)
hq_dp$user_id_hq <- as.character(hq_dp$user_id_hq)
hq_dp$user_id_dp <- as.character(hq_dp$user_id_dp)
hq_dp$domain_dp <- as.character(hq_dp$domain_dp)
hq_dp$domain_hq <- as.character(hq_dp$domain_hq)

#Tables of users in HQ but not in DP and vice versa
hq_not_in_dp <- filter(hq_dp, is.na(dp))
dp_not_in_hq <- filter(hq_dp, is.na(hq))
hq_dp_same <- filter(hq_dp, dp == T & hq == T)

#Flag users in HQ but not at all in DP user table
hq_not_in_dp$not_dp_user <- !(hq_not_in_dp$user_id_hq %in% users$user_id)

#Flag domains in HQ that do not have any forms per DP
domains_dp_nov <- as.character(unique(forms_dp$domain_dp))
domains_hq_nov <- as.character(unique(forms_hq$domain_hq))
hq_not_in_dp$not_dp_domain_nov <- !(hq_not_in_dp$domain_hq %in% domains_dp_nov)
dp_domain_pk_with_forms <- read.csv(file = "dp_domain_pk_with_forms.csv")
dp_domain_pk_with_forms <- select(dp_domain_pk_with_forms, -X)
names(dp_domain_pk_with_forms)[names(dp_domain_pk_with_forms) == 
                                 "unique.form_table.domain_id."] = "domain_pk"
dp_domain_pk_with_forms <- merge(dp_domain_pk_with_forms, domains, 
                                 by="domain_pk", all.x = T)
dp_domain_pk_with_forms$name <- as.character(dp_domain_pk_with_forms$name)
hq_not_in_dp$not_dp_domain_ever <- !(hq_not_in_dp$domain_hq %in% 
                                       dp_domain_pk_with_forms$name)

#Flag users in HQ that do no have any forms per DP
user_dp_has_forms <- read.csv(file = "user_dp_has_forms.csv")
user_dp_has_forms <- select(user_dp_has_forms, -X)
names(user_dp_has_forms)[names(user_dp_has_forms) == 
                           "unique.form_table.user_id."] = "user_pk"
user_dp_has_forms <- merge(user_dp_has_forms, users, 
                           by="user_pk", all.x = T)
user_dp_has_forms$user_id <- as.character(user_dp_has_forms$user_id)
hq_not_in_dp$not_dp_user_ever <- !(hq_not_in_dp$user_id_hq %in% 
                                     user_dp_has_forms$user_id)

sample(hq_not_in_dp$user_id_hq[hq_not_in_dp$not_dp_user_ever == F], 5, 
       replace = FALSE)
test <- filter(hq_dp, user_id_hq == "59f0d35f0cd17844630742144a1b0c20")

#Analyze discrepancies
summary(hq_dp$user_id_hq == hq_dp$user_id_dp)
summary(hq_dp$hq)
summary(hq_dp$dp)

table(hq_not_in_dp$domain_hq, useNA = "always")
length(unique(hq_not_in_dp$user_id_hq))

table(dp_not_in_hq$domain_dp, useNA = "always")
length(unique(dp_not_in_hq$user_id_dp))

#DP not in HQ
#dp_users_not_in_hq <- unique(dp_not_in_hq$user_id_dp)
#Which of these users are not in November HQ?
dp_not_in_hq$received_on <- as.Date(dp_not_in_hq$received_on)
forms_hq$user_id_hq <- as.character(forms_hq$user_id_hq)
dp_not_in_hq$user_not_in_hq_nov <- !(dp_not_in_hq$user_id_dp %in% 
                                       forms_hq$user_id_hq)
test <- filter(dp_not_in_hq, user_not_in_hq_nov == T)
user_type <- read.csv(file="user_type.csv")
user_type <- select(user_type, user_pk, deactivated, deleted)
test <- merge(test, user_type, by="user_pk", all.x = T)
deleted_users <- filter(test, deleted == T)
non_deleted_users <- filter(test, deleted == F | is.na(deleted))
non_deleted_users <- filter(non_deleted_users, deactivated == F | is.na(deactivated))


# 2/15/15
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