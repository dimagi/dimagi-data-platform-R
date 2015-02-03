#This code is to get 100% alignment between DP and HQ, especially in terms 
#(1) number of active users per month, (2) user type, (3) general # of forms.
#Number of forms might be difficult to get 100% alignment on. 
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