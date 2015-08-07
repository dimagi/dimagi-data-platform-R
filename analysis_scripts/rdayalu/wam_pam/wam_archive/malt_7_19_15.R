#This code is to create the WAM table based on the DP MALT and HQ MALT
#The process is described in the following document:
#https://docs.google.com/document/d/1qJZyui7bvyS-Gh7t5nE3O3a1rLqUNZQzNrqH5S4YNPU/edit

#------------------------------------------------------------------------#
#Open database connection: be sure to set config_system first
#------------------------------------------------------------------------#

library(dplyr)
library(zoo)
library(lubridate)

# Load system config file
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))
# Get db connection
db <- get_db_connection(system_conf)

#------------------------------------------------------------------------#
#Import forms and create dp_malt
#------------------------------------------------------------------------#

#First we need to generate DP MALT based on the current form_table in DP
#Import form table
form_table <- tbl(db, "form")
#form_table <- get_data_source(db, "form", 10000)
form_table <- collect(form_table)
names(form_table)[names(form_table) == "user_id"] = "user_pk"
form_table <- select(form_table, id, user_pk, received_on, time_start, formdef_id, visit_id, domain_id, application_id)
names(form_table)[names(form_table) == "id"] = "form_pk"
names(form_table)[names(form_table) == "domain_id"] = "domain_pk"
names(form_table)[names(form_table) == "application_id"] = "application_pk"

#Create form_date and form_month based on server time
form_table$form_date <- as.Date(substr(form_table$received_on, 1, 10))
form_table$form_month <- floor_date(form_table$form_date, "month")

#Create DP MALT
dp_malt <- form_table %>% group_by(domain_pk, user_pk, application_pk, form_month) %>% 
  summarise(nforms = length(unique(form_pk)))

#------------------------------------------------------------------------#
#Merge in users, domain and app tables
#------------------------------------------------------------------------#

#Merge in user table: user_id, username, email
#Import user table and user_type table
users <- tbl(db, "users") 
users <- collect(users)
user_type <- get_user_type_table(db)
user_type <- collect(user_type)
#Merge user tables
users <- merge(users, user_type, by.x = "id", by.y = "user_pk", all.x = T)
users <- select(users, -c(user_id.y, username.y, first_name, last_name, default_phone_number,
                          groups, phone_numbers, deactivated, deleted))
names(users)[names(users) == "username.x"] = "username"
names(users)[names(users) == "id"] = "user_pk"
names(users)[names(users) == "user_id.x"] = "user_id"
#users$is_web_user <- users$user_type == "web"
dp_malt <- merge(dp_malt, select(users, user_pk, user_id, username, email, user_type), 
                 by = "user_pk", all.x = T)
dp_malt$user_type[dp_malt$user_type == "mobile"] <- "CommCareUser"
dp_malt$user_type[dp_malt$user_type == "web"] <- "WebUser"


#Merge in domain table: name
#Pull domain table with new business unit info
domain <- get_domain_table(db)
names(domain)[names(domain) == "id"] = "domain_pk"
dp_malt <- merge(dp_malt, select(domain, domain_pk, name), by.x = "domain_pk", by.y = "domain_pk", 
                 all.x = T)
names(dp_malt)[names(dp_malt) == "name"] = "domain"

#Merge in app table: app_id, amplifies_workers, amplifies_project
#Pull in app table from db
app <- tbl(db, "application")
app <- collect(app)
names(app)[names(app) == "id"] = "application_pk"
#Remove all double quotes from inside attributes string. Replace with underscores
app$attributes <- gsub('"', "_", app$attributes)
#Parse amplifies_X values from app table
#Add amplifies_workers/project to form_table by app_id
#amplifies_workers
app$amplifies_workers <- NA
app$amplifies_workers[grep("_amplifies_workers_=>_yes_", app$attributes, fixed=T)] <- T
app$amplifies_workers[grep("_amplifies_workers_=>_no_", app$attributes, fixed=T)] <- F
#Manually tag domains
#app$amplifies_workers[app$id == 339] <- T #tulasalud "Kawok Vigilancia Comunitario 3.0"
#amplifies_project
app$amplifies_project <- NA
app$amplifies_project[grep("_amplifies_project_=>_yes_", app$attributes, fixed=T)] <- T
app$amplifies_project[grep("_amplifies_project_=>_no_", app$attributes, fixed=T)] <- F
#Manually tag domains
#app$amplifies_project[app$id == 339] <- T #tulasalud "Kawok Vigilancia Comunitario 3.0"
dp_malt <- merge(dp_malt, select(app, application_pk, app_id, amplifies_workers, amplifies_project), 
                 by = "application_pk", all.x = T)

#Remove columns not in HQ MALT
dp_malt <- select(dp_malt, -c(application_pk, domain_pk, user_pk))

#------------------------------------------------------------------------#
#Merge in HQ MALT
#------------------------------------------------------------------------#

#Import HQ MALT
hq_malt <- read.csv(file = "malt_may_june.csv")

#Delete is_app_deleted, id row
#This was added to the HQ MALT and represents apps that were deleted 
#some time after forms were submitted using the app. We don't treat these rows differently
#while calculating WAMs. Per Neal, we don't need to worry about this right now.
hq_malt <- select(hq_malt, -c(id, is_app_deleted))

#Rename columns
names(hq_malt)[names(hq_malt) == "month"] = "form_month"
names(hq_malt)[names(hq_malt) == "domain_name"] = "domain"
names(hq_malt)[names(hq_malt) == "num_of_forms"] = "nforms"
names(hq_malt)[names(hq_malt) == "wam"] = "amplifies_workers"
names(hq_malt)[names(hq_malt) == "pam"] = "amplifies_project"

#Convert all variables to correct class
hq_malt$form_month <- as.Date(hq_malt$form_month, format="%m/%d/%y")
hq_malt$user_id <- as.character(hq_malt$user_id) 
hq_malt$username <- as.character(hq_malt$username)
hq_malt$email <- as.character(hq_malt$email)
hq_malt$domain <- as.character(hq_malt$domain)
hq_malt$app_id <- as.character(hq_malt$app_id)
hq_malt$amplifies_workers <- as.character(hq_malt$amplifies_workers)
hq_malt$amplifies_project <- as.character(hq_malt$amplifies_project)
hq_malt$is_web_user <- as.character(hq_malt$is_web_user)

#Convert relevant variables to logical class
#is_web_user
hq_malt$is_web_user2 <- NA
hq_malt$is_web_user2[hq_malt$is_web_user == "f"] <- F
hq_malt$is_web_user2[hq_malt$is_web_user == "t"] <- T
hq_malt <- select(hq_malt, -is_web_user)
names(hq_malt)[names(hq_malt) == "is_web_user2"] = "is_web_user"
#amplifies_workers
hq_malt$amplifies_workers2 <- NA
hq_malt$amplifies_workers2[hq_malt$amplifies_workers == "no"] <- F
hq_malt$amplifies_workers2[hq_malt$amplifies_workers == "yes"] <- T
hq_malt <- select(hq_malt, -amplifies_workers)
names(hq_malt)[names(hq_malt) == "amplifies_workers2"] = "amplifies_workers"
#amplifies_project
hq_malt$amplifies_project2 <- NA
hq_malt$amplifies_project2[hq_malt$amplifies_project == "no"] <- F
hq_malt$amplifies_project2[hq_malt$amplifies_project == "yes"] <- T
hq_malt <- select(hq_malt, -amplifies_project)
names(hq_malt)[names(hq_malt) == "amplifies_project2"] = "amplifies_project"

#Check dp_malt and hq_malt tables before appending together
summary(names(dp_malt) %in% names(hq_malt))

#Delete rows that cause monthly overlap between dp_malt and hq_malt
table(dp_malt$form_month)
table(hq_malt$form_month)

dp_malt <- filter(dp_malt, form_month <= "2015-05-01")
hq_malt <- filter(hq_malt, form_month == "2015-06-01")

#Append together
all_malt <- rbind(dp_malt, hq_malt)
table(all_malt$form_month)

#Add unique integer column
all_malt$unique_int <- 1:nrow(all_malt)

#------------------------------------------------------------------------#
#User exclusions
#------------------------------------------------------------------------#

#Exclude demo users and NA/NONE users
#First count these users per domain per month
user_exclusion <- all_malt %>% group_by(domain) %>% 
  summarise(users_initial = length(unique(user_id)),
            user_app_months_initial = length(unique(unique_int)),
            user_app_months_user_id_demo_na = length(unique(unique_int[user_id =="demo_user" | 
                                                                         user_id =="NONE" | 
                                                                         user_id == "none" | 
                                                                         is.na(user_id)])))


all_malt = all_malt[!(all_malt$user_id =="demo_user"),]
all_malt = all_malt[!(all_malt$user_id =="NONE"),]
all_malt = all_malt[!(all_malt$user_id =="none"),]
all_malt = all_malt[!is.na(all_malt$user_id),]

#Exclude users who submit to multiple domains
#First count these users per domain per month
chw_single_domain <- all_malt %>% group_by(user_id) %>% 
  summarise(n_domains = length(unique(domain)))
chw_single_domain <- filter(chw_single_domain, n_domains == 1)
all_malt$user_multiple_domain <- !(all_malt$user_id %in% chw_single_domain$user_id)
all_malt <- all_malt %>% group_by(domain) %>% 
  mutate(user_months_user_pk_multiple_domains = sum(user_multiple_domain))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_months_user_pk_multiple_domains))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_user_id_multiple_domains"
all_malt <- all_malt[all_malt$user_id %in% chw_single_domain$user_id,]

#Used to exclude rows with nforms = NA: Did this with WAM calculations based on monthly table - 
#not necessary to do this now since the MALT is based on forms that have been submitted. 
#users_active_days_na

#Used to exclude rows with calendar_month = NA: Did this with WAM calculations based on monthly table - 
#Don't need to do this since the form_table has all valid form dates
#users_calendar_month_na 

#Exclude all users with user_type = web
#Used to exclude users with user_type = web and summary_device_type != cloud care during any month - 
#but we are simplifying this now and excluding all web users
#First count these users per domain per month
all_malt$is_web_user <- all_malt$user_type == "WebUser"
all_malt <- all_malt %>% group_by(domain) %>% 
  mutate(user_app_months_web = sum(is_web_user, na.rm=T))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_app_months_web))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_web"
all_malt <- filter(all_malt, is_web_user == F | is.na(is_web_user))

#Exclude dimagi users based on username and email
#Used to exclude dimagi users and superusers, but excluding dimagi users only 
#is fine.
#First count these users per domain per month
dimagi_users <- c(all_malt$user_id[grep("dimagi", all_malt$email, fixed=T)], 
                  all_malt$user_id[grep("dimagi", all_malt$username, fixed=T)])
all_malt$dimagi_user <- all_malt$user_id %in% dimagi_users
all_malt <- all_malt %>% group_by(domain) %>% 
  mutate(user_app_months_dimagi_user = sum(dimagi_user, na.rm=T))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_app_months_dimagi_user))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_dimagi_user"
all_malt <- filter(all_malt, dimagi_user == F)

#Used to exclude users with summary_device_type = SMS during any month
#Don't need to do this anymore because dp_malt and hq_malt don't include
#SMS submissions
#First count these users per domain per month
users_sms_any_month <- unique(monthly_table$user_id[monthly_table$summary_device_type == "Sms"])
all_malt$users_sms_any_month <- all_malt$user_id %in% users_sms_any_month
all_malt <- all_malt %>% group_by(domain) %>% 
  mutate(user_app_months_sms = sum(users_sms_any_month, na.rm=T))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_app_months_sms))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_sms"
all_malt <- filter(all_malt, users_sms_any_month == F)

#Used to exclude users without form_ids and app_ids
#We are going keep all rows with app_id because dp_malt
#currently has some unresolved issues with the app_ids taken from 
#the app table in the DP (We are missing fair number of app_ids, so 
#we have a lot of app_id = NA in dp_malt). Following up with Yedi on this.
#all_monthly$form_app_id_na <- all_monthly$submitted_single_app == F & 
#  all_monthly$submitted_multiple_app == F
#all_monthly <- all_monthly %>% group_by(domain) %>% 
#  mutate(user_months_form_app_id_na = sum(form_app_id_na))
#test <- all_monthly %>% group_by(domain) %>% summarise(check=unique(user_months_form_app_id_na))
#user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
#names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_form_app_id_na"
#all_monthly <- all_monthly[!(all_monthly$submitted_single_app == F & 
#                               all_monthly$submitted_multiple_app == F),]

#Add final columns to user_exclusion table
all_malt <- all_malt %>% group_by(domain) %>% 
  mutate(user_app_months_final = length(unique(unique_int)), 
         users_final = length(unique(user_id)))
test <- all_malt %>% group_by(domain) %>% summarise(check=unique(user_app_months_final), 
                                                    check2=unique(users_final))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_app_months_final"
names(user_exclusion)[names(user_exclusion) == "check2"] = "users_final"
#Convert all NA values to 0
user_exclusion[is.na(user_exclusion)] <- 0

write.csv(user_exclusion, file = "user_exclusions.csv", row.names = F)

all_malt$used_wam_elig_true <- all_malt$amplifies_workers == T
all_malt$used_wam_elig_true[is.na(all_malt$used_wam_elig_true)] <- F
all_malt$used_wam_elig_false <- all_malt$amplifies_workers == F
all_malt$used_wam_elig_false[is.na(all_malt$used_wam_elig_false)] <- F
all_malt$used_wam_elig_na <- is.na(all_malt$amplifies_workers)

#------------------------------------------------------------------------#
#Consolidate WAM categories for output tables so that each user has only
#one wam_eligible, wam_experienced and wam_using value. We don't want a 
#user to count in more than one of our 12 WAM categories
#------------------------------------------------------------------------#

#Create all_malt_user_month table with just one row per user per month
all_malt_user_month <- all_malt %>% group_by(user_id, form_month) %>% 
  summarise(domain = unique(domain),
            n_apps = length(unique(app_id)), 
            used_wam_elig_true = sum(used_wam_elig_true) >= 1,
            used_wam_elig_false = sum(used_wam_elig_false) >= 1,
            used_wam_elig_na = sum(used_wam_elig_na) >= 1, 
            nforms = sum(nforms)) 

#------------------------------------------------------------------------#
#wam_eligible
#------------------------------------------------------------------------#

all_malt_user_month$wam_eligible <- NA
all_malt_user_month$wam_eligible[all_malt_user_month$used_wam_elig_true == T] <- T
all_malt_user_month$wam_eligible[all_malt_user_month$used_wam_elig_true == F &
                                   all_malt_user_month$used_wam_elig_false == T] <- F

#------------------------------------------------------------------------#
#wam_experienced, pam_experienced
#------------------------------------------------------------------------#

#Add rolling # active months to each user-app row
all_malt_user_month <- arrange(all_malt_user_month, user_id, form_month)
all_malt_user_month <- all_malt_user_month %>% group_by(user_id) %>% 
  mutate(previous_active_months_rolling = seq_along(form_month)-1)

#wam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_malt_user_month$wam_experienced <- all_malt_user_month$previous_active_months_rolling >= 3


#------------------------------------------------------------------------#
#wam_using, pam_using
#------------------------------------------------------------------------#

#wam_using == T or pam_using == T
#if the user met our criteria this month for sufficient usage of the app. 
#Specifically, they submitted >= 15 forms during the user-app-month in question
all_malt_user_month$wam_using <- all_malt_user_month$nforms >= 15

#------------------------------------------------------------------------#
#WAM categories for output tables
#------------------------------------------------------------------------#

#We have 12 combinations for of wam_eligible(T/F/NA), wam_experienced(T/F) 
#and wam_using(T/F)
all_malt_user_month$elig_exp_using <- all_malt_user_month$wam_eligible == T & all_malt_user_month$wam_experienced == T & 
  all_malt_user_month$wam_using == T
all_malt_user_month$elig_exp_notusing <- all_malt_user_month$wam_eligible == T & all_malt_user_month$wam_experienced == T & 
  all_malt_user_month$wam_using == F
all_malt_user_month$elig_notexp_using <- all_malt_user_month$wam_eligible == T & all_malt_user_month$wam_experienced == F & 
  all_malt_user_month$wam_using == T
all_malt_user_month$elig_notexp_notusing <- all_malt_user_month$wam_eligible == T & all_malt_user_month$wam_experienced == F & 
  all_malt_user_month$wam_using == F

all_malt_user_month$notelig_exp_using <- all_malt_user_month$wam_eligible == F & all_malt_user_month$wam_experienced == T & 
  all_malt_user_month$wam_using == T
all_malt_user_month$notelig_exp_notusing <- all_malt_user_month$wam_eligible == F & all_malt_user_month$wam_experienced == T & 
  all_malt_user_month$wam_using == F
all_malt_user_month$notelig_notexp_using <- all_malt_user_month$wam_eligible == F & all_malt_user_month$wam_experienced == F & 
  all_malt_user_month$wam_using == T
all_malt_user_month$notelig_notexp_notusing <- all_malt_user_month$wam_eligible == F & all_malt_user_month$wam_experienced == F & 
  all_malt_user_month$wam_using == F

all_malt_user_month$na_exp_using <- is.na(all_malt_user_month$wam_eligible)& all_malt_user_month$wam_experienced == T & 
  all_malt_user_month$wam_using == T
all_malt_user_month$na_exp_notusing <- is.na(all_malt_user_month$wam_eligible) & all_malt_user_month$wam_experienced == T & 
  all_malt_user_month$wam_using == F
all_malt_user_month$na_notexp_using <- is.na(all_malt_user_month$wam_eligible)& all_malt_user_month$wam_experienced == F & 
  all_malt_user_month$wam_using == T
all_malt_user_month$na_notexp_notusing <- is.na(all_malt_user_month$wam_eligible) & all_malt_user_month$wam_experienced == F & 
  all_malt_user_month$wam_using == F

#We have 4 combinations for wam_experienced(T/F) and wam_using(T/F)
all_malt_user_month$using_exp <- all_malt_user_month$wam_experienced == T & all_malt_user_month$wam_using == T
all_malt_user_month$notusing_exp <- all_malt_user_month$wam_experienced == T & all_malt_user_month$wam_using == F
all_malt_user_month$using_notexp <- all_malt_user_month$wam_experienced == F & all_malt_user_month$wam_using == T
all_malt_user_month$notusing_notexp <- all_malt_user_month$wam_experienced == F & all_malt_user_month$wam_using == F

#Create denominators for Table 1

#d2: all users who submitted forms at least 4+ months ago, regardless of whether or not they 
#are experienced as of the month in question (i.e., these users could theoretically be 
#experienced by the month in question. For example, a user who submitted just one form 
#6 months ago. This is just whether the user’s first form was more than 3 months ago)

#Calculate user_start_date
all_malt_user_month <- all_malt_user_month %>% group_by(user_id) %>% 
  mutate(user_start_month = min(form_month, na.rm=T))

#Calculate user_experienced_date
all_malt_user_month$user_experienced_date <- all_malt_user_month$user_start_month + months(3)

#Based on user_experienced_date, we will count the # of theoretically experienced users
#for each month in question
all_malt_user_month <- all_malt_user_month %>% group_by(form_month) %>% 
  mutate(total_users_possibly_experienced = length(unique(user_id[user_experienced_date <= form_month])))

#d3: all experienced users (all users who are actually experienced by the time they reach 
#the month in question, regardless of whether or not they are active during the month 
#in question)
#We are flagging the calendar month in which each user actually becomes experienced
all_malt_user_month$new_exp_user <- all_malt_user_month$previous_active_months_rolling == 3
#Sort by calendar_month before calculating the cumulative sum 
#of new users in the next step
all_malt_user_month <- ungroup(all_malt_user_month)
all_malt_user_month <- arrange(all_malt_user_month, form_month)
#Calculate cumulative sum of new experienced users
all_malt_user_month$max_exp_users <- cumsum(all_malt_user_month$new_exp_user)
# Create row for total experienced users to date 
all_malt_user_month <- all_malt_user_month %>% group_by(form_month) %>% 
  mutate(total_exp_users_to_date = max(max_exp_users))

#------------------------------------------------------------------------#
#Prepare domain table
#------------------------------------------------------------------------#

#Prepare domain table for merging in domain facets
#Bring in sector information
sector <- tbl(db, "sector")
sector <- collect(sector)
names(sector)[names(sector) == "name"] = "sector_final"
domain_sector <- tbl(db, "domain_sector")
domain_sector <- collect(domain_sector)
domain_sector <- select(domain_sector, domain_id, sector_id)
domain <- merge(domain, domain_sector, by = "domain_id", all.x = T)
domain <- merge(domain, sector, by.x = "sector_id", by.y = "id", all.x = T)
#Bring in subsector information
subsector <- tbl(db, "subsector")
subsector <- collect(subsector)
subsector <- select(subsector, id, name)
subsector <- filter(subsector, !is.na(name))
subsector <- filter(subsector, name != "")
names(subsector)[names(subsector) == "name"] = "subsector_final"
domain_subsector <- tbl(db, "domain_subsector")
domain_subsector <- collect(domain_subsector)
domain_subsector <- select(domain_subsector, domain_id, subsector_id)
domain <- merge(domain, domain_subsector, by = "domain_id", all.x = T)
domain <- merge(domain, subsector, by.x = "subsector_id", by.y = "id", all.x = T)

#Consolidate country information
#Remove all double quotes from inside countries string.
domain$countries <- gsub('"', "", domain$countries)
is.na(domain$deployment.country) <- domain$deployment.country == ""
is.na(domain$countries) <- domain$countries == "{}" | 
  domain$countries == "{No country}"
domain$country_final <- domain$countries
use_deployment_country <- which(is.na(domain$countries) & !is.na(domain$deployment.country))
domain$country_final[use_deployment_country] <- 
  domain$deployment.country[use_deployment_country]

#Flag Dimagi self-starts
domain$self_start <- domain$internal.self_started

#Domain start date
domain$domain_start_date <- as.Date(substr(domain$cp_first_form, 1, 10))

#------------------------------------------------------------------------#
#Add amplifies_workers/project to domain table
#------------------------------------------------------------------------#

app_amplifies <- app

#Create vectors of domain_ids by value of amplifies_workers and amplifies_project
#These vectors are not mutually exclusive, meaning that some domains can be in more 
#than one vector
domain_has_amp_w_true <- unique(filter(app_amplifies, amplifies_workers == T)$domain_id)
domain_has_amp_w_false <- unique(filter(app_amplifies, amplifies_workers == F)$domain_id)
domain_has_amp_w_na <- unique(filter(app_amplifies, is.na(amplifies_workers))$domain_id) 

domain_has_amp_p_true <- unique(filter(app_amplifies, amplifies_project == T)$domain_id)
domain_has_amp_p_false <- unique(filter(app_amplifies, amplifies_project == F)$domain_id)
domain_has_amp_p_na <- unique(filter(app_amplifies, is.na(amplifies_project))$domain_id)

#Create mutually exclusive vectors, meaning that no domains overlap between vectors 
#Keep domain_has_amp_w_true as is
#Remove domain_has_amp_w_true from the other two vectors
domain_has_amp_w_false <- domain_has_amp_w_false[!(domain_has_amp_w_false %in% domain_has_amp_w_true)]
domain_has_amp_w_na <- domain_has_amp_w_na[!(domain_has_amp_w_na %in% domain_has_amp_w_true)]
#Remove domain_has_amp_w_false from domain_has_amp_w_na
domain_has_amp_w_na <- domain_has_amp_w_na[!(domain_has_amp_w_na %in% domain_has_amp_w_false)]

#Keep domain_has_amp_p_true as is
#Remove domain_has_amp_p_true from the other two vectors
domain_has_amp_p_false <- domain_has_amp_p_false[!(domain_has_amp_p_false %in% domain_has_amp_p_true)]
domain_has_amp_p_na <- domain_has_amp_p_na[!(domain_has_amp_p_na %in% domain_has_amp_p_true)]
#Remove domain_has_amp_p_false from domain_has_amp_p_na
domain_has_amp_p_na <- domain_has_amp_p_na[!(domain_has_amp_p_na %in% domain_has_amp_p_false)]

domain$domain_has_amplifies_workers <- NA
domain$domain_has_amplifies_workers[domain$domain_id %in% domain_has_amp_w_true] <- T
domain$domain_has_amplifies_workers[domain$domain_id %in% domain_has_amp_w_false] <- F

domain$domain_has_amplifies_project <- NA
domain$domain_has_amplifies_project[domain$domain_id %in% domain_has_amp_p_true] <- T
domain$domain_has_amplifies_project[domain$domain_id %in% domain_has_amp_p_false] <- F

test <- get_post_processed_domain_table(db)
test <- test[,c("id", "new_business_unit")]
domain <- merge(domain, test, by.x = "domain_id", by.y = "id", all.x = T)
#------------------------------------------------------------------------#
#Table 1: WAM OVERVIEW
#------------------------------------------------------------------------#

#Create Table 1
table_1_wam <- all_malt_user_month %>% group_by(form_month) %>% 
  summarise(nusers = length(unique(user_id)),
            eligible = sum(wam_eligible, na.rm = T), 
            using = sum(wam_using, na.rm = T), 
            experienced = sum(wam_experienced, na.rm=T), 
            d2_exp_theory = unique(total_users_possibly_experienced), 
            d3_exp_real = unique(total_exp_users_to_date),
            
            elig_using_exp = sum(elig_exp_using, na.rm=T),
            elig_notusing_exp = sum(elig_exp_notusing, na.rm=T),
            elig_using_notexp = sum(elig_notexp_using, na.rm=T), 
            elig_notusing_notexp = sum(elig_notexp_notusing, na.rm=T),
            
            notelig_using_exp = sum(notelig_exp_using, na.rm=T),
            notelig_notusing_exp = sum(notelig_exp_notusing, na.rm=T),
            notelig_using_notexp = sum(notelig_notexp_using, na.rm=T), 
            notelig_notusing_notexp = sum(notelig_notexp_notusing, na.rm=T), 
            
            naelig_using_exp = sum(na_exp_using, na.rm=T),
            naelig_notusing_exp = sum(na_exp_notusing, na.rm=T), 
            naelig_using_notexp = sum(na_notexp_using, na.rm=T),
            naelig_notusing_notexp = sum(na_notexp_notusing, na.rm=T))

table_1_wam$per_wams_d4 <- round((table_1_wam$elig_using_exp/table_1_wam$experienced)*100, digits = 1)

#Filter table_1
table_1_wam <- filter(table_1_wam, form_month >= "2013-01-01", form_month <= "2015-06-01")
names(table_1_wam)[names(table_1_wam) == "form_month"] = "Calendar Month"
names(table_1_wam)[names(table_1_wam) == "nusers"] = "Active Users"
names(table_1_wam)[names(table_1_wam) == "eligible"] = "Eligible Users"
names(table_1_wam)[names(table_1_wam) == "using"] = "Sufficient Users"
names(table_1_wam)[names(table_1_wam) == "experienced"] = "Experienced Users"
names(table_1_wam)[names(table_1_wam) == "d2_exp_theory"] = "D2 All Possibly Exp Users"
names(table_1_wam)[names(table_1_wam) == "d3_exp_real"] = "D3 All Actually Exp Users"
write.csv(table_1_wam, file = "table_1_wam.csv", row.names=F)

#Merge domain facets to all_malt_user_month
facets_to_merge <- select(domain, name, domain_id, country_final, sector_final, 
                          subsector_final, self_start, domain_has_amplifies_workers, 
                          domain_has_amplifies_project, is_test, domain_start_date, new_business_unit)
all_malt_user_month <- merge(all_malt_user_month, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)
names(all_malt_user_month)[names(all_malt_user_month) == "domain_id"] = "domain_numeric"

# new_business_unit is currently based on deployment.country in Yedi's code:
# https://github.com/dimagi/dimagi-data-platform-R/blob/bf6e55f639abfd84a4310bc0bfa6578c8c3aa01c/function_libraries/report_utils.R#L169-L192
# Therefore the BU won't be specified for the countries variable which has the two-letter country code
# Need to manually define BU for these two-letter country codes
all_malt_user_month$new_business_unit[is.na(all_malt_user_month$new_business_unit)] <- "None"

missing_bu <- filter(all_malt_user_month, new_business_unit == "None")
assign_bu <- unique(missing_bu$country_final)
table(all_malt_user_month$new_business_unit, useNA = "always")

all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{BR,HT,IN,KE,MW,PE,ZA,TH,UG,ZW}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{GB}"] <- "Inc"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{MX}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{HT,VN}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{RW}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{KE,ZM}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{PE}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{IN}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{PH}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{TL}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{LK}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{US}"] <- "Inc"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{NE}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{MM}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{NG}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{SN}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{NP}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{HT}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{ID}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{TZ}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{ET}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{GD}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{ZA}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{ML,SN}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{LA}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{AF}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{GT}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{KE}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{BF}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{ZM}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{TH}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{SL}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{CO}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{BJ}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{BJ}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{GN}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{ET,UG}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{LS}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{GH}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{MW}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{MY}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{SS}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "Syria"] <- "Inc"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{SY}"] <- "Inc"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{IQ}"] <- "Inc"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{BD}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{JO}"] <- "Inc"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{MZ}"] <- "DMOZ"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{CN,GN}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{ZA,UG}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{VN}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{TR}"] <- "Inc"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{BZ,GT,HN}"] <- "DLAC"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{LR}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{BF,TD,NE}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{PK}"] <- "DSI"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{ML}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{SY,TR}"] <- "Inc"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{TD}"] <- "DWA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{MG}"] <- "DSA"
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{PH,TH}"] <- "DSI"

business_unit <- unique(all_malt_user_month$new_business_unit)
for (i in business_unit) {
  single_bu <- all_malt_user_month[all_malt_user_month$new_business_unit == i,]
  #Create Table 1
  table_1_wam_bu <- single_bu %>% group_by(form_month) %>% 
    summarise(nusers = length(unique(user_id)),
              eligible = sum(wam_eligible, na.rm = T), 
              using = sum(wam_using, na.rm = T), 
              experienced = sum(wam_experienced, na.rm=T), 
              d2_exp_theory = unique(total_users_possibly_experienced), 
              d3_exp_real = unique(total_exp_users_to_date),
              
              elig_using_exp = sum(elig_exp_using, na.rm=T),
              elig_notusing_exp = sum(elig_exp_notusing, na.rm=T),
              elig_using_notexp = sum(elig_notexp_using, na.rm=T), 
              elig_notusing_notexp = sum(elig_notexp_notusing, na.rm=T),
              
              notelig_using_exp = sum(notelig_exp_using, na.rm=T),
              notelig_notusing_exp = sum(notelig_exp_notusing, na.rm=T),
              notelig_using_notexp = sum(notelig_notexp_using, na.rm=T), 
              notelig_notusing_notexp = sum(notelig_notexp_notusing, na.rm=T), 
              
              naelig_using_exp = sum(na_exp_using, na.rm=T),
              naelig_notusing_exp = sum(na_exp_notusing, na.rm=T), 
              naelig_using_notexp = sum(na_notexp_using, na.rm=T),
              naelig_notusing_notexp = sum(na_notexp_notusing, na.rm=T)) 
  
  table_1_wam_bu$per_wams_d4 <- round((table_1_wam_bu$elig_using_exp/table_1_wam_bu$experienced)*100, digits = 1)
  
  #Filter table_1
  table_1_wam_bu <- filter(table_1_wam_bu, form_month >= "2013-01-01", form_month <= "2015-06-01")
  names(table_1_wam_bu)[names(table_1_wam_bu) == "form_month"] = "Calendar Month"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "nusers"] = "Active Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "eligible"] = "Eligible Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "using"] = "Sufficient Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "experienced"] = "Experienced Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "d2_exp_theory"] = "D2 All Possibly Exp Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "d3_exp_real"] = "D3 All Actually Exp Users"
  
  #Rename table specific to BU
  assign(paste("table_1_wam", i, sep="_"), table_1_wam_bu)
}

write.csv(table_1_wam_None, file = "table_1_wam_None.csv", row.names=F)
write.csv(table_1_wam_Inc, file = "table_1_wam_Inc.csv", row.names=F)
write.csv(table_1_wam_DSI, file = "table_1_wam_DSI.csv", row.names=F)
write.csv(table_1_wam_DWA, file = "table_1_wam_DWA.csv", row.names=F)
write.csv(table_1_wam_DSA, file = "table_1_wam_DSA.csv", row.names=F)
write.csv(table_1_wam_DLAC, file = "table_1_wam_DLAC.csv", row.names=F)
write.csv(table_1_wam_DMOZ, file = "table_1_wam_DMOZ.csv", row.names=F)

#------------------------------------------------------------------------#
#Table 2: WAM DATA (complete)
#------------------------------------------------------------------------#

#Set table parameters
all_malt_user_month$start_month <- as.Date("2015-05-01")
all_malt_user_month$end_month <- as.Date("2015-05-01") 

all_malt_table2 <- filter(all_malt_user_month, form_month >= start_month, 
                          form_month <= end_month)

#Create Table 2
table_2_wam <- all_malt_table2 %>% group_by(domain) %>% 
  summarise(calendar_month = unique(start_month),
            nusers = length(unique(user_id)),
            elig_using_exp = sum(elig_exp_using, na.rm=T),
            elig_notusing_exp = sum(elig_exp_notusing, na.rm=T),
            elig_using_notexp = sum(elig_notexp_using, na.rm=T), 
            elig_notusing_notexp = sum(elig_notexp_notusing, na.rm=T),
            
            notelig_using_exp = sum(notelig_exp_using, na.rm=T),
            notelig_notusing_exp = sum(notelig_exp_notusing, na.rm=T),
            notelig_using_notexp = sum(notelig_notexp_using, na.rm=T), 
            notelig_notusing_notexp = sum(notelig_notexp_notusing, na.rm=T), 
            
            naelig_using_exp = sum(na_exp_using, na.rm=T),
            naelig_notusing_exp = sum(na_exp_notusing, na.rm=T), 
            naelig_using_notexp = sum(na_notexp_using, na.rm=T),
            naelig_notusing_notexp = sum(na_notexp_notusing, na.rm=T))

#------------------------------------------------------------------------#
#Table 2_1: WAM DATA (modified from Table 2)
#------------------------------------------------------------------------#

#Set table parameters for current_month
all_malt_user_month$current_month <- as.Date("2015-06-01")
all_malt_user_month$prior_1_month <- floor_date(rollback(all_malt_user_month$current_month), "month")
all_malt_user_month$prior_2_months <- floor_date(rollback(all_malt_user_month$prior_1_month), "month")

#Create denominators for Table 2_1_4

#d1: all users who ever submitted forms as of the month in question
#We are flagging the first calendar month of each user
all_malt_user_month <- all_malt_user_month %>% group_by(domain, user_id) %>% 
  mutate(new_user = form_month == min(form_month))
#Sort by domain and calendar_month before calculating the cumulative sum 
#of new users in the next step
all_malt_user_month <- ungroup(all_malt_user_month)
all_malt_user_month <- arrange(all_malt_user_month, domain, form_month)
#Calculate cumulative sum of new users by domain
all_malt_user_month <- all_malt_user_month %>% group_by(domain) %>% 
  mutate(max_users = cumsum(new_user))
# Create variable for total users to date 
all_malt_user_month <- all_malt_user_month %>% group_by(domain, form_month) %>% 
  mutate(total_users_to_date = max(max_users))

#d2: all users who submitted forms at least 4+ months ago, regardless of whether or not they 
#are experienced as of the month in question (i.e., these users could theoretically be 
#experienced by the month in question. For example, a user who submitted just one form 
#6 months ago. This is just whether the user’s first form was more than 3 months ago)

#Calculate user_start_date
all_malt_user_month <- all_malt_user_month %>% group_by(domain, user_id) %>% 
  mutate(user_start_month = min(form_month, na.rm=T))

#Calculate user_experienced_date
all_malt_user_month <- all_malt_user_month %>% group_by(domain, user_id) %>% 
  mutate(user_experienced_date = user_start_month + months(3))

#Based on user_experienced_date, we will count the # of theoretically experienced users
#for each domain for each month in question
all_malt_user_month <- all_malt_user_month %>% group_by(domain, form_month) %>% 
  mutate(total_users_possibly_experienced = length(unique(user_id[user_experienced_date <= form_month])))

#d3: all experienced users (all users who are actually experienced by the time they reach 
#the month in question, regardless of whether or not they are active during the month 
#in question)
#We are flagging the calendar month in which each user actually becomes experienced
all_malt_user_month$new_exp_user <- all_malt_user_month$previous_active_months_rolling == 3
#Sort by domain and calendar_month before calculating the cumulative sum 
#of new users in the next step
all_malt_user_month <- ungroup(all_malt_user_month)
all_malt_user_month <- arrange(all_malt_user_month, domain, form_month)
#Calculate cumulative sum of new experienced users by domain
all_malt_user_month <- all_malt_user_month %>% group_by(domain) %>% 
  mutate(max_exp_users = cumsum(new_exp_user))
# Create row for total experienced users to date 
all_malt_user_month <- all_malt_user_month %>% group_by(domain, form_month) %>% 
  mutate(total_exp_users_to_date = max(max_exp_users))

#Create subset for time range parameters
all_monthly_table2 <- filter(all_malt_user_month, form_month >= prior_2_months, 
                             form_month <= current_month) 

#d6: total # users active over the past 3 months (current month and past 2 months) 
all_monthly_table2 <- all_monthly_table2 %>% group_by(domain) %>% 
  mutate(total_users_past_3_mos = length(unique(user_id)))

#Create Table 2_1_1
table_2_1_1 <- all_monthly_table2 %>% group_by(domain) %>% 
  summarise(Country = unique(country_final),
            Sector = unique(sector_final),
            Subsector = unique(subsector_final),
            business_unit = unique(new_business_unit), 
            self_service = unique(self_start), 
            test = unique(is_test), 
            domain_start_date = unique(domain_start_date, na.rm=T), 
            nusers = length(unique(user_id[form_month == current_month])), 
            domain_has_amplifies_workers = unique(domain_has_amplifies_workers),
            domain_has_amplifies_project = unique(domain_has_amplifies_project))

names(table_2_1_1)[names(table_2_1_1) == "domain"] = "Domain"
names(table_2_1_1)[names(table_2_1_1) == "business_unit"] = "Business Unit"
names(table_2_1_1)[names(table_2_1_1) == "self_service"] = "Self Service"
names(table_2_1_1)[names(table_2_1_1) == "test"] = "Test Domain"
names(table_2_1_1)[names(table_2_1_1) == "domain_start_date"] = "Domain Start Date"
names(table_2_1_1)[names(table_2_1_1) == "nusers"] = "Active Users"
names(table_2_1_1)[names(table_2_1_1) == "domain_has_amplifies_workers"] = "Eligible for WAMs"
names(table_2_1_1)[names(table_2_1_1) == "domain_has_amplifies_project"] = "Eligible for PAMs"

#write.csv(table_2_1_1, file = "table_2_1_1.csv", row.names = F)

table_2_1_2 <- all_monthly_table2 %>% group_by(domain) %>% 
  summarise(prior_2_months = unique(prior_2_months), 
            prior_1_month = unique(prior_1_month),
            current_month = unique(current_month), 
            wams_2_mos_prior = sum(elig_exp_using[form_month == prior_2_months], na.rm=T), 
            wams_1_mo_prior = sum(elig_exp_using[form_month == prior_1_month], na.rm=T), 
            wams_current_mo = sum(elig_exp_using[form_month == current_month], na.rm=T))
names(table_2_1_2)[names(table_2_1_2) == "domain"] = "Domain"
names(table_2_1_2)[names(table_2_1_2) == "prior_2_months"] = "Prior 2 Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "prior_1_month"] = "Prior 1 Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "current_month"] = "Current Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "wams_2_mos_prior"] = "WAMs 2 months prior"
names(table_2_1_2)[names(table_2_1_2) == "wams_1_mo_prior"] = "WAMs 1 month prior"
names(table_2_1_2)[names(table_2_1_2) == "wams_current_mo"] = "WAMs current month"

#write.csv(table_2_1_2, file = "table_2_1_2.csv", row.names = F)

table_2_1_3 <- all_monthly_table2 %>% group_by(domain) %>%
  summarise(using_exp = sum(using_exp[form_month == current_month], na.rm=T),
            inelig_exp_using = sum(notelig_exp_using[form_month == current_month] == T | 
                                     na_exp_using[form_month == current_month] == T, na.rm = T),
            notusing_exp = sum(notusing_exp[form_month == current_month], na.rm=T),
            using_notexp = sum(using_notexp[form_month == current_month], na.rm=T), 
            notusing_notexp = sum(notusing_notexp[form_month == current_month], na.rm=T))
names(table_2_1_3)[names(table_2_1_3) == "domain"] = "Domain"
names(table_2_1_3)[names(table_2_1_3) == "using_exp"] = "Sufficient Use and Experience"
names(table_2_1_3)[names(table_2_1_3) == "inelig_exp_using"] = "Ineligible - Sufficient Use and Experience"
names(table_2_1_3)[names(table_2_1_3) == "notusing_exp"] = "Low Use"
names(table_2_1_3)[names(table_2_1_3) == "using_notexp"] = "Not Experienced"
names(table_2_1_3)[names(table_2_1_3) == "notusing_notexp"] = "Low Use + Not Experienced"

#write.csv(table_2_1_3, file = "table_2_1_3.csv", row.names = F)

table_2_1_4 <- filter(all_monthly_table2, form_month == current_month) %>% 
  group_by(domain) %>% 
  summarise(d1_total_users_to_date = unique(total_users_to_date), 
            d2_exp_theory = unique(total_users_possibly_experienced), 
            d3_exp_real = unique(total_exp_users_to_date), 
            d4_exp_active_current = sum(wam_experienced, na.rm=T),
            d5_active_current = length(unique(user_id)),
            d6_active_3_mos = unique(total_users_past_3_mos), 
            wams_current_mo = sum(elig_exp_using, na.rm=T))
table_2_1_4$per_wams_current <- round((table_2_1_4$wams_current_mo/table_2_1_4$d4_exp_active_current)*100, digits = 1)
table_2_1_4$per_wams_current[table_2_1_4$d4_exp_active_current == 0] <- 0

names(table_2_1_4)[names(table_2_1_4) == "domain"] = "Domain"
names(table_2_1_4)[names(table_2_1_4) == "d1_total_users_to_date"] = "D1 All Users Ever Active"
names(table_2_1_4)[names(table_2_1_4) == "d2_exp_theory"] = "D2 All Possibly Exp Users"
names(table_2_1_4)[names(table_2_1_4) == "d3_exp_real"] = "D3 All Actually Exp Users"
names(table_2_1_4)[names(table_2_1_4) == "d4_exp_active_current"] = "D4 All Experienced + Active Users"
names(table_2_1_4)[names(table_2_1_4) == "d5_active_current"] = "D5 All Active Users"
names(table_2_1_4)[names(table_2_1_4) == "d6_active_3_mos"] = "D6 All Active Users Current + Prior 2 Mos"
names(table_2_1_4)[names(table_2_1_4) == "wams_current_mo"] = "WAMs Current"
names(table_2_1_4)[names(table_2_1_4) == "per_wams_current"] = "Per WAMs Current"

#Here is the code if we want to add back in denomiantors from prior 1 month or
#prior 2 months
#dplyr code
#exp_prior_2 = sum(wam_experienced[calendar_month == prior_2_months], na.rm=T), 
#exp_prior_1 = sum(wam_experienced[calendar_month == prior_1_month], na.rm=T),
#wams_2_mos_prior = sum(elig_exp_using[calendar_month == prior_2_months], na.rm=T), 
#wams_1_mo_prior = sum(elig_exp_using[calendar_month == prior_1_month], na.rm=T),
#regular code after table has been created through dplyr
#table_2_1_4$per_wams_prior_2 <- round((table_2_1_4$wams_2_mos_prior/table_2_1_4$exp_prior_2)*100, digits = 1)
#table_2_1_4$per_wams_prior_2[table_2_1_4$exp_prior_2 == 0] <- 0
#table_2_1_4$per_wams_prior_1 <- round((table_2_1_4$wams_1_mo_prior/table_2_1_4$exp_prior_1)*100, digits = 1)
#table_2_1_4$per_wams_prior_1[table_2_1_4$exp_prior_1 == 0] <- 0
#table_2_1_4$slope_prior_2_to_current <- (table_2_1_4$per_wams_current - table_2_1_4$per_wams_prior_2) / 2
#table_2_1_4$slope_prior_1_to_current <- (table_2_1_4$per_wams_current - table_2_1_4$per_wams_prior_1) / 1
#names(table_2_1_4)[names(table_2_1_4) == "exp_prior_2"] = "Experienced + Active Users Prior 2"
#names(table_2_1_4)[names(table_2_1_4) == "exp_prior_1"] = "Experienced + Active Users Prior 1"
#names(table_2_1_4)[names(table_2_1_4) == "wams_2_mos_prior"] = "WAMs Prior 2"
#names(table_2_1_4)[names(table_2_1_4) == "wams_1_mo_prior"] = "WAMs Prior 1"
#names(table_2_1_4)[names(table_2_1_4) == "per_wams_prior_2"] = "Per WAMs Prior 2"
#names(table_2_1_4)[names(table_2_1_4) == "per_wams_prior_1"] = "Per WAMs Prior 1"
#names(table_2_1_4)[names(table_2_1_4) == "slope_prior_2_to_current"] = "Slope Prior 2 to Current"
#names(table_2_1_4)[names(table_2_1_4) == "slope_prior_1_to_current"] = "Slope Prior 2 to Current"
#write.csv(table_2_1_4, file = "table_2_1_4.csv", row.names = F)

#Merge all sections to make overall table_2_1
table_2_1 <- merge(table_2_1_1, table_2_1_2, by = "Domain", all = T)
table_2_1 <- merge(table_2_1, table_2_1_3, by = "Domain", all = T)
table_2_1 <- merge(table_2_1, table_2_1_4, by = "Domain", all = T)

#IN THE FUTURE, NEED TO CONVERT ALL NAs IN DENOMINATOR COLUMNS TO 0

write.csv(table_2_1, file = "table_2_1_update.csv", row.names = F)

