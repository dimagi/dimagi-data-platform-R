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
form_table <- collect(form_table)
names(form_table)[names(form_table) == "user_id"] = "user_pk"
form_table <- select(form_table, id, user_pk, received_on, domain_id, application_id)
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
users$is_web_user <- users$user_type == "web"
dp_malt <- merge(dp_malt, select(users, user_pk, user_id, username, email, is_web_user), 
                 by = "user_pk", all.x = T)

#Merge in domain table: name
#Pull domain table with new business unit info
domain <- get_domain_table(db)
names(domain)[names(domain) == "id"] = "domain_pk"
dp_malt <- merge(dp_malt, select(domain, domain_pk, name), by = "domain_pk", all.x = T)
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
hq_malt$form_month <- as.Date(hq_malt$form_month)
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
#all_malt <- dp_malt
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

#Used to exclude rows with calendar_month = NA: Did this with WAM calculations based on monthly table - 
#Don't need to do this since the form_table has all valid form dates

#Exclude all users with user_type = web
#Used to exclude users with user_type = web and summary_device_type != cloud care during any month - 
#but we are simplifying this now and excluding all web users
#First count these users per domain per month
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

#Check user_exclusion table
#user_exclusion$sum_exclude_months <- rowSums(user_exclusion[,c(4:10)])
#user_exclusion$calc_user_months_final <- user_exclusion$user_months_initial - user_exclusion$sum_exclude_months
#user_exclusion$equal_final <- user_exclusion$calc_user_months_final == user_exclusion$user_months_final
#user_exclusion <- arrange(user_exclusion, equal_final)

#------------------------------------------------------------------------#
#wam_eligible and pam_eligible
#------------------------------------------------------------------------#

all_malt$wam_eligible <- all_malt$amplifies_workers
all_malt$pam_eligible <- all_malt$amplifies_project

#------------------------------------------------------------------------#
#wam_experienced, pam_experienced
#------------------------------------------------------------------------#

#Add rolling # active months to each user-app row
all_malt <- arrange(all_malt, user_id, app_id, form_month)
all_malt <- all_malt %>% group_by(user_id, app_id) %>% 
  mutate(previous_active_months_rolling = seq_along(form_month)-1)

#wam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_malt$wam_experienced <- all_malt$previous_active_months_rolling >= 3

#pam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_malt$pam_experienced <- all_malt$previous_active_months_rolling >= 3

#------------------------------------------------------------------------#
#wam_using, pam_using
#------------------------------------------------------------------------#

#wam_using == T or pam_using == T
#if the user met our criteria this month for sufficient usage of the app. 
#Specifically, they submitted >= 15 forms during the user-app-month in question
all_malt$wam_using <- all_malt$nforms >= 15
all_malt$pam_using <- all_malt$nforms >= 15

#------------------------------------------------------------------------#
#Consolidate WAM categories for output tables so that each user has only
#one wam_eligible, wam_experienced and wam_using value. We don't want a 
#user to count in more than one of our 12 WAM categories
#------------------------------------------------------------------------#

#First flag any user app rows that generate WAMs
#A user who generates a WAM in at least one user app row will be counted as one WAM
#for that month, regardless of whether or not they generated WAMs in the other user app 
#rows for that same month
all_malt$wam_generated <- all_malt$wam_eligible == T & all_malt$wam_experienced == T & 
  all_malt$wam_using == T

#Create all_malt_user_month table with just one row per user per month
all_malt_user_month <- all_malt %>% group_by(user_id, form_month) %>% 
  summarise(domain = unique(domain),
            n_apps = length(unique(app_id)),
            wam_generated = sum(wam_generated, na.rm=T) > 0,
            wam_eligible = sum(wam_eligible, na.rm = T) > 0, 
            wam_experienced = sum(wam_experienced, na.rm=T) > 0, 
            wam_using = sum(wam_using, na.rm = T) >0)

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

#------------------------------------------------------------------------#
#Table 1: WAM OVERVIEW
#------------------------------------------------------------------------#

#Create Table 1
table_1_wam <- all_malt_user_month %>% group_by(form_month) %>% 
  summarise(nusers = length(unique(user_id)),
            eligible = sum(wam_eligible, na.rm = T), 
            using = sum(wam_using, na.rm = T), 
            experienced = sum(wam_experienced, na.rm=T), 
            wams_generated = sum(wam_generated, na.rm=T),
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
#Filter table_1
table_1_wam <- filter(table_1_wam, form_month >= "2013-01-01", form_month <= "2015-06-01")
names(table_1_wam)[names(table_1_wam) == "form_month"] = "Calendar Month"
names(table_1_wam)[names(table_1_wam) == "nusers"] = "Active Users"
names(table_1_wam)[names(table_1_wam) == "eligible"] = "Eligible Users"
names(table_1_wam)[names(table_1_wam) == "using"] = "Sufficient Users"
names(table_1_wam)[names(table_1_wam) == "experienced"] = "Experienced Users"
names(table_1_wam)[names(table_1_wam) == "wams_generated"] = "WAMs Generated"
write.csv(table_1_wam, file = "table_1_wam.csv", row.names=F)

#Merge domain facets to all_malt_user_month
facets_to_merge <- select(domain, name, domain_id, country_final, sector_final, 
                          subsector_final, self_start, domain_has_amplifies_workers, 
                          domain_has_amplifies_project, new_business_unit, is_test, domain_start_date)
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
all_malt_user_month$new_business_unit[all_malt_user_month$country_final == "{IN}"] <- "DSI"
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
            wams_generated = sum(wam_generated, na.rm=T),
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
#Older code: do not run
#------------------------------------------------------------------------#

#7/13/15
june_old <- read.csv(file = "malt_hq.csv")
june_old <- select(june_old, user_id, domain_name, num_of_forms, app_id)
names(june_old)[names(june_old) == "num_of_forms"] = "nforms_old"

june_new <- read.csv(file = "malt_may_june.csv")
june_new  <- filter(june_new, month == "2015-06-01")
june_new <- select(june_new, user_id, domain_name, num_of_forms, app_id)
names(june_new)[names(june_new) == "num_of_forms"] = "nforms_new"

june <- merge(june_old, june_new, by = c("user_id", "domain_name", "app_id"), all = T)
june$nforms_old[is.na(june$nforms_old)] <- 0
june$diff <- june$nforms_new - june$nforms_old
june <- arrange(june, desc(diff))

may_dp <- filter(dp_malt, form_month == "2015-05-01") %>% group_by(user_id, domain) %>% 
  summarise(nforms_dp = sum(nforms, na.rm=T), 
            used_app_id_na = sum(is.na(app_id)) > 0)

may_hq  <- filter(hq_malt, form_month == "2015-05-01") %>% group_by(user_id, domain) %>% 
  summarise(nforms_hq = sum(nforms, na.rm=T), 
            used_deleted_app = sum(is_app_deleted == "t") > 0)

may <- merge(may_dp, may_hq, by = c("user_id", "domain"), all = T)

may$nforms_hq[is.na(may$nforms_hq)] <- 0
may$nforms_dp[is.na(may$nforms_dp)] <- 0
may$diff <- may$nforms_dp - may$nforms_hq
may <- arrange(may, desc(diff))

may_domain <- may %>% group_by(domain) %>% 
  summarise(nforms_diff_total = sum(diff, na.rm = T), 
            has_app_id_na = sum(used_app_id_na, na.rm = T) > 0, 
            has_deleted_app = sum(used_deleted_app, na.rm = T) > 0)
may_domain <- arrange(may_domain, desc(nforms_diff_total))

#test <- filter(may, is.na(nforms_dp))
#test2 <- test %>% group_by(domain_name) %>% summarise(nforms = sum(nforms_hq, na.rm = T))
#test2 <- arrange(test2, desc(nforms))

#7/10/15
#Check HQ MALT generated by Sravan
malt_hq <- read.csv(file = "malt_hq.csv")

dp_summary <- may %>% group_by(domain) %>% 
  summarise(nusers_may_dp = length(unique(user_id)))

hq_summary <- malt_hq %>% group_by(domain_name) %>% 
  summarise(nusers_june_malt_hq = length(unique(user_id)))

nusers_summary <- merge(dp_summary, hq_summary, by.x = "domain", by.y = "domain_name", all = T)
nusers_summary <- arrange(nusers_summary, desc(nusers_may_dp), desc(nusers_june_malt_hq))