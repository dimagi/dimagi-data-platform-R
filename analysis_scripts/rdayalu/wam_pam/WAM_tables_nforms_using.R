#This code is to create the WAM table as described in the following document
#https://docs.google.com/a/dimagi.com/document/d/1VwaJm_wUJmHWOH0aUsAbwbWplTyzUSo0dkN30R1en7s/edit
#First we need to add three variables to each row of the monthly table:
#wam_eligible, wam_experienced, wam_using
#From this table, we will calculate the WAM overview table (Table 1) and
#the WAM data table (Table 2), and the WAM/PAM annotation stats (Table 3) 
#as described in the google doc.

#NOTE: BE SURE TO INCORPORATE FUTURE CHANGES FROM NEAL:
#DESCRIBED IN 5/22/15: RD Ongoing google doc

#------------------------------------------------------------------------#
#Import data
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

#Pull monthly_table for all domains
#Be sure to set config_run first
#permitted_data_only = F and filter = include-all
source(file.path("analysis_scripts","raw_data","data_import.R", fsep = .Platform$file.sep))
all_monthly <- monthly_table
#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)
#Concat user and month in monthly table
all_monthly$user_month_concat <- paste(all_monthly$user_pk, 
                                                all_monthly$calendar_month, sep = "_")
#Add unique integer column
all_monthly$unique_int <- 1:nrow(all_monthly)

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

#Pull the app table directly from the db
app <- tbl(db, "application")
app <- collect(app)
app_amplifies <- app

#Remove all double quotes from inside attributes string. Replace with underscores
app_amplifies$attributes <- gsub('"', "_", app_amplifies$attributes)

#Import form table
form_table <- tbl(db, "form")
form_table <- collect(form_table)
names(form_table)[names(form_table) == "user_id"] = "user_pk"
#Remove forms with application_id = NA
form_table <- filter(form_table, !(is.na(application_id)))

#Pull domain table with new business unit info
domain <- get_post_processed_domain_table(db)

#------------------------------------------------------------------------#
#User exclusions
#------------------------------------------------------------------------#

#Exclude demo users and NA/NONE users
#First count these users per domain per month
user_exclusion <- all_monthly %>% group_by(domain) %>% 
  summarise(users_initial = length(unique(user_pk)),
            user_months_initial = length(unique(unique_int)),
            user_months_user_id_demo_na = length(unique(unique_int[user_id =="demo_user" | 
                                                                user_id =="NONE" | 
                                                                user_id == "none" | 
                                                                is.na(user_id)])))


all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
all_monthly = all_monthly[!(all_monthly$user_id =="NONE"),]
all_monthly = all_monthly[!(all_monthly$user_id =="none"),]
all_monthly = all_monthly[!is.na(all_monthly$user_id),]

#Exclude users who submit to multiple domains
#First count these users per domain per month
chw_single_domain <- all_monthly %>% group_by(user_pk) %>% 
  summarise(n_domains = length(unique(domain)))
chw_single_domain <- filter(chw_single_domain, n_domains == 1)
all_monthly$user_multiple_domain <- !(all_monthly$user_pk %in% chw_single_domain$user_pk)
all_monthly <- all_monthly %>% group_by(domain) %>% 
  mutate(user_months_user_pk_multiple_domains = sum(user_multiple_domain))
test <- all_monthly %>% group_by(domain) %>% summarise(check=unique(user_months_user_pk_multiple_domains))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_user_pk_multiple_domains"
all_monthly <- all_monthly[all_monthly$user_pk %in% chw_single_domain$user_pk,]

#Exclude rows with active_days = NA. These are not rows where the user is truly active
#First count nrows with active_days = NA per domain per month
#Did nforms previously - don't know if there is a difference...
all_monthly$active_days_na <- is.na(all_monthly$active_days)
test <- all_monthly %>% group_by(domain) %>% summarise(check=sum(active_days_na))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_active_days_na"
all_monthly <- filter(all_monthly, !is.na(active_days))

#Exclude rows with calendar_month = NA
all_monthly$calendar_month_na <- is.na(all_monthly$calendar_month)
test <- all_monthly %>% group_by(domain) %>% summarise(check=sum(calendar_month_na))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_calendar_month_na"
all_monthly <- filter(all_monthly, !is.na(calendar_month))

#Exclude users with user_type = web and summary_device_type != cloud care during any month
#First count these users per domain per month
all_monthly <- merge(all_monthly, users, by = "user_pk", all.x = T)
exclude1 <- filter(all_monthly, user_type == "web" & 
                     summary_device_type != "Cloudcare")
all_monthly$user_web_notCC <- all_monthly$user_pk %in% exclude1$user_pk
all_monthly <- all_monthly %>% group_by(domain) %>% 
  mutate(user_months_web_not_cloudcare = sum(user_web_notCC))
test <- all_monthly %>% group_by(domain) %>% summarise(check=unique(user_months_web_not_cloudcare))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_web_not_cloudcare"
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude1$user_pk),]

#Exclude dimagi users and superusers
#First count these users per domain per month
dimagi_users <- all_monthly$user_pk[grep("dimagi", all_monthly$email, fixed=T)]
all_monthly$dimagi_user <- all_monthly$user_pk %in% dimagi_users
all_monthly$user_dimagi_super <- all_monthly$dimagi_user == T | all_monthly$is_superuser == T
all_monthly <- all_monthly %>% group_by(domain) %>% 
  mutate(user_months_dimagi_superuser = sum(user_dimagi_super, na.rm=T))
test <- all_monthly %>% group_by(domain) %>% summarise(check=unique(user_months_dimagi_superuser))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_dimagi_superuser"
all_monthly <- filter(all_monthly, dimagi_user == F)
all_monthly <- filter(all_monthly, is_superuser == F | is.na(is_superuser))

#Exclude users with summary_device_type = SMS during any month
#First count these users per domain per month
exclude3 <- filter(all_monthly, summary_device_type == "Sms")
all_monthly$sms_any_month <- all_monthly$user_pk %in% exclude3$user_pk
all_monthly <- all_monthly %>% group_by(domain) %>% 
  mutate(user_months_sms_any_month = sum(sms_any_month))
test <- all_monthly %>% group_by(domain) %>% summarise(check=unique(user_months_sms_any_month))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_sms_any_month"
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude3$user_pk),]

#Based on form table...
#ID users who have submitted data through just 1 app or > 1 app
#We are going to handle single_app and multiple_app users differently
chw_n_app <- form_table %>% group_by(user_pk) %>%
  summarise(n_applications = length(unique(application_id)))
#write.csv(chw_n_app, file = "chw_n_app.csv")
chw_multiple_app <- filter(chw_n_app, n_applications > 1)
chw_single_app <- filter(chw_n_app, n_applications == 1)
#Flag users in monthly table
all_monthly$submitted_single_app <- all_monthly$user_pk %in% chw_single_app$user_pk
all_monthly$submitted_multiple_app <- all_monthly$user_pk %in% chw_multiple_app$user_pk

#Exclude users without form_ids and app_ids
all_monthly$form_app_id_na <- all_monthly$submitted_single_app == F & 
  all_monthly$submitted_multiple_app == F
all_monthly <- all_monthly %>% group_by(domain) %>% 
  mutate(user_months_form_app_id_na = sum(form_app_id_na))
test <- all_monthly %>% group_by(domain) %>% summarise(check=unique(user_months_form_app_id_na))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_form_app_id_na"
all_monthly <- all_monthly[!(all_monthly$submitted_single_app == F & 
                               all_monthly$submitted_multiple_app == F),]

#Single_app users
all_monthly_single <- filter(all_monthly, submitted_single_app == T)

#Multiple_app users
all_monthly_multiple <- filter(all_monthly, submitted_multiple_app == T)

#Add final columns to user_exclusion table
all_monthly <- all_monthly %>% group_by(domain) %>% 
  mutate(user_months_final = length(unique(unique_int)), 
         users_final = length(unique(user_pk)))
test <- all_monthly %>% group_by(domain) %>% summarise(check=unique(user_months_final), 
                                                       check2=unique(users_final))
user_exclusion <- merge(user_exclusion, test, by = "domain", all.x = T)
names(user_exclusion)[names(user_exclusion) == "check"] = "user_months_final"
names(user_exclusion)[names(user_exclusion) == "check2"] = "users_final"
#Convert all NA values to 0
user_exclusion[is.na(user_exclusion)] <- 0

write.csv(user_exclusion, file = "table_6_user_exclusion.csv", row.names = F)

#Check user_exclusion table
#user_exclusion$sum_exclude_months <- rowSums(user_exclusion[,c(4:10)])
#user_exclusion$calc_user_months_final <- user_exclusion$user_months_initial - user_exclusion$sum_exclude_months
#user_exclusion$equal_final <- user_exclusion$calc_user_months_final == user_exclusion$user_months_final
#user_exclusion <- arrange(user_exclusion, equal_final)

#------------------------------------------------------------------------#
#Parse amplifies_X values from app table
#Add amplifies_workers/project to form_table by app_id
#------------------------------------------------------------------------#

#amplifies_workers
app_amplifies$amplifies_workers <- NA
app_amplifies$amplifies_workers[grep("_amplifies_workers_=>_yes_", app_amplifies$attributes, 
                                     fixed=T)] <- T
app_amplifies$amplifies_workers[grep("_amplifies_workers_=>_no_", app_amplifies$attributes, 
                                     fixed=T)] <- F
#Manually tag domains
app_amplifies$amplifies_workers[app_amplifies$id == 339] <- T #tulasalud "Kawok Vigilancia Comunitario 3.0"
app_amplifies$amplifies_workers[app_amplifies$id == 1140] <- T #tulasalud "Kawok Vigilancia Comunitario 2.0"
app_amplifies$amplifies_workers[app_amplifies$id == 1567] <- T #tulasalud "Kawok Vigilancia Comunitario 2.1"
app_amplifies$amplifies_workers[app_amplifies$id == 109] <- T #opm "Bihar Child Support Programme"

#amplifies_project
app_amplifies$amplifies_project <- NA
app_amplifies$amplifies_project[grep("_amplifies_project_=>_yes_", app_amplifies$attributes, 
                                     fixed=T)] <- T
app_amplifies$amplifies_project[grep("_amplifies_project_=>_no_", app_amplifies$attributes, 
                                     fixed=T)] <- F
#Manually tag domains
app_amplifies$amplifies_project[app_amplifies$id == 339] <- T #tulasalud "Kawok Vigilancia Comunitario 3.0"
app_amplifies$amplifies_project[app_amplifies$id == 1140] <- T #tulasalud "Kawok Vigilancia Comunitario 2.0"
app_amplifies$amplifies_project[app_amplifies$id == 1567] <- T #tulasalud "Kawok Vigilancia Comunitario 2.1"
app_amplifies$amplifies_project[app_amplifies$id == 109] <- T #opm "Bihar Child Support Programme"

#Add amplifies_workers and amplifies_project to form_table by app_id
form_table <- merge(form_table, 
                    select(app_amplifies, id, amplifies_workers, amplifies_project), 
                    by.x = "application_id", by.y = "id", all.x = T)

#Filter form_table by multiple_app users
#Create form_month and concat with user_pk
forms_users_multiple_apps <- filter(form_table, user_pk %in% all_monthly_multiple$user_pk)
#forms_users_multiple_apps$form_date <- as.Date(substr(forms_users_multiple_apps$time_start, 1, 10))
forms_users_multiple_apps$form_date <- as.Date(substr(forms_users_multiple_apps$received_on, 1, 10))
forms_users_multiple_apps$form_month <- floor_date(forms_users_multiple_apps$form_date, "month")

forms_users_multiple_apps$user_month_concat <- paste(forms_users_multiple_apps$user_pk, 
                                                     forms_users_multiple_apps$form_month, sep = "_") 

#Vector of user_month_concat for aw and ap
forms_users_multiple_apps_aw <- unique(filter(forms_users_multiple_apps, amplifies_workers == T)$user_month_concat)
forms_users_multiple_apps_ap <- unique(filter(forms_users_multiple_apps, amplifies_project == T)$user_month_concat)
forms_users_multiple_apps_aw_f <- unique(filter(forms_users_multiple_apps, amplifies_workers == F)$user_month_concat)
forms_users_multiple_apps_ap_f <- unique(filter(forms_users_multiple_apps, amplifies_project == F)$user_month_concat)
forms_users_multiple_apps_aw_na <- unique(filter(forms_users_multiple_apps, is.na(amplifies_workers))$user_month_concat)
forms_users_multiple_apps_ap_na <- unique(filter(forms_users_multiple_apps, is.na(amplifies_project))$user_month_concat)

#------------------------------------------------------------------------#
#Add amplifies_workers/project to domain table
#------------------------------------------------------------------------#

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

#Flag domains based on exclusive vectors above
names(domain)[names(domain) == "id"] = "domain_id"

domain$domain_has_amplifies_workers <- NA
domain$domain_has_amplifies_workers[domain$domain_id %in% domain_has_amp_w_true] <- T
domain$domain_has_amplifies_workers[domain$domain_id %in% domain_has_amp_w_false] <- F

domain$domain_has_amplifies_project <- NA
domain$domain_has_amplifies_project[domain$domain_id %in% domain_has_amp_p_true] <- T
domain$domain_has_amplifies_project[domain$domain_id %in% domain_has_amp_p_false] <- F

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

all_monthly$amp_w_true <- NA
all_monthly$amp_w_false <- NA
all_monthly$amp_w_na <- NA
all_monthly$wam_eligible <- NA

all_monthly$amp_p_true <- NA
all_monthly$amp_p_false <- NA
all_monthly$amp_p_na <- NA
all_monthly$pam_eligible <- NA

#Single_app users
all_monthly_single <- filter(all_monthly, submitted_single_app == T)

#Multiple_app users
all_monthly_multiple <- filter(all_monthly, submitted_multiple_app == T)

#------------------------------------------------------------------------#
#wam_eligible and pam_eligible
#Single app users
#------------------------------------------------------------------------#

#Single app users, wam_eligible == T, pam_eligible == T
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker or amplifies_project set to true
all_monthly_single$amp_w_true <- all_monthly_single$user_pk %in% 
  filter(form_table, amplifies_workers == T)$user_pk
all_monthly_single$wam_eligible[all_monthly_single$amp_w_true == T] <- T

all_monthly_single$amp_p_true <- all_monthly_single$user_pk %in% 
  filter(form_table, amplifies_project == T)$user_pk
all_monthly_single$pam_eligible[all_monthly_single$amp_p_true == T] <- T

#Single app users, wam_eligible == F, pam_eligible == F
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker or amplifies_project set to false
all_monthly_single$amp_w_false <- all_monthly_single$user_pk %in% 
  filter(form_table, amplifies_workers == F)$user_pk
all_monthly_single$wam_eligible[all_monthly_single$amp_w_false == T] <- F

all_monthly_single$amp_p_false <- all_monthly_single$user_pk %in% 
  filter(form_table, amplifies_project == F)$user_pk
all_monthly_single$pam_eligible[all_monthly_single$amp_p_false == T] <- F

#Don't need to run this
#Single app users, wam_eligible == NA
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker or amplifies_project not set
#all_monthly_single$amp_w_na <- all_monthly_single$user_pk %in% 
#  filter(form_table, is.na(amplifies_workers))$user_pk
#all_monthly_single$wam_eligible[all_monthly_single$amp_w_na == T] <- NA

#------------------------------------------------------------------------#
#wam_eligible and pam_eligible
#Multiple app users
#------------------------------------------------------------------------#

#Multiple app users
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker or amplifies_project set to true
all_monthly_multiple$amp_w_true <- all_monthly_multiple$user_month_concat %in% 
  forms_users_multiple_apps_aw

all_monthly_multiple$amp_p_true <- all_monthly_multiple$user_month_concat %in% 
  forms_users_multiple_apps_ap

#Multiple app users
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker or amplifies_project set to false
all_monthly_multiple$amp_w_false <- all_monthly_multiple$user_month_concat %in% 
  forms_users_multiple_apps_aw_f

all_monthly_multiple$amp_p_false <- all_monthly_multiple$user_month_concat %in% 
  forms_users_multiple_apps_ap_f

#Multiple app users 
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker or amplifies_project not set
all_monthly_multiple$amp_w_na <- all_monthly_multiple$user_month_concat %in% 
  forms_users_multiple_apps_aw_na
all_monthly_multiple$amp_p_na <- all_monthly_multiple$user_month_concat %in% 
  forms_users_multiple_apps_ap_na

#Multiple app users, wam_eligible == T, pam_eligible == T
#If a user submitted data through an amplifies_workers app or 
#amplifies_project app at any time that month
all_monthly_multiple$wam_eligible[all_monthly_multiple$amp_w_true == T] <- T
all_monthly_multiple$pam_eligible[all_monthly_multiple$amp_p_true == T] <- T

#Multiple app users, wam_eligible == F, pam_eligible == F
#If the user submitted forms using amplfies_workers = F or amplifies_project = F but
#not if amplifies_workers or amplifies_project was ever = T in that month
all_monthly_multiple$wam_eligible[all_monthly_multiple$amp_w_false == T & 
                       all_monthly_multiple$amp_w_true == F] <- F
all_monthly_multiple$pam_eligible[all_monthly_multiple$amp_p_false == T & 
                                    all_monthly_multiple$amp_p_true == F] <- F

#Don't need to run this
#wam_eligible == NA
#This will be NA if the user submitted forms using amplifies_workers = NA
#for all their apps
#all_monthly_multiple[all_monthly_multiple$amp_w_na == T & 
#                       all_monthly_multiple$amp_w_true == F & 
#                       all_monthly_multiple$amp_w_false == F,]$wam_eligible <- NA

#Check single app and multiple app tables before appending together
summary(names(all_monthly_single) == names(all_monthly_multiple))

#Append together
all_monthly <- rbind(all_monthly_single, all_monthly_multiple)

#------------------------------------------------------------------------#
#wam_experienced, pam_experienced
#------------------------------------------------------------------------#

#Add rolling # active months to each user's rows
all_monthly <- arrange(all_monthly, user_pk, calendar_month)
all_monthly <- all_monthly %>% group_by(user_pk) %>% 
  mutate(previous_active_months_rolling = seq_along(calendar_month)-1)

#wam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_monthly$wam_experienced <- all_monthly$previous_active_months_rolling >= 3

#pam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_monthly$pam_experienced <- all_monthly$previous_active_months_rolling >= 3

#------------------------------------------------------------------------#
#wam_using, pam_using - nforms instead of active_days
#------------------------------------------------------------------------#

#wam_using == T or pam_using == T
#if the user met our criteria this month for sufficient usage of the app. 
#Specifically, they submitted >= 15 forms in a given month. This is a universal
#threshold that we are using right now, but this will probably be customized for
#some specific domains in the future
all_monthly$wam_using <- all_monthly$nforms >= 15
all_monthly$pam_using <- all_monthly$nforms >= 15

#------------------------------------------------------------------------#
#WAM categories for output tables
#------------------------------------------------------------------------#

#We have 12 combinations for of wam_eligible(T/F/NA), wam_experienced(T/F) 
#and wam_using(T/F)
all_monthly$elig_exp_using <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == T
all_monthly$elig_exp_notusing <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == F
all_monthly$elig_notexp_using <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == T
all_monthly$elig_notexp_notusing <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == F

all_monthly$notelig_exp_using <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == T
all_monthly$notelig_exp_notusing <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == F
all_monthly$notelig_notexp_using <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == T
all_monthly$notelig_notexp_notusing <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == F

all_monthly$na_exp_using <- is.na(all_monthly$wam_eligible)& all_monthly$wam_experienced == T & 
  all_monthly$wam_using == T
all_monthly$na_exp_notusing <- is.na(all_monthly$wam_eligible) & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == F
all_monthly$na_notexp_using <- is.na(all_monthly$wam_eligible)& all_monthly$wam_experienced == F & 
  all_monthly$wam_using == T
all_monthly$na_notexp_notusing <- is.na(all_monthly$wam_eligible) & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == F

#We have 4 combinations for wam_experienced(T/F) and wam_using(T/F)
all_monthly$using_exp <- all_monthly$wam_experienced == T & all_monthly$wam_using == T
all_monthly$notusing_exp <- all_monthly$wam_experienced == T & all_monthly$wam_using == F
all_monthly$using_notexp <- all_monthly$wam_experienced == F & all_monthly$wam_using == T
all_monthly$notusing_notexp <- all_monthly$wam_experienced == F & all_monthly$wam_using == F


#------------------------------------------------------------------------#
#PAM categories for output tables
#------------------------------------------------------------------------#

#We have 12 combinations of pam_eligible(T/F/NA), pam_experienced(T/F) 
#and pam_using(T/F)

all_monthly$elig_exp_using_pam <- all_monthly$pam_eligible == T & all_monthly$pam_experienced == T & 
  all_monthly$pam_using == T
all_monthly$elig_exp_notusing_pam <- all_monthly$pam_eligible == T & all_monthly$pam_experienced == T & 
  all_monthly$pam_using == F
all_monthly$elig_notexp_using_pam <- all_monthly$pam_eligible == T & all_monthly$pam_experienced == F & 
  all_monthly$pam_using == T
all_monthly$elig_notexp_notusing_pam <- all_monthly$pam_eligible == T & all_monthly$pam_experienced == F & 
  all_monthly$pam_using == F

all_monthly$notelig_exp_using_pam <- all_monthly$pam_eligible == F & all_monthly$pam_experienced == T & 
  all_monthly$pam_using == T
all_monthly$notelig_exp_notusing_pam <- all_monthly$pam_eligible == F & all_monthly$pam_experienced == T & 
  all_monthly$pam_using == F
all_monthly$notelig_notexp_using_pam <- all_monthly$pam_eligible == F & all_monthly$pam_experienced == F & 
  all_monthly$pam_using == T
all_monthly$notelig_notexp_notusing_pam <- all_monthly$pam_eligible == F & all_monthly$pam_experienced == F & 
  all_monthly$pam_using == F


all_monthly$na_exp_using_pam <- is.na(all_monthly$pam_eligible)& all_monthly$pam_experienced == T & 
  all_monthly$pam_using == T
all_monthly$na_exp_notusing_pam <- is.na(all_monthly$pam_eligible) & all_monthly$pam_experienced == T & 
  all_monthly$pam_using == F
all_monthly$na_notexp_using_pam <- is.na(all_monthly$pam_eligible)& all_monthly$pam_experienced == F & 
  all_monthly$pam_using == T
all_monthly$na_notexp_notusing_pam <- is.na(all_monthly$pam_eligible) & all_monthly$pam_experienced == F & 
  all_monthly$pam_using == F

#------------------------------------------------------------------------#
#Table 1: WAM OVERVIEW
#------------------------------------------------------------------------#

#Create Table 1
table_1_wam <- all_monthly %>% group_by(calendar_month) %>% 
  summarise(nusers = length(unique(user_pk)),
            eligible = sum(wam_eligible, na.rm = T), 
            using = sum(wam_using, na.rm = T), 
            experienced = sum(wam_experienced, na.rm=T), 
            wams_generated = sum(elig_exp_using, na.rm=T),
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
table_1_wam <- filter(table_1_wam, calendar_month >= "2013-01-01", calendar_month <= "2015-05-01")
names(table_1_wam)[names(table_1_wam) == "calendar_month"] = "Calendar Month"
names(table_1_wam)[names(table_1_wam) == "nusers"] = "Active Users"
names(table_1_wam)[names(table_1_wam) == "eligible"] = "Eligible Users"
names(table_1_wam)[names(table_1_wam) == "using"] = "Sufficient Users"
names(table_1_wam)[names(table_1_wam) == "experienced"] = "Experienced Users"
names(table_1_wam)[names(table_1_wam) == "wams_generated"] = "WAMs Generated"
write.csv(table_1_wam, file = "table_1_wam.csv", row.names=F)

#Merge domain facets to all_monthly
facets_to_merge <- select(domain, name, domain_id, country_final, sector_final, 
                          subsector_final, self_start, domain_has_amplifies_workers, 
                          domain_has_amplifies_project, new_business_unit, is_test, domain_start_date)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)
names(all_monthly)[names(all_monthly) == "domain_id"] = "domain_numeric"

# new_business_unit is currently based on deployment.country in Yedi's code:
# https://github.com/dimagi/dimagi-data-platform-R/blob/bf6e55f639abfd84a4310bc0bfa6578c8c3aa01c/function_libraries/report_utils.R#L169-L192
# Therefore the BU won't be specified for the countries variable which has the two-letter country code
# Need to manually define BU for these two-letter country codes
all_monthly$new_business_unit[is.na(all_monthly$new_business_unit)] <- "None"

missing_bu <- filter(all_monthly, new_business_unit == "None")
assign_bu <- unique(missing_bu$country_final)
table(all_monthly$new_business_unit, useNA = "always")

all_monthly$new_business_unit[all_monthly$country_final == "{BR,HT,IN,KE,MW,PE,ZA,TH,UG,ZW}"] <- "DLAC"
all_monthly$new_business_unit[all_monthly$country_final == "{IN}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{US}"] <- "Inc"
all_monthly$new_business_unit[all_monthly$country_final == "{NE}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{MM}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{NG}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{SN}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{NP}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{HT}"] <- "DLAC"
all_monthly$new_business_unit[all_monthly$country_final == "{ID}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{TZ}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{ET}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{GD}"] <- "DLAC"
all_monthly$new_business_unit[all_monthly$country_final == "{ZA}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{ML,SN}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{LA}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{AF}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{GT}"] <- "DLAC"
all_monthly$new_business_unit[all_monthly$country_final == "{KE}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{BF}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{ZM}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{TH}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{SL}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{CO}"] <- "DLAC"
all_monthly$new_business_unit[all_monthly$country_final == "{BJ}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{BJ}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{GN}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{ET,UG}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{LS}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{GH}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{MW}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{MY}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{SS}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "Syria"] <- "Inc"
all_monthly$new_business_unit[all_monthly$country_final == "{SY}"] <- "Inc"
all_monthly$new_business_unit[all_monthly$country_final == "{IQ}"] <- "Inc"
all_monthly$new_business_unit[all_monthly$country_final == "{BD}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{JO}"] <- "Inc"
all_monthly$new_business_unit[all_monthly$country_final == "{MZ}"] <- "DMOZ"
all_monthly$new_business_unit[all_monthly$country_final == "{CN,GN}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{ZA,UG}"] <- "DSA"
all_monthly$new_business_unit[all_monthly$country_final == "{VN}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{TR}"] <- "Inc"
all_monthly$new_business_unit[all_monthly$country_final == "{BZ,GT,HN}"] <- "DLAC"
all_monthly$new_business_unit[all_monthly$country_final == "{LR}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{BF,TD,NE}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{PK}"] <- "DSI"
all_monthly$new_business_unit[all_monthly$country_final == "{ML}"] <- "DWA"
all_monthly$new_business_unit[all_monthly$country_final == "{SY,TR}"] <- "Inc"
all_monthly$new_business_unit[all_monthly$country_final == "{TD}"] <- "DWA"

business_unit <- unique(all_monthly$new_business_unit)
for (i in business_unit) {
  single_bu <- all_monthly[all_monthly$new_business_unit == i,]
  #Create Table 1
  table_1_wam_bu <- single_bu %>% group_by(calendar_month) %>% 
    summarise(nusers = length(unique(user_pk)),
              eligible = sum(wam_eligible, na.rm = T), 
              using = sum(wam_using, na.rm = T), 
              experienced = sum(wam_experienced, na.rm=T),
              wams_generated = sum(elig_exp_using, na.rm=T),
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
  table_1_wam_bu <- filter(table_1_wam_bu, calendar_month >= "2013-01-01", calendar_month <= "2015-05-01")
  names(table_1_wam_bu)[names(table_1_wam_bu) == "calendar_month"] = "Calendar Month"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "nusers"] = "Active Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "eligible"] = "Eligible Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "using"] = "Sufficient Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "experienced"] = "Experienced Users"
  names(table_1_wam_bu)[names(table_1_wam_bu) == "wams_generated"] = "WAMs Generated"
  
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
all_monthly$start_month <- as.Date("2015-05-01")
all_monthly$end_month <- as.Date("2015-05-01") 

all_monthly_table2 <- filter(all_monthly, calendar_month >= start_month, 
                             calendar_month <= end_month)

#Create Table 2
table_2_wam <- all_monthly_table2 %>% group_by(domain) %>% 
  summarise(calendar_month = unique(start_month),
            Country = unique(country_final),
            Sector = unique(sector_final),
            Subsector = unique(subsector_final),
            business_unit = unique(new_business_unit), 
            self_service = unique(self_start), 
            test = unique(is_test), 
            domain_start_date = unique(domain_start_date, na.rm=T), 
            nusers = length(unique(user_pk)), 
            domain_has_amplifies_workers = unique(domain_has_amplifies_workers),
            domain_has_amplifies_project = unique(domain_has_amplifies_project),
            
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

names(table_2_wam)[names(table_2_wam) == "domain"] = "Domain"
names(table_2_wam)[names(table_2_wam) == "calendar_month"] = "Calendar Month"
names(table_2_wam)[names(table_2_wam) == "business_unit"] = "Business Unit"
names(table_2_wam)[names(table_2_wam) == "self_service"] = "Self Service"
names(table_2_wam)[names(table_2_wam) == "test"] = "Test Domain"
names(table_2_wam)[names(table_2_wam) == "domain_start_date"] = "Domain Start Date"
names(table_2_wam)[names(table_2_wam) == "nusers"] = "Active Users"
names(table_2_wam)[names(table_2_wam) == "domain_has_amplifies_workers"] = "Eligible for WAMs"
names(table_2_wam)[names(table_2_wam) == "domain_has_amplifies_project"] = "Eligible for PAMs"

write.csv(table_2_wam, file = "table_2_wam_original.csv")

#------------------------------------------------------------------------#
#Table 2_1: WAM DATA (modified from Table 2
#------------------------------------------------------------------------#

#Set table parameters for current_month
all_monthly$current_month <- as.Date("2015-05-01")
all_monthly$prior_1_month <- floor_date(rollback(all_monthly$current_month), "month")
all_monthly$prior_2_months <- floor_date(rollback(all_monthly$prior_1_month), "month")

#Create denominators for table 2_1, section 4

#d1: all users who ever submitted forms as of the month in question
#We are flagging the first calendar month of each user
all_monthly <- all_monthly %>% group_by(domain_numeric, user_pk) %>% 
  mutate(new_user = calendar_month == min(calendar_month))
#Sort by domain and calendar_month before calculating the cumulative sum 
#of new users in the next step
all_monthly <- ungroup(all_monthly)
all_monthly <- arrange(all_monthly, domain_numeric, calendar_month)
#Calculate cumulative sum of new users by domain
all_monthly <- all_monthly %>% group_by(domain_numeric) %>% 
  mutate(max_users = cumsum(new_user))
# Create variable for total users to date 
all_monthly <- all_monthly %>% group_by(domain_numeric, calendar_month) %>% 
  mutate(total_users_to_date = max(max_users))

#d2: all users who submitted forms at least 4+ months ago, regardless of whether or not they 
#are experienced as of the month in question (i.e., these users could theoretically be 
#experienced by the month in question. For example, a user who submitted just one form 
#6 months ago)
#Calculate user_start_date
all_monthly <- all_monthly %>% group_by(domain, user_pk) %>% 
  mutate(user_start_month = min(calendar_month, na.rm=T))
#Based on user_start_date for each user, we will count out three months
#This is when the user will be theoretically experienced
all_monthly$user_exp_month_theoretical <- all_monthly$user_start_month %m+% months(3)
#Calculate the number of new theoretically experienced users per user_exp_month_theoretical
all_monthly <- all_monthly %>% group_by(domain, user_exp_month_theoretical) %>% 
  mutate(nusers_exp_month_theoretical = length(unique(user_pk)))
#Keep only one row per domain, user_exp_month_theoretical combination. 
d2_calc <- all_monthly[!duplicated(all_monthly[c("domain", "user_exp_month_theoretical")]),]
d2_calc <- select(d2_calc, domain, user_exp_month_theoretical, nusers_exp_month_theoretical)
d2_calc <- arrange(d2_calc, domain, user_exp_month_theoretical)
#Calculate cumulative sum of theoretical experienced users by domain
d2_calc <- d2_calc %>% group_by(domain) %>% 
  mutate(total_exp_theoretical_users_to_date = cumsum(nusers_exp_month_theoretical), 
         last_calc_month = max(user_exp_month_theoretical), 
         max_total_exp = max(total_exp_theoretical_users_to_date))
#Merge to all_monthly by domain, user_exp_month_theoretical
all_monthly <- merge(all_monthly, d2_calc, by.x=c("domain","calendar_month"), 
              by.y = c("domain", "user_exp_month_theoretical"), all.x=T)
all_monthly <- all_monthly %>% group_by(domain) %>% 
  mutate(last_calc_month2 = max(last_calc_month, na.rm=T), 
         max_total_exp2 = max(max_total_exp, na.rm=T))
all_monthly$last_calc_month2[is.na(all_monthly$last_calc_month2)] <- as.Date("1900-01-01")
all_monthly$total_exp_theoretical_users_to_date[all_monthly$calendar_month > all_monthly$last_calc_month2] <- 
  all_monthly$max_total_exp2[all_monthly$calendar_month > all_monthly$last_calc_month2]
all_monthly$total_exp_theoretical_users_to_date[is.na(all_monthly$total_exp_theoretical_users_to_date)] <- 
  0

#d3: all experienced users (all users who are actually experienced by the time they reach 
#the month in question, regardless of whether or not they are active during the month 
#in question)
#We are flagging the calendar month in which each user actually becomes experienced
all_monthly$new_exp_user <- all_monthly$previous_active_months_rolling == 3
#Sort by domain and calendar_month before calculating the cumulative sum 
#of new users in the next step
all_monthly <- ungroup(all_monthly)
all_monthly <- arrange(all_monthly, domain_numeric, calendar_month)
#Calculate cumulative sum of new experienced users by domain
all_monthly <- all_monthly %>% group_by(domain_numeric) %>% 
  mutate(max_exp_users = cumsum(new_exp_user))
# Create row for total experienced users to date 
all_monthly <- all_monthly %>% group_by(domain_numeric, calendar_month) %>% 
  mutate(total_exp_users_to_date = max(max_exp_users))

#Create all_monthly subset for time range parameters
all_monthly_table2 <- filter(all_monthly, calendar_month >= prior_2_months, 
                             calendar_month <= current_month) 

#Create Table 2_1_1
table_2_1_1 <- all_monthly_table2 %>% group_by(domain) %>% 
  summarise(Country = unique(country_final),
            Sector = unique(sector_final),
            Subsector = unique(subsector_final),
            business_unit = unique(new_business_unit), 
            self_service = unique(self_start), 
            test = unique(is_test), 
            domain_start_date = unique(domain_start_date, na.rm=T), 
            nusers = length(unique(user_pk[calendar_month == current_month])), 
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
            wams_2_mos_prior = sum(elig_exp_using[calendar_month == prior_2_months], na.rm=T), 
            wams_1_mo_prior = sum(elig_exp_using[calendar_month == prior_1_month], na.rm=T), 
            wams_current_mo = sum(elig_exp_using[calendar_month == current_month], na.rm=T))
names(table_2_1_2)[names(table_2_1_2) == "domain"] = "Domain"
names(table_2_1_2)[names(table_2_1_2) == "prior_2_months"] = "Prior 2 Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "prior_1_month"] = "Prior 1 Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "current_month"] = "Current Calendar Month"
names(table_2_1_2)[names(table_2_1_2) == "wams_2_mos_prior"] = "WAMs 2 months prior"
names(table_2_1_2)[names(table_2_1_2) == "wams_1_mo_prior"] = "WAMs 1 month prior"
names(table_2_1_2)[names(table_2_1_2) == "wams_current_mo"] = "WAMs current month"

#write.csv(table_2_1_2, file = "table_2_1_2.csv", row.names = F)

table_2_1_3 <- all_monthly_table2 %>% group_by(domain) %>%
  summarise(using_exp = sum(using_exp[calendar_month == current_month], na.rm=T),
            inelig_exp_using = sum(notelig_exp_using[calendar_month == current_month] == T | 
                                     na_exp_using[calendar_month == current_month] == T, na.rm = T),
            notusing_exp = sum(notusing_exp[calendar_month == current_month], na.rm=T),
            using_notexp = sum(using_notexp[calendar_month == current_month], na.rm=T), 
            notusing_notexp = sum(notusing_notexp[calendar_month == current_month], na.rm=T))
names(table_2_1_3)[names(table_2_1_3) == "domain"] = "Domain"
names(table_2_1_3)[names(table_2_1_3) == "using_exp"] = "Sufficient Use and Experience"
names(table_2_1_3)[names(table_2_1_3) == "inelig_exp_using"] = "Ineligible - Sufficient Use and Experience"
names(table_2_1_3)[names(table_2_1_3) == "notusing_exp"] = "Low Use"
names(table_2_1_3)[names(table_2_1_3) == "using_notexp"] = "Not Experienced"
names(table_2_1_3)[names(table_2_1_3) == "notusing_notexp"] = "Low Use + Not Experienced"

#write.csv(table_2_1_3, file = "table_2_1_3.csv", row.names = F)
            
table_2_1_4 <- filter(all_monthly_table2, calendar_month == current_month) %>% 
  group_by(domain) %>% 
  summarise(d1_total_users_to_date = unique(total_users_to_date), 
            #d2_exp_theory = unique(total_exp_theoretical_users_to_date), 
            d3_exp_real = unique(total_exp_users_to_date), 
            d4_exp_active_current = sum(wam_experienced, na.rm=T),
            d5_active = length(unique(user_pk)), 
            wams_current_mo = sum(elig_exp_using, na.rm=T))
table_2_1_4$per_wams_current <- round((table_2_1_4$wams_current_mo/table_2_1_4$d4_exp_active_current)*100, digits = 1)
table_2_1_4$per_wams_current[table_2_1_4$d4_exp_active_current == 0] <- 0

names(table_2_1_4)[names(table_2_1_4) == "domain"] = "Domain"
names(table_2_1_4)[names(table_2_1_4) == "d1_total_users_to_date"] = "D1 All Users Ever Active"
#names(table_2_1_4)[names(table_2_1_4) == "d2_exp_theory"] = "D2 All Possibly Exp Users"
names(table_2_1_4)[names(table_2_1_4) == "d3_exp_real"] = "D3 All Actually Exp Users"
names(table_2_1_4)[names(table_2_1_4) == "d4_exp_active_current"] = "D4 All Experienced + Active Users"
names(table_2_1_4)[names(table_2_1_4) == "d5_active"] = "D5 All Active Users"
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

#------------------------------------------------------------------------#
#Table 3: WAM Delta Table
#------------------------------------------------------------------------#

#Table parameters already set for Table 2
#Just use those tables

table_3 <- all_monthly_table2 %>% group_by(domain) %>% 
  summarise(Country = unique(country_final),
            Sector = unique(sector_final),
            Subsector = unique(subsector_final),
            business_unit = unique(new_business_unit), 
            self_service = unique(self_start), 
            test = unique(is_test), 
            domain_start_date = min(calendar_month, na.rm=T), 
            nusers = length(unique(user_pk)), 
            domain_has_amplifies_workers = unique(domain_has_amplifies_workers),
            domain_has_amplifies_project = unique(domain_has_amplifies_project),
            M1 = unique(current_month)
            nusers)

names(table_2_1_1)[names(table_2_1_1) == "domain"] = "Domain"
names(table_2_1_1)[names(table_2_1_1) == "business_unit"] = "Business Unit"
names(table_2_1_1)[names(table_2_1_1) == "self_service"] = "Self Service"
names(table_2_1_1)[names(table_2_1_1) == "test"] = "Test Domain"
names(table_2_1_1)[names(table_2_1_1) == "domain_start_date"] = "Domain Start Date"
names(table_2_1_1)[names(table_2_1_1) == "nusers"] = "Active Users"
names(table_2_1_1)[names(table_2_1_1) == "domain_has_amplifies_workers"] = "Eligible for WAMs"
names(table_2_1_1)[names(table_2_1_1) == "domain_has_amplifies_project"] = "Eligible for PAMs"

#Create Table 2 Month 1
table_2_M1 <- all_monthly_table2 %>% group_by(domain) %>% 
  summarise(month_M1 = unique(start_month),
            country_M1 = unique(country_final),
            sector_M1 = unique(sector_final),
            subsector_M1 = unique(subsector_final),
            nusers_active_M1 = length(unique(user_pk)),
            domain_has_amplifies_workers_M1 = unique(domain_has_amplifies_workers),
            domain_has_amplifies_project_M1 = unique(domain_has_amplifies_project), 
            
            elig_using_exp_M1 = sum(elig_exp_using, na.rm=T),
            elig_notusing_exp_M1 = sum(elig_exp_notusing, na.rm=T),
            elig_using_notexp_M1 = sum(elig_notexp_using, na.rm=T), 
            elig_notusing_notexp_M1 = sum(elig_notexp_notusing, na.rm=T),
            
            notelig_using_exp_M1 = sum(notelig_exp_using, na.rm=T),
            notelig_notusing_exp_M1 = sum(notelig_exp_notusing, na.rm=T),
            notelig_using_notexp_M1 = sum(notelig_notexp_using, na.rm=T), 
            notelig_notusing_notexp_M1 = sum(notelig_notexp_notusing, na.rm=T), 
            
            naelig_using_exp_M1 = sum(na_exp_using, na.rm=T),
            naelig_notusing_exp_M1 = sum(na_exp_notusing, na.rm=T), 
            naelig_using_notexp_M1 = sum(na_notexp_using, na.rm=T),
            naelig_notusing_notexp_M1 = sum(na_notexp_notusing, na.rm=T))

month_sms <- filter(domain_nusers_month_sms, calendar_month == unique(all_monthly$start_month))
month_sms <- select(month_sms, -calendar_month)
names(month_sms)[names(month_sms) == "nusers_month_sms"] = "nusers_month_sms_M1"
table_2_M1 <- merge(table_2_M1, month_sms, by = "domain", all.x = T)
table_2_M1 <- table_2_M1[c(1:5, 7:8, 6, 21, 9:20)]

#Set table parameters for Month 2
all_monthly$start_month <- as.Date("2015-03-01")
all_monthly$end_month <- as.Date("2015-03-01") 

all_monthly_table2 <- filter(all_monthly, calendar_month >= start_month, 
                             calendar_month <= end_month)
all_monthly_table2$unique_number <- 1:nrow(all_monthly_table2)

#Create Table 2 Month 2
table_2_M2 <- all_monthly_table2 %>% group_by(domain) %>% 
  summarise(month_M2 = unique(start_month),
            country_M2 = unique(country_final),
            sector_M2 = unique(sector_final),
            subsector_M2 = unique(subsector_final),
            nusers_active_M2 = length(unique(user_pk)),
            domain_has_amplifies_workers_M2 = unique(domain_has_amplifies_workers),
            domain_has_amplifies_project_M2 = unique(domain_has_amplifies_project), 
            
            elig_using_exp_M2 = sum(elig_exp_using, na.rm=T),
            elig_notusing_exp_M2 = sum(elig_exp_notusing, na.rm=T),
            elig_using_notexp_M2 = sum(elig_notexp_using, na.rm=T), 
            elig_notusing_notexp_M2 = sum(elig_notexp_notusing, na.rm=T),
            
            notelig_using_exp_M2 = sum(notelig_exp_using, na.rm=T),
            notelig_notusing_exp_M2 = sum(notelig_exp_notusing, na.rm=T),
            notelig_using_notexp_M2 = sum(notelig_notexp_using, na.rm=T), 
            notelig_notusing_notexp_M2 = sum(notelig_notexp_notusing, na.rm=T), 
            
            naelig_using_exp_M2 = sum(na_exp_using, na.rm=T),
            naelig_notusing_exp_M2 = sum(na_exp_notusing, na.rm=T), 
            naelig_using_notexp_M2 = sum(na_notexp_using, na.rm=T),
            naelig_notusing_notexp_M2 = sum(na_notexp_notusing, na.rm=T))

month_sms <- filter(domain_nusers_month_sms, calendar_month == unique(all_monthly$start_month))
month_sms <- select(month_sms, -calendar_month)
names(month_sms)[names(month_sms) == "nusers_month_sms"] = "nusers_month_sms_M2"
table_2_M2 <- merge(table_2_M2, month_sms, by = "domain", all.x = T)
table_2_M2 <- table_2_M2[c(1:5, 7:8, 6, 21, 9:20)]

#Create Table 5: Delta WAM Table
table_5_wam <- merge(table_2_M1, table_2_M2, by = "domain", all = T)
table_5_wam$country <- ifelse(!is.na(table_5_wam$country_M1), table_5_wam$country_M1, table_5_wam$country_M2)
table_5_wam$sector <- ifelse(!is.na(table_5_wam$sector_M1), table_5_wam$sector_M1, table_5_wam$sector_M2)
table_5_wam$subsector <- ifelse(!is.na(table_5_wam$subsector_M1), table_5_wam$subsector_M1, table_5_wam$subsector_M2)
table_5_wam$domain_has_amplifies_workers <- ifelse(!is.na(table_5_wam$domain_has_amplifies_workers_M1), 
                                                   table_5_wam$domain_has_amplifies_workers_M1, table_5_wam$domain_has_amplifies_workers_M2)
table_5_wam$domain_has_amplifies_project <- ifelse(!is.na(table_5_wam$domain_has_amplifies_project_M1), 
                                                   table_5_wam$domain_has_amplifies_project_M1, table_5_wam$domain_has_amplifies_project_M2)
table_5_wam <- select(table_5_wam, -c(country_M1, sector_M1, subsector_M1, domain_has_amplifies_workers_M1, domain_has_amplifies_project_M1, 
                                      country_M2, sector_M2, subsector_M2, domain_has_amplifies_workers_M2, domain_has_amplifies_project_M2))
table_5_wam <- table_5_wam[c(1:2, 17, 32:36, 3, 18, 4, 19, 5, 20, 6, 21, 7, 22, 8, 23, 9, 24, 10, 25, 11, 26, 12, 27, 13, 28, 
                             14, 29, 15, 30, 16, 31)]

table_5_wam$delta_nusers_active <- table_5_wam$nusers_active_M2 - table_5_wam$nusers_active_M1 
table_5_wam$delta_nusers_sms <- table_5_wam$nusers_month_sms_M2 - table_5_wam$nusers_month_sms_M1 
table_5_wam$delta_elig_using_exp <- table_5_wam$elig_using_exp_M2 - table_5_wam$elig_using_exp_M1
table_5_wam$delta_elig_notusing_exp <- table_5_wam$elig_notusing_exp_M2 - table_5_wam$elig_notusing_exp_M1
table_5_wam$delta_elig_using_notexp <- table_5_wam$elig_using_notexp_M2 - table_5_wam$elig_using_notexp_M1
table_5_wam$delta_elig_notusing_notexp <- table_5_wam$elig_notusing_notexp_M2 - table_5_wam$elig_notusing_notexp_M1
table_5_wam$delta_notelig_using_exp <- table_5_wam$notelig_using_exp_M2 - table_5_wam$notelig_using_exp_M1
table_5_wam$delta_notelig_notusing_exp <- table_5_wam$notelig_notusing_exp_M2 - table_5_wam$notelig_notusing_exp_M1
table_5_wam$delta_notelig_using_notexp <- table_5_wam$notelig_using_notexp_M2 - table_5_wam$notelig_using_notexp_M1
table_5_wam$delta_notelig_notusing_notexp <- table_5_wam$notelig_notusing_notexp_M2 - table_5_wam$notelig_notusing_notexp_M1
table_5_wam$delta_naelig_using_exp <- table_5_wam$naelig_using_exp_M2 - table_5_wam$naelig_using_exp_M1
table_5_wam$delta_naelig_notusing_exp <- table_5_wam$naelig_notusing_exp_M2 - table_5_wam$naelig_notusing_exp_M1
table_5_wam$delta_naelig_using_notexp <- table_5_wam$naelig_using_notexp_M2 - table_5_wam$naelig_using_notexp_M1
table_5_wam$delta_naelig_notusing_notexp <- table_5_wam$naelig_notusing_notexp_M2 - table_5_wam$naelig_notusing_notexp_M1

table_5_wam <- table_5_wam[c(1:10, 37, 11:12, 38, 13:14, 39, 15:16, 40, 17:18, 41, 19:20, 42, 
                             21:22, 43, 23:24, 44, 25:26, 45, 27:28, 46, 29:30, 47, 31:32, 48, 
                             33:34, 49, 35:36, 50)]
write.csv(table_5_wam, file = "table_5_wam.csv")

#------------------------------------------------------------------------#
#Table 4: PAM OVERVIEW
#------------------------------------------------------------------------#

#Create Table 4
table_4_pam <- all_monthly %>% group_by(calendar_month) %>% 
  summarise(nusers = length(unique(user_pk)),
            elig_using_exp_pam = sum(elig_exp_using_pam, na.rm=T),
            elig_notusing_exp_pam = sum(elig_exp_notusing_pam, na.rm=T),
            elig_using_notexp_pam = sum(elig_notexp_using_pam, na.rm=T), 
            elig_notusing_notexp_pam = sum(elig_notexp_notusing_pam, na.rm=T),
            
            notelig_using_exp_pam = sum(notelig_exp_using_pam, na.rm=T),
            notelig_notusing_exp_pam = sum(notelig_exp_notusing_pam, na.rm=T),
            notelig_using_notexp_pam = sum(notelig_notexp_using_pam, na.rm=T), 
            notelig_notusing_notexp_pam = sum(notelig_notexp_notusing_pam, na.rm=T), 
            
            naelig_using_exp_pam = sum(na_exp_using_pam, na.rm=T),
            naelig_notusing_exp_pam = sum(na_exp_notusing_pam, na.rm=T), 
            naelig_using_notexp_pam = sum(na_notexp_using_pam, na.rm=T),
            naelig_notusing_notexp_pam = sum(na_notexp_notusing_pam, na.rm=T))

#Filter table_4
table_4_pam <- filter(table_4_pam, calendar_month >= "2013-01-01", calendar_month <= "2015-05-01")
write.csv(table_4_pam, file = "table_4_pam.csv")


#------------------------------------------------------------------------#
#Table 6: User Exclusion Table
#------------------------------------------------------------------------#

#Already generated above

#------------------------------------------------------------------------#
#Table 7: ANNOTATION STATS
#------------------------------------------------------------------------#

#Create Table 3
table_3_wam <- table(app_amplifies$amplifies_project, 
                     app_amplifies$amplifies_workers, 
                     dnn=c("amplifies_project","amplifies_workers"), useNA = "always")
write.csv(table_3_wam, file = "table_3_wam.csv")

#------------------------------------------------------------------------#
#Old code
#------------------------------------------------------------------------#

form_table$form_date <- substr(form_table$time_start, 1, 10)
form_table$form_date <- as.Date(form_table$form_date)
test <- filter(form_table, amplifies_workers == T)
test <- filter(form_table, amplifies_workers == T & form_date >= "2015-01-01")

test2 <- app_amplifies[!(app_amplifies$id %in% test$application_id),]
test2 <- filter(test2, amplifies_workers == T)
test2 <- merge(test2, select(domain, domain_id, name), by = "domain_id", all.x = T)
test2 <- select(test2, domain_id, app_name, name)

crs <- filter(form_table, application_id == 428)

#Import amplifies_worker data for each app
#DUMMY CODE START#####################################
#We are using typical_flw app annotations as dummy data till we get the WAM annotations
#app <- tbl(db, "application")
#app <- collect(app)
#typical_flw_apps <- read.csv(file = "typical_flw_apps.csv")
#app <- merge(app, typical_flw_apps, by.x = "app_id", all.x = T)
#app$typical_flw_app[app$typical_flw_app == 1] <- "yes"
#app$typical_flw_app[app$typical_flw_app == 0] <- "no"
#app$typical_flw_app[is.na(app$typical_flw_app)] <- "not_set"
#names(app)[names(app) == "typical_flw_app"] = "amplifies_workers"
#DUMMY CODE END######################################
#The next two lines are suggested by Yedi, but the parsing is incorrect, so the resulting table 
#has wrong values for amplifies_X variables.
#app <- get_application_table(db)
#app <- select(app, app_id, domain_name, amplifies_project, amplifies_workers, application_version)

