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

#Pull domain table
domain <- get_domain_table(db)

#------------------------------------------------------------------------#
#User exclusions
#------------------------------------------------------------------------#

#Exclude demo users and NA/NONE users
#First count these users per domain per month
#all_monthly <- all_monthly %>% group_by(domain, calendar_month) %>% 
#  mutate(nusers_id_demo_na_none = sum(user_id =="demo_user" | user_id =="NONE" | 
#                                   user_id =="none" | is.na(user_id)))
all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
all_monthly = all_monthly[!(all_monthly$user_id =="NONE"),]
all_monthly = all_monthly[!(all_monthly$user_id =="none"),]
all_monthly = all_monthly[!is.na(all_monthly$user_id),]

#Exclude users who submit to multiple domains
#First count these users per domain per month
#chw_single_domain <- all_monthly %>% group_by(user_pk) %>% 
#  summarise(n_domains = length(unique(domain)))
#chw_single_domain <- filter(chw_single_domain, n_domains >1)
#all_monthly <- all_monthly %>% group_by(domain, calendar_month) %>% 
#  mutate(nusers_multiple_domains = sum(user_pk %in% chw_single_domain$user_pk))
chw_single_domain <- all_monthly %>% group_by(user_pk) %>% 
  summarise(n_domains = length(unique(domain)))
chw_single_domain <- filter(chw_single_domain, n_domains == 1)
all_monthly <- all_monthly[all_monthly$user_pk %in% chw_single_domain$user_pk,]

#Exclude rows with nforms = NA. These are not rows where the user is truly active
#First count nrows with nforms = NA per domain per month
#all_monthly <- all_monthly %>% group_by(domain, calendar_month) %>% 
#  mutate(nusers_forms_na = sum(is.na(nforms)))
all_monthly <- filter(all_monthly, !is.na(nforms))

#Exclude users with user_type = web and summary_device_type != cloud care during any month
#First count these users per domain per month
all_monthly <- merge(all_monthly, users, by = "user_pk", all.x = T)
exclude1 <- filter(all_monthly, user_type == "web" & 
                     summary_device_type != "Cloudcare")
#all_monthly <- all_monthly %>% group_by(domain, calendar_month) %>% 
#  mutate(nusers_web_not_cloudcare = sum(user_pk %in% exclude1$user_pk))
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude1$user_pk),]

#Exclude dimagi users and superusers
#First count these users per domain per month
exclude2 <- all_monthly[grep("dimagi", all_monthly$email, fixed=T),]
#all_monthly <- all_monthly %>% group_by(domain, calendar_month) %>% 
#  mutate(nusers_dimagi_superuser = sum(user_pk %in% exclude2$user_pk | is_superuser == T))
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude2$user_pk),]
all_monthly <- filter(all_monthly, is_superuser == F | is.na(is_superuser))

#Exclude users with summary_device_type = SMS during any month
#First count these users per domain per month
exclude3 <- filter(all_monthly, summary_device_type == "Sms")
all_monthly <- all_monthly %>% group_by(domain, calendar_month) %>% 
  mutate(nusers_sms_any_month = sum(user_pk %in% exclude3$user_pk))
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
all_monthly <- all_monthly[!(all_monthly$submitted_single_app == F & 
                               all_monthly$submitted_multiple_app == F),]

#Single_app users
all_monthly_single <- filter(all_monthly, submitted_single_app == T)

#Multiple_app users
all_monthly_multiple <- filter(all_monthly, submitted_multiple_app == T)

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
app_amplifies$amplifies_workers[app_amplifies$id == 10724] <- F #pradan-mis "CDC"
app_amplifies$amplifies_workers[app_amplifies$id == 339] <- T #tulasalud "Kawok Vigilancia Comunitario 3.0"
app_amplifies$amplifies_workers[app_amplifies$id == 1140] <- T #tulasalud "Kawok Vigilancia Comunitario 2.0"
app_amplifies$amplifies_workers[app_amplifies$id == 1567] <- T #tulasalud "Kawok Vigilancia Comunitario 2.1"
app_amplifies$amplifies_workers[app_amplifies$id == 109] <- T #opm "Bihar Child Support Programme"
app_amplifies$amplifies_workers[app_amplifies$id == 290] <- T #ssqh-cs "mSante"
app_amplifies$amplifies_workers[app_amplifies$id == 2014] <- F #ssqh-cs "Referans Klinik"


#amplifies_project
app_amplifies$amplifies_project <- NA
app_amplifies$amplifies_project[grep("_amplifies_project_=>_yes_", app_amplifies$attributes, 
                                     fixed=T)] <- T
app_amplifies$amplifies_project[grep("_amplifies_project_=>_no_", app_amplifies$attributes, 
                                     fixed=T)] <- F
#Manually tag domains
app_amplifies$amplifies_project[app_amplifies$id == 10724] <- T #pradan-mis "CDC"
app_amplifies$amplifies_project[app_amplifies$id == 339] <- T #tulasalud "Kawok Vigilancia Comunitario 3.0"
app_amplifies$amplifies_project[app_amplifies$id == 1140] <- T #tulasalud "Kawok Vigilancia Comunitario 2.0"
app_amplifies$amplifies_project[app_amplifies$id == 1567] <- T #tulasalud "Kawok Vigilancia Comunitario 2.1"
app_amplifies$amplifies_project[app_amplifies$id == 109] <- T #opm "Bihar Child Support Programme"
app_amplifies$amplifies_project[app_amplifies$id == 290] <- F #ssqh-cs "mSante"
app_amplifies$amplifies_project[app_amplifies$id == 2014] <- F #ssqh-cs "Referans Klinik"

#Add amplifies_workers and amplifies_project to form_table by app_id
form_table <- merge(form_table, 
                    select(app_amplifies, id, amplifies_workers, amplifies_project), 
                    by.x = "application_id", by.y = "id", all.x = T)

#Filter form_table by multiple_app users
#Create form_month and concat with user_pk
#USE received_on instead of time_start??
forms_users_multiple_apps <- filter(form_table, user_pk %in% all_monthly_multiple$user_pk)
forms_users_multiple_apps$form_date <- as.Date(substr(forms_users_multiple_apps$time_start, 1, 10))
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
is.na(domain$deployment.country) <- domain$deployment.country == ""
is.na(domain$country) <- domain$country == ""
domain$country_final <- domain$deployment.country
keep_country <- which(is.na(domain$deployment.country) & !is.na(domain$country))
domain$country_final[keep_country] <- domain$country[keep_country]
#Consolidate Dimagi level of support
is.na(domain$internal.services) <- domain$internal.services == ""
is.na(domain$internal.self_started) <- domain$internal.self_started == ""
domain$self_start[domain$internal.self_started == "True"] <- "self"
domain$dimagi_services <- domain$internal.services
keep_self <- which(is.na(domain$internal.services) & !is.na(domain$self_start))
domain$dimagi_services[keep_self] <- domain$self_start[keep_self]


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

#Append single app and multiple app tables together
summary(names(all_monthly_single) == names(all_monthly_multiple))
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
#wam_using, pam_using
#------------------------------------------------------------------------#

#wam_using == T or pam_using == T
#if the user met our criteria this month for sufficient usage of the app. 
#Specifically, they submitted forms on at least 4 different days. That is, they have at
#least for active days in the month in question.
all_monthly$wam_using <- all_monthly$active_days >= 4
all_monthly$pam_using <- all_monthly$active_days >= 4

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
table_1_wam <- filter(table_1_wam, calendar_month >= "2013-01-01", calendar_month <= "2015-04-01")

write.csv(table_1_wam, file = "table_1_wam.csv")

#------------------------------------------------------------------------#
#Table 2: WAM DATA
#------------------------------------------------------------------------#

#Merge domain facets to all_monthly
facets_to_merge <- select(domain, name, domain_id, country_final, sector_final, 
                          subsector_final, self_start, domain_has_amplifies_workers, 
                          domain_has_amplifies_project)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)
names(all_monthly)[names(all_monthly) == "domain_id"] = "domain_numeric"

#Table of #SMS users per domain per month
domain_nusers_month_sms <- all_monthly %>% group_by(domain, calendar_month) %>% 
  summarise(nusers_month_sms = unique(nusers_sms_any_month))

#Set table parameters
all_monthly$start_month <- as.Date("2015-04-01")
all_monthly$end_month <- as.Date("2015-04-01") 

all_monthly_table2 <- filter(all_monthly, calendar_month >= start_month, 
                             calendar_month <= end_month)
all_monthly_table2$unique_number <- 1:nrow(all_monthly_table2)

#Create Table 2
table_2_wam <- all_monthly_table2 %>% group_by(domain) %>% 
  summarise(start_month = unique(start_month),
            end_month = unique(end_month),
            country = unique(country_final),
            sector = unique(sector_final),
            subsector = unique(subsector_final),
            nusers_active = length(unique(user_pk)),
            domain_has_amplifies_workers = unique(domain_has_amplifies_workers),
            domain_has_amplifies_project = unique(domain_has_amplifies_project), 
            nuser_months = length(unique(unique_number)),
            
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

month_sms <- filter(domain_nusers_month_sms, calendar_month == unique(all_monthly$start_month))
month_sms <- select(month_sms, -calendar_month)
table_2_wam <- merge(table_2_wam, month_sms, by = "domain", all.x = T)
table_2_wam <- table_2_wam[c(1:7, 23, 8:22)]

write.csv(table_2_wam, file = "table_2_wam_apr_2015.csv")


#------------------------------------------------------------------------#
#Table 3: ANNOTATION STATS
#------------------------------------------------------------------------#

#Create Table 3
table_3_wam <- table(app_amplifies$amplifies_project, 
                     app_amplifies$amplifies_workers, 
                     dnn=c("amplifies_project","amplifies_workers"), useNA = "always")
write.csv(table_3_wam, file = "table_3_wam.csv")

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
table_4_pam <- filter(table_4_pam, calendar_month >= "2013-01-01", calendar_month <= "2015-04-01")
write.csv(table_4_pam, file = "table_4_pam.csv")

#------------------------------------------------------------------------#
#Table 5: WAM Delta Table
#------------------------------------------------------------------------#

#Set table parameters for Month 1
all_monthly$start_month <- as.Date("2015-02-01")
all_monthly$end_month <- as.Date("2015-02-01") 

all_monthly_table2 <- filter(all_monthly, calendar_month >= start_month, 
                             calendar_month <= end_month)
all_monthly_table2$unique_number <- 1:nrow(all_monthly_table2)

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
#Table 6: User Exclusion Table
#------------------------------------------------------------------------#

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

