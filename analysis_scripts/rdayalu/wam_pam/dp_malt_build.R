#This code is to build the DP MALT table that will be stored in dropbox
#and used in WAM table creation, in conjunction with the main HQ MALT

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
form_table <- select(form_table, id, user_id, received_on, formdef_id, domain_id, application_id)
names(form_table)[names(form_table) == "user_id"] = "user_pk"
names(form_table)[names(form_table) == "id"] = "form_pk"
names(form_table)[names(form_table) == "domain_id"] = "domain_pk"
names(form_table)[names(form_table) == "application_id"] = "application_pk"

#Create form_date and form_month based on server time
form_table$form_date <- as.Date(substr(form_table$received_on, 1, 10))
form_table$form_month <- floor_date(form_table$form_date, "month")

#Remove forms received after May 2015 - HQ MALT will count those forms and we don't want to 
#double count them
form_table <- filter(form_table, form_month <= "2015-05-01")

#Remove forms with app_pk = NA - these don't have corresponding app rows in the app table
form_table <- filter(form_table, !is.na(form_table$application_pk))

#Create DP MALT
dp_malt <- form_table %>% group_by(domain_pk, user_pk, application_pk, form_month) %>% 
  summarise(nforms = length(unique(form_pk)))

#------------------------------------------------------------------------#
#Merge in users, domain and app tables
#------------------------------------------------------------------------#

#Merge in user table: user_id, username, email
#Import user table and user_type table
#We are going to keep only mobile users (user_type == CommCareUser)
#Need to confirm that we don't want any of the other user types from HQ MALT
#These should only be admin, demo and unknown in addition to CommCareUser and 
#WebUser, so keeping on CommCareUser makes sense for now.
users <- tbl(db, "users") 
users <- collect(users)
user_type <- get_user_type_table(db)
user_type <- collect(user_type)
#Merge user tables
users <- merge(users, user_type, by.x = "id", by.y = "user_pk", all.x = T)
users <- select(users, -c(user_id.y, username.y, first_name, last_name, default_phone_number,
                          groups, phone_numbers, deactivated, deleted, is_superuser))
names(users)[names(users) == "username.x"] = "username"
names(users)[names(users) == "id"] = "user_pk"
names(users)[names(users) == "user_id.x"] = "user_id"
dp_malt <- merge(dp_malt, users, by = "user_pk", all.x = T)
dp_malt$user_type[dp_malt$user_type == "mobile"] <- "CommCareUser"
dp_malt$user_type[dp_malt$user_type == "web"] <- "WebUser"

#Merge in domain table: name
#We are not taking the country information or first form 
#submission date from DP since we are going to take it directly from 
#the project space list that we download from HQ. We will use that project space
#list in the generation of the WAM tables since those values are not currently 
#in HQ MALT, so we don't want them in DP MALT. 
domain <- get_domain_table(db)
names(domain)[names(domain) == "id"] = "domain_pk"
dp_malt <- merge(dp_malt, select(domain, domain_pk, name), by = "domain_pk", all.x = T)
names(dp_malt)[names(dp_malt) == "name"] = "domain"

#Merge in summary_device_type from monthly table
#We want to exclude monthly rows that have summary_device_type = SMS 
#Not sure yet what to do with NA | None | Other
#Pull monthly_table for all domains
#Be sure to set config_run first
#permitted_data_only = F and filter = include-all
source(file.path("analysis_scripts","raw_data","data_import.R", fsep = .Platform$file.sep))
#Change column names as needed
names(monthly_table)[names(monthly_table) == "month.index"] = "form_month"
names(monthly_table)[names(monthly_table) == "numeric_index"] = "month_index"
#Convert calendar month to actual date
monthly_table$form_month <- parse_date_time(paste('01', monthly_table$form_month), '%d %b %Y!')
monthly_table$form_month <- as.Date(monthly_table$form_month)
#Merge to dp_malt
dp_malt <- merge(dp_malt, select(monthly_table, domain, user_id, form_month, summary_device_type), 
                 by = c("domain", "user_id", "form_month"), all.x = T)
#Remove rows with summary_device_type = Sms
#Not sure yet what to do with NA | None | Other
dp_malt$keep_device_row <- dp_malt$summary_device_type == "Android" | dp_malt$summary_device_type == "Cloudcare" | 
  dp_malt$summary_device_type == "Multi" | dp_malt$summary_device_type == "Nokia" | dp_malt$summary_device_type == "None" | 
  dp_malt$summary_device_type == "Other" | is.na(dp_malt$summary_device_type)
dp_malt <- filter(dp_malt, keep_device_row == T)

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

#Change column names and wam/pam values to match HQ MALT
dp_malt <- select(dp_malt, -c(application_pk, domain_pk, user_pk, summary_device_type, keep_device_row))
names(dp_malt)[names(dp_malt) == "form_month"] = "month"
names(dp_malt)[names(dp_malt) == "nforms"] = "num_of_forms"
names(dp_malt)[names(dp_malt) == "domain"] = "domain_name"
names(dp_malt)[names(dp_malt) == "amplifies_workers"] = "wam"
names(dp_malt)[names(dp_malt) == "amplifies_project"] = "pam"

dp_malt$wam[dp_malt$wam == T] <- "yes"
dp_malt$wam[dp_malt$wam == F] <- "no"
dp_malt$wam[is.na(dp_malt$wam)] <- "not_set"

dp_malt$pam[dp_malt$pam == T] <- "yes"
dp_malt$pam[dp_malt$pam == F] <- "no"
dp_malt$pam[is.na(dp_malt$pam)] <- "not_set"

write.csv(dp_malt, file = "malt_dp.csv", row.names = F)

