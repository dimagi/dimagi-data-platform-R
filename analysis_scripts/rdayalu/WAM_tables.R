#This code is to create the WAM table as described in the following document
#https://docs.google.com/a/dimagi.com/document/d/1VwaJm_wUJmHWOH0aUsAbwbWplTyzUSo0dkN30R1en7s/edit
#First we need to add three variables to each row of the monthly table:
#wam_eligible, wam_experienced, wam_using
#From this table, we will calculate the WAM overview table (Table 1) and
#the WAM data table (Table 2), and the WAM/PAM annotation stats (Table 3) 
#as described in the google doc.

#------------------------------------------------------------------------#
#Import data
#------------------------------------------------------------------------#

library(dplyr)
library(data.table)
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
#permitted_data_only = T and is_test = F
source(file.path("analysis_scripts","raw_data","data_import.R", fsep = .Platform$file.sep))
all_monthly <- monthly_table
#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"
#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

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
all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
all_monthly = all_monthly[!(all_monthly$user_id =="NONE"),]
all_monthly = all_monthly[!(all_monthly$user_id =="none"),]
all_monthly = all_monthly[!is.na(all_monthly$user_id),]

#Exclude users who submit to multiple domains
chw_single_domain <- all_monthly %>% group_by(user_pk) %>% 
  summarise(n_domains = length(unique(domain)))
chw_single_domain <- filter(chw_single_domain, n_domains == 1)
all_monthly <- all_monthly[all_monthly$user_pk %in% chw_single_domain$user_pk,]

#Exclude users with user_type = web and summary_device_type != cloud care during any month
all_monthly <- merge(all_monthly, users, by = "user_pk", all.x = T)
exclude1 <- all_monthly[all_monthly$user_type == "web" & 
                          all_monthly$summary_device_type != "Cloudcare",]
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude1$user_pk),]

#Exclude dimagi users and superusers
exclude2 <- all_monthly[grep("dimagi", all_monthly$email, fixed=T),]
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude2$user_pk),]
all_monthly <- filter(all_monthly, is_superuser == F | is.na(is_superuser))

#Exclude users with summary_device_type = SMS during any month
exclude3 <- all_monthly[all_monthly$summary_device_type == "Sms",]
all_monthly <- all_monthly[!(all_monthly$user_pk %in% exclude3$user_pk),]

#Exclude users without form_ids and app_ids
#Flag users who have submitted data through just 1 app or > 1 app
#We are going to handle single_app and multiple_app users differently
chw_n_app <- form_table %>% group_by(user_pk) %>%
  summarise(n_applications = length(unique(application_id)))
#write.csv(chw_n_app, file = "chw_n_app.csv")
chw_multiple_app <- filter(chw_n_app, n_applications > 1)
chw_single_app <- filter(chw_n_app, n_applications == 1)
all_monthly$submitted_single_app <- all_monthly$user_pk %in% chw_single_app$user_pk
all_monthly$submitted_multiple_app <- all_monthly$user_pk %in% chw_multiple_app$user_pk
all_monthly <- all_monthly[!(all_monthly$submitted_single_app == F & 
                               all_monthly$submitted_multiple_app == F),]

#------------------------------------------------------------------------#
#Parse amplifies_X values from app table
#Add amplifies_workers to form_table by app_id
#------------------------------------------------------------------------#

#amplifies_workers
app_amplifies$amplifies_workers <- NA
app_amplifies$amplifies_workers[grep("_amplifies_workers_=>_yes_", app_amplifies$attributes, 
                                     fixed=T)] <- T
app_amplifies$amplifies_workers[grep("_amplifies_workers_=>_no_", app_amplifies$attributes, 
                                     fixed=T)] <- F
#Manually tag mvp domains
app_amplifies$amplifies_workers[app_amplifies$id %in% test2$application_id] <- T

#amplifies_project
app_amplifies$amplifies_project <- NA
app_amplifies$amplifies_project[grep("_amplifies_project_=>_yes_", app_amplifies$attributes, 
                                     fixed=T)] <- T
app_amplifies$amplifies_project[grep("_amplifies_project_=>_no_", app_amplifies$attributes, 
                                     fixed=T)] <- F
#Manually tag mvp domains
app_amplifies$amplifies_project[app_amplifies$id %in% test2$application_id] <- T

#Add amplifies_workers to form_table by app_pk
form_table <- merge(form_table, 
                    select(app_amplifies, id, amplifies_workers), 
                    by.x = "application_id", by.y = "id", all.x = T)

#Add amplifies_workers to domain table
domain$domain_has_amplifies_workers <- domain$id %in% 
  app_amplifies[app_amplifies$amplifies_workers == T,]$domain_id
domain$domain_has_amplifies_project <- domain$id %in% 
  app_amplifies[app_amplifies$amplifies_project == T,]$domain_id 
#Prepare domain table for merging in domain facets
#Bring in sector information
sector <- tbl(db, "sector")
sector <- collect(sector)
names(sector)[names(sector) == "name"] = "sector_final"
domain_sector <- tbl(db, "domain_sector")
domain_sector <- collect(domain_sector)
domain_sector <- select(domain_sector, domain_id, sector_id)
domain <- merge(domain, domain_sector, by.x = "id", by.y = "domain_id", all.x = T)
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
domain <- merge(domain, domain_subsector, by.x = "id", by.y = "domain_id", all.x = T)
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

#------------------------------------------------------------------------#
#wam_eligible
#------------------------------------------------------------------------#

#Single_app users = 12420 (As of 3/15/15)
all_monthly_single <- filter(all_monthly, submitted_single_app == T)

#Multiple_app users = 1314 (As of 3/15/15)
all_monthly_multiple<- filter(all_monthly, submitted_multiple_app == T)

#Single app users, wam_eligible == T
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker set to true
all_monthly_single$amp_w_true <- all_monthly_single$user_pk %in% 
  form_table[form_table$amplifies_workers == T,]$user_pk
all_monthly_single$wam_eligible[all_monthly_single$amp_w_true == T] <- T

#Single app users, wam_eligible == F
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker set to false
all_monthly_single$amp_w_false <- all_monthly_single$user_pk %in% 
  form_table[form_table$amplifies_workers == F,]$user_pk
all_monthly_single$wam_eligible[all_monthly_single$amp_w_false == T] <- F

#Single app users, wam_eligible == NA
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker not set
all_monthly_single$amp_w_na <- all_monthly_single$user_pk %in% 
  form_table[is.na(form_table$amplifies_workers),]$user_pk
all_monthly_single$wam_eligible[is.na(all_monthly_single$amp_w_na)] <- NA

#Multiple app users
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker set to true
all_monthly_multiple$amp_w_true <- all_monthly_multiple$user_pk %in% 
  form_table[form_table$amplifies_workers == T,]$user_pk

#Multiple app users
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker set to false
all_monthly_multiple$amp_w_false <- all_monthly_multiple$user_pk %in% 
  form_table[form_table$amplifies_workers == F,]$user_pk

#Multiple app users 
#The FLW submitted at least one form that month on a CommCare app with 
#amplifies_worker not set
all_monthly_multiple$amp_w_na <- all_monthly_multiple$user_pk %in% 
  form_table[is.na(form_table$amplifies_workers),]$user_pk

#Multiple app users, wam_eligible == T
#This will be true if a user submitted data through an amplifies_workers app at 
#any point in time
all_monthly_multiple$wam_eligible <- all_monthly_multiple$user_pk %in% 
  all_monthly_multiple[all_monthly_multiple$amp_w_true == T,]$user_pk

#Multiple app users, wam_eligible == F
#This will be false if the user submitted forms using amplfies_workers = F but
#not if amplifies_workers was ever = T
#Might not actually need to run this code!
all_monthly_multiple[all_monthly_multiple$amp_w_false == T & 
                       all_monthly_multiple$amp_w_true == F,]$wam_eligible <- F

#wam_eligible == NA
#This will be NA if the user submitted forms using amplifies_workers = NA
#for all their apps
all_monthly_multiple[all_monthly_multiple$amp_w_na == T & 
                       all_monthly_multiple$amp_w_true == F & 
                       all_monthly_multiple$amp_w_false == F,]$wam_eligible <- NA

#Append single app and multiple app tables together
all_monthly <- rbind(all_monthly_single, all_monthly_multiple)

#------------------------------------------------------------------------#
#wam_experienced
#------------------------------------------------------------------------#

#Add rolling # active months to each user's rows
all_monthly <- arrange(all_monthly, user_pk, calendar_month)
all_monthly <- all_monthly %>% group_by(user_pk) %>% 
  mutate(previous_active_months_rolling = seq_along(calendar_month)-1)

#wam_experienced == T if the user has submitted data for at least three months prior
#to the month in question
all_monthly$wam_experienced <- all_monthly$previous_active_months_rolling >= 3

#------------------------------------------------------------------------#
#wam_using
#------------------------------------------------------------------------#

#wam_using == T if the user met our criteria this month for sufficient usage of the app. 
#Specifically, they submitted forms on at least 4 different days. That is, they have at
#least for active days in the month in question.
all_monthly$active_days <- as.numeric(all_monthly$active_days)
all_monthly$wam_using <- all_monthly$active_days >= 4

#------------------------------------------------------------------------#
#Table 1: WAM OVERVIEW
#------------------------------------------------------------------------#

#We have 12 combinations for of wam_eligible(T/F/NA), wam_experienced(T/F) 
#and wam_using(T/F)
all_monthly$elig_exp_using <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == T
all_monthly$elig_exp_notusing <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == F
#all_monthly$elig_exp_na <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == T & 
#  is.na(all_monthly$wam_using)

all_monthly$notelig_exp_using <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == T
all_monthly$notelig_exp_notusing <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == F
#all_monthly$notelig_exp_na <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == T & 
#  is.na(all_monthly$wam_using)

all_monthly$na_exp_using <- is.na(all_monthly$wam_eligible)& all_monthly$wam_experienced == T & 
  all_monthly$wam_using == T
all_monthly$na_exp_notusing <- is.na(all_monthly$wam_eligible) & all_monthly$wam_experienced == T & 
  all_monthly$wam_using == F
#all_monthly$na_exp_na <- is.na(all_monthly$wam_eligible) & all_monthly$wam_experienced == T & 
#  is.na(all_monthly$wam_using)

all_monthly$elig_notexp_using <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == T
all_monthly$elig_notexp_notusing <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == F
#all_monthly$elig_notexp_na <- all_monthly$wam_eligible == T & all_monthly$wam_experienced == F & 
#  is.na(all_monthly$wam_using)

all_monthly$notelig_notexp_using <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == T
all_monthly$notelig_notexp_notusing <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == F
#all_monthly$notelig_notexp_na <- all_monthly$wam_eligible == F & all_monthly$wam_experienced == F & 
#  is.na(all_monthly$wam_using)

all_monthly$na_notexp_using <- is.na(all_monthly$wam_eligible)& all_monthly$wam_experienced == F & 
  all_monthly$wam_using == T
all_monthly$na_notexp_notusing <- is.na(all_monthly$wam_eligible) & all_monthly$wam_experienced == F & 
  all_monthly$wam_using == F
#all_monthly$na_notexp_na <- is.na(all_monthly$wam_eligible) & all_monthly$wam_experienced == F & 
#  is.na(all_monthly$wam_using)

#Create Table 1
table_1_wam <- all_monthly %>% group_by(calendar_month) %>% 
  summarise(nusers = length(unique(user_pk)),
            elig_exp_using_nusers = sum(elig_exp_using, na.rm=T),
            elig_exp_notusing_nusers = sum(elig_exp_notusing, na.rm=T),               
            #elig_exp_na_nusers = sum(elig_exp_na, na.rm=T),
            notelig_exp_using_nusers = sum(notelig_exp_using, na.rm=T),
            notelig_exp_notusing_nusers = sum(notelig_exp_notusing, na.rm=T),
            #notelig_exp_na_nusers = sum(notelig_exp_na, na.rm=T),
            na_exp_using_nusers = sum(na_exp_using, na.rm=T),
            na_exp_notusing_nusers = sum(na_exp_notusing, na.rm=T), 
            #na_exp_na_nusers = sum(na_exp_na, na.rm=T), 
            elig_notexp_using_nusers = sum(elig_notexp_using, na.rm=T), 
            elig_notexp_notusing_nusers = sum(elig_notexp_notusing, na.rm=T), 
            #elig_notexp_na_nusers = sum(elig_notexp_na, na.rm=T),
            notelig_notexp_using_nusers = sum(notelig_notexp_using, na.rm=T), 
            notelig_notexp_notusing_nusers = sum(notelig_notexp_notusing, na.rm=T), 
            #notelig_notexp_na_nusers = sum(notelig_notexp_na, na.rm=T), 
            na_notexp_using_nusers = sum(na_notexp_using, na.rm=T),
            na_notexp_notusing_nusers = sum(na_notexp_notusing, na.rm=T)) 
            #na_notexp_na_nusers = sum(na_notexp_na, na.rm=T))

#Filter table_1
table_1_wam <- filter(table_1_wam, calendar_month >= "2013-01-01", calendar_month <= "2015-02-01")

write.csv(table_1_wam, file = "table_1_wam.csv")

#------------------------------------------------------------------------#
#Table 2: WAM DATA
#------------------------------------------------------------------------#

#Merge domain facets to all_monthly
names(domain)[names(domain) == "id"] = "domain_id"
facets_to_merge <- select(domain, name, domain_id, country_final, sector_final, 
                          subsector_final, self_start, domain_has_amplifies_workers, 
                          domain_has_amplifies_project)
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)
names(all_monthly)[names(all_monthly) == "domain_id"] = "domain_numeric"
all_monthly$start_month <- as.Date("2015-01-01")
all_monthly$end_month <- as.Date("2015-01-01") 

all_monthly_table2 <- filter(all_monthly, calendar_month >= start_month, 
                             calendar_month <= end_month)
all_monthly_table2$unique_number <- 1:nrow(all_monthly_table2)

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
            elig_exp_using_nusers = sum(elig_exp_using, na.rm=T),
            elig_exp_notusing_nusers = sum(elig_exp_notusing, na.rm=T),               
            #elig_exp_na_nusers = sum(elig_exp_na, na.rm=T),
            notelig_exp_using_nusers = sum(notelig_exp_using, na.rm=T),
            notelig_exp_notusing_nusers = sum(notelig_exp_notusing, na.rm=T),
            #notelig_exp_na_nusers = sum(notelig_exp_na, na.rm=T),
            na_exp_using_nusers = sum(na_exp_using, na.rm=T),
            na_exp_notusing_nusers = sum(na_exp_notusing, na.rm=T), 
            #na_exp_na_nusers = sum(na_exp_na, na.rm=T), 
            elig_notexp_using_nusers = sum(elig_notexp_using, na.rm=T), 
            elig_notexp_notusing_nusers = sum(elig_notexp_notusing, na.rm=T), 
            #elig_notexp_na_nusers = sum(elig_notexp_na, na.rm=T),
            notelig_notexp_using_nusers = sum(notelig_notexp_using, na.rm=T), 
            notelig_notexp_notusing_nusers = sum(notelig_notexp_notusing, na.rm=T), 
            #notelig_notexp_na_nusers = sum(notelig_notexp_na, na.rm=T), 
            na_notexp_using_nusers = sum(na_notexp_using, na.rm=T),
            na_notexp_notusing_nusers = sum(na_notexp_notusing, na.rm=T))
            #na_notexp_na_nusers = sum(na_notexp_na, na.rm=T))

write.csv(table_2_wam, file = "table_2_wam_nov_2014.csv")

#------------------------------------------------------------------------#
#Table 3: ANNOTATION STATS
#------------------------------------------------------------------------#

table_3_wam <- table(app_amplifies$amplifies_project, 
                     app_amplifies$amplifies_workers, useNA = "always")
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

