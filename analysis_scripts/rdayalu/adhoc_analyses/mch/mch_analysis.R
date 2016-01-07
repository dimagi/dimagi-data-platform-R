# 5/22/15
# This script is for an analysis of MCH domain data, looking at CommCare activity and how it relates
# to impact data. 
# Outline of analysis here:
# https://docs.google.com/document/d/1GJA85jrlNd9VYbcHP00zn9G4e6jH-e-IBGBaT_ijLWk/edit
# The code here creates the general case table for all MCH domains based on DP tables alone. 
# We do not use any domain specific forms for the general case table creation

#setwd("C:/Users/Rashmi/dimagi-data-platform-R")
Sys.getenv("TZ")
Sys.setenv(TZ='UTC')
Sys.getenv("TZ")
#load(".Rdata")

# Get db connection
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))
db <- get_db_connection(system_conf)

library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

#Import form annotations file
form_annotations <- read.csv(file = "form_annotations.csv", stringsAsFactors = F)
form_annotations <- form_annotations[,1:34]
names(form_annotations)[names(form_annotations) == "Domain.name"] = "domain"
names(form_annotations)[names(form_annotations) == "Form.xmlns"] = "xmlns"
names(form_annotations)[names(form_annotations) == "Application.ID"] = "app_id"
#Correct spelling errors in annotation file
form_annotations$domain[form_annotations$domain == "Ceasefire"] <- "ceasefire"

#Get domain table
domain <- get_domain_table(db)
names(domain)[names(domain) == "id"] = "domain_pk"
names(domain)[names(domain) == "name"] = "domain"
domain$subsector <- lapply(domain$subsector, as.character)
domains_mch_subsector <- seq_along(domain$subsector)[sapply(domain$subsector, FUN=function(X) "Maternal, Newborn, & Child Health" %in% X)]
domain$mch_subsector <- F
domain$mch_subsector[domains_mch_subsector] <- T
domain$countries <- gsub('[{]', "", domain$countries)
domain$countries <- gsub('[}]', "", domain$countries)
domain$countries <- gsub('"', "", domain$countries)
domain$countries[domain$countries == "No country"] <- NA
domain$countries[domain$countries == ""] <- NA
domain$self_start <- NA
domain$self_start[domain$internal.self_started == "False"] <- F
domain$self_start[domain$internal.self_started == "True"] <- T
domain$domain_start_date <- as.Date(substr(domain$cp_first_form, 1, 10))
domain$domain_last_form_date <- as.Date(substr(domain$cp_last_form, 1, 10))
domain$domain_has_travel_annotations <- domain$domain %in% form_annotations$domain
remove
#Keep only MCH domains in domain table
domain <- filter(domain, mch_subsector == T & is_test == "false")

#Get form table
#Importing the form table takes almost 1 hour.
#We need the form table so that we map the case_event table to the visit table. 
#The case_event table doesn't have visit_pk info, it only has form_pk
form <- tbl(db, "form")
form <- collect(form)
names(form)[names(form) == "id"] = "form_pk"
names(form)[names(form) == "user_id"] = "user_pk"
names(form)[names(form) == "visit_id"] = "visit_pk"
names(form)[names(form) == "domain_id"] = "domain_pk"
names(form)[names(form) == "formdef_id"] = "formdef_pk"
names(form)[names(form) == "application_id"] = "app_pk"
#Keep only MCH domains and non-test domains
form <- filter(form, domain_pk %in% domain$domain_pk)
#Calculate days to server per form
form$days_to_server <- as.numeric(difftime(form$received_on, form$time_end, units="days"))

#Get case_event table (has ONE ROW PER CASE PER FORM)
#Takes ~10 minutes to collect
case_event <- tbl(db, "case_event")
case_event <- collect(case_event)
names(case_event)[names(case_event) == "case_id"] = "case_pk"
names(case_event)[names(case_event) == "form_id"] = "form_pk"
#Merge form table details to case_event
case_event <- merge(case_event, select(form, form_pk, visit_pk, user_pk, days_to_server, domain_pk), 
                    by = "form_pk", all.x = T)
#Keep only MCH domains and non-test domains
case_event <- filter(case_event, domain_pk %in% domain$domain_pk)
remove(form)
#Flag rows with visit_pk = NA
case_event$visit_pk_na <- is.na(case_event$visit_pk)
#Save NA rows in a separate table in case we need them later
#case_event_na <- filter(case_event, visit_pk_na == T)
#Delete visit_pk = NA rows from case_event table
case_event <- filter(case_event, visit_pk_na == F)

#Get visit_detail data source
#Takes ~50 minutes to load
#Visit detail has additional information such as nforms, visit duration, home_visit annotations that the main visit table
#doesn't have
visit <- get_visit_detail(db)
names(visit)[names(visit) == "id"] <- "visit_pk"
names(visit)[names(visit) == "num_forms"] <- "nforms"
#Don't use total_forms because it looks like it carries over nforms from other visits (e.g. demo_user has total_forms ~6K)
#Have confirmed that num_forms is fine to use for the overwhelming majority of visits that don't have duplicate visit_pk
visit$visit_duration <- round(as.numeric(difftime(visit$time_end, visit$time_start, units = "mins")), digits = 2)
#Merge in domain table and keep only MCH domains with is_test = F
visit <- merge(visit, select(domain, domain_pk, domain, is_test, mch_subsector, countries, self_start, domain_start_date, 
                             domain_last_form_date, domain_has_travel_annotations), by = "domain", all.x = T)
#Keep only MCH domains and non-test domains
visit <- filter(visit, mch_subsector == T)
visit <- filter(visit, is_test == "false")

#Get user table and user_type table
users <- tbl(db, "users") 
users <- collect(users)
names(users)[names(users) == "id"] = "user_pk"
users <- select(users, user_pk, username, email)
user_type <- get_user_type_table(db)
user_type <- collect(user_type)
user_type <- select(user_type, user_pk, user_type)
users <- merge(users, user_type, by = "user_pk", all.x = T)
#Merge to visit table 
#Don't exclude based on user_type here because we will account for those visits in the case table 
visit <- merge(visit, users, by = "user_pk", all.x = T)
remove(users)
remove(user_type)

#visit_detail has duplicate visit_pks (~703K rows have duplicated visit_pk)
#89.2% of these dup visit_pks are from demo users
#Need to flag test visits: 
#(1)flag visit_pks created by a demo user and (2) flag visits with dup_visit_pk, regardless of user_id
#Any cases with these test visits will be flagged as test cases later on
visit$dup_visit_pk_f <- duplicated(visit$visit_pk)
visit$dup_visit_pk_r <- duplicated(visit$visit_pk, fromLast = T)
visit$dup_visit_pk <- visit$dup_visit_pk_f == T | visit$dup_visit_pk_r == T
visit <- select(visit, -c(dup_visit_pk_f, dup_visit_pk_r))
visit$demo_visit_pk <- visit$visit_pk %in% filter(visit, user_id == "demo_user")$visit_pk
visit$test_visit <- visit$dup_visit_pk == T | visit$demo_visit_pk == T  
visit$row_num <- 1:nrow(visit)

#Get case tables
#Takes ~15 minutes to collect
#Keep only MCH domains and non-test domains (based on visit table)
cases <- tbl(db, "cases")
cases <- collect(cases)
names(cases)[names(cases) == "id"] = "case_pk"
names(cases)[names(cases) == "domain_id"] = "domain_pk"
names(cases)[names(cases) == "user_id"] = "user_pk"
names(cases)[names(cases) == "owner_id"] = "owner_pk"
cases <- filter(cases, domain_pk %in% visit$domain_pk)

#Create single case_event table (want ONE ROW PER CASE PER VISIT)
#Have to group by case_pk and visit_pk and user_pk
#This takes ~15 minutes to run
#We will get many more dup_visit_pks, even for non-demo users because case events
#are on the case level and one visit (vist_pk) can have multiple cases.
#So we need to merge the visit table to the single_ce table, keeping all single_ce rows
single_ce <- case_event %>% group_by(case_pk, user_pk, visit_pk) %>% 
  summarise(registration = sum(created) > 0, 
            follow_up = sum(updated) > 0 | sum(closed) > 0, 
            closed = sum(closed) > 0,
            med_days_to_server = median(days_to_server, na.rm = T))
single_ce$row_num <- 1:nrow(single_ce)
#Need to flag test visits that have been been created by a demo user
single_ce$test_visit <- single_ce$user_pk == "66080"
remove(case_event) 

#Count nvisits_raw per case_pk from single_ce table
nvisits_per_case <- single_ce %>% group_by(case_pk) %>% 
  summarise(nvisits_raw_ce = length(unique(row_num)),
            nvisits_test_ce = sum(test_visit == T),
            nusers_ce = length(unique(user_pk)))
#Merge to case table
cases <- merge(cases, nvisits_per_case, by = "case_pk", all.x = T)
remove(nvisits_per_case)

#Flag test_case in case table before filtering test visits from the single_ce table and visit table
#for merging. Note that these cases will NOT have accurate visit indicators in the case table, so we shouldn't 
#depend on these cases too much
test_visit_pks <- unique(filter(visit, test_visit == T)$visit_pk)
single_ce$test_visit2 <- single_ce$visit_pk %in% test_visit_pks
remove(test_visit_pks)
single_ce$test_case <- single_ce$test_visit == T | single_ce$test_visit2 == T
cases$test_case <- cases$case_pk %in% filter(single_ce, test_case == T)$case_pk

#Note that these test_cases will NOT have accurate visit indicators in the case table 
#because I am removing visit rows with test_visit == T from the visit table and single_ce table
single_ce <- filter(single_ce, test_case == F)
visit <- filter(visit, test_visit == F)

#Merge visit table to single_ce table
single_ce <- select(single_ce, -c(row_num, test_visit, test_visit2, test_case))
visit <- select(visit, -c(dup_visit_pk, demo_visit_pk, test_visit, row_num))
visit <- merge(single_ce, visit, by = "visit_pk", all.x = T)
visit <- select(visit, -user_pk.x)
names(visit)[names(visit) == "user_pk.y"] = "user_pk"
remove(single_ce)

#Add domain info from domain table to case table
cases <- merge(cases, select(domain, domain_pk, domain, domain_start_date, domain_last_form_date, countries, 
                             self_start, domain_has_travel_annotations, is_test, mch_subsector), by = "domain_pk", all.x = T)

#Add dataset end date: DP goes through 6/30/2015
cases$dataset_end_date <- as.Date("2015-06-30")

#Import monthly table
monthly_table <- tbl(db, "aggregate_monthly_interactions")
monthly_table <- collect(monthly_table)
#Merge in domain_pk
#Keep only MCH domains in monthly table
monthly_table <- merge(monthly_table, select(domain, domain, domain_pk), by = "domain", all.x = T)
monthly_table <- filter(monthly_table, domain_pk %in% visit$domain_pk)
#Change column names
names(monthly_table)[names(monthly_table) == "month.index"] = "calendar_month"
names(monthly_table)[names(monthly_table) == "numeric_index"] = "month_index"
#Convert calendar month to actual date
monthly_table$calendar_month <- parse_date_time(paste('01', monthly_table$calendar_month), '%d %b %Y!')
monthly_table$calendar_month <- as.Date(monthly_table$calendar_month)
#Add rolling active_months column to each user
monthly_table <- arrange(monthly_table, user_pk, calendar_month)
monthly_table$num_row <- 1:nrow(monthly_table)
monthly_table <- monthly_table %>% group_by(user_pk) %>% 
  mutate(active_months_rolling = 1:length(unique(num_row)))

#Clean visit_duration indicator
#Set the following improbable visit duration values to NA:
#~0.03% of case visits have negative duration
#~2.0% of case visits have duration > 60 minutes
visit$visit_dur_neg <- visit$visit_duration < 0
visit$visit_dur_gt_60 <- visit$visit_duration > 60
visit$visit_duration[visit$visit_dur_neg == T | visit$visit_dur_gt_60 == T] <- NA

#General visit table stats
visit$num_row <- 1:nrow(visit)
#visit$visit_month <- floor_date(visit$visit_date, unit = "month")
#Case level
case_level <- visit %>% group_by(case_pk) %>% 
  summarise(nusers_total = length(unique(user_pk)), 
            nusers_mobile = length(unique(user_pk[user_type == "mobile"])), 
            registered_mobile = sum(registration[user_type == "mobile"]) > 0, 
            closed_mobile = sum(closed[user_type == "mobile"]) > 0, 
            date_last_visit = max(visit_date))
#Merge to case table
cases <- merge(cases, case_level, by = "case_pk", all.x = T)
#Set closed_mobile to NA for cases with closed == F
cases$closed_mobile[cases$closed == F] <- NA

#User level
user_level <- monthly_table %>% group_by(user_pk) %>% 
  summarise(lifetime_visits_user = sum(nvisits, na.rm = T))
#Merge to case table
cases <- merge(cases, user_level, by = "user_pk", all.x = T)

#There's something weird with date_opened for about 10% of the cases - it doesn't look correct
#Create a new date_opened_final and date_closed_final based on the visit table
#Since some visits have been excluded, we will take the original date_opened and date_closed values
#for cases that have calculated registration and closed dates = NA
case_open_close <- visit %>% group_by(case_pk) %>% 
  summarise(date_opened2 = min(visit_date), 
            date_closed2 = max(visit_date))
cases <- merge(cases, case_open_close, by = "case_pk", all.x = T)
cases$date_opened <- as.Date(substr(cases$date_opened, 1, 10))
cases$date_closed <- as.Date(substr(cases$date_closed, 1, 10))

cases$date_opened_final <- cases$date_opened2
cases$date_opened_final[is.na(cases$date_opened2)] <- cases$date_opened[is.na(cases$date_opened2)] 

cases$date_closed_final <- cases$date_closed2
cases$date_closed_final[is.na(cases$date_closed2)] <- cases$date_closed[is.na(cases$date_closed2)] 
cases$date_closed_final[cases$closed == F] <- NA

#User-month level
#Merge user-month stats from monthly_table to case table
#Case table has 1,062,776 rows
#NOTE: mvp domains and tulasalud have a lot of cases that don't map to the these monthly rows
#Will look into these later
user_month_level <- select(monthly_table, user_pk, domain_pk, calendar_month, month_index, active_months_rolling)
#cases$reg_month <- floor_date(cases$date_opened, unit = "month")
#cases$reg_month <- as.Date(substr(cases$reg_month, 1, 10))
cases$date_opened_final[is.na(cases$date_opened_final)] <- as.Date("1111-11-11")
cases$reg_month <- floor_date(cases$date_opened_final, unit = "month")
cases$date_opened_final[cases$date_opened_final == as.Date("1111-11-11")] <- NA
cases$reg_month[cases$reg_month == as.Date("1111-11-01")] <- NA
cases <- merge(cases, user_month_level, by.x = c("user_pk", "domain_pk", "reg_month"), 
               by.y = c("user_pk", "domain_pk", "calendar_month"), all.x = T)

#Mobile visit table stats
visit <- filter(visit, user_type == "mobile")
visit$follow_up_only <- (visit$follow_up == T | visit$closed == T) & visit$registration == F
case_level <- visit %>% group_by(case_pk) %>% 
  summarise(nvisits_mobile = length(unique(num_row)), 
            nvisits_mobile_travel = length(unique(num_row[home_visit == T])), 
            nvisits_mobile_fu = length(unique(num_row[follow_up_only == T])), 
            nvisits_daytime = length(unique(num_row[visit_time == "morning" | visit_time == "afternoon"])), 
            has_improbable_visit_duration = sum(visit_dur_neg) > 0 | sum(visit_dur_gt_60) > 0, 
            time_on_cc = sum(visit_duration, na.rm = T), 
            med_visit_dur = median(visit_duration, na.rm = T))
#Merge to case table
cases <- merge(cases, case_level, by = "case_pk", all.x = T)


#------------------------------------------------------------------------#
#Analysis of visit/case data as needed
#------------------------------------------------------------------------#

#Create table of nvisits, nusers by domain
domain_size <- visit %>% group_by(domain) %>% 
  summarise(nusers = length(unique(user_pk)), 
            nvisits = length(unique(visit_id)))
domain_size <- arrange(domain_size, desc(nvisits))

#visit_duration histogram: need to set a max visit duration at 30 mins or 60 mins, depending on 
#the distribution for duration that we see here
#Include all observations
myhist <- ggplot(visit, aes(x=visit_duration)) + 
  geom_histogram(binwidth=5, colour="black", fill="lightblue") + 
  geom_vline(aes(xintercept=median(visit_duration, na.rm=T)),
             color="red", linetype="dashed", size=1) + 
  scale_x_continuous(limits=c(0,60)) + 
  scale_y_continuous(labels = comma) + 
  xlab("Visit duration (minutes)") +
  ylab("Total # of visits")







