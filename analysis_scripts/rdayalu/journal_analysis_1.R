# 12/26/14
# This analysis is for the ITI journal article titled "Measuring mobile technology 
# usage to inform public health programmatic design: a metadata analysis"
# The manuscript for this article is here:
# https://docs.google.com/a/dimagi.com/document/d/1AuFF40FMGfe49wWYVhvLQ0DEvGuSayAHGk4o8b67N4I/edit

#------------------------------------------------------------------------#
#SELECT CHWs TO KEEP FOR ANALYSIS
#------------------------------------------------------------------------#

#Exclude users who have submitted forms using > 1 appplication_id
#First import full form_table from db
library(dplyr)
# Load system config file
source(file.path("function_libraries","config_file_funcs.R", fsep = .Platform$file.sep))
source(file.path("data_sources.R"))
system_conf <- get_system_config(file.path("config_system.json"))
# Get db connection
db <- get_db_connection(system_conf)
form_table <- tbl(db, "form")
form_table <- collect(form_table)
names(form_table)[names(form_table) == "user_id"] = "user_pk"
chw_single_app <- form_table %>% group_by(user_pk) %>%
  summarise(n_applications = length(unique(application_id)))
chw_single_app <- filter(chw_single_app, n_applications == 1)

#List of users by user_type, keeping only mobile users
#Get user_type table from db (mobile, web, superuser, etc.)
user_type <- get_user_type_table(db)
user_type <- filter(user_type, user_type == "mobile")
user_type <- select(user_type, user_pk, user_id, user_type)

#Merge these two lists together, keeping only mobile users
chw_single_app <- merge(chw_single_app, user_type, by = "user_pk", all.x = T)
chw_single_app <- filter(chw_single_app, user_type == "mobile")

#Exclude users who submitted through one of the three atypical apps in this google doc:
#https://docs.google.com/a/dimagi.com/spreadsheets/d/1QwkgRZPR81rQF9h-E7END_ontZye8xQW_WojAObKsgU/edit#gid=0
app <- tbl(db, "application")
app <- collect(app)
app <- filter(app, app_id == "4c4c0f5a7bd4834994e93cc7dde8b91a" | 
                app_id == "237e500aed04239b98f0aecb904227ac" | 
                app_id == "223c5ff68da7d1805fbf8224976f2587")
atypical <- form_table[form_table$application_id %in% app$id,]
chw_single_app <- chw_single_app[!(chw_single_app$user_pk %in% atypical$user_pk),] 
write.csv(chw_single_app, file = "chw_single_app.csv")
#------------------------------------------------------------------------#
#PREPARE MONTHLY TABLE
#------------------------------------------------------------------------#

#Pull monthly_table for all domains
#Be sure to set config_run first
source(file.path("analysis_scripts","raw_data","data_import.R", fsep = .Platform$file.sep))
all_monthly <- monthly_table

#Get required libraries
library(data.table)
library(zoo)
detach("package:lubridate")
library(lubridate)
library(ggplot2)

#Keep only the "typical FLW" domains. There are 39 of these domains
#I set the config_run to "permitted_data_only" : false otherwise tulasalud 
#will be excluded, which isn't correct.
#However, keiskamma and ssqh-cs have opted out per EULA, so we shouldn't include those?
#TALK TO NEAL ABOUT THIS. These domains have 352 users (out of 2779 users) 
#that contribute 1162 monthly rows. ssqh has not actually 
#opted out per Sheel's knowledge
typical_FLW_domains <- c("aaharsneha", "aarohi", "acf", "aed-hth", "arogyasarita",
                         "care-ecd", "chasssmt-moz", "crc-intervention", "crhp", 
                         "crs-catch", "crs-remind", "crs-senegal", "dtree-familyplanning", 
                         "engender-ethiopia-pilot", "icap-tb", "kawok-malaria-p", 
                         "keiskamma", "kgvk", "maternalznz", "nutritionmeast", "opm", 
                         "pasmo-nicaragua-dmg", "pci-india", "project", "puami-tsf-mnch-myanmar", 
                         "rdi-hiht", "savethechildren", "savethechildren-nepal", 
                         "slttc", "ssqh-cs", "teba-hbc", "tulasalud", "world-renew", 
                         "wvindia", "wvindia-nutrition", "wvindia2", "wvindonesia", 
                         "wvug", "yonsei-emco")
all_monthly <- all_monthly[all_monthly$domain %in% typical_FLW_domains,]

#Keep only the users in the chw_single_app list
all_monthly <- all_monthly[all_monthly$user_pk %in% chw_single_app$user_pk,]

#Get report_options from config run file
report <- run_conf$reports$modules$name
report_options <- get_report_options(run_conf,report)

#Keep rows only from 1/1/10 - 11/30/14 (based on config run file)
#This leaves us with 2746 FLWs
all_monthly$date_first_visit = as.Date(all_monthly$date_first_visit)
all_monthly$date_last_visit = as.Date(all_monthly$date_last_visit)
start_date = as.Date(report_options$start_date)
end_date = as.Date(report_options$end_date)
all_monthly = subset(all_monthly, all_monthly$date_first_visit >= start_date
                     & all_monthly$date_last_visit <= end_date)

#Remove demo users and NA/NONE users
#This does not exclude any FLWs, so we still have 2746 FLWs
all_monthly = all_monthly[!(all_monthly$user_id =="demo_user"),]
all_monthly = all_monthly[!(all_monthly$user_id =="NONE"),]
all_monthly = all_monthly[!(all_monthly$user_id =="none"),]
all_monthly = all_monthly[!is.na(all_monthly$user_id),]

#Change column names as needed
names(all_monthly)[names(all_monthly) == "month.index"] = "calendar_month"
names(all_monthly)[names(all_monthly) == "numeric_index"] = "month_index"

#Prepare domain_table for merging in domain facets
#Bring in sector information
sector <- tbl(db, "sector")
sector <- collect(sector)
names(sector)[names(sector) == "name"] = "sector_final"
domain_sector <- tbl(db, "domain_sector")
domain_sector <- collect(domain_sector)
domain_sector <- select(domain_sector, domain_id, sector_id)
domain_table <- merge(domain_table, domain_sector, by.x = "id", by.y = "domain_id", all.x = T)
domain_table <- merge(domain_table, sector, by.x = "sector_id", by.y = "id", all.x = T)
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
domain_table <- merge(domain_table, domain_subsector, by.x = "id", by.y = "domain_id", all.x = T)
domain_table <- merge(domain_table, subsector, by.x = "subsector_id", by.y = "id", all.x = T)
#Consolidate country information
is.na(domain_table$deployment.country) <- domain_table$deployment.country == ""
is.na(domain_table$country) <- domain_table$country == ""
domain_table$country_final <- domain_table$deployment.country
keep_country <- which(is.na(domain_table$deployment.country) & !is.na(domain_table$country))
domain_table$country_final[keep_country] <- domain_table$country[keep_country]
#Consolidate Dimagi level of support
is.na(domain_table$internal.services) <- domain_table$internal.services == ""
is.na(domain_table$internal.self_started) <- domain_table$internal.self_started == ""
domain_table$self_start[domain_table$internal.self_started == "True"] <- "self"
domain_table$dimagi_services <- domain_table$internal.services
keep_self <- which(is.na(domain_table$internal.services) & !is.na(domain_table$self_start))
domain_table$dimagi_services[keep_self] <- domain_table$self_start[keep_self]

#Keep only columns of interest
names(domain_table)[names(domain_table) == "id"] = "domain_id"
facets_to_merge <- select(domain_table, name, domain_id, country_final, sector_final, 
                          subsector_final, dimagi_services, test)

#Merge domain facets from domain table into all_monthly table
all_monthly <- merge(all_monthly, facets_to_merge, by.x = "domain", 
                     by.y = "name", all.x = T)
names(all_monthly)[names(all_monthly) == "domain_id"] = "domain_numeric"

#Convert calendar month to actual date
all_monthly$calendar_month <- parse_date_time(paste('01', all_monthly$calendar_month), '%d %b %Y!')
all_monthly$calendar_month <- as.Date(all_monthly$calendar_month)
all_monthly$month_abbr <- month(all_monthly$calendar_month, label = T, abbr = T)

#Exclude any users who logged > 100 visits in any month
#These are probably atypical users
#We lose one domain because of this step (crc-intervention)
#We are left with 2461 FLWs that have only <= 100 visits per month
all_monthly$visits_ge_100 <- all_monthly$nvisits > 100
user_ge_100 <- all_monthly %.%
  group_by(user_pk) %.%
  summarise(ge_100 = sum(visits_ge_100))
user_le_100 <- filter(user_ge_100, ge_100 == 0)
all_monthly <- all_monthly[all_monthly$user_pk %in% user_le_100$user_pk, ]

#Get lifetime table for total nunique_followups, active_months per user
#lifetime_table <- get_aggregate_table(db, "aggregate_lifetime_interactions", domains_for_run)
#lifetime_table <- lifetime_table[lifetime_table$user_pk %in% all_monthly$user_pk,]
#Merge nunique_followups, active_months to all_monthly
#lifetime_table <- select(lifetime_table, user_pk, nunique_followups, active_months, calendar_month_on_cc)
#names(lifetime_table)[names(lifetime_table) == "nunique_followups"] = "lifetime_followup"
#names(lifetime_table)[names(lifetime_table) == "calendar_month_on_cc"] = "months_on_cc"
#all_monthly <- merge(all_monthly, lifetime_table, by = "user_pk", all.x = T)

#Lifetime aggregate table is not available on the db as of 12/28/14.
#I will calculate months_on_cc, active_months here until the lifetime table is available.
total_months_cc <- all_monthly %>% group_by(user_pk) %>% 
  summarise(first_month = min(calendar_month),
            last_month = max(calendar_month), 
            active_months = length(unique(calendar_month)))
total_months_cc$months_on_cc <- (interval(total_months_cc$first_month, 
                                         total_months_cc$last_month) %/% months(1))+1
total_months_cc <- select(total_months_cc, user_pk, active_months, months_on_cc)
all_monthly <- merge(all_monthly, total_months_cc, by = "user_pk", all.x = T)

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
all_monthly <- arrange(all_monthly, domain_numeric, user_pk, calendar_month)
df <- data.table(all_monthly)
setkey(df,user_pk)
df[,diff_days:=c(NA,diff(calendar_month)),by=user_pk]
all_monthly <- as.data.frame(df)
all_monthly$previous_month_active <- all_monthly$diff_days <= 31
all_monthly$previous_two_months_active <- all_monthly$diff_days <= 62
all_monthly$previous_three_months_active <- all_monthly$diff_days <= 93

users <- unique(all_monthly$user_pk)

next_month_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_pk == i,]
  next_active <- c()
  next_active <- append(single_user$previous_month_active[-1], F)
  next_month_active <- append(next_month_active, next_active)
}
all_monthly$next_month_active <- next_month_active

next_two_months_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_pk == i,]
  next_active <- c()
  next_active <- append(single_user$previous_two_months_active[-1], F)
  next_two_months_active <- append(next_two_months_active, next_active)
}
all_monthly$next_two_months_active <- next_two_months_active

next_three_months_active <- c()
for (i in users) {
  single_user <- all_monthly[all_monthly$user_pk == i,]
  next_active <- c()
  next_active <- append(single_user$previous_three_months_active[-1], F)
  next_three_months_active <- append(next_three_months_active, next_active)
}
all_monthly$next_three_months_active <- next_three_months_active

#If calendar_month = 11/1/14 then next_month_active = NA
#because we don't know if the user will be active in the following month
is.na(all_monthly$next_month_active) <- all_monthly$calendar_month == "2014-11-01"
is.na(all_monthly$next_two_months_active) <- all_monthly$calendar_month >= "2014-10-01"
is.na(all_monthly$next_three_months_active) <- all_monthly$calendar_month >= "2014-09-01"

#Was the user ever active again after an attrition event (defined as next_month_active == F)?
all_monthly$attrition_event <- !(all_monthly$next_month_active == T | is.na(all_monthly$next_month_active))
all_monthly$continuing <- all_monthly$month_index < all_monthly$months_on_cc
all_monthly$ever_active_again <- all_monthly$attrition_event == T & all_monthly$continuing == T
is.na(all_monthly$ever_active_again) <- all_monthly$attrition_event == F

training_typical <- all_monthly

#Exclude any users that don't have a month_index = 1
#These users have months that started outside our data range for this dataset
#so we shouldn't include them. There are 3 of these users.
training_typical$has_index_1 <- training_typical$month_index == 1
user_index_1 <- training_typical %.%
  group_by(user_pk) %.%
  summarise(keep_user = sum(has_index_1))
user_index_1 <- filter(user_index_1, keep_user != 0)
#Keep users that have a month_index = 1. We now have 2458 users
training_typical <- 
  training_typical[training_typical$user_pk %in% user_index_1$user_pk, ]

#In the blog, I only kept users who have been active for 
#at least 6 mos and I only counted their rows from month 6 onwards 
#See this document for further details:
#https://docs.google.com/a/dimagi.com/spreadsheets/d/1weMI03KGQPffWHM3y2AR1VbSVCZIbJEEXiFd_0jRL7A/edit#gid=0
#Excluded rows before the 6th month on CC AND users with < 6 active months on CC
#We now have 962 users
training_typical <- filter(training_typical, month_index >= 6)
training_typical <- filter(training_typical, active_months >= 6)

#Check number of users/domain for sampling purposes
n_chw <- training_typical %>% group_by(domain) %>% 
  summarise(nusers = length(unique(user_pk)))
n_chw <- arrange(n_chw, desc(nusers))
n_chw$total_users <- sum(n_chw$nusers)
n_chw$per_users <- (n_chw$nusers/n_chw$total_users)*100

#Exclude a sample of 54 users from crs-remind and 3 users from maternalznz 
#so that each domain contributes <= 20% of the total users
#We are left with 905 users
exclude_users <- c(sample(unique(training_typical$user_pk[training_typical$domain == "crs-remind"]), 54),
                            sample(unique(training_typical$user_pk[training_typical$domain == "maternalznz"]), 3))
training_typical <- training_typical[!(training_typical$user_pk %in% exclude_users),]

#Add sample_increase variable and sample_decrease variables
training_typical <- arrange(training_typical, user_pk, calendar_month)
users <- unique(training_typical$user_pk)
sample_increase <- c()
sample_decrease <- c()
for (i in users) {
  single_user <- training_typical[training_typical$user_pk == i,]
  sample_increase <- append(sample_increase, cumsum(sample(1:5, nrow(single_user), replace=T)))
  sample_decrease <- append(sample_decrease, rev(cumsum(sample(1:5, nrow(single_user), replace=T))))
}
training_typical$sample_increase <- sample_increase
training_typical$sample_decrease <- sample_decrease

#Calculate differences between month_index to calculate next_month_active and 
#previous_month_active variables
#Also want differences between indicators for each user from one month to the next
#Differences in indicators will be used for tests 1a/b
training_typical <- arrange(training_typical, user_pk, calendar_month)
df <- data.table(training_typical)
setkey(df,user_pk)
df[,diff_nvisits:=c(NA,diff(nvisits)),by=user_pk]
df[,diff_active_day_percent:=c(NA,diff(active_day_percent)),by=user_pk]
df[,diff_nforms:=c(NA,diff(nforms)),by=user_pk]
df[,diff_median_visit_duration:=c(NA,diff(median_visit_duration)),by=user_pk]
df[,diff_median_visits_per_day:=c(NA,diff(median_visits_per_day)),by=user_pk]
df[,diff_time_using_cc:=c(NA,diff(time_using_cc)),by=user_pk]
df[,diff_ninteractions:=c(NA,diff(ninteractions)),by=user_pk]
df[,diff_ncases_registered:=c(NA,diff(ncases_registered)),by=user_pk]
df[,diff_register_followup:=c(NA,diff(register_followup)),by=user_pk]
df[,diff_case_register_followup_rate:=c(NA,diff(case_register_followup_rate)),by=user_pk]
df[,diff_ncases_touched:=c(NA,diff(ncases_touched)),by=user_pk]
df[,diff_nunique_followups:=c(NA,diff(nunique_followups)),by=user_pk]
df[,diff_sample_increase:=c(NA,diff(sample_increase)),by=user_pk]
df[,diff_sample_decrease:=c(NA,diff(sample_decrease)),by=user_pk]
training_typical <- as.data.frame(df)

indicators <- c("nvisits", "active_day_percent", "nforms", 
                "median_visit_duration", "median_visits_per_day", 
                "time_using_cc", "ninteractions", "ncases_registered", 
                "register_followup", "case_register_followup_rate", 
                "ncases_touched", "nunique_followups", "sample_increase", 
                "sample_decrease")

#------------------------------------------------------------------------#
#Code for Test 1
#------------------------------------------------------------------------#
# % difference in indicators for each user for consectutive months
# This isn't for truly consecutive months, so later on, 
# we will only use rows with previous_month_active == T
#This will be used for test 1b
source(file.path("analysis_scripts","rdayalu","test_1b_journal.R", fsep = .Platform$file.sep))

#Must only include rows with previous_month_active == T. Exclude F & NA 
training_consec <- filter(training_typical, previous_month_active == T)
training_consec$concat <- paste(training_consec$user_pk, training_consec$calendar_month, 
                                sep = "_") 


#Exclude domain calendar_months with nusers < 5 for that domain
#Use this dataset only for test 1a/1b
nusers <- training_consec %>% 
  group_by(domain, calendar_month) %>% 
  summarise(nusers = length(unique(user_pk)))
nusers <- filter(nusers, nusers >= 5)
nusers$concat <- paste(nusers$domain, nusers$calendar_month, sep = "_")
training_consec <- 
  training_consec[paste(training_consec$domain, training_consec$calendar_month, sep = "_") %in% 
                    nusers$concat, ]

#Domain median ABSOLUTE change per user per calendar month, 
#excluding each user from the domain median for that user's row
#This is used for test 1a
source(file.path("analysis_scripts","rdayalu","test_1a_journal.R", fsep = .Platform$file.sep))

#Domain median PERCENTAGE change per user per calendar month, 
#excluding each user from the domain median for that user's row
source(file.path("analysis_scripts","rdayalu","test_1b_2_journal.R", fsep = .Platform$file.sep))

names(training_consec)

test_1a <- 
  c(cor(training_consec$med_nvisits_1a, training_consec$diff_nvisits, use = "complete.obs"),
    cor(training_consec$med_active_day_percent_1a, training_consec$diff_active_day_percent, use = "complete.obs"),
    cor(training_consec$med_nforms_1a, training_consec$diff_nforms, use = "complete.obs"),
    cor(training_consec$med_median_visit_duration_1a, training_consec$diff_median_visit_duration, use = "complete.obs"),
    cor(training_consec$med_median_visits_per_day_1a, training_consec$diff_median_visits_per_day, use = "complete.obs"),
    cor(training_consec$med_time_using_cc_1a, training_consec$diff_time_using_cc, use = "complete.obs"),
    cor(training_consec$med_ninteractions_1a, training_consec$diff_ninteractions, use = "complete.obs"),
    cor(training_consec$med_ncases_registered_1a, training_consec$diff_ncases_registered, use = "complete.obs"),
    cor(training_consec$med_register_followup_1a, training_consec$diff_register_followup, use = "complete.obs"),
    cor(training_consec$med_case_register_followup_rate_1a, training_consec$diff_case_register_followup_rate, use = "complete.obs"),
    cor(training_consec$med_ncases_touched_1a, training_consec$diff_ncases_touched, use = "complete.obs"),
    cor(training_consec$med_nunique_followups_1a, training_consec$diff_nunique_followups, use = "complete.obs"),
    cor(training_consec$med_sample_increase_1a, training_consec$diff_sample_increase, use = "complete.obs"),
    cor(training_consec$med_sample_decrease_1a, training_consec$diff_sample_decrease, use = "complete.obs"))
names(test_1a) <- indicators

test_1b <- 
  c(cor(training_consec$med_nvisits_1b, training_consec$per_diff_nvisits, use = "complete.obs"),
    cor(training_consec$med_active_day_percent_1b, training_consec$per_diff_active_day_percent, use = "complete.obs"),
    cor(training_consec$med_nforms_1b, training_consec$per_diff_nforms, use = "complete.obs"),
    cor(training_consec$med_median_visit_duration_1b, training_consec$per_diff_median_visit_duration, use = "complete.obs"),
    cor(training_consec$med_median_visits_per_day_1b, training_consec$per_diff_median_visits_per_day, use = "complete.obs"),
    cor(training_consec$med_time_using_cc_1b, training_consec$per_diff_time_using_cc, use = "complete.obs"),
    cor(training_consec$med_ninteractions_1b, training_consec$per_diff_ninteractions, use = "complete.obs"),
    cor(training_consec$med_ncases_registered_1b, training_consec$per_diff_ncases_registered, use = "complete.obs"),
    cor(training_consec$med_register_followup_1b, training_consec$per_diff_register_followup, use = "complete.obs"),
    cor(training_consec$med_case_register_followup_rate_1b, training_consec$per_diff_case_register_followup_rate, use = "complete.obs"),
    cor(training_consec$med_ncases_touched_1b, training_consec$per_diff_ncases_touched, use = "complete.obs"),
    cor(training_consec$med_nunique_followups_1b, training_consec$per_diff_nunique_followups, use = "complete.obs"),
    cor(training_consec$med_sample_increase_1b, training_consec$per_diff_sample_increase, use = "complete.obs"),
    cor(training_consec$med_sample_decrease_1b, training_consec$per_diff_sample_decrease, use = "complete.obs"))
names(test_1b) <- indicators

#------------------------------------------------------------------------#
#Code for Test 2
#------------------------------------------------------------------------#

#Previous month's indicator value
training_typical$prev_nvisits <- training_typical$nvisits - training_typical$diff_nvisits
training_typical$prev_active_day_percent <- training_typical$active_day_percent - training_typical$diff_active_day_percent
training_typical$prev_nforms<- training_typical$nforms - training_typical$diff_nforms
training_typical$prev_median_visit_duration <- training_typical$median_visit_duration - training_typical$diff_median_visit_duration
training_typical$prev_median_visits_per_day <- training_typical$median_visits_per_day - training_typical$diff_median_visits_per_day
training_typical$prev_time_using_cc <- training_typical$time_using_cc - training_typical$diff_time_using_cc
training_typical$prev_ninteractions <- training_typical$ninteractions - training_typical$diff_ninteractions
training_typical$prev_ncases_registered <- training_typical$ncases_registered - training_typical$diff_ncases_registered
training_typical$prev_register_followup <- training_typical$register_followup - training_typical$diff_register_followup
training_typical$prev_case_register_followup_rate <- training_typical$case_register_followup_rate - training_typical$diff_case_register_followup_rate
training_typical$prev_ncases_touched <- training_typical$ncases_touched - training_typical$diff_ncases_touched
training_typical$prev_nunique_followups <- training_typical$nunique_followups - training_typical$diff_nunique_followups
training_typical$prev_sample_increase <- training_typical$sample_increase - training_typical$diff_sample_increase
training_typical$prev_sample_decrease <- training_typical$sample_decrease - training_typical$diff_sample_decrease

test2_data <- training_typical[training_typical$previous_month_active == T,]

test_2a <- 
  c(cor(training_typical$prev_nvisits, training_typical$nvisits, use = "complete.obs"),
    cor(training_typical$prev_active_day_percent, training_typical$active_day_percent, use = "complete.obs"),
    cor(training_typical$prev_nforms, training_typical$nforms, use = "complete.obs"),
    cor(training_typical$prev_median_visit_duration, training_typical$median_visit_duration, use = "complete.obs"),
    cor(training_typical$prev_median_visits_per_day, training_typical$median_visits_per_day, use = "complete.obs"),
    cor(training_typical$prev_time_using_cc, training_typical$time_using_cc, use = "complete.obs"),
    cor(training_typical$prev_ninteractions, training_typical$ninteractions, use = "complete.obs"),
    cor(training_typical$prev_ncases_registered, training_typical$ncases_registered, use = "complete.obs"),
    cor(training_typical$prev_register_followup, training_typical$register_followup, use = "complete.obs"),
    cor(training_typical$prev_case_register_followup_rate, training_typical$case_register_followup_rate, use = "complete.obs"),
    cor(training_typical$prev_ncases_touched, training_typical$ncases_touched, use = "complete.obs"),
    cor(training_typical$prev_nunique_followups, training_typical$nunique_followups, use = "complete.obs"), 
    cor(training_typical$prev_sample_increase, training_typical$sample_increase, use = "complete.obs"), 
    cor(training_typical$prev_sample_decrease, training_typical$sample_decrease, use = "complete.obs"))
names(test_2a) <- indicators

g <- ggplot(training_typical, aes(x=prev_nunique_followups, y=nunique_followups )) +
  geom_point(shape=1) +
  scale_x_continuous(limits=c(0,100)) +
  scale_y_continuous(limits=c(0,100)) +
  geom_smooth(method=lm)

test <- data.frame(cbind(test_1a, test_1b, test_2a))
write.csv(test, file = "journal_set_results.csv")
